open Grammar

  let id x = x
  let some x = Some x

let update_ref r f : bool =
  let v = !r in
  let v' = f v in
  v != v' && (r := v'; true)

module MyMap (X : Map.OrderedType) = struct
  include Map.Make(X)
  let update none some key m f =
    match find key m with
    | data ->
        let data' = f (some data) in
        if data' == data then
          m
        else
          add key data' m
    | exception Not_found ->
        let data' = f none in
        add key data' m
end

module W : sig

  type word
  val epsilon: word
  val singleton: Terminal.t -> word
  val append: word -> word -> word
  val length: word -> int
  val first: word -> Terminal.t (* word must be nonempty *)
  val print: word -> string

end = struct

  type word = {
    data: Terminal.t Seq.seq;
    length: int;
  }

  let epsilon = {
    data = Seq.empty;
    length = 0;
  }

  (* TEMPORARY tabulate? *)
  let singleton t = {
    data = Seq.singleton t;
    length = 1;
  }

  let append w1 w2 =
    if w1.length = 0 then
      w2
    else if w2.length = 0 then
      w1
    else {
      data = Seq.append w1.data w2.data;
      length = w1.length + w2.length;
    }

  let length w =
    w.length

  let first w =
    Seq.first w.data

  let print w =
    string_of_int w.length ^ " " ^
    String.concat " " (List.map Terminal.print (Seq.elements w.data))

end

module Q = LowIntegerPriorityQueue

type fact = {
  source: Lr1.node;
  height: int;
  target: Lr1.node;
  word: W.word;
  lookahead: Terminal.t
}

let print_fact fact =
  Printf.fprintf stderr
    "from state %d in %d steps to state %d via %s . %s\n%!"
    (Lr1.number fact.source)
    fact.height
    (Lr1.number fact.target)
    (W.print fact.word)
    (Terminal.print fact.lookahead)

(* TEMPORARY not really satisfactory; conservative bound *)
let max_height (s : Lr1.node) =
  let items = Lr0.items (Lr0.core (Lr1.state s)) in
  Item.Set.fold (fun item accu ->
    let prod, i = Item.export item in
    let height = Production.length prod - i in
    max height accu
  ) items 0

let extensible fact =
  fact.height < max_height fact.source

let foreach_terminal f =
  Terminal.iter (fun t ->
    if not (Terminal.equal t Terminal.error) then
      f t
  )

exception Found

let has_nonterminal_transition s =
  try
    SymbolMap.iter (fun sym _ ->
      match sym with
      | Symbol.T _ ->
          ()
      | Symbol.N _ ->
          raise Found
    ) (Lr1.transitions s);
    false
  with Found ->
    true

(* This returns the list of reductions of [state] on token [z]. This
   is a list of zero or one elements. *)

let reductions s z =
  assert (not (Terminal.equal z Terminal.error));
  try
    TerminalMap.find z (Lr1.reductions s)
  with Not_found ->
    []

(* This tests whether state [s] is willing to reduce some production
   when the lookahead symbol is [z]. This test takes a possible default
   reduction into account. *)

let has_reduction s z : Production.index option =
  assert (not (Terminal.equal z Terminal.error));
  match Invariant.has_default_reduction s with
  | Some (prod, _) ->
      Some prod
  | None ->
      match reductions s z with
      | prod :: prods ->
          assert (prods = []);
          Some prod
      | [] ->
          None

let q =
  Q.create()

let add fact =
  (* The length of the word serves as the priority of this fact. *)
  Q.add q fact (W.length fact.word)

let init s =
  if has_nonterminal_transition s then
    foreach_terminal (fun z ->
      add {
        source = s;
        height = 0;
        target = s;
        word = W.epsilon;
        lookahead = z
      }
    )

let first w z =
  if W.length w > 0 then W.first w else z

module T : sig

  (* [register fact] registers the fact [fact]. It returns [true] if this fact
     is new, i.e., no fact concerning the same quintuple of [source], [height],
     [target], [a], and [z] was previously known. *)
  val register: fact -> bool

  (* [query target z f] enumerates all known facts whose target state is [target]
     and whose lookahead assumption is [z]. *)
  val query: Lr1.node -> Terminal.t -> (fact -> unit) -> unit

end = struct

  (* We use a map of [target, z] to a map of [source, height, a] to facts. *)

  module M1 =
    MyMap(struct
      type t = Lr1.node * Terminal.t
      let compare (target1, z1) (target2, z2) =
        let c = Lr1.Node.compare target1 target2 in
        if c <> 0 then c else
        Terminal.compare z1 z2
    end)

  module M2 =
    MyMap(struct
      type t = Lr1.node * int * Terminal.t
      let compare (source1, height1, a1) (source2, height2, a2) =
        let c = Lr1.Node.compare source1 source2 in
        if c <> 0 then c else
        let c = Pervasives.compare height1 height2 in
        if c <> 0 then c else
        Terminal.compare a1 a2
    end)

  let m : fact M2.t M1.t ref =
    ref M1.empty

  let register fact =
    let z = fact.lookahead in
    let a = first fact.word z in
    update_ref m (fun m1 ->
      M1.update M2.empty id (fact.target, z) m1 (fun m2 ->
        M2.update None some (fact.source, fact.height, a) m2 (function
          | None ->
              fact
          | Some earlier_fact ->
              assert (W.length earlier_fact.word <= W.length fact.word);
              earlier_fact
        )
      )
    )

  let query target z f =
    match M1.find (target, z) !m with
    | m2 ->
        M2.iter (fun _ fact ->
          f fact
        ) m2
    | exception Not_found ->
        ()

end

(* The module [E] is in charge of recording the non-terminal edges that we have
   discovered, or more precisely, the conditions under which these edges can be
   taken. *)

module E : sig

  (* [register s nt w z] records that, in state [s], the outgoing edge labeled
     [nt] can be taken by consuming the word [w], if the next symbol is [z].
     It returns [true] if this information is new. *)
  val register: Lr1.node -> Nonterminal.t -> W.word -> Terminal.t -> bool

  (* [query s nt a z] answers whether, in state [s], the outgoing edge labeled
     [nt] can be taken by consuming some word [w], under the assumption that
     the next symbol is [z], and under the constraint that the first symbol of
     [w.z] is [a]. *)
  val query: Lr1.node -> Nonterminal.t -> Terminal.t -> Terminal.t -> (W.word -> unit) -> unit

end = struct

  (* For now, we implement a mapping of [s, nt, a, z] to [w]. *)

  module M =
    MyMap(struct
      type t = Lr1.node * Nonterminal.t * Terminal.t * Terminal.t
      let compare (s1, nt1, a1, z1) (s2, nt2, a2, z2) =
        let c = Lr1.Node.compare s1 s2 in
        if c <> 0 then c else
        let c = Nonterminal.compare nt1 nt2 in
        if c <> 0 then c else
        let c = Terminal.compare a1 a2 in
        if c <> 0 then c else
        Terminal.compare z1 z2
    end)

  let m =
    ref M.empty

  let register s nt w z =
    let a = first w z in
    update_ref m (fun m ->
      M.update None some (s, nt, a, z) m (function
      | None ->
          w
      | Some earlier_w ->
          assert (W.length earlier_w <= W.length w);
          earlier_w
      )
    )

  let query s nt a z f =
    match M.find (s, nt, a, z) !m with
    | w -> f w
    | exception Not_found -> ()

end

let extend fact target w z =
  assert (Terminal.equal fact.lookahead (first w z));
  {
    source = fact.source;
    height = fact.height + 1;
    target = target;
    word = W.append fact.word w;
    lookahead = z
  }

let new_edge s nt w z =
  if E.register s nt w z then
    T.query s (first w z) (fun fact ->
      if extensible fact then
        add (extend fact s w z)
    )

(* [consequences fact] is invoked when we discover a new fact (i.e., one that
   was not previously known). It studies the consequences of this fact. These
   consequences are of two kinds:

   - As in Dijkstra's algorithm, the new fact can be viewed as a newly
   discovered vertex. We study its (currently known) outgoing edges,
   and enqueue new facts in the priority queue.

   - Sometimes, a fact can also be viewed as a newly discovered edge.
   This is the case when the word from [fact.source] to [fact.target]
   represents a production of the grammar and [fact.target] is willing
   to reduce this production. We record the existence of this edge,
   and re-inspect any previously discovered vertices which are
   interested in this outgoing edge.
*)
(**)

let consequences fact =

  (* 1. View [fact] as a vertex. Examine the transitions out of [fact.target]. *)
  
  if extensible fact then
    SymbolMap.iter (fun sym s ->
      match sym with
      | Symbol.T t ->

          (* 1a. There is a transition labeled [t] out of [fact.target]. If the
             lookahead assumption [fact.lookahead] accepts [t], then we derive a
             new fact, where one more edge has been taken. We enqueue this new
             fact for later examination. *)
          (**)

          if Terminal.equal fact.lookahead t then
            foreach_terminal (fun z ->
              add (extend fact s (W.singleton t) z)
            )

      | Symbol.N nt ->

          (* 1b. There is a transition labeled [nt] out of [fact.target]. We
             need to know how this nonterminal edge can be taken. We query for a
             word [w] that allows us to take this edge. The answer depends on
             the terminal symbol [z] that comes *after* this word: we try all
             such symbols. Furthermore, we need the first symbol of [w.z] to
             satisfy the lookahead assumption [fact.lookahead], so the answer
             also depends on this assumption. *)
          (**)

          foreach_terminal (fun z ->
            E.query fact.target nt fact.lookahead z (fun w ->
              add (extend fact s w z)
            )
          )

    ) (Lr1.transitions fact.target);

  (* 2. View [fact] as a possible edge. This is possible if the path from
     [fact.source] to [fact.target] represents a production [prod] and
     [fact.target] is willing to reduce this production. We check that
     [fact.height] equals the length of [prod]. This guarantees that
     reducing [prod] takes us all the way back to [fact.source]. Thus,
     this production gives rise to an edge labeled [nt] -- the left-hand
     side of [prod] -- out of [fact.source]. This edge is subject to the
     lookahead assumption [fact.lookahead], so we record that. *)
  (**)

  match has_reduction fact.target fact.lookahead with
  | Some prod when Production.length prod = fact.height ->
      new_edge fact.source (Production.nt prod) fact.word fact.lookahead
  | _ ->
      ()

let facts = ref 0

let discover fact =
  if T.register fact then begin

    incr facts;
    Printf.fprintf stderr "facts = %d, current length = %d\n%!"
      !facts (W.length fact.word);

    print_fact fact;

    consequences fact
  end

let main =
  Lr1.iter init;
  Q.repeat q discover
