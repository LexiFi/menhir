open Grammar

(* ------------------------------------------------------------------------ *)

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

(* This tests whether state [s] will initiate an error on the lookahead
   symbol [z]. *)

let causes_an_error s z =
  assert (not (Terminal.equal z Terminal.error));
  match Invariant.has_default_reduction s with
  | Some _ ->
      false
  | None ->
      reductions s z = [] &&
      not (SymbolMap.mem (Symbol.T z) (Lr1.transitions s))

  let id x = x
  let some x = Some x

let update_ref r f : bool =
  let v = !r in
  let v' = f v in
  v != v' && (r := v'; true)

let update add find none some key m f =
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

module MyMap (X : Map.OrderedType) = struct
  include Map.Make(X)
  let update none some key m f =
    update add find none some key m f
end

module W : sig

  type word
  val epsilon: word
  val singleton: Terminal.t -> word
  val append: word -> word -> word
  val length: word -> int
  val first: word -> Terminal.t -> Terminal.t
  val elements: word -> Terminal.t list
  val print: word -> string

end = struct

  type word = Terminal.t list
  let epsilon = []
  let singleton t = [t]
  let append = (@)
  let length = List.length
  let first w z = match w with a :: _ -> a | [] -> z
  let elements w = w
  let print w =
    string_of_int (length w) ^ " " ^
    String.concat " " (List.map Terminal.print (elements w))

end

module Q = LowIntegerPriorityQueue

module Trie = struct

  let c = ref 0

  type trie = {
    identity: int;
    source: Lr1.node;
    target: Lr1.node;
    productions: Production.index list;
    transitions: trie SymbolMap.t;
  }

  let mktrie source target productions transitions =
    let identity = Misc.postincrement c in
    { identity; source; target; productions; transitions }

  let empty source =
    mktrie source source [] SymbolMap.empty

  let is_empty t =
    t.productions = [] && SymbolMap.is_empty t.transitions

  let accepts prod t =
    List.mem prod t.productions

  let rec insert target w prod t =
    match w with
    | [] ->
        mktrie t.source target (prod :: t.productions) t.transitions
    | a :: w ->
        match SymbolMap.find a (Lr1.transitions target) with
        | successor ->
            let child = mktrie t.source successor [] SymbolMap.empty in
            mktrie t.source target t.productions
              (update SymbolMap.add SymbolMap.find child id a t.transitions (insert successor w prod))
        | exception Not_found ->
            t

  let insert w prod t =
    insert t.source w prod t

  let derivative a t =
    try
      SymbolMap.find a t.transitions
    with Not_found ->
      assert false

  let has_derivative a t =
    SymbolMap.mem a t.transitions

  let compare t1 t2 =
    Pervasives.compare (t1.identity : int) t2.identity

  let rec size t =
    SymbolMap.fold (fun _ child accu -> size child + accu) t.transitions 1

end

type fact = {
  future: Trie.trie;
  word: W.word;
  lookahead: Terminal.t
}

let source fact =
  fact.future.Trie.source

let target fact =
  fact.future.Trie.target

let extensible fact sym =
  Trie.has_derivative sym fact.future

let foreach_terminal f =
  Terminal.iter (fun t ->
    if not (Terminal.equal t Terminal.error) then
      f t
  )

let star s : Trie.trie =
  SymbolMap.fold (fun sym _ accu ->
    match sym with
    | Symbol.T _ ->
        accu
    | Symbol.N nt ->
        Production.foldnt nt accu (fun prod accu ->
          let w = Array.to_list (Production.rhs prod) in
          (* could insert this branch only if viable -- leads to 12600 instead of 12900 in ocaml.mly --lalr *)
          Trie.insert w prod accu
        )
  ) (Lr1.transitions s) (Trie.empty s)

let q =
  Q.create()

let add fact =
  (* The length of the word serves as the priority of this fact. *)
  Q.add q fact (W.length fact.word)
    (* In principle, there is no need to insert the fact into the queue
       if [T] already stores a comparable fact. *)

let stars = ref 0

let init s =
  let trie = star s in
  let size = (Trie.size trie) in
  stars := !stars + size;
  Printf.fprintf stderr "State %d has a star of size %d\n.%!"
    (Lr1.number s) size;
  if not (Trie.is_empty trie) then
    foreach_terminal (fun z ->
      add {
        future = trie;
        word = W.epsilon;
        lookahead = z
      }
    )

module T : sig

  (* [register fact] registers the fact [fact]. It returns [true] if this fact
     is new, i.e., no fact concerning the same quintuple of [source], [future],
     [target], [a], and [z] was previously known. *)
  val register: fact -> bool

  (* [query target z f] enumerates all known facts whose target state is [target]
     and whose lookahead assumption is [z]. *)
  val query: Lr1.node -> Terminal.t -> (fact -> unit) -> unit

  val stats: unit -> unit

end = struct

  (* This module implements a set of facts. Two facts are considered equal
     (for the purposes of this set) if they have the same [future], [a], and
     [z] fields. The [word] is not considered. Indeed, we are not interested
     in keeping track of several words that produce the same effect. Only the
     shortest such word is of interest. *)

  (* We need to query the set of facts in two ways. In [register], we need to
     test whether a fact is in the set. In [query], we need to find all facts
     that match a pair [target, z]. For this reason, we use a two-level table.
     The first level is a matrix indexed by [target] and [z]. At the second
     level, we find sets of facts. *)
(**)

  module M =
    MySet.Make(struct
      type t = fact
      let compare fact1 fact2 =
        let c = Trie.compare fact1.future fact2.future in
        if c <> 0 then c else
        let a1 = W.first fact1.word fact1.lookahead
        and a2 = W.first fact2.word fact2.lookahead in
        Terminal.compare a1 a2
    end)

  let table = (* a pretty large table... *)
    Array.make (Lr1.n * Terminal.n) M.empty

  let index target z =
    Terminal.n * (Lr1.number target) + Terminal.t2i z

  let count = ref 0

  let register fact =
    let target = target fact in
    let z = fact.lookahead in
    let i = index target z in
    let m = table.(i) in
    (* We crucially rely on the fact that [M.add] guarantees not to
       change the set if an ``equal'' fact already exists. Thus, a
       later, longer path is ignored in favor of an earlier, shorter
       path. *)
    let m' = M.add fact m in
    m != m' && begin
      incr count;
      table.(i) <- m';
      true
    end

  let query target z f =
    let i = index target z in
    let m = table.(i) in
    M.iter f m

  let stats () =
    Printf.fprintf stderr "T stores %d facts.\n%!" !count

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

  val stats: unit -> unit

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

  let count = ref 0

  let register s nt w z =
    let a = W.first w z in
    update_ref m (fun m ->
      M.update None some (s, nt, a, z) m (function
      | None ->
          incr count;
          w
      | Some earlier_w ->
          (* assert (W.length earlier_w <= W.length w); *)
          earlier_w
      )
    )

  let query s nt a z f =
    match M.find (s, nt, a, z) !m with
    | w -> f w
    | exception Not_found -> ()

  let stats () =
    Printf.fprintf stderr "E stores %d facts.\n%!" !count

end

let extend fact sym w z =
  assert (Terminal.equal fact.lookahead (W.first w z));
  {
    future = Trie.derivative sym fact.future;
    word = W.append fact.word w;
    lookahead = z
  }

let new_edge s nt w z =
  (*
  Printf.fprintf stderr "Considering reduction on %s in state %d\n"
    (Terminal.print z) (Lr1.number s);
  *)
  if E.register s nt w z then
    let sym = (Symbol.N nt) in
    T.query s (W.first w z) (fun fact ->
      if extensible fact sym then
        add (extend fact sym w z)
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
  
  SymbolMap.iter (fun sym _ ->
    if extensible fact sym then
      match sym with
      | Symbol.T t ->

          (* 1a. There is a transition labeled [t] out of [fact.target]. If
             the lookahead assumption [fact.lookahead] is compatible with [t],
             then we derive a new fact, where one more edge has been taken. We
             enqueue this new fact for later examination. *)
          (**)

          if Terminal.equal fact.lookahead t then
            foreach_terminal (fun z ->
              add (extend fact sym (W.singleton t) z)
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
            E.query (target fact) nt fact.lookahead z (fun w ->
              add (extend fact sym w z)
            )
          )

  ) (Lr1.transitions (target fact));

  (* 2. View [fact] as a possible edge. This is possible if the path from
     [fact.source] to [fact.target] represents a production [prod] and
     [fact.target] is willing to reduce this production. We check that
     [fact.future] accepts [epsilon]. This guarantees that reducing [prod]
     takes us all the way back to [fact.source]. Thus, this production gives
     rise to an edge labeled [nt] -- the left-hand side of [prod] -- out of
     [fact.source]. This edge is subject to the lookahead assumption
     [fact.lookahead], so we record that. *)
  (**)

  match has_reduction (target fact) fact.lookahead with
  | Some prod when Trie.accepts prod fact.future ->
      new_edge (source fact) (Production.nt prod) fact.word fact.lookahead
  | _ ->
      ()

let level = ref 0

let done_with_level () =
  Printf.fprintf stderr "Done with level %d.\n" !level;
  T.stats();
  E.stats();
  Printf.fprintf stderr "Q stores %d facts.\n%!" (Q.cardinal q)

let discover fact =
  if T.register fact then begin
    if W.length fact.word > ! level then begin
      done_with_level();
      level := W.length fact.word;
    end;
    consequences fact
  end

let () =
  Lr1.iter init;
  Printf.fprintf stderr "Cumulated star size: %d\n%!" !stars;
  Q.repeat q discover;
  Time.tick "Running LRijkstra";
  done_with_level()

(* ------------------------------------------------------------------------ *)

(* The following code validates the fact that an error can be triggered in
   state [s'] by beginning in the initial state [s] and reading the
   sequence of terminal symbols [w]. We use this for debugging purposes. *)

let fail msg =
  Printf.fprintf stderr "coverage: internal error: %s.\n%!" msg;
  false

open ReferenceInterpreter

let validate s s' w : bool =
  match
    ReferenceInterpreter.check_error_path (Lr1.nt_of_entry s) (W.elements w)
  with
  | OInputReadPastEnd ->
      fail "input was read past its end"
  | OInputNotFullyConsumed ->
      fail "input was not fully consumed"
  | OUnexpectedAccept ->
      fail "input was unexpectedly accepted"
  | OK state ->
      Lr1.Node.compare state s' = 0 ||
      fail (
        Printf.sprintf "error occurred in state %d instead of %d"
          (Lr1.number state)
          (Lr1.number s')
      )

(* ------------------------------------------------------------------------ *)

(* We now wish to determine, given a state [s'] and a terminal symbol [z], a
   minimal path that takes us from some entry state to state [s'] with [z] as
   the next (unconsumed) symbol. *)

(* This can be formulated as a search for a shortest path in a graph. The
   graph is not just the automaton, though. It is a (much) larger graph whose
   vertices are pairs [s, z] and whose edges are obtained by querying the
   module [E] above. *)

let forward () =

  let module A = Astar.Make(struct

    (* A vertex is a pair [s, z].
       [z] cannot be the [error] token. *)
    type node =
        Lr1.node * Terminal.t

    let equal (s'1, z1) (s'2, z2) =
      Lr1.Node.compare s'1 s'2 = 0 && Terminal.compare z1 z2 = 0

    let hash (s, z) =
      Hashtbl.hash (Lr1.number s, z)

    (* An edge is labeled with a word. *)
    type label =
      W.word

    (* Forward search from every [s, z], where [s] is an initial state. *)
    let sources f =
      foreach_terminal (fun z ->
        ProductionMap.iter (fun _ s ->
          f (s, z)
        ) Lr1.entry
      )

    let successors (s, z) edge =
      assert (not (Terminal.equal z Terminal.error));
      SymbolMap.iter (fun sym s' ->
        match sym with
        | Symbol.T t ->
            if Terminal.equal z t then
              let w = W.singleton t in
              foreach_terminal (fun z ->
                edge w 1 (s', z)
              )
        | Symbol.N nt ->
           foreach_terminal (fun z' ->
             E.query s nt z z' (fun w ->
               edge w (W.length w) (s', z')
             )
           )
      ) (Lr1.transitions s)

    let estimate _ =
      0

  end) in

  (* Search forward. *)

  Printf.fprintf stderr "Forward search:\n%!";
  let seen = ref Lr1.NodeSet.empty in
  let _, _ = A.search (fun ((s', z), path) ->
    if causes_an_error s' z && not (Lr1.NodeSet.mem s' !seen) then begin
      seen := Lr1.NodeSet.add s' !seen;
      (* An error can be triggered in state [s'] by beginning in the initial
         state [s] and reading the sequence of terminal symbols [w]. *)
      let (s, _), ws = A.reverse path in
      let w = List.fold_right W.append ws (W.singleton z) in
      Printf.fprintf stderr
        "An error can be reached from state %d to state %d:\n%!"
        (Lr1.number s)
        (Lr1.number s');
      Printf.fprintf stderr "%s\n%!" (W.print w);
      assert (validate s s' w)
    end
  ) in
  Printf.fprintf stderr "Reachable (forward): %d states\n%!"
    (Lr1.NodeSet.cardinal !seen);
  !seen

(* TEMPORARY what about the pseudo-token [#]? *)
(* TEMPORARY the code in this module should run only if --coverage is set *)

let () =
  let f = forward() in
  Time.tick "Forward search";
  ignore f
