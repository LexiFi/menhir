open Grammar

(* ------------------------------------------------------------------------ *)

(* First, we implement the computation of forward shortest paths in the
   automaton. We view the automaton as a graph whose vertices are states. We
   label each edge with the minimum length of a word that it generates. This
   yields a lower bound on the actual distance to every state from any entry
   state. *)

let approximate : Lr1.node -> int =

  let module A = Astar.Make(struct

    type node =
      Lr1.node

    let equal s1 s2 =
      Lr1.Node.compare s1 s2 = 0

    let hash s =
      Hashtbl.hash (Lr1.number s)

    type label =
      unit

    let sources f =
      (* The sources are the entry states. *)
      ProductionMap.iter (fun _ s -> f s) Lr1.entry

    let successors s edge =
      SymbolMap.iter (fun sym s' ->
        (* The weight of the edge from [s] to [s'] is given by the function
           [Grammar.Analysis.minimal_symbol]. If [sym] produces the empty
           language, this could be infinite, in which case no edge exists. *)
        match Analysis.minimal_symbol sym with
        | CompletedNatWitness.Finite (w, _) ->
            edge () w s'
        | CompletedNatWitness.Infinity ->
            ()
      ) (Lr1.transitions s)

    let estimate _ =
      (* A* with a zero [estimate] behaves like Dijkstra's algorithm. *)
      0

  end) in
        
  let distance, _ = A.search (fun (_, _) -> ()) in
  distance

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
  val first: word -> Terminal.t (* word must be nonempty *)
  val elements: word -> Terminal.t list
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

  let elements w =
    Seq.elements w.data

  let print w =
    string_of_int w.length ^ " " ^
    String.concat " " (List.map Terminal.print (elements w))

end

module Q = LowIntegerPriorityQueue

module Trie = struct

  let c = ref 0

  type trie =
    | Trie of int * Production.index list * trie SymbolMap.t

  let mktrie prods children =
    let stamp = Misc.postincrement c in
    Trie (stamp, prods, children)

  let empty =
    mktrie [] SymbolMap.empty

  let is_empty (Trie (_, prods, children)) =
    prods = [] && SymbolMap.is_empty children

  let accepts prod (Trie (_, prods, _)) =
    List.mem prod prods

  let update : Symbol.t -> trie SymbolMap.t -> (trie -> trie) -> trie SymbolMap.t =
    update SymbolMap.add SymbolMap.find empty id

  let rec insert w prod (Trie (_, prods, children)) =
    match w with
    | [] ->
        mktrie (prod :: prods) children
    | a :: w ->
        mktrie prods (update a children (insert w prod))

  let derivative a (Trie (_, _, children)) =
    try
      SymbolMap.find a children
    with Not_found ->
      empty

  let compare (Trie (stamp1, _, _)) (Trie (stamp2, _, _)) =
    Pervasives.compare (stamp1 : int) stamp2

end

type fact = {
  source: Lr1.node;
  target: Lr1.node;
  future: Trie.trie;
  word: W.word;
  lookahead: Terminal.t
}

let print_fact fact =
  Printf.fprintf stderr
    "from state %d to state %d via %s . %s\n%!"
    (Lr1.number fact.source)
    (Lr1.number fact.target)
    (W.print fact.word)
    (Terminal.print fact.lookahead)

let extensible fact sym =
  not (Trie.is_empty (Trie.derivative sym fact.future))

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
          Trie.insert w prod accu
        )
  ) (Lr1.transitions s) Trie.empty

let q =
  Q.create()

let add fact =
  (* The length of the word serves as the priority of this fact. *)
  Q.add q fact (W.length fact.word)

let init s =
  let trie = star s in
  if not (Trie.is_empty trie) then
    foreach_terminal (fun z ->
      add {
        source = s;
        target = s;
        future = trie;
        word = W.epsilon;
        lookahead = z
      }
    )

let first w z =
  if W.length w > 0 then W.first w else z

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

  (* We use a map of [target, z] to a map of [future, a] to facts. *)

  (* A minor and subtle optimization: we need not use [source] as part
     of the key in [M2], because [future] determines [source]. Indeed,
     [future] is (a sub-trie of) the trie generated by [init source],
     and every trie contains unique stamps. *)

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
      type t = Trie.trie * Terminal.t
      let compare (future1, a1) (future2, a2) =
        let c = Trie.compare future1 future2 in
        if c <> 0 then c else
        Terminal.compare a1 a2
    end)

  let m : fact M2.t M1.t ref =
    ref M1.empty

  let count = ref 0

  let register fact =
    let z = fact.lookahead in
    let a = first fact.word z in
    update_ref m (fun m1 ->
      M1.update M2.empty id (fact.target, z) m1 (fun m2 ->
        M2.update None some (fact.future, a) m2 (function
          | None ->
              incr count;
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
    let a = first w z in
    update_ref m (fun m ->
      M.update None some (s, nt, a, z) m (function
      | None ->
          incr count;
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

  let stats () =
    Printf.fprintf stderr "E stores %d facts.\n%!" !count

end

let extend fact target sym w z =
  assert (Terminal.equal fact.lookahead (first w z));
  let future = Trie.derivative sym fact.future in
  assert (not (Trie.is_empty future));
  {
    source = fact.source;
    target = target;
    future = future;
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
    let s' = try SymbolMap.find sym (Lr1.transitions s) with Not_found -> assert false in
    T.query s (first w z) (fun fact ->
      if extensible fact sym then
        add (extend fact s' sym w z)
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
  
  SymbolMap.iter (fun sym s ->
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
              add (extend fact s sym (W.singleton t) z)
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
              add (extend fact s sym w z)
            )
          )

  ) (Lr1.transitions fact.target);

  (* 2. View [fact] as a possible edge. This is possible if the path from
     [fact.source] to [fact.target] represents a production [prod] and
     [fact.target] is willing to reduce this production. We check that
     [fact.future] accepts [epsilon]. This guarantees that reducing [prod]
     takes us all the way back to [fact.source]. Thus, this production gives
     rise to an edge labeled [nt] -- the left-hand side of [prod] -- out of
     [fact.source]. This edge is subject to the lookahead assumption
     [fact.lookahead], so we record that. *)
  (**)

  match has_reduction fact.target fact.lookahead with
  | Some prod when Trie.accepts prod fact.future ->
      new_edge fact.source (Production.nt prod) fact.word fact.lookahead
  | _ ->
      ()

let level = ref 0

let discover fact =
  if T.register fact then begin

    if W.length fact.word > ! level then begin
      Printf.fprintf stderr "Done with level %d.\n" !level;
      level := W.length fact.word;
      T.stats();
      E.stats()
    end;
(*
    incr facts;
    Printf.fprintf stderr "Facts = %d, current length = %d\n%!"
      !facts ();
    Printf.fprintf stderr "New fact:\n";
    print_fact fact;
*)
    consequences fact
  end

let main =
  Lr1.iter init;
  Q.repeat q discover;
  Time.tick "Running LRijkstra";
  T.stats();
  E.stats()

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
   module [E] above. Because we perform a backward search, from [s', z] to any
   entry state, we use reverse edges, from a state to its predecessors in the
   automaton. *)

(* Debugging. TEMPORARY *)
let es = ref 0

exception Success of Lr1.node * W.word

let backward (s', z) : unit =

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

    (* Backward search from the single source [s', z]. *)
    let sources f = f (s', z)

    let successors (s', z) edge =
      assert (not (Terminal.equal z Terminal.error));
      match Lr1.incoming_symbol s' with
      | None ->
          (* An entry state has no predecessor states. *)
          ()

      | Some (Symbol.T t) ->
          if not (Terminal.equal t Terminal.error) then
            (* There is an edge from [s] to [s'] labeled [t] in the automaton.
               Thus, our graph has an edge from [s', z] to [s, t], labeled [t]. *)
            let w = W.singleton t in
            List.iter (fun s ->
              edge w 1 (s, t)
            ) (Lr1.predecessors s')

      | Some (Symbol.N nt) ->
          (* There is an edge from [s] to [s'] labeled [nt] in the automaton.
             For every letter [a], we query [E] for a word [w] that begins in
             [s] and allows us to take the edge labeled [nt] when the
             lookahead symbol is [z]. Such a path [w] takes us from [s, a] to
             [s', z]. Thus, our graph has an edge, labeled [w], in the reverse
             direction. *)
          (**)
          List.iter (fun s ->
            foreach_terminal (fun a ->
              assert (not (Terminal.equal a Terminal.error));
              E.query s nt a z (fun w ->
                edge w (W.length w) (s, a)
              )
            )
          ) (Lr1.predecessors s')

    let estimate (s', _z) =
      approximate s'

  end) in

  (* Search backwards from [s', z], stopping as soon as an entry state [s] is
     reached. In that case, return the state [s] and the path that has been
     found. *)

  let _, _ = A.search (fun ((s, _), path) ->
    (* Debugging. TEMPORARY *)
    incr es;
    if !es mod 10000 = 0 then
      Printf.fprintf stderr "es = %d\n%!" !es;
    (* If [s] is a start state... *)
    let _, ws = A.reverse path in
    let ws = List.rev ws in
    if Lr1.incoming_symbol s = None then
      (* [labels] is a list of properties. Projecting onto the second
         component yields a list of paths (sequences of terminal symbols),
         which we concatenate to obtain a path. Because the edges that were
         followed last are in front of the list, and because this is a
         reverse graph, we obtain a path that makes direct sense: it is a
         sequence of terminal symbols that will take the automaton into
         state [s'] if the next (unconsumed) symbol is [z]. We append [z]
         at the end of this path. *)
      let w = List.fold_right W.append ws (W.singleton z) in
      raise (Success (s, w))
  ) in
  ()

(* ------------------------------------------------------------------------ *)

(* Forward search. *)

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
  let es = ref 0 in
  let seen = ref Lr1.NodeSet.empty in
  let _, _ = A.search (fun ((s', z), (path : A.path)) ->
    (* Debugging. TEMPORARY *)
    incr es;
    if !es mod 10000 = 0 then
      Printf.fprintf stderr "es = %d\n%!" !es;
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
      (*
      let approx = approximate s'
      and real = W.length w - 1 in
      assert (approx <= real);
      if approx < real then
        Printf.fprintf stderr "Approx = %d, real = %d\n" approx real;
      *)
      assert (validate s s' w)
    end
  ) in
  Printf.fprintf stderr "Reachable (forward): %d states\n%!"
    (Lr1.NodeSet.cardinal !seen);
  !seen

(* ------------------------------------------------------------------------ *)

(* For each state [s'] and for each terminal symbol [z] such that [z] triggers
   an error in [s'], backward search is performed. For each state [s'], we
   stop as soon as one [z] is found, i.e., as soon as one way of causing an
   error in state [s'] is found. *)

let backward s' : W.word option =
  
  (* Debugging. TEMPORARY *)
  Printf.fprintf stderr
    "Attempting to reach an error in state %d:\n%!"
    (Lr1.number s');

  try

    (* This loop stops as soon as we are able to reach one error at [s']. *)
    Terminal.iter (fun z ->
      if not (Terminal.equal z Terminal.error) && causes_an_error s' z then
        backward (s', z)
    );
    (* No error can be triggered in state [s']. *)
    None

  with Success (s, w) ->
    (* An error can be triggered in state [s'] by beginning in the initial
       state [s] and reading the sequence of terminal symbols [w]. *)
    assert (validate s s' w);
    Some w

(* Test. TEMPORARY *)

let backward () =
  let reachable = ref Lr1.NodeSet.empty in
  Lr1.iter (fun s' ->
    begin match backward s' with
    | None ->
        Printf.fprintf stderr "infinity\n%!"
    | Some w ->
        Printf.fprintf stderr "%s\n%!" (W.print w);
        reachable := Lr1.NodeSet.add s' !reachable
    end;
    Printf.fprintf stderr "Edges so far: %d\n" !es
  );
  Printf.fprintf stderr "Reachable (backward): %d states\n%!"
    (Lr1.NodeSet.cardinal !reachable);
  !reachable

(* TEMPORARY what about the pseudo-token [#]? *)
(* TEMPORARY the code in this module should run only if --coverage is set *)

let () =
(*
  let b = backward() in
  Time.tick "Backward search";
*)
  let f = forward() in
  Time.tick "Forward search";
  ignore f
(*
  assert (Lr1.NodeSet.equal b f)
*)
