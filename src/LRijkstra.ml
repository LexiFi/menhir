open Grammar
module W = Terminal.Word(struct end) (* TEMPORARY wrap side effect in functor *)

(* Throughout, we ignore the [error] pseudo-token completely. We consider that
   it never appears on the input stream. Hence, any state whose incoming
   symbol is [error] is considered unreachable. *)

(* ------------------------------------------------------------------------ *)

(* We begin with a number of auxiliary functions that provide information
   about the LR(1) automaton. These functions could perhaps be moved to a
   separate module. We keep them here, for the moment, because they are not
   used anywhere else. *)

(* [reductions_on s z] is the list of reductions permitted in state [s] when
   the lookahead symbol is [z]. This is a list of zero or one elements. This
   does not take default reductions into account. *)

let reductions_on s z : Production.index list =
  try
    TerminalMap.find z (Lr1.reductions s)
  with Not_found ->
    []

(* [has_reduction s z] tells whether state [s] is willing to reduce some
   production (and if so, which one) when the lookahead symbol is [z]. It
   takes a possible default reduction into account. *)

let has_reduction s z : Production.index option =
  match Invariant.has_default_reduction s with
  | Some (prod, _) ->
      Some prod
  | None ->
      match reductions_on s z with
      | prod :: prods ->
          assert (prods = []);
          Some prod
      | [] ->
          None

(* [can_reduce s prod] indicates whether state [s] is able to reduce
   production [prod] (either as a default reduction, or as a normal
   reduction). *)

let can_reduce s prod =
  match Invariant.has_default_reduction s with
  | Some (prod', _) when prod = prod' ->
      true
  | _ ->
      TerminalMap.fold (fun _ prods accu ->
        accu || List.mem prod prods
      ) (Lr1.reductions s) false

(* [causes_an_error s z] tells whether state [s] will initiate an error on the
   lookahead symbol [z]. *)

let causes_an_error s z : bool =
  match Invariant.has_default_reduction s with
  | Some _ ->
      false
  | None ->
      reductions_on s z = [] &&
      not (SymbolMap.mem (Symbol.T z) (Lr1.transitions s))

(* [foreach_terminal f] applies the function [f] to every terminal symbol in
   turn, except [error]. *)

let foreach_terminal f =
  Terminal.iter (fun t ->
    if not (Terminal.equal t Terminal.error) then
      f t
  )

(* [foreach_terminal_not_causing_an_error s f] applies the function [f] to
   every terminal symbol [z] such that [causes_an_error s z] is false. This
   could be implemented in a naive manner using [foreach_terminal] and
   [causes_an_error]. This implementation is slightly more efficient. *)

let foreach_terminal_not_causing_an_error s f =
  match Invariant.has_default_reduction s with
  | Some _ ->
      (* There is a default reduction. No symbol causes an error. *)
      foreach_terminal f
  | None ->
      (* Enumerate every terminal symbol [z] for which there is a
         reduction. *)
      TerminalMap.iter (fun z _ ->
        if not (Terminal.equal z Terminal.error) then
          f z
      ) (Lr1.reductions s);
      (* Enumerate every terminal symbol [z] for which there is a
         transition. *)
      SymbolMap.iter (fun sym _ ->
        match sym with
        | Symbol.T z ->
            if not (Terminal.equal z Terminal.error) then
              f z
        | Symbol.N _ ->
            ()
      ) (Lr1.transitions s)

(* ------------------------------------------------------------------------ *)

(* Suppose [s] is a state that carries an outgoing edge labeled with a
   non-terminal symbol [nt]. We are interested in finding out how this edge
   can be taken. In order to do that, we must determine how, by starting in
   [s], one can follow a path that corresponds to (the right-hand side of) a
   production [prod] associated with [nt]. There are in general several such
   productions. The paths that they determine in the automaton form a "star".
   We represent the star rooted at [s] as a trie. For every state [s], the
   star rooted at [s] is constructed in advance, before the algorithm runs.
   While the algorithm runs, a point in the trie (that is, a sub-trie) tells
   us where we come form, where we are, and which production(s) we are hoping
   to reduce in the future. *)

module Trie : sig

  type trie

  (* [star s] creates a (new) trie whose source is [s], populated with its
     branches. (There is one branch for every production [prod] associated
     with every non-terminal symbol [nt] for which [s] carries an outgoing
     edge.) If the star turns out to be trivial then [None] is returned. *)
  val star: Lr1.node -> trie option

  (* After [star s] has been called, [size (Lr1.number s)] reports the size
     of the trie that has been constructed for state [s]. *)
  val size: int -> int

  (* Every (sub-)trie has a unique identity. (One can think of it as its
     address.) [compare] compares the identity of two tries. This can be
     used, e.g., to set up a map whose keys are tries. *)
  val compare: trie -> trie -> int

  (* [source t] returns the source state of the (sub-)trie [t]. This is
     the root of the star of which [t] is a sub-trie. In other words, this
     tells us "where we come from". *)
  val source: trie -> Lr1.node

  (* [current t] returns the current state of the (sub-)trie [t]. This is
     the root of the sub-trie [t]. In other words, this tells us "where
     we are". *)
  val current: trie -> Lr1.node

  (* [accepts prod t] tells whether the current state of the trie [t] is
     the end of a branch associated with production [prod]. If so, this
     means that we have successfully followed a path that corresponds to
     the right-hand side of production [prod]. *)
  val accepts: Production.index -> trie -> bool

  (* [step sym t] is the immediate sub-trie of [t] along the symbol [sym].
     This function raises [Not_found] if [t] has no child labeled [sym]. *)
  val step: Symbol.t -> trie -> trie

  (* [verbose()] outputs debugging & performance information. *)
  val verbose: unit -> unit

end = struct

  (* A trie has the following structure. *)

  type trie = {
    (* A unique identity, used by [compare]. The trie construction code
       ensures that these numbers are indeed unique: see [fresh], [insert],
       [star]. *)
    identity: int;
    (* The root state of this star: "where we come from". *)
    source: Lr1.node;
    (* The current state, i.e., the root of this sub-trie: "where we are". *)
    current: Lr1.node;
    (* The productions that we can reduce in the current state. In other
       words, if this list is nonempty, then the current state is the end
       of one (or several) branches. It can nonetheless have children. *)
    productions: Production.index list;
    (* The children, or sub-tries. *)
    transitions: trie SymbolMap.t
  }

  (* This counter is used by [mktrie] to produce unique identities. *)
  let c = ref 0

  (* This smart constructor creates a new trie with a unique identity. *)
  let mktrie source current productions transitions =
    let identity = Misc.postincrement c in
    { identity; source; current; productions; transitions }

  exception DeadBranch

  let rec insert w prod t =
    match w with
    | [] ->
        (* We check whether the current state [t.current] is able to reduce
           production [prod]. (If [prod] cannot be reduced, the reduction
           action must have been suppressed by conflict resolution.) If not,
           then this branch is dead. This test is superfluous (i.e., it would
           be OK to conservatively assume that [prod] can be reduced) but
           allows us to build a slightly smaller star in some cases. *)
        if can_reduce t.current prod then
          (* We consume (update) the trie [t], so there is no need to allocate
             a new stamp. (Of course we could allocate a new stamp, but I prefer
             to be precise.) *)
          { t with productions = prod :: t.productions }
        else
          raise DeadBranch
    | a :: w ->
        (* Check if there is a transition labeled [a] out of [t.current]. If
           there is, we add a child to the trie [t]. If there isn't, then it
           must have been removed by conflict resolution. (Indeed, it must be
           present in a canonical automaton.) We could in this case return an
           unchanged sub-trie. We can do slightly better: we abort the whole
           insertion, so as to return an unchanged toplevel trie. *)
        match SymbolMap.find a (Lr1.transitions t.current) with
        | successor ->
            (* Find our child at [a], or create it. *)
            let t' =
              try
                SymbolMap.find a t.transitions
              with Not_found ->
                mktrie t.source successor [] SymbolMap.empty
            in
            (* Update the child [t']. *)
            let t' = insert w prod t' in
            (* Update [t]. Again, no need to allocate a new stamp. *)
            { t with transitions = SymbolMap.add a t' t.transitions }
        | exception Not_found ->
            raise DeadBranch

  (* [insert prod t] inserts a new branch, corresponding to production
     [prod], into the trie [t]. This function consumes its argument,
     which should no longer be used afterwards. *)
  let insert prod t =
    let w = Array.to_list (Production.rhs prod) in
    let save = !c in
    try
      insert w prod t
    with DeadBranch ->
      c := save;
      t

  (* [fresh s] creates a new empty trie whose source is [s]. *)
  let fresh source =
    mktrie source source [] SymbolMap.empty

  let star s =
    SymbolMap.fold (fun sym _ accu ->
      match sym with
      | Symbol.T _ ->
          accu
      | Symbol.N nt ->
          Production.foldnt nt accu insert
    ) (Lr1.transitions s) (fresh s)

  (* [nontrivial t] tests whether the trie [t] has any branches, i.e.,
     contains at least one sub-trie whose [productions] field is nonempty.
     Trivia: a trie of size greater than 1 is necessarily nontrivial, but the
     converse is not true: a nontrivial trie can have size 1. (This occurs
     when all productions have zero length.) *)
  let nontrivial t =
    not (t.productions = [] && SymbolMap.is_empty t.transitions)

  (* Redefine [star] to include a [nontrivial] test and to record the size
     of the newly built trie. *)

  let size =
    Array.make Lr1.n (-1)

  let star s =
    let initial = !c in
    let t = star s in
    let final = !c in
    size.(Lr1.number s) <- final - initial;
    if nontrivial t then
      Some t
    else
      None

  let size s =
    assert (size.(s) >= 0);
    size.(s)

  let compare t1 t2 =
    Pervasives.compare (t1.identity : int) t2.identity

  let source t =
    t.source

  let current t =
    t.current

  let accepts prod t =
    List.mem prod t.productions

  let step a t =
    SymbolMap.find a t.transitions (* careful: may raise [Not_found] *)

  let verbose () =
    Printf.fprintf stderr "Cumulated star size: %d\n%!" !c

end

(* ------------------------------------------------------------------------ *)

(* The main algorithm, [LRijkstra], accumulates facts. A fact is a triple of a
   position (that is, a sub-trie), a word, and a lookahead assumption. Such a
   fact means that this position can be reached, from the source state
   [Trie.source fact.position], by consuming [fact.word], under the assumption
   that the next input symbol is [fact.lookahead]. *)

(* The first symbol of the input, [first fact.word fact.lookahead], plays a
   special role. Indeed, for every position, for every first symbol, and for
   every lookahead symbol, we keep track of at most one fact. Thus, the total
   number of facts accumulated by the algorithm is at most [T.n^2], where [T]
   is the total size of the tries that we have constructed, and [n] is the
   number of terminal symbols. (This number can be quite large. [T] can be in
   the tens of thousands, and [n] can be over one hundred. These figures lead
   to a theoretical upper bound of 100M. In practice, for T=25K and n=108, we
   observe that the algorithm gathers about 7M facts.) *)

type fact = {
  position: Trie.trie;
  word: W.word;
  lookahead: Terminal.t
}

let source fact =
  Trie.source fact.position

let current fact =
  Trie.current fact.position

(* ------------------------------------------------------------------------ *)

(* As in Dijkstra's algorithm, a priority queue contains the facts that await
   examination. The length of [fact.word] serves as the priority of a fact.
   This guarantees that we discover shortest paths. (We never insert into the
   queue a fact whose priority is less than the priority of the last fact
   extracted out of the queue.) *)

(* [LowIntegerPriorityQueue] offers very efficient operations (essentially
   constant time, for a small constant). It exploits the fact that priorities
   are low nonnegative integers. *)

module Q = LowIntegerPriorityQueue

let q =
  Q.create()

(* We never insert into the queue a fact that immediately causes an error,
   i.e., a fact such that [causes_an_error (current fact) fact.lookahead]
   holds. In practice, this convention allows reducing the number of facts
   that go through the queue by a factor of two. *)

(* In principle, there is no need to insert the fact into the queue if [T]
   already stores a comparable fact. We could perform this test in [add].
   However, a quick experiment suggests that this is not worthwhile. The run
   time augments (because membership in [T] is tested twice, upon inserting
   and upon extracting) and the memory consumption does not seem to go down
   significantly. *)

let add fact =
  (* assert (not (causes_an_error (current fact) fact.lookahead)); *)
  (* The length of [fact.word] serves as the priority of this fact. *)
  Q.add q fact (W.length fact.word)

(* Construct the [star] of every state [s]. Initialize the priority queue. *)

let () =
  Lr1.iter (fun s ->
    match Trie.star s with
    | Some trie ->
        foreach_terminal_not_causing_an_error s (fun z ->
          add {
            position = trie;
            word = W.epsilon;
            lookahead = z
          }
        )
    | None ->
        ()
  )

(* ------------------------------------------------------------------------ *)

module T : sig

  (* [register fact] registers the fact [fact]. It returns [true] if this fact
     is new, i.e., no fact concerning the same triple of [position], [a], and
     [z] was previously known. *)
  val register: fact -> bool

  (* [query current z f] enumerates all known facts whose current state is
     [current] and whose lookahead assumption is [z]. *)
  val query: Lr1.node -> Terminal.t -> (fact -> unit) -> unit

  val verbose: unit -> unit

end = struct

  (* This module implements a set of facts. Two facts are considered equal
     (for the purposes of this set) if they have the same [position], [a], and
     [z] fields. The [word] is not considered. Indeed, we are not interested
     in keeping track of several words that produce the same effect. Only the
     shortest such word is of interest. *)

  (* We need to query the set of facts in two ways. In [register], we need to
     test whether a fact is in the set. In [query], we need to find all facts
     that match a pair [current, z]. For this reason, we use a two-level table.
     The first level is a matrix indexed by [current] and [z]. At the second
     level, we find sets of facts. *)
(**)

  module M =
    MySet.Make(struct
      type t = fact
      let compare fact1 fact2 =
        let c = Trie.compare fact1.position fact2.position in
        if c <> 0 then c else
        let a1 = W.first fact1.word fact1.lookahead
        and a2 = W.first fact2.word fact2.lookahead in
        Terminal.compare a1 a2
    end)

  let table = (* a pretty large table... *)
    Array.make (Lr1.n * Terminal.n) M.empty

  let index current z =
    Terminal.n * (Lr1.number current) + Terminal.t2i z

  let count = ref 0

  let register fact =
    let current = current fact in
    let z = fact.lookahead in
    let i = index current z in
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

  let query current z f =
    let i = index current z in
    let m = table.(i) in
    M.iter f m

  let verbose () =
    Printf.fprintf stderr "T stores %d facts.\n%!" !count

end

(* ------------------------------------------------------------------------ *)

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

  val verbose: unit -> unit

end = struct

  (* At a high level, we must implement a mapping of [s, nt, a, z] to [w]. In
     practice, we can implement this specification using any combination of
     arrays, hash tables, balanced binary trees, and perfect hashing (i.e.,
     packing several of [s], [nt], [a], [z] in one word.) Here, we choose to
     use an array, indexed by [s], of hash tables, indexed by a key that packs
     [nt], [a], and [z] in one word. According to a quick experiment, the
     final population of the hash table [table.(index s)] seems to be roughly
     [Terminal.n * Trie.size s]. We note that using an initial capacity
     of 0 and relying on the hash table's resizing mechanism has a significant
     cost, which is why we try to guess a good initial capacity. *)

  module H = Hashtbl

  let table = (* a pretty large table... *)
    Array.init (Lr1.n) (fun i ->
      let size = Trie.size i in
      H.create (if size = 1 then 0 else Terminal.n * size)
    )

  let index s =
    Lr1.number s

  let pack nt a z : int =
    (Nonterminal.n2i nt lsl 16) lor
    (Terminal.t2i a lsl 8) lor
    (Terminal.t2i z)

  let count = ref 0

  let register s nt w z =
    let i = index s in
    let m = table.(i) in
    let a = W.first w z in
    let key = pack nt a z in
    if H.mem m key then
      false
    else begin
      incr count;
      H.add m key w;
      true
    end

  let query s nt a z f =
    let i = index s in
    let m = table.(i) in
    let key = pack nt a z in
    match H.find m key with
    | w -> f w
    | exception Not_found -> ()

  let verbose () =
    Printf.fprintf stderr "E stores %d facts.\n%!" !count

end

(* ------------------------------------------------------------------------ *)

let new_edge s nt w z =
  (*
  Printf.fprintf stderr "Considering reduction on %s in state %d\n"
    (Terminal.print z) (Lr1.number s);
  *)
  if E.register s nt w z then
    let sym = Symbol.N nt in
    T.query s (W.first w z) (fun fact ->
      assert (Terminal.equal fact.lookahead (W.first w z));
      match Trie.step sym fact.position with
      | position ->
          if not (causes_an_error (Trie.current position) z) then
            add {
              position;
              word = W.append fact.word w;
              lookahead = z
            }
      | exception Not_found ->
          ()
    )

(* [consequences fact] is invoked when we discover a new fact (i.e., one that
   was not previously known). It studies the consequences of this fact. These
   consequences are of two kinds:

   - As in Dijkstra's algorithm, the new fact can be viewed as a newly
   discovered vertex. We study its (currently known) outgoing edges,
   and enqueue new facts in the priority queue.

   - Sometimes, a fact can also be viewed as a newly discovered edge.
   This is the case when the word from [fact.source] to [fact.current]
   represents a production of the grammar and [fact.current] is willing
   to reduce this production. We record the existence of this edge,
   and re-inspect any previously discovered vertices which are
   interested in this outgoing edge.
*)
(**)

let consequences fact =

  let current = current fact in

  (* 1. View [fact] as a vertex. Examine the transitions out of [current]. *)
  
  SymbolMap.iter (fun sym s' ->
    match Trie.step sym fact.position, sym with
    | exception Not_found -> ()
    | position, Symbol.T t ->

        (* 1a. There is a transition labeled [t] out of [current]. If
           the lookahead assumption [fact.lookahead] is compatible with [t],
           then we derive a new fact, where one more edge has been taken. We
           enqueue this new fact for later examination. *)
        (**)

        if Terminal.equal fact.lookahead t then
          let word = W.append fact.word (W.singleton t) in
          (* assert (Lr1.Node.compare position.Trie.current s' = 0); *)
          foreach_terminal_not_causing_an_error s' (fun z ->
            add { position; word; lookahead = z }
          )

    | position, Symbol.N nt ->

        (* 1b. There is a transition labeled [nt] out of [current]. We
           need to know how this nonterminal edge can be taken. We query for a
           word [w] that allows us to take this edge. The answer depends on
           the terminal symbol [z] that comes *after* this word: we try all
           such symbols. Furthermore, we need the first symbol of [w.z] to
           satisfy the lookahead assumption [fact.lookahead], so the answer
           also depends on this assumption. *)
        (**)

        foreach_terminal_not_causing_an_error s' (fun z ->
          E.query current nt fact.lookahead z (fun w ->
            assert (Terminal.equal fact.lookahead (W.first w z));
            add {
              position;
              word = W.append fact.word w;
              lookahead = z
            }
          )
        )

  ) (Lr1.transitions current);

  (* 2. View [fact] as a possible edge. This is possible if the path from
     [fact.source] to [current] represents a production [prod] and
     [current] is willing to reduce this production. We check that
     [fact.position] accepts [epsilon]. This guarantees that reducing [prod]
     takes us all the way back to [fact.source]. Thus, this production gives
     rise to an edge labeled [nt] -- the left-hand side of [prod] -- out of
     [fact.source]. This edge is subject to the lookahead assumption
     [fact.lookahead], so we record that. *)
  (**)

  match has_reduction current fact.lookahead with
  | Some prod when Trie.accepts prod fact.position ->
      new_edge (source fact) (Production.nt prod) fact.word fact.lookahead
  | _ ->
      ()

let level = ref 0

let done_with_level () =
  Printf.fprintf stderr "Done with level %d.\n" !level;
  W.verbose();
  T.verbose();
  E.verbose();
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
  Trie.verbose();
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

(* TEMPORARY the code in this module should run only if --coverage is set *)

let () =
  let f = forward() in
  Time.tick "Forward search";
  let stat = Gc.quick_stat() in
  Printf.fprintf stderr
    "Maximum size reached by the major heap: %dM\n"
    (stat.Gc.top_heap_words * (Sys.word_size / 8) / 1024 / 1024);
  ignore f

(* TODO:
  can we store fewer facts when we hit a default reduction?
  subject to --coverage
  write to .coverage file
  remove Coverage, remove CompletedNatWitness?, revert Fix
  collect performance data, correlated with star size and alphabet size; draw a graph
  count the unreachable states and see if they are numerous in practice
*)

(* One could approach the problem just by exploring the (infinite) graph whose
   vertices are configurations of the LR automaton (i.e., stacks, or perhaps
   pairs of a stack and a lookahead symbol) and transitions are determined by
   feeding one symbol to the automaton. A small-step version of the reference
   interpreter would allow us to set this up easily. One could then run a
   breadth-first exploration of this graph and stop when desired, e.g., as
   soon as all automaton states have been reached. However, this process does
   not necessarily terminate, and could be very costly -- e.g. enumerating all
   sentences of length 10 when the alphabet has size 100 costs 10^20. Also,
   this approach cannot prove that a state is unreachable. *)
