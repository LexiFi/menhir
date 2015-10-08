(* The purpose of this algorithm is to find, for each pair of a state [s]
   and a terminal symbol [z] such that looking at [z] in state [s] causes
   an error, a minimal path (starting in some initial state) that actually
   triggers this error. *)

(* This is potentially useful for grammar designers who wish to better
   understand the properties of their grammar, or who wish to produce a
   list of all possible syntax errors (or, at least, one syntax error in
   each automaton state where an error may occur). *)

(* The problem seems rather tricky. One might think that it suffices to
   compute shortest paths in the automaton, and to use [Analysis.minimal] to
   replace each non-terminal symbol in a path with a minimal word that this
   symbol generates. One can indeed do so, but this yields only a lower bound
   on the actual shortest path to the error at [s, z]. Indeed, several
   difficulties arise, including the fact that reductions are subject to a
   lookahead hypothesis; the fact that some states have a default reduction,
   hence will never trigger an error; the fact that conflict resolution
   removes some (shift or reduce) actions, hence may suppress the shortest
   path. *)

(* We explicitly choose to ignore the [error] token. Thus, we disregard any
   reductions or transitions that take place when the lookahead symbol is
   [error]. As a result, any state whose incoming symbol is [error] is found
   unreachable. It would be too complicated to have to create a first error in
   order to be able to take certain transitions or drop certain parts of the
   input. *)

(* We never work with the terminal symbol [#] either. This symbol never
   appears in the maps returned by [Lr1.transitions] and [Lr1.reductions].
   Thus, in principle, we work with ``real'' terminal symbols only. However,
   we encode [any] as [#] -- see below. *)

(* NOTE: THIS FILE IS COMPILED WITH -noassert BY DEFAULT. If you would like
   the assertions to be tested at runtime, change that in the file _tags.
   The performance impact of the assertions is about 10%. *)

(* ------------------------------------------------------------------------ *)

(* To delay the side effects performed by this module, we wrap everything in
   in a big functor. The functor also serves to pass verbosity parameters. *)

module Run (X : sig
  (* If [verbose] is set, produce various messages on [stderr]. *)
  val verbose: bool
  (* If [statistics] is defined, it is interpreted as the name of
     a file to which one line of statistics is appended. *)
  val statistics: string option
end) = struct

open Grammar

(* ------------------------------------------------------------------------ *)

(* Record our start time. *)

let now () =
  match X.statistics with
  | Some _ ->
      Unix.((times()).tms_utime)
  | None ->
      0.0

let start =
  now()

(* ------------------------------------------------------------------------ *)

(* Because of our encoding of terminal symbols as 8-bit characters, this
   algorithm supports at most 256 terminal symbols. *)

let () =
  if Terminal.n > 256 then
    Error.error [] (Printf.sprintf
      "--list-errors supports at most 256 terminal symbols.\n\
       The grammar has %d terminal symbols." Terminal.n
    )

(* ------------------------------------------------------------------------ *)

(* Build a module that represents words as (hash-consed) strings. Note:
   this functor application has a side effect (it allocates memory, and
   more importantly, it may fail). *)

module W = Terminal.Word(struct end)

(* ------------------------------------------------------------------------ *)

(* The [error] token may appear in the maps returned by [Lr1.transitions]
   and [Lr1.reductions], so we sometimes need to explicitly check for it. *)

let non_error z =
  not (Terminal.equal z Terminal.error)

(* We introduce a pseudo-terminal symbol [any]. It is used in several places
   later on, in particular in the field [fact.lookahead], to encode the
   absence of a lookahead hypothesis -- i.e., any terminal symbol will do. *)

(* We choose to encode [any] as [#]. There is no risk of confusion, since we
   do not use [#] anywhere. Thus, the assertion [Terminal.real z] implies
   [z <> any]. *)

let any =
  Terminal.sharp

(* ------------------------------------------------------------------------ *)

(* We begin with a number of auxiliary functions that provide information
   about the LR(1) automaton. These functions could perhaps be moved to a
   separate module. We keep them here, for the moment, because they are not
   used anywhere else. *)

(* [reductions_on s z] is the list of reductions permitted in state [s] when
   the lookahead symbol is [z]. This is a list of zero or one elements. This
   does not take default reductions into account. *)

let reductions_on s z : Production.index list =
  assert (Terminal.real z);
  try
    TerminalMap.find z (Lr1.reductions s)
  with Not_found ->
    []

(* [has_reduction s z] tells whether state [s] is willing to reduce some
   production (and if so, which one) when the lookahead symbol is [z]. It
   takes a possible default reduction into account. *)

let has_reduction s z : Production.index option =
  assert (Terminal.real z);
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
      TerminalMap.fold (fun z prods accu ->
        (* A reduction on [#] is always a default reduction. (See [lr1.ml].) *)
        assert (not (Terminal.equal z Terminal.sharp));
        accu || non_error z && List.mem prod prods
      ) (Lr1.reductions s) false

(* [causes_an_error s z] tells whether state [s] will initiate an error on the
   lookahead symbol [z]. *)

let causes_an_error s z : bool =
  assert (Terminal.real z);
  match Invariant.has_default_reduction s with
  | Some _ ->
      false
  | None ->
      reductions_on s z = [] &&
      not (SymbolMap.mem (Symbol.T z) (Lr1.transitions s))

(* [foreach_terminal f] applies the function [f] to every terminal symbol in
   turn, except [error] and [#]. *)

let foreach_terminal =
  Terminal.iter_real

(* [foreach_terminal_not_causing_an_error s f] applies the function [f] to
   every terminal symbol [z] such that [causes_an_error s z] is false. This
   could be implemented in a naive manner using [foreach_terminal] and
   [causes_an_error]. This implementation is significantly more efficient. *)

let foreach_terminal_not_causing_an_error s f =
  match Invariant.has_default_reduction s with
  | Some _ ->
      (* There is a default reduction. No symbol causes an error. *)
      foreach_terminal f
  | None ->
      (* Enumerate every terminal symbol [z] for which there is a
         reduction. *)
      TerminalMap.iter (fun z _ ->
        (* A reduction on [#] is always a default reduction. (See [lr1.ml].) *)
        assert (not (Terminal.equal z Terminal.sharp));
        if non_error z then
          f z
      ) (Lr1.reductions s);
      (* Enumerate every terminal symbol [z] for which there is a
         transition. *)
      SymbolMap.iter (fun sym _ ->
        match sym with
        | Symbol.T z ->
            assert (not (Terminal.equal z Terminal.sharp));
            if non_error z then
              f z
        | Symbol.N _ ->
            ()
      ) (Lr1.transitions s)

(* Let us say a state [s] is solid if its incoming symbol is a terminal symbol
   (or if it has no incoming symbol at all, i.e., it is an initial state). It
   is fragile if its incoming symbol is a non-terminal symbol. *)

let is_solid s =
  match Lr1.incoming_symbol s with
  | None
  | Some (Symbol.T _) ->
    true
  | Some (Symbol.N _) ->
    false

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
   us where we come from, where we are, and which production(s) we are hoping
   to reduce in the future. *)

let grammar_uses_error =
  ref false

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

  (* After [star] has been called a number of times, [cumulated_size()]
     reports the total size of the tries that have been constructed. *)
  val cumulated_size: unit -> int

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
    | (Symbol.T t) :: _ when Terminal.equal t Terminal.error ->
         grammar_uses_error := true;
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

  (* The star at [s] is obtained by starting with a fresh empty trie and
     inserting into it every production [prod] whose left-hand side [nt]
     is the label of an outgoing edge at [s]. *)
  let star s =
    SymbolMap.fold (fun sym _ accu ->
      match sym with
      | Symbol.T _ ->
          accu
      | Symbol.N nt ->
          Production.foldnt nt accu insert
    ) (Lr1.transitions s) (fresh s)

  (* A trie [t] is nontrivial if it has at least one branch, i.e., contains at
     least one sub-trie whose [productions] field is nonempty. Trivia: a trie
     of size greater than 1 is necessarily nontrivial, but the converse is not
     true: a nontrivial trie can have size 1. (This occurs if all productions
     have zero length.) *)
  let trivial t =
    t.productions = [] && SymbolMap.is_empty t.transitions

  (* Redefine [star] to include a [nontrivial] test and to record the size of
     the newly built trie. *)

  let size =
    Array.make Lr1.n (-1)

  let star s =
    let initial = !c in
    let t = star s in
    let final = !c in
    size.(Lr1.number s) <- final - initial;
    if trivial t then None else Some t

  let size s =
    assert (size.(s) >= 0);
    size.(s)

  let cumulated_size () =
    !c

  let compare t1 t2 =
    Pervasives.compare t1.identity t2.identity

  let source t =
    t.source

  let current t =
    t.current

  let accepts prod t =
    List.mem prod t.productions

  let step a t =
    SymbolMap.find a t.transitions (* careful: may raise [Not_found] *)

  let verbose () =
    Printf.eprintf "Cumulated star size: %d\n%!" (cumulated_size())

end

(* ------------------------------------------------------------------------ *)

(* The main algorithm, [LRijkstra], accumulates facts. A fact is a triple of a
   position (that is, a sub-trie), a word, and a lookahead assumption. Such a
   fact means that this position can be reached, from the source state
   [Trie.source fact.position], by consuming [fact.word], under the assumption
   that the next input symbol is [fact.lookahead]. *)

(* We allow [fact.lookahead] to be [any] so as to indicate that this fact does
   not have a lookahead assumption. *)

type fact = {
  position: Trie.trie;
  word: W.word;
  lookahead: Terminal.t (* may be [any] *)
}

(* Accessors. *)

let source fact =
  Trie.source fact.position

let current fact =
  Trie.current fact.position

(* Two invariants reduce the number of facts that we consider:

   1. If [fact.lookahead] is a real terminal symbol [z] (i.e., not [any]),
      then [z] does not cause an error in the current state [current fact].
      It would be useless to consider a fact that violates this property;
      this cannot possibly lead to a successful reduction. In practice,
      this refinement allows reducing the number of facts that go through
      the queue by a factor of two.

   2. [fact.lookahead] is [any] iff the current state [current fact] is
      solid. This sounds rather reasonable (when a state is entered
      by shifting, it is entered regardless of which symbol follows)
      and simplifies the implementation of the sub-module [T].

*)

let invariant1 fact =
  fact.lookahead = any || not (causes_an_error (current fact) fact.lookahead)

let invariant2 fact =
  (fact.lookahead = any) = is_solid (current fact)

(* [compatible z a] checks whether the terminal symbol [a] satisfies the
   lookahead assumption [z] -- which can be [any]. *)

let compatible z a =
  assert (non_error z);
  assert (Terminal.real a);
  z = any || z = a

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

(* In principle, there is no need to insert the fact into the queue if [T]
   already stores a comparable fact. We could perform this test in [add].
   However, a quick experiment suggests that this is not worthwhile. The run
   time augments (because membership in [T] is tested twice, upon inserting
   and upon extracting) and the memory consumption does not seem to go down
   significantly. *)

let add fact =
  (* [fact.lookahead] can be [any], but cannot be [error] *)
  assert (non_error fact.lookahead);
  assert (invariant1 fact);
  assert (invariant2 fact);
  (* The length of [fact.word] serves as the priority of this fact. *)
  Q.add q fact (W.length fact.word)

(* ------------------------------------------------------------------------ *)

(* Construct the [star] of every state [s]. Initialize the priority queue. *)

let () =
  (* For every state [s]... *)
  Lr1.iter (fun s ->
    (* If the trie rooted at [s] is nontrivial...*)
    match Trie.star s with
    | None ->
        ()
    | Some position ->
        (* ...then insert an initial fact into the priority queue. *)
        (* In order to respect invariants 1 and 2, we must distinguish two
           cases. If [s] is solid, then we insert a single fact, whose
           lookahead assumption is [any]. Otherwise, we must insert one
           initial fact for every terminal symbol [z] that does not cause
           an error in state [s]. *)
        let word = W.epsilon in
        if is_solid s then
          add { position; word; lookahead = any }
        else
          foreach_terminal_not_causing_an_error s (fun z ->
            add { position; word; lookahead = z }
          )
  );
  if X.verbose then
    Trie.verbose()

(* Produce a warning if the grammar uses the [error] pseudo-token. *)

let () =
  if !grammar_uses_error then
    Error.warning []
      "--list-errors ignores all productions that involve the error token."

(* ------------------------------------------------------------------------ *)

(* The module [T] maintains a set of known facts. *)

(* Three aspects of a fact are of particular interest:
   - its position [position], given by [fact.position];
   - its first symbol [a], given by [W.first fact.word fact.lookahead];
   - its lookahead assumption [z], given by [fact.lookahead].

   For every triple of [position], [a], and [z], we store at most one fact,
   (whose word has minimal length). Indeed, we are not interested in keeping
   track of several words that produce the same effect. Only the shortest such
   word is of interest.
   
   Thus, the total number of facts accumulated by the algorithm is at most
   [T.n^2], where [T] is the total size of the tries that we have constructed,
   and [n] is the number of terminal symbols. (This number can be quite large.
   [T] can be in the tens of thousands, and [n] can be over one hundred. These
   figures lead to a theoretical upper bound of 100M. In practice, for T=25K
   and n=108, we observe that the algorithm gathers about 7M facts.) *)

module T : sig

  (* [register fact] registers the fact [fact]. It returns [true] if this fact
     is new, i.e., no fact concerning the same triple of [position], [a], and
     [z] was previously known. *)
  val register: fact -> bool

  (* [query current z f] enumerates all known facts whose current state is
     [current] and whose lookahead assumption is compatible with [z]. The
     symbol [z] must a real terminal symbol, i.e., cannot be [any]. *)
  val query: Lr1.node -> Terminal.t -> (fact -> unit) -> unit

  (* [size()] returns the number of facts currently stored in the set. *)
  val size: unit -> int

  (* [verbose()] outputs debugging & performance information. *)
  val verbose: unit -> unit

end = struct

  (* We need to query the set of facts in two ways. In [register], we must test
     whether a proposed triple of [position], [a], [z] already appears in the
     set. In [query], we must find all facts that match a pair [current, z],
     where [current] is a state. (Note that [position] determines [current], but
     the converse is not true: a position contains more information besides the
     current state.)

     To address these needs, we use a two-level table. The first level is a
     matrix indexed by [current] and [z]. At the second level, we find sets of
     facts, where two facts are considered equal if they have the same triple of
     [position], [a], and [z]. In fact, we know at this level that all facts
     have the same [z] component, so only [position] and [a] are compared.

     Because our facts satisfy invariant 2, [z] is [any] if and only if the
     state [current fact] is solid. This means that we are wasting quite a
     lot of space in the matrix (for a solid state, the whole line is empty,
     except for the [any] column). *)

  (* The level-2 sets. *)

  module M =
    MySet.Make(struct
      type t = fact
      let compare fact1 fact2 =
        assert (fact1.lookahead = fact2.lookahead);
        let c = Trie.compare fact1.position fact2.position in
        if c <> 0 then c else
        let z = fact1.lookahead in
        let a1 = W.first fact1.word z
        and a2 = W.first fact2.word z in
        (* note: [a1] and [a2] can be [any] here *)
        Terminal.compare a1 a2
    end)

  (* The level-1 matrix. *)

  let table =
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
    assert (not (Terminal.equal z any));
    (* If the state [current] is solid then the facts that concern it are
       stored in the column [any], and all of them are compatible with [z].
       Otherwise, they are stored in all columns except [any], and only
       those stored in the column [z] are compatible with [z]. *)
    let i = index current (if is_solid current then any else z) in
    let m = table.(i) in
    M.iter f m

  let size () =
    !count

  let verbose () =
    Printf.eprintf "T stores %d facts.\n%!" (size())

end

(* ------------------------------------------------------------------------ *)

(* The module [E] is in charge of recording the non-terminal edges that we have
   discovered, or more precisely, the conditions under which these edges can be
   taken.
   
   It maintains a set of quadruples [s, nt, w, z], where such a quadruple means
   that in the state [s], the outgoing edge labeled [nt] can be taken by
   consuming the word [w], under the assumption that the next symbol is [z].

   Again, the terminal symbol [a], given by [W.first w z], plays a role. For
   each quadruple [s, nt, a, z], we store at most one quadruple [s, nt, w, z].
   Thus, internally, we maintain a mapping of [s, nt, a, z] to [w].

   For greater simplicity, we do not allow [z] to be [any] in [register] or
   [query]. Allowing it would complicate things significantly, it seems. *)

module E : sig

  (* [register s nt w z] records that, in state [s], the outgoing edge labeled
     [nt] can be taken by consuming the word [w], if the next symbol is [z].
     It returns [true] if this information is new, i.e., if the underlying
     quadruple [s, nt, a, z] is new. The symbol [z] cannot be [any]. *)
  val register: Lr1.node -> Nonterminal.t -> W.word -> Terminal.t -> bool

  (* [query s nt a z] enumerates all words [w] such that, in state [s], the
     outgoing edge labeled [nt] can be taken by consuming the word [w], under
     the assumption that the next symbol is [z], and the first symbol of the
     word [w.z] is [a]. The symbol [a] can be [any]. The symbol [z] cannot be
     [any]. *)
  val query: Lr1.node -> Nonterminal.t -> Terminal.t -> Terminal.t ->
             (W.word -> unit) -> unit

  (* [size()] returns the number of edges currently stored in the set. *)
  val size: unit -> int

  (* [verbose()] outputs debugging & performance information. *)
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

  let table =
    Array.init Lr1.n (fun i ->
      let size = Trie.size i in
      H.create (if size = 1 then 0 else Terminal.n * size)
    )

  let index s =
    Lr1.number s

  let pack nt a z : int =
    (* We rely on the fact that we have at most 256 terminal symbols. *)
    (Nonterminal.n2i nt lsl 16) lor
    (Terminal.t2i a lsl 8) lor
    (Terminal.t2i z)

  let count = ref 0

  let register s nt w z =
    assert (Terminal.real z);
    let i = index s in
    let m = table.(i) in
    let a = W.first w z in
    (* Note that looking at [a] in state [s] cannot cause an error. *)
    assert (not (causes_an_error s a));
    let key = pack nt a z in
    if H.mem m key then
      false
    else begin
      incr count;
      H.add m key w;
      true
    end

  let rec query s nt a z f =
    assert (Terminal.real z);
    if Terminal.equal a any then begin
      (* If [a] is [any], we query the table for every real symbol [a].
         We can limit ourselves to symbols that do not cause an error
         in state [s]. Those that do certainly do not have an entry;
         see the assertion in [register] above. *)
      foreach_terminal_not_causing_an_error s (fun a ->
        query s nt a z f
      )
    end
    else begin
      let i = index s in
      let m = table.(i) in
      let key = pack nt a z in
      match H.find m key with
      | w -> f w
      | exception Not_found -> ()
    end

  let size () =
    !count

  let verbose () =
    Printf.eprintf "E stores %d edges.\n%!" (size())

end

(* ------------------------------------------------------------------------ *)

(* [new_edge s nt w z] is invoked when we discover that in the state [s], the
   outgoing edge labeled [nt] can be taken by consuming the word [w], under
   the assumption that the next symbol is [z]. We check whether this quadruple
   already exists in the set [E]. If not, then we add it, and we compute its
   consequences, in the form of new facts, which we insert into the priority
   queue for later examination. *)

let new_edge s nt w z =
  assert (Terminal.real z);
  if E.register s nt w z then
    let sym = Symbol.N nt in
    (* Query [T] for existing facts which could be extended by following
       this newly discovered edge. They must be facts whose current state
       is [s] and whose lookahead assumption is compatible with [a]. For
       each such fact, ... *)
    T.query s (W.first w z) (fun fact ->
      assert (compatible fact.lookahead (W.first w z));
      (* ... try to take one step in the trie along an edge labeled [nt]. *)
      match Trie.step sym fact.position with
      | position ->
          (* This takes up to a new state whose incoming symbol is [nt].
             Hence, this state is not solid. In order to satisfy invariant 2,
             we must create fact whose lookahead assumption is not [any].
             That's fine, since our lookahead assumption is [z]. In order to
             satisfy invariant 1, we must check that [z] does not cause an
             error in this state. *)
          assert (not (is_solid (Trie.current position)));
          if not (causes_an_error (Trie.current position) z) then
            let word = W.append fact.word w in
            add { position; word; lookahead = z }
      | exception Not_found ->
          (* Could not take a step in the trie. This means this branch
             leads nowhere of interest, and was pruned when the trie
             was constructed. *)
          ()
    )

(* ------------------------------------------------------------------------ *)

(* [new_fact fact] is invoked when we discover a new fact (i.e., one that was
   not previously known). It studies the consequences of this fact. These
   consequences are of two kinds:

   - As in Dijkstra's algorithm, the new fact can be viewed as a newly
     discovered vertex. We study its (currently known) outgoing edges,
     and enqueue new facts in the priority queue.

   - Sometimes, a fact can also be viewed as a newly discovered edge. This is
     the case when the word that took us from [fact.source] to [fact.current]
     represents a production of the grammar and [fact.current] is willing to
     reduce this production. We record the existence of this edge, and
     re-inspect any previously discovered vertices which are interested in
     this outgoing edge. *)

let new_fact fact =

  let current = current fact in

  (* 1. View [fact] as a vertex. Examine the transitions out of [current].
     For every transition labeled by a symbol [sym] and into a state
     [target], ... *)
  
  Lr1.transitions current |> SymbolMap.iter (fun sym target ->
    (* ... try to follow this transition in the trie [fact.position],
       down to a child which we call [position]. *)
    match Trie.step sym fact.position, sym with

    | exception Not_found ->

        (* Could not take a step in the trie. This means this transition
           leads nowhere of interest. *)
        ()

    | position, Symbol.T t ->
          
        (* 1a. The transition exists in the trie, and [sym] is in fact a
           terminal symbol [t]. We note that [t] cannot be the [error] token,
           because the trie does not have any edges labeled [error]. *)
        assert (Lr1.Node.compare (Trie.current position) target = 0);
        assert (is_solid target);
        assert (non_error t);

        (* If the lookahead assumption [fact.lookahead] is compatible with
           [t], then we derive a new fact, where one more edge has been taken,
           and enqueue this new fact for later examination. *)
        
        (* The state [target] is solid, i.e., its incoming symbol is terminal.
           This state is always entered without consideration for the next
           lookahead symbol. Thus, we can use [any] as the lookahead assumption
           in the new fact that we produce. If we did not have [any], we would
           have to produce one fact for every possible lookahead symbol. *)

        if compatible fact.lookahead t then
          let word = W.append fact.word (W.singleton t) in
          add { position; word; lookahead = any }

    | position, Symbol.N nt ->

        (* 1b. The transition exists in the trie, and [sym] is in fact a
           nonterminal symbol [nt]. *)
         assert (Lr1.Node.compare (Trie.current position) target = 0);
         assert (not (is_solid target));

        (* We need to know how this nonterminal edge can be taken. We query
           [E] for a word [w] that allows us to take this edge. In general,
           the answer depends on the terminal symbol [z] that comes *after*
           this word: we try all such symbols. We must make sure that the
           first symbol of the word [w.z] satisfies the lookahead assumption
           [fact.lookahead]; this is ensured by passing this information to
           [E.query]. *)

        (* It could be the case that, due to a default reduction, the answer
           to our query does not depend on [z], and we are wasting work.
           However, allowing [z] to be [any] in [E.query], and taking 
           advantage of this to increase performance, seems difficult. *)

        foreach_terminal_not_causing_an_error target (fun z ->
          E.query current nt fact.lookahead z (fun w ->
            assert (compatible fact.lookahead (W.first w z));
            let word = W.append fact.word w in
            add { position; word; lookahead = z }
          )
        )

  );

  (* 2. View [fact] as a possible edge. This is possible if the path from
     [fact.source] to the [current] state represents a production [prod] and
     [current] is willing to reduce this production. Then, reducing [prod]
     takes us all the way back to [fact.source]. Thus, this production gives
     rise to an edge labeled [nt] -- the left-hand side of [prod] -- out of
     [fact.source]. *)

  let z = fact.lookahead in
  if not (Terminal.equal z any) then begin

    (* 2a. The lookahead assumption [z] is a real terminal symbol. We check
       whether [current] is willing to reduce some production [prod] on [z],
       and whether the sub-trie [fact.position] accepts [prod], which means
       that this reduction takes us back to the root of the trie. If so, we
       have discovered a new edge. *)

    match has_reduction current z with
    | Some prod when Trie.accepts prod fact.position ->
        new_edge (source fact) (Production.nt prod) fact.word z
    | _ ->
        ()

  end
  else begin

    (* 2b. The lookahead assumption is [any]. We must consider every pair
       [prod, z] such that the [current] state can reduce [prod] on [z]
       and [fact.position] accepts [prod]. *)

    match Invariant.has_default_reduction current with
    | Some (prod, _) ->
        if Trie.accepts prod fact.position then
          (* [new_edge] does not accept [any] as its 4th parameter, so we
             must iterate over all terminal symbols. *)
          foreach_terminal (fun z ->
            new_edge (source fact) (Production.nt prod) fact.word z
          )
    | None ->
       TerminalMap.iter (fun z prods ->
         if non_error z then
           let prod = Misc.single prods in
           if Trie.accepts prod fact.position then
             new_edge (source fact) (Production.nt prod) fact.word z
       ) (Lr1.reductions current)

  end

(* ------------------------------------------------------------------------ *)

(* The main loop of the algorithm. *)

(* [level] is the length of [fact.word] for the facts that we are examining
   at the moment. [extracted] counts how many facts we have extracted out of
   the priority queue. [considered] counts how many of these were found to
   be new, and subsequently passed to [new_fact]. *)

let level, extracted, considered =
  ref 0, ref 0, ref 0

let done_with_level () =
  Printf.eprintf "Done with level %d.\n" !level;
  W.verbose();
  T.verbose();
  E.verbose();
  Printf.eprintf "Q stores %d facts.\n" (Q.cardinal q);
  Printf.eprintf "%d facts extracted out of Q, of which %d considered.\n%!"
    !extracted !considered

let () =
  Q.repeat q (fun fact ->
    incr extracted;
    if T.register fact then begin
      if X.verbose && W.length fact.word > !level then begin
        done_with_level();
        level := W.length fact.word;
      end;
      incr considered;
      new_fact fact
    end
  );
  if X.verbose then
    done_with_level();
  Time.tick "Running LRijkstra"

(* ------------------------------------------------------------------------ *)

(* The following code validates the fact that an error can be triggered in
   state [s'] by beginning at the start symbol [nt] and reading the
   sequence of terminal symbols [w]. We use this for debugging purposes.
   Furthermore, this gives us a list of spurious reductions, which we use
   to produce a comment. *)

let fail msg =
  Printf.eprintf "LRijkstra: internal error: %s.\n%!" msg;
  exit 1

let validate nt s' w : ReferenceInterpreter.target =
  let open ReferenceInterpreter in
  match
    check_error_path nt (W.elements w)
  with
  | OInputReadPastEnd ->
      fail "input was read past its end"
  | OInputNotFullyConsumed ->
      fail "input was not fully consumed"
  | OUnexpectedAccept ->
      fail "input was unexpectedly accepted"
  | OK ((state, _) as target) ->
      if Lr1.Node.compare state s' <> 0 then
        fail (
          Printf.sprintf "error occurred in state %d instead of %d"
            (Lr1.number state)
            (Lr1.number s')
        )
      else
        target

(* ------------------------------------------------------------------------ *)

(* We now wish to determine, given a state [s'] and a terminal symbol [z], a
   minimal path that takes us from some entry state to state [s'] with [z] as
   the next (unconsumed) symbol. *)

(* This can be formulated as a search for a shortest path in a graph. The
   graph is not just the automaton, though. It is a (much) larger graph whose
   vertices are pairs [s, z] and whose edges are obtained by querying the
   module [E] above. For this purpose, we use Dijkstra's algorithm,
   unmodified. Experiments show that the running time of this phase is
   typically 10x shorter than the running time of the main loop above. *)

module A = Astar.Make(struct

  (* A vertex is a pair [s, z], where [z] is a real terminal symbol. *)
  type node =
      Lr1.node * Terminal.t

  let equal (s'1, z1) (s'2, z2) =
    Lr1.Node.compare s'1 s'2 = 0 && Terminal.compare z1 z2 = 0

  let hash (s, z) =
    Hashtbl.hash (Lr1.number s, z)

  (* An edge is labeled with a word. *)
  type label =
    W.word

  (* We search forward from every [s, z], where [s] is an initial state. *)
  let sources f =
    foreach_terminal (fun z ->
      ProductionMap.iter (fun _ s ->
        f (s, z)
      ) Lr1.entry
    )

  (* The successors of [s, z] are defined as follows. *)
  let successors (s, z) edge =
    assert (Terminal.real z);
    (* For every transition out of [s], labeled [sym], leading to [s']... *)
    Lr1.transitions s |> SymbolMap.iter (fun sym s' ->
      match sym with
      | Symbol.T t ->
          if Terminal.equal z t then
            (* If [sym] is the terminal symbol [z], then this transition
               matches our lookahead assumption, so we can take it. For
               every [z'], we have an edge to [s', z'], labeled with the
               singleton word [z]. *)
            let w = W.singleton z in
            foreach_terminal (fun z' ->
              edge w 1 (s', z')
            )
      | Symbol.N nt ->
          (* If [sym] is a nonterminal symbol [nt], then we query [E]
             in order to find out which (minimal) words [w] allow us
             to take this transition. We must again try every [z'],
             and must respect the constraint that the first symbol
             of the word [w.z'] is [z]. For every [z'] and [w] that
             fulfill these requirements, we have an edge to [s', z'],
             labeled with the word [w]. *)
         foreach_terminal (fun z' ->
           E.query s nt z z' (fun w ->
             edge w (W.length w) (s', z')
           )
         )
    )

  (* Algorithm A*, used with a zero estimate, is Dijkstra's algorithm.
     We have experimented with a non-zero estimate, but the performance
     increase was minimal. *)
  let estimate _ =
    0

end)

(* ------------------------------------------------------------------------ *)

(* [explored] counts how many graph nodes we have discovered during the
   search. *)

let explored =
  ref 0

(* We wish to store a set of triples [nt, w, (s', spurious)], meaning that an
   error can be triggered in state [s'] by beginning in the initial state that
   corresponds to [nt] and by reading the sequence of terminal symbols [w]. We
   wish to store at most one such triple for every state [s'], so we organize
   the data as a set [domain] of states [s'] and a list [data] of triples [nt,
   w, (s', spurious)]. The list [spurious] documents the spurious reductions
   that are performed by the parser at the end. *)

(* We could print this data as we go, which would naturally result in sorting
   the output by increasing word sizes. However, it seems preferable to sort
   the sentences lexicographically, so that similar sentences end up close to
   one another. (We could also sort them by state number. The result would be
   roughly similar.) This is why we store a list of triples and sort it before
   printing it out. *)

let domain =
  ref Lr1.NodeSet.empty

let data : (Nonterminal.t * W.word * ReferenceInterpreter.target) list ref =
  ref []

(* The set [reachable] stores every reachable state (regardless of whether an
   error can be triggered in that state). *)

let reachable =
  ref Lr1.NodeSet.empty

(* Perform the forward search. *)

let _, _ =
  A.search (fun ((s', z), path) ->
    incr explored;
    reachable := Lr1.NodeSet.add s' !reachable;
    (* If [z] causes an error in state [s'] and this is the first time
       we are able to trigger an error in this state, ... *)
    if causes_an_error s' z && not (Lr1.NodeSet.mem s' !domain) then begin
      (* Reconstruct the initial state [s] and the word [w] that lead
         to this error. *)
      let (s, _), ws = A.reverse path in
      let w = List.fold_right W.append ws (W.singleton z) in
      (* Check that the reference interpreter confirms our finding.
         At the same time, compute a list of spurious reductions. *)
      let nt = Lr1.nt_of_entry s in
      let target = validate nt s' w in
      (* Store this new data. *)
      domain := Lr1.NodeSet.add s' !domain;
      data := (nt, w, target) :: !data
    end
  )

(* Sort and output the data. *)

let () =
  !data
  |> List.fast_sort (fun (nt1, w1, _) (nt2, w2, _) ->
    let c = Nonterminal.compare nt1 nt2 in
    if c <> 0 then c else W.compare w2 w1
  )
  |> List.map (fun (nt, w, target) -> (nt, W.elements w, target))
  |> List.iter Interpret.print_messages_item

(* ------------------------------------------------------------------------ *)

(* Verbosity. *)

let max_heap_size =
  if X.verbose || X.statistics <> None then
    let stat = Gc.quick_stat() in
    (stat.Gc.top_heap_words * (Sys.word_size / 8) / 1024 / 1024)
  else
    0 (* dummy *)

let () =
  Time.tick "Forward search";
  if X.verbose then begin
    Printf.eprintf
      "%d graph nodes explored by forward search.\n\
       %d out of %d states are reachable.\n\
       Found %d states where an error can occur.\n\
       Maximum size reached by the major heap: %dM\n%!"
    !explored
    (Lr1.NodeSet.cardinal !reachable) Lr1.n
    (Lr1.NodeSet.cardinal !domain)
    max_heap_size
  end

(* ------------------------------------------------------------------------ *)

(* If requested by the client, write one line of statistics to a .csv file. *)

let stop =
  now()

let () =
  X.statistics |> Option.iter (fun filename ->
    let c = open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 filename in
    Printf.fprintf c
      "%s,%d,%d,%d,%d,%d,%d,%d,%.2f,%d\n%!"
      (* Grammar name. *)
      Settings.base
      (* Number of terminal symbols. *)
      Terminal.n
      (* Number of nonterminal symbols. *)
      Nonterminal.n
      (* Grammar size (not counting the error productions). *)
      begin
        Production.foldx (fun prod accu ->
          let rhs = Production.rhs prod in
          if List.mem (Symbol.T Terminal.error) (Array.to_list rhs) then
            accu
          else
            accu + Array.length rhs
        ) 0
      end
      (* Automaton size (i.e., number of states). *)
      Lr1.n
      (* Cumulated trie size. *)
      (Trie.cumulated_size())
      (* Size of [T]. *)
      (T.size())
      (* Size of [E]. *)
      (E.size())
      (* Elapsed user time, in seconds. *)
      (stop -. start)
      (* Max heap size, in megabytes. *)
      max_heap_size
    ;
    close_out c
  )

(* ------------------------------------------------------------------------ *)

end

