(* The purpose of this algorithm is to find, for each pair of a state [s]
   and a terminal symbol [z] such that looking at [z] in state [s] causes
   an error, a minimal path (starting in some initial state) that actually
   triggers this error. *)

(* This is potentially useful for grammar designers who wish to better
   understand the properties of their grammar, or who wish to produce a
   list of all possible syntax errors (or, at least, one syntax error in
   each automaton state where an error may occur). *)

(* The problem seems rather tricky. One might think that it suffices to
   compute shortest paths in the automaton, and to use [Analysis.minimal]
   to replace each non-terminal symbol in a path with a minimal word
   that it generates. One can indeed do so, but this yields only a lower
   bound on the actual shortest path to the error at [s, z]. Indeed, two
   difficulties arise:

   - Some states have a default reduction. Thus, they will not trigger
     an error, even though they should. The error is triggered in some
     other state, after reduction takes place.

   - If the grammar has conflicts, conflict resolution removes some
     (shift or reduce) actions, hence may suppress the shortest path. *)

(* We explicitly ignore the [error] token. If the grammar mentions [error]
   and if some state is reachable only via an [error] transition, too bad;
   we report this state as unreachable. It would be too complicated to have
   to create a first error in order to be able to take certain transitions
   or drop certain parts of the input. *)
(* TEMPORARY warn if --coverage is set AND the grammar uses [error] *)

(* TEMPORARY explain how we approach the problem *)

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
           [Grammar.Analysis.minimal_symbol]. *)
        edge () (CompletedNatWitness.to_int (Analysis.minimal_symbol sym)) s'
      ) (Lr1.transitions s)

    let estimate _ =
      (* A* with a zero [estimate] behaves like Dijkstra's algorithm. *)
      0

  end) in
        
  let distance, _ = A.search (fun (_, _) -> ()) in
  distance

(* ------------------------------------------------------------------------ *)

(* Auxiliary functions. *)

(* This tests whether state [s] has a default reduction on [prod]. *)

let has_default_reduction_on s prod =
  match Invariant.has_default_reduction s with
  | Some (prod', _) ->
      prod = prod'
  | None ->
      false

(* This returns the list of reductions of [state] on token [z]. This
   should be a list of zero or one elements. *)

let reductions s z =
  assert (not (Terminal.equal z Terminal.error));
  try
    TerminalMap.find z (Lr1.reductions s)
  with Not_found ->
    []

(* This tests whether state [s] is willing to reduce production [prod]
   when the lookahead symbol is [z]. This test takes a possible default
   reduction into account. *)

let has_reduction s prod z : bool =
  assert (not (Terminal.equal z Terminal.error));
  has_default_reduction_on s prod ||
  List.mem prod (reductions s z)

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

(* ------------------------------------------------------------------------ *)

(* The analysis that follows is formulated as a least fixed point computation.
   The ordering is [CompletedNatWitness]. This represents the natural integers
   completed with infinity. The bottom element is [infinity], which means that
   no path is known, and the computation progresses towards smaller (finite)
   numbers, which means that shorter paths become known. *)

(* Using [CompletedNatWitness] means that we wish to compute shortest paths.
   We could instead use [BooleanWitness], which offers the same interface.
   That would mean we are happy as soon as we know an arbitrary path. That
   would be faster, but (according to a quick experiment) the paths thus
   obtained would be really far from optimal. *)

module P = struct
  include CompletedNatWitness
  type property = Terminal.t t
end

(* ------------------------------------------------------------------------ *)

(* More auxiliary functions for the analysis. *)

(* This tests whether state [s] has an outgoing transition labeled [sym].
   If so, [s'] is passed to the continuation [k]. Otherwise, [P.bottom] is
   returned. *)

let has_transition s sym k : P.property =
  try
    let s' = SymbolMap.find sym (Lr1.transitions s) in
    k s'
  with Not_found ->
    P.bottom

(* This computes a minimum over a set of terminal symbols. *)

exception Stop of P.property

let foreach_terminal_in toks bound (f : Terminal.t -> P.property) : P.property =
  (* Explicitly ignore the [error] token. *)
  let toks = TerminalSet.remove Terminal.error toks in
  (* Fast path. *)
  if TerminalSet.is_singleton toks then
    f (TerminalSet.choose toks)
  else try
    TerminalSet.fold (fun t accu ->
      let accu = P.min accu (f t) in
      match accu with
      | P.Finite (i, _) when i <= bound ->
          raise (Stop accu)
      | _ ->
          accu
    ) toks P.bottom
  with Stop accu ->
    accu

(* This computes a minimum over the productions associated with [nt]. *)

let productions nt =
  Production.foldnt nt [] (fun prod prods -> prod :: prods)

let productions nt =
  List.sort (fun prod1 prod2 ->
    P.compare (Analysis.minimal_prod prod1 0) (Analysis.minimal_prod prod2 0)
  ) (productions nt)

let productions : Nonterminal.t -> Production.index list =
  Obj.magic Misc.tabulate Nonterminal.n (Obj.magic productions) (* TEMPORARY *)

let foreach_production nt bound (f : Production.index -> P.property) : P.property =
  let prods = productions nt in
  match prods with
  (* Fast paths. *)
  | [] ->
      P.bottom
  | [ prod ] ->
      f prod
  | _ ->
  try
    List.fold_left (fun accu prod ->
      let accu = P.min accu (f prod) in
      match accu with
      | P.Finite (i, _) when i <= bound ->
          assert (i = bound);
          raise (Stop accu)
      | _ ->
          accu
    ) P.bottom prods
  with Stop accu ->
    accu

(* ------------------------------------------------------------------------ *)

(* The main analysis. *)

(* This analysis maps questions to properties.
   It is defined as a fixed point computation. *)

(* A question takes the form [s, a, prod, i, z], as defined by the record
   type below.

   [s] is a state. [prod/i] is a production suffix, which appears in the
   closure of state [s]. [a] and [z] are terminal symbols.

   This question means: what is the minimal length of a word [w] such that:

   1- the production suffix [prod/i] generates the word [w].

   2- [a] is in [FIRST(w.z)]
      i.e. either [w] is not epsilon and [a] is the first symbol in [w]
           or [w] is epsilon and [a] is [z].

   3- if, starting in state [s], the LR(1) automaton consumes [w] and looks at [z],
      then it reaches a state that is willing to reduce [prod] when looking at [z].

   The necessity for the parameter [z] arises from the fact that a state may
   have a reduction for some, but not all, values of [z]. (After conflicts
   have been resolved, we cannot predict which reduction actions we have.)
   This in turn makes the parameter [a] necessary: when trying to analyze a
   concatenation [AB], we must try all terminal symbols that come after [A]
   and form the beginning of [B]. *)

(* This analysis is very costly. Indeed, the number of possible questions is
   O(#items) * O(#alphabet^2), where #items is the total number of items in
   (the closure? of) all states of the automaton, and #alphabet is the number
   of terminal symbols. Furthermore, the lattice [CompletedNatWitness] is
   quite high (in fact, it does not have bounded height...) and it can take a
   while before the fixed point is reached. *)

(* The product [Lr1.n * Terminal.n * Terminal.n] is about 12 million when
   dealing with OCaml's grammar, in [--lalr] mode. *)

type question = {
  s: Lr1.node;
  a: Terminal.t;
  prod: Production.index;
  i: int;
  z: Terminal.t;
}

(* Debugging. TEMPORARY *)
let print_question q =
  Printf.fprintf stderr
    "{ s = %d; a = %s; prod/i = %s; z = %s }\n"
    (Lr1.number q.s)
    (Terminal.print q.a)
    (Item.print (Item.import (q.prod, q.i)))
    (Terminal.print q.z)

module QuestionMap =
  Map.Make(struct
    type t = question
    let compare q1 q2 =
      let c = Lr1.Node.compare q1.s q2.s in
      if c <> 0 then c else
      let c = Terminal.compare q1.a q2.a in
      if c <> 0 then c else
      let c = Production.compare q1.prod q2.prod in
      if c <> 0 then c else
      let c = Pervasives.compare q1.i q2.i in
      if c <> 0 then c else
      Terminal.compare q1.z q2.z
  end)

let first =
  Analysis.first_prod_lookahead

(* The following function defines the analysis. *)

(* We have a certain amount of flexibility in how much information we memoize;
   if we use a recursive call to [answer], we re-compute; if we use a call to
   [get], we memoize. As long as every direct recursive call is decreasing,
   either choice is acceptable. A quick experiment suggests that memoization
   everywhere is cost-effective. *)

let answer (q : question) (get : question -> P.property) : P.property =

  let rhs = Production.rhs q.prod in
  let n = Array.length rhs in
  assert (0 <= q.i && q.i <= n);

  (* According to conditions 2 and 3, the answer to this question is the empty
     set unless [a] is in [FIRST(prod/i.z)]. Thus, by convention, we will ask
     this question only when this precondition is satisfied. *)
  assert (TerminalSet.mem q.a (first q.prod q.i q.z));
  (* TEMPORARY ultimately disable this assertion? *)

  (* Now, three cases arise: *)
  if q.i = n then begin

    (* Case 1. The suffix determined by [prod] and [i] is epsilon. To satisfy
       condition 1, [w] must be the empty word. Condition 2 is implied by our
       precondition. There remains to check whether condition 3 is satisfied.
       If so, we return the empty word; otherwise, no word exists. *)

    assert (Terminal.equal q.a q.z); (* per our precondition *)
    if has_reduction q.s q.prod q.z  (* condition 3 *)
    then P.epsilon
    else P.bottom

  end
  else begin

    match Analysis.minimal_prod q.prod q.i with
    | P.Infinity ->
        P.bottom
    | P.Finite (bound, _) ->

        (* Case 2. The suffix determined by [prod] and [i] begins with a symbol
           [sym]. The state [s] must have an outgoing transition along [sym];
           otherwise, no word exists. *)

        let sym = rhs.(q.i) in
        has_transition q.s sym (fun s' ->
          match sym with
          | Symbol.T t ->

              (* Case 2a. [sym] is a terminal symbol [t]. Our precondition implies
                 that [t] is equal to [a]. [w] must begin with [a]. The rest must
                 be some word [w'] such that, by starting from [s'] and by reading
                 [w'], we reach our goal. The first letter in [w'] could be any
                 terminal symbol [c], so we try all of them. *)

              assert (Terminal.equal q.a t); (* per our precondition *)
              assert (1 <= bound);
              P.add (P.singleton q.a) (
                foreach_terminal_in (first q.prod (q.i + 1) q.z) (bound - 1) (fun c ->
                  get { s = s'; a = c; prod = q.prod; i = q.i + 1; z = q.z }
                )
              )

          | Symbol.N nt ->

              (* Case 2b. [sym] is a nonterminal symbol [nt]. For each letter [c],
                 for each production [prod'] associated with [nt], we concatenate:
                 1- a word that takes us from [s], beginning with [a], to a state
                    where we can reduce [prod'], looking at [c];
                 2- a word that takes us from [s'], beginning with [c], to a state
                    where we reach our original goal. *)

              foreach_terminal_in (first q.prod (q.i + 1) q.z) bound (fun c ->
                foreach_production nt bound (fun prod' ->
                  if TerminalSet.mem q.a (first prod' 0 c) then
                    P.add_lazy
                      (get { s = q.s; a = q.a; prod = prod'; i = 0; z = c })
                      (fun () -> get { s = s'; a = c; prod = q.prod; i = q.i + 1; z = q.z })
                  else
                    P.bottom
                )
              )

        )

  end

(* Debugging. TEMPORARY *)
let qs = ref 0
let answer q get =
  incr qs;
  if !qs mod 10000 = 0 then
    Printf.fprintf stderr "qs = %d\n%!" !qs;
  answer q get

(* The fixed point. *)

module F =
  Fix.Make
    (Maps.PersistentMapsToImperativeMaps(QuestionMap))
    (P)

let answer : question -> P.property =
  F.lfp answer

(* ------------------------------------------------------------------------ *)

(* We now wish to determine, given a state [s'] and a terminal symbol [z],
   a minimal path that takes us from some entry state to state [s'] with
   [z] as the next (unconsumed) symbol. *)

(* This can be formulated as a search for a shortest path in a graph. The
   graph is not just the automaton, though. It is a (much) larger graph
   whose vertices are pairs [s, z] and whose edges are obtained by calling
   the expensive analysis above. Because we perform a backward search, from
   [s', z] to any entry state, we use reverse edges, from a state to its
   predecessors in the automaton. *)

(* Debugging. TEMPORARY *)
let es = ref 0

exception Success of Lr1.node * P.property

let backward (s', z) : unit =

  let module A = Astar.Make(struct

    (* A vertex is a pair [s, z]. *)
    type node =
        Lr1.node * Terminal.t

    let equal (s'1, z1) (s'2, z2) =
      Lr1.Node.compare s'1 s'2 = 0 && Terminal.compare z1 z2 = 0

    let hash (s, z) =
      Hashtbl.hash (Lr1.number s, z)

    (* An edge is labeled with a property of the form [Finite (i, pi)],
       that is, a distance [i] and a witness path [pi]. *)
    type label =
      P.property

    (* Backward search from the single source [s', z]. *)
    let sources f = f (s', z)

    let successors (s', z) edge =
      match Lr1.incoming_symbol s' with
      | None ->
          (* An entry state has no predecessor states. *)
          ()

      | Some (Symbol.T t) ->
          (* There is an edge from [s] to [s'] labeled [t] in the automaton.
             Thus, our graph has an edge from [s', z] to [s, t], labeled [t]. *)
          let label = P.singleton t in
          List.iter (fun s ->
            edge label 1 (s, t)
          ) (Lr1.predecessors s')

      | Some (Symbol.N nt) ->
          (* There is an edge from [s] to [s'] labeled [nt] in the automaton.
             For every production [prod] associated with [nt], for every
             letter [a], we query the analysis for a path that begins in [s]
             and leads to reducing [prod] when the lookahead symbol is [z].
             Such a path [w] takes us from [s, a] to [s', z]. Thus, our graph
             has an edge, labeled [w], in the reverse direction. *)
          List.iter (fun s ->
            Production.foldnt nt () (fun prod () ->
              TerminalSet.iter (fun a ->
                let p = answer { s = s; a = a; prod = prod; i = 0; z = z } in
                match p with
                | P.Infinity ->
                    ()
                | P.Finite (w, _) ->
                    edge p w (s, a)
              ) (first prod 0 z)
            )
          ) (Lr1.predecessors s')

    let estimate (s', _z) =
      approximate s'

  end) in

  (* Search backwards from [s', z], stopping as soon as an entry state [s] is
     reached. In that case, return the state [s] and the path that has been
     found. *)

  let _, _ = A.search (fun ((s, _), ps) ->
    (* Debugging. TEMPORARY *)
    incr es;
    if !es mod 10000 = 0 then
      Printf.fprintf stderr "es = %d\n%!" !es;
    (* If [s] is a start state... *)
    if Lr1.incoming_symbol s = None then
      (* [labels] is a list of properties. Projecting onto the second
         component yields a list of paths (sequences of terminal symbols),
         which we concatenate to obtain a path. Because the edges that were
         followed last are in front of the list, and because this is a
         reverse graph, we obtain a path that makes direct sense: it is a
         sequence of terminal symbols that will take the automaton into
         state [s'] if the next (unconsumed) symbol is [z]. We append [z]
         at the end of this path. *)
      let p = List.fold_right P.add ps (P.singleton z) in
      raise (Success (s, p))
  ) in
  ()

(* ------------------------------------------------------------------------ *)

(* The following code validates the fact that an error can be triggered in
   state [s'] by beginning in the initial state [s] and reading the
   sequence of terminal symbols [p]. We use this for debugging purposes. *)

let fail msg =
  Printf.fprintf stderr "coverage: internal error: %s.\n%!" msg;
  false

open ReferenceInterpreter

let validate s s' p : bool =
  match
    ReferenceInterpreter.check_error_path (Lr1.nt_of_entry s) (P.extract p)
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

(* For each state [s'] and for each terminal symbol [z] such that [z] triggers
   an error in [s'], backward search is performed. For each state [s'], we
   stop as soon as one [z] is found, i.e., as soon as one way of causing an
   error in state [s'] is found. *)

let backward s' : P.property =
  
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
    P.bottom

  with Success (s, p) ->
    (* An error can be triggered in state [s'] by beginning in the initial
       state [s] and reading the sequence of terminal symbols [p]. *)
    assert (validate s s' p);
    p

(* Test. TEMPORARY *)

let () =
  Lr1.iter (fun s' ->
    let p = backward s' in
    Printf.fprintf stderr "%s\n%!" (P.print Terminal.print p);
    let approx = approximate s'
    and real = P.to_int p - 1 in
    assert (approx <= real);
    if approx < real && real < max_int - 1 then
        Printf.fprintf stderr "Approx = %d, real = %d\n" approx real;
    Printf.fprintf stderr "Questions asked so far: %d\n" !qs;
    Printf.fprintf stderr "Edges so far: %d\n" !es
  )

(* TEMPORARY what about the pseudo-token [#]? *)
(* TEMPORARY the code in this module should run only if --coverage is set *)
