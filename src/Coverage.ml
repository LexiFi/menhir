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

open Grammar

(* Using [CompletedNatWitness] means that we wish to compute shortest paths.
   An alternative would be to use [BooleanWitness], which offers the same
   interface. That would mean we wish to compute an arbitrary path. That
   would be faster, but the paths thus obtained are (according to a quick
   experiment) really far from optimal. *)
module P =
  CompletedNatWitness
       
(* TEMPORARY avoid [error] token unless forced to use it *)
(* TEMPORARY implement and exploit [Lr1.ImperativeNodeMap] using an array *)

type property =
  Terminal.t P.t

(* This tests whether state [s] has a default reduction on [prod]. *)

let has_default_reduction s prod =
  match Invariant.has_default_reduction s with
  | Some (prod', _) ->
      prod = prod'
  | None ->
      false

(* This tests whether state [s] is willing to reduce production [prod]
   when the lookahead symbol is [z]. This test takes a possible default
   reduction into account. *)

let reductions s z =
  try
    TerminalMap.find z (Lr1.reductions s)
  with Not_found ->
    []

let has_reduction s prod z : bool =
  has_default_reduction s prod ||
  List.mem prod (reductions s z)

(* This tests whether state [s] has an outgoing transition labeled [sym].
   If so, [s'] is passed to the continuation [k]. Otherwise, [P.bottom] is
   returned. *)

let has_transition s sym k : property =
  try
    let s' = SymbolMap.find sym (Lr1.transitions s) in
    k s'
  with Not_found ->
    P.bottom

(* This tests whether state [s] will initiate an error on the lookahead
   symbol [z]. *)

let causes_an_error s z =
  not (Terminal.equal z Terminal.error) &&
  match Invariant.has_default_reduction s with
  | Some _ ->
      false
  | None ->
      reductions s z = [] &&
      not (SymbolMap.mem (Symbol.T z) (Lr1.transitions s))

(* This computes [FIRST(alpha.z)], where [alpha] is the suffix determined
   by [prod] and [i]. *)

let first prod i z =
  let nullable, first = Analysis.nullable_first_prod prod i in
  if nullable then
    TerminalSet.add z first
  else
    first

(* This computes a minimum over a set of terminal symbols. *)

let foreach_terminal_in toks (f : Terminal.t -> property) : property =
  TerminalSet.fold (fun t accu ->
    P.min_lazy accu (fun () -> f t)
  ) toks P.bottom

let foreach_terminal_until_finite (f : Terminal.t -> property) : property =
  Terminal.fold (fun t accu ->
    (* We stop as soon as we obtain a finite result. *)
    P.until_finite accu (fun () -> f t)
  ) P.bottom

(* This computes a minimum over the productions associated with [nt]. *)

let foreach_production nt (f : Production.index -> property) : property =
  Production.foldnt nt P.bottom (fun prod accu ->
    P.min_lazy accu (fun () -> f prod)
  )

(* The automaton, viewed as a graph, whose vertices are states.
   We label each edge with the minimum length of a word that it
   generates. This allows us to compute a lower bound on the
   distance to every state from any entry state. *)

module ForwardAutomaton = struct

  type vertex =
    Lr1.node

  let equal s1 s2 =
    Lr1.Node.compare s1 s2 = 0

  let hash s =
    Hashtbl.hash (Lr1.number s)

  type label =
    int

  let weight w = w

  let sources f =
    (* The sources are the entry states. *)
    ProductionMap.iter (fun _ s -> f s) Lr1.entry

  let successors edge s =
    SymbolMap.iter (fun sym s' ->
      edge (P.to_int (Analysis.minimal_symbol sym)) s'
    ) (Lr1.transitions s)

end

let approximate : Lr1.node -> int =
  let module D = Dijkstra.Make(ForwardAutomaton) in
  D.search (fun (_, _, _) -> ())

let () =
  Lr1.iter (fun s ->
    Printf.fprintf stderr
      "State %d is at least %d steps away from an initial state.\n%!"
      (Lr1.number s) (approximate s)
  )

(* A question takes the form [s, a, prod, i, z], as defined below.

   Such a question means: what is the set of terminal words [w]
   (or, rather, what is a minimal word [w])
   such that:

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

type question = {
  s: Lr1.node;
  a: Terminal.t;
  prod: Production.index;
  i: int;
  z: Terminal.t;
}

let print_question q =
  Printf.fprintf stderr
    "{ s = %d; a = %s; prod/i = %s; z = %s }\n"
    (Lr1.number q.s)
    (Terminal.print q.a)
    (Item.print (Item.import (q.prod, q.i)))
    (Terminal.print q.z)

module Question = struct
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
end

module QuestionMap =
  Map.Make(Question)

(* The following function answers a question. This requires a fixed point
   computation. We have a certain amount of flexibility in how much
   information we memoize; if we use a recursive call to [answer], we
   re-compute; if we use a call to [get], we memoize. *)

let answer (q : question) (get : question -> property) : property =

  let rhs = Production.rhs q.prod in
  let n = Array.length rhs in
  assert (0 <= q.i && q.i <= n);

  (* According to conditions 2 and 3, the answer to this question is the empty
     set unless [a] is in [FIRST(prod/i.z)]. Thus, by convention, we will ask
     this question only when this precondition is satisfied. *)
  assert (TerminalSet.mem q.a (first q.prod q.i q.z));

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

    (* Case 2. The suffix determined by [prod] and [i] begins with a symbol
       [sym]. The state [s] must have an outgoing transition along [sym];
       otherwise, no word exists. *)

    let sym = rhs.(q.i) in
    has_transition q.s sym (fun s' ->
      match sym with
      | Symbol.T t ->

          (* Case 2a. [sym] is a terminal symbol [t]. Our precondition
             implies that [t] is equal to [a]. [w] must begin with [a]
             The rest must be some word [w'] such that, by starting from
             [s'] and by reading [w'], we reach our goal. The first letter
             in [w'] could be any terminal symbol [c], so we try all of
             them. *)

          assert (Terminal.equal q.a t); (* per our precondition *)
          P.add (P.singleton q.a) (
            foreach_terminal_in (first q.prod (q.i + 1) q.z) (fun c ->
              get { s = s'; a = c; prod = q.prod; i = q.i + 1; z = q.z }
            )
          )

      | Symbol.N nt ->

          (* Case 2b. [sym] is a nonterminal symbol [nt]. For each letter [c],
             for each production [prod'] associated with [nt], we must
             concatenate: 1- a word that takes us from [s], beginning with [a],
             to a state where we can reduce [prod'], looking at [c]; and 2- a
             word that takes us from [s'], beginning with [c], to a state where
             we reach our original goal. *)

          foreach_terminal_in (first q.prod (q.i + 1) q.z) (fun c ->
            foreach_production nt (fun prod' ->
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

(* Debugging wrapper. TEMPORARY *)
let qs = ref 0
let answer q get =
  incr qs;
  if !qs mod 10000 = 0 then
    Printf.fprintf stderr "qs = %d\n%!" !qs;
  answer q get

module F =
  Fix.Make
    (Maps.PersistentMapsToImperativeMaps(QuestionMap))
    (struct
      include P
      type property = Terminal.t t
     end)

let answer : question -> property =
  F.lfp answer

let es = ref 0

exception Success of property
let backward (s', z) : property =
  let module G = struct
    type vertex = Lr1.node * Terminal.t
    let equal (s'1, z1) (s'2, z2) =
      Lr1.Node.compare s'1 s'2 = 0 && Terminal.compare z1 z2 = 0
    let hash (s, z) = Hashtbl.hash (Lr1.number s, z)
    type label = int * Terminal.t Seq.seq
    let weight (w, _) = w
    let sources f = f (s', z)
    let successors edge (s', z) =
      match Lr1.incoming_symbol s' with
      | None ->
          (* A start symbol has no predecessors. *)
          ()
      | Some (Symbol.T t) ->
          List.iter (fun pred ->
            edge (1, Seq.singleton t) (pred, t)
          ) (Lr1.predecessors s')
      | Some (Symbol.N nt) ->
          List.iter (fun pred ->
            Production.foldnt nt () (fun prod () ->
              TerminalSet.iter (fun a ->
                match answer { s = pred; a = a; prod = prod; i = 0; z = z } with
                | P.Infinity ->
                    ()
                | P.Finite (w, ts) ->
                    edge (w, ts) (pred, a)
              ) (first prod 0 z)
            )
          ) (Lr1.predecessors s')
  end in
  let module D = Dijkstra.Make(G) in
  try
    let _ = D.search (fun (distance, (v', _), path) ->
      incr es;
      if !es mod 10000 = 0 then
        Printf.fprintf stderr "es = %d\n%!" !es;
      if Lr1.incoming_symbol v' = None then
        let path = List.map snd path in
        raise (Success (P.Finite (distance, Seq.concat path))) (* TEMPORARY keep path *)
    ) in
    P.bottom
  with Success p ->
    p

let backward s' : property =
  
  Printf.fprintf stderr
    "Attempting to reach an error in state %d:\n%!"
    (Lr1.number s');

  foreach_terminal_until_finite (fun z ->
    if causes_an_error s' z then
      P.add (backward (s', z)) (P.singleton z)
    else
      P.bottom
  )

(* Test. *)

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

(* TEMPORARY
   does it make sense to use A*?
   use [MINIMAL] and [Dijkstra] to compute an under-approximation of the distance
   of any state [s'] to an entry state
   It should work very well, because most of the time this should be a very good
   under-approximation (it should be equal to the true distance).
*)
(* TEMPORARY
   also: first compute an optimistic path using the simple algorithm
   and check if this path is feasible in the real automaton
*)
