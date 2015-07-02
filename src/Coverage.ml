module P = CompletedNatWitness
open Grammar

(* TEMPORARY check no symbol produces the empty language *)

type property =
  Terminal.t P.t

(* This tests whether state [s] is willing to reduce production [prod]
   when the lookahead symbol is [z]. *)

let has_reduction s prod z : bool =
  let prods =
    try
      TerminalMap.find z (Lr1.reductions s)
    with Not_found ->
      []
  in
  List.mem prod prods

(* This tests whether state [s] has an outgoing transition labeled [sym].
   If so, [s'] is passed to the continuation [k]. Otherwise, [P.bottom] is
   returned. *)

let has_transition s sym k : property =
  try
    let s' = SymbolMap.find sym (Lr1.transitions s) in
    k s'
  with Not_found ->
    P.bottom

(* This computes [FIRST(alpha.z)], where [alpha] is the suffix determined
   by [prod] and [i]. *)

let first prod i z =
  let nullable, first = Analysis.nullable_first_prod prod i in
  if nullable then
    TerminalSet.add z first
  else
    first

(* This computes a minimum over a set of terminal symbols. *)

let foreach_terminal (f : Terminal.t -> property) : property =
  Terminal.fold (fun t accu ->
    (* A feeble attempt at being slightly lazy. Not essential. *)
    P.min_lazy accu (lazy (f t))
  ) P.bottom

let foreach_terminal_in toks (f : Terminal.t -> property) : property =
  TerminalSet.fold (fun t accu ->
    (* A feeble attempt at being slightly lazy. Not essential. *)
    P.min_lazy accu (lazy (f t))
  ) toks P.bottom

(* This computes a minimum over the productions associated with [nt]. *)

let foreach_production nt (f : Production.index -> property) : property =
  Production.foldnt nt P.bottom (fun prod accu ->
    (* A feeble attempt at being slightly lazy. Not essential. *)
    P.min_lazy accu (lazy (f prod))
  )

(* A question takes the form [s, a, prod, i, z, goal], as defined below.

   Such a question means: what is the set of terminal words [w]
   (or, rather, what is a minimal word [w])
   such that:

   1- the production suffix [prod/i] generates the word [w], if the goal is [Reduce];
      the production suffix [prod/i] generates a word that begins with [w], if the goal is [Reach _].

   2- [a] is in [FIRST(w.z)]
      i.e. either [w] is not epsilon and [a] is the first symbol in [w]
           or [w] is epsilon and [a] is [z].

   3- if, starting in state [s], the LR(1) automaton consumes [w] and looks at [z],
      then it reaches a state [s'] that matches the goal, i.e.:
      - either the goal is [Reduce]
        and [s'] is willing to reduce [prod] when looking at [z]
      - or the goal is [Reach s'].

   The necessity for the parameter [z] arises from the fact that a state may
   have a reduction for some, but not all, values of [z]. (After conflicts
   have been resolved, we cannot predict which reduction actions we have.)
   This in turn makes the parameter [a] necessary: when trying to analyze a
   concatenation [AB], we must try all terminal symbols that come after [A]
   and form the beginning of [B]. *)

type goal =
| Reach of Lr1.node
| Reduce

type question = {
  s: Lr1.node;
  a: Terminal.t;
  prod: Production.index;
  i: int;
  z: Terminal.t;
  goal: goal
}

let print_goal = function
  | Reach s ->
      Printf.sprintf "reach %d" (Lr1.number s)
  | Reduce ->
      "reduce"

let print_question q =
  Printf.fprintf stderr
    "{ s = %d; a = %s; prod/i = %s; z = %s; goal = %s }\n"
    (Lr1.number q.s)
    (Terminal.print q.a)
    (Item.print (Item.import (q.prod, q.i)))
    (Terminal.print q.z)
    (print_goal q.goal)

let lexico cmp x1 x2 rest =
  let c = cmp x1 x2 in
  if c <> 0 then c else Lazy.force rest

let compare_goals goal1 goal2 =
  match goal1, goal2 with
  | Reach s1, Reach s2 ->
      Lr1.Node.compare s1 s2
  | Reach _, Reduce ->
      -1
  | Reduce, Reach _ ->
      1
  | Reduce, Reduce ->
      0

module Question = struct
  type t = question
  let compare q1 q2 =
    lexico Lr1.Node.compare q1.s q2.s (lazy (
      lexico Terminal.compare q1.a q2.a (lazy (
        lexico Production.compare q1.prod q2.prod (lazy (
          lexico Pervasives.compare q1.i q2.i (lazy (
            lexico Terminal.compare q1.z q2.z (lazy (
              compare_goals q1.goal q2.goal
            ))
          ))
        ))
      ))
    ))
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

  (* If our goal is to reach a certain state, check if by any chance we have
     achieved it. In that case, we cut short and return the empty word. *)

  let happy =
    match q.goal with
    | Reduce ->
        false
    | Reach s ->
        Lr1.Node.compare q.s s = 0 &&
        Terminal.equal q.a q.z (* TEMPORARY ? *)
  in
  if happy then
    P.epsilon

  else begin

(* TEMPORARY
    (* According to conditions 2 and 3, the answer to this question is the empty
       set unless [a] is in [FIRST(prod/i.z)]. Thus, by convention, we will ask
       this question only when this precondition is satisfied. *)
    assert (TerminalSet.mem q.a (first q.prod q.i q.z));
*)

    (* Now, three cases arise: *)
    if q.i = n then begin
    
      (* Case 1. The suffix determined by [prod] and [i] is epsilon. To satisfy
         condition 1, [w] must be the empty word. Condition 2 is implied by our
         precondition. There remains to check whether condition 3 is satisfied.
         If so, we return the empty word; otherwise, no word exists. *)

      let happy =
        Terminal.equal q.a q.z &&        (* condition 2 *)
        match q.goal with                (* condition 3 *)
        | Reduce ->
            has_reduction q.s q.prod q.z
        | Reach _ ->
            false
      in
      if happy then P.epsilon else P.bottom

    end
    else

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

            if Terminal.equal q.a t (* condition 2 *)
            then
              P.add (P.singleton q.a) (
                foreach_terminal_in (first q.prod (q.i + 1) q.z) (fun c ->
                  get { s = s'; a = c; prod = q.prod; i = q.i + 1; z = q.z; goal = q.goal }
                )
              )
            else
              P.bottom

        | Symbol.N nt ->

            (* Case 2b. [sym] is a nonterminal symbol [nt]. For each letter [c],
               for each production [prod'] associated with [nt], we must
               concatenate: 1- a word that takes us from [s], beginning with [a],
               to a state where we can reduce [prod'], looking at [c]; and 2- a
               word that takes us from [s'], beginning with [c], to a state where
               we reach our original goal. *)

            (* TEMPORARY not quite sure that the reduction of [prod'] will take
               us back to state [s], as hoped. *)

            P.min begin

            foreach_terminal_in (first q.prod (q.i + 1) q.z) (fun c ->
              foreach_production nt (fun prod' ->
                P.add_lazy
                  (get { s = q.s; a = q.a; prod = prod'; i = 0; z = c; goal = Reduce })
                  (lazy (get { s = s'; a = c; prod = q.prod; i = q.i + 1; z = q.z; goal = q.goal }))
              )
            )

            end begin

              match q.goal with Reduce -> P.bottom | Reach _ ->
                foreach_production nt (fun prod' ->
                  get { s = q.s; a = q.a; prod = prod'; i = 0; z = q.z; goal = q.goal }
                )

            end

      )

  end

(* Debugging wrapper. TEMPORARY *)
(*
let answer (q : question) (get : question -> property) : property =
  print_question q;
  let p = answer q get in
  Printf.fprintf stderr "%s\n%!" (P.print Terminal.print p);
  p
 *)

(* Debugging wrapper. TEMPORARY *)
let qs = ref 0
let answer q get =
  incr qs;
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

(* How to go from the entry state [s] associated with the start production [prod]
   to the goal state [s']. *)

let path s prod s' =
  foreach_terminal (fun z ->
    P.add (
      foreach_terminal_in (first prod 0 z) (fun a ->
        answer {
          s = s;
          a = a;
          prod = prod;
          i = 0;
          z = z;
          goal = Reach s'
        }
      )
    ) (P.singleton z)
  )

(* Test. *)

let () =
  Lr1.iter (fun s' ->
    Lr1.fold_entry (fun prod s _ _ () ->
      Printf.fprintf stderr "Attempting to go from state %d to state %d:\n%!"
        (Lr1.number s) (Lr1.number s');
      let p = path s prod s' in
      Printf.fprintf stderr "%s\n%!" (P.print Terminal.print p);
      Printf.fprintf stderr "Questions asked so far: %d\n" !qs
    ) ()
  )
(* Note the last symbol in the path plays a special role. It is not consumed. *)
