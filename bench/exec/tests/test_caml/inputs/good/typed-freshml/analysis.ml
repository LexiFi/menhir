open Printf
open Print
open Source
open Typed
open Anf
open ExtendedSetConstraint

(* ------------------------------------------------------------------------- *)

(* Building the mapping of variables to values that arises out of a
   structured tuple. *)

let mapof tuple =
   Layout.zip tuple

let zip xs vs =
  List.fold_right2 Var.AtomMap.add xs vs Var.AtomMap.empty

(* ------------------------------------------------------------------------- *)

(* Instantiating a function's precondition or postcondition with values. *)

let precondition callee =
  let xs, pre, _, _ = Primitive.specification callee in
  fun vs ->
    substitute (zip xs vs) pre

let postcondition callee =
  let xs, _, ys, post = Primitive.specification callee in
  fun vs ws ->
    substitute (zip xs vs) (substitute (zip ys ws) post)

(* ------------------------------------------------------------------------- *)

(* Proof obligations have origins that must be reported to the user when
   a proof fails. *)

type origin =
  | OFresh
  | OClaim
  | OPre of callee
  | OPost of callee
  | OGuard of tag

(* We combine assumptions, environment, and goal in a single structure, which
   we refer to as ``judgements''. *)

type judgement = {

    (* We map value variables to unique strings, for printing. The
       domain of this map is also used when generating freshness
       hypotheses for a newly introduced name. *)

    name: Var.AtomIdMap.t;

    (* Among our assumptions is a conjunction of equations between
       values. *)

    equations: Unify.state;

    (* Among our assumptions is a conjunction of (positive and
       negative) set constraints. *)

    hypotheses: contrainte;

    (* Our goal is a conjunction of set constraints that is abstracted
       over a vector of values. Each goal has a specific arity. *)

    goal: value list -> (origin Location.t, contrainte) Annotation.t list;

    (* This is the source code location of the value, pattern, or
       expression that is being examined. *)

    location: unit Location.t

  }

(* ------------------------------------------------------------------------- *)

(* Operations over judgements. *)

(* [print j] is a variable printer. *)

let print j buffer v =
  try
    bprintf buffer "%s" (Annotation.content (Var.AtomIdMap.find v j.name))
  with Var.Atom.Unknown _ ->
    assert false

(* [empty] is an empty judgement. *)

let empty = {
  name = Var.AtomIdMap.empty;
  equations = Unify.empty;
  hypotheses = FTrue;
  goal = (fun _ -> []);
  location = Location.vdummy
}

(* [locate j e] changes the location carried by [j] to reflect that
   of [e]. *)

let locate j e =
  { j with location = Annotation.transfer e () }

(* [alsorequire j origin p] introduces a new goal predicate in
   addition to the existing one. *)

let alsorequire j (origin : origin) (p : value list -> contrainte list) =
  let goal vs =
    List.map (Annotation.make (Annotation.transfer j.location origin)) (p vs) @ j.goal vs
  in
  { j with goal = goal }

(* [require j origin p] replaces the goal with the predicate [p]. *)

let require j (origin : origin) (p : value list -> contrainte list) =
  alsorequire { j with goal = fun _ -> [] } origin p

let require1 j (origin : origin) (p : value list -> contrainte) =
  require j origin (fun vs -> [ p vs ])

(* [introduce1 j x] brings the name [x] into scope. This means, in
   particular, that future ``fresh'' names will be considered fresh
   for [x].

   We look up the type of [x] and determine whether values of this
   type are known to have nonempty support. If so, we add this
   hypothesis. *)

let introduce1 j x =
   { j with
     name = Var.AtomIdMap.add x j.name;
     hypotheses =
       if Lemmas.has_nonempty_support (Anf.typeof x) then
	 conjunction [ nonempty (supportof x); j.hypotheses ]
       else
	 j.hypotheses
   }

let introduce j xs =
  List.fold_left introduce1 j xs

(* [assume1 j c] adds the constraint [c] to the assumptions. *)

let assume1 j c =
  { j with
    hypotheses = conjunction [ c; j.hypotheses ] }

let assume j cs =
  List.fold_left assume1 j cs

(* [freshen j s] adds assumptions expressing the fact that the set [s]
   is fresh with respect to all previous objects. It also adds a proof
   obligation stating that [s] should be fresh for the values that are
   eventually produced. *)

let freshen j s =
  let world =
    union (
      Var.AtomIdMap.fold (fun y _ world ->
	supportof y :: world
      ) j.name []
    )
  in
  let j = assume1 j (disjoint s world) in
  let j = alsorequire j OFresh (fun vs -> [ disjoint s (supportofvs vs) ]) in
  j

(* [fresh1 j x] brings the name [x] into scope and freshens (the
   support of) [x]. *)

let fresh1 j x =
  introduce1 (freshen j (supportof x)) x

let fresh j xs =
  List.fold_left fresh1 j xs

(* [equate j vs1 vs2] adds the equations [vs1 = vs2] to the assumptions. *)

let equate j vs1 vs2 =
  { j with
    equations = List.fold_left2 Unify.unify j.equations vs1 vs2 }

(* [create xs pre ys post] is the toplevel judgement that is associated
   with a function declaration. *)

let create f xs pre ys post =
  let j = assume1 (introduce empty xs) pre in
  let j = locate j (Valfun.Atom.basename f) in
  let goal vs = [ substitute (zip ys vs) post ] in
  require j (OPost (CUser f)) goal

(* ------------------------------------------------------------------------- *)

(* [report location goal msg] reports the error message [msg], which
   corresponds to failure of the goal [goal] at location [location]. *)

let report location goal msg =
   Error.signal location (msg ^ "The reason why I am attempting to prove this assertion is...");
   let origin = Annotation.get goal in
   Error.signal origin
     begin match Annotation.content origin with
     | OFresh ->
	 "A freshly created atom must not appear in the support of the\n\
	  value that is eventually produced.\n"
     | OClaim ->
	 "It is part of a user-provided claim.\n"
     | OPre f ->
	 sprintf "It is part of the precondition for function %s.\n" (Primitive.print_callee f)
     | OPost f ->
	 sprintf "It is part of the postcondition for function %s.\n" (Primitive.print_callee f)
     | OGuard tag ->
	 sprintf "It is part of the invariant for data constructor %s.\n" (SymbolTable.tagname tag)
     end

(* [annotate location goal] creates a pair of an error reporting 
   function and a goal. *)

let annotate location goal =
  report location goal, Annotation.content goal

(* ------------------------------------------------------------------------- *)

(* [valid j vs] checks that the judgement [j] is valid of the values
   [vs] -- that is, the assumptions do imply the application of the
   goal to [vs]. *)

(* TEMPORARY change this API if unsuitable *)

let valid (j : judgement) (vs : value list) : unit =
  let goals = j.goal vs in
  let goals = List.map (annotate j.location) goals in
  entailment (print j) (j.equations, j.hypotheses, GConjunction goals)

let absurd (j : judgement) : unit =
  entailment (print j) (j.equations, j.hypotheses, GFalsity (Error.signal j.location))

(* ------------------------------------------------------------------------- *)

(* Matching against a pattern enriches the current judgement with new
   variables, assumptions, and proof obligations. The judgement is
   threaded through the pattern as the pattern is examined. *)

let rec check_pattern (j : judgement) (p : pattern) : judgement =
  match p with
  | VVar x ->
      introduce1 j x
  | VBool _ ->
      j
  | VTagTuple (_, tuple, guard) ->
      let j = locate j guard in
      let guard = Annotation.content guard in
      let j = assume1 j (substitute (mapof tuple) guard) in
      check_tuple j tuple

and check_tuple j tuple =
  match tuple with
  | SComponent (_, _, p) ->
      check_pattern j p
  | SInner tuple
  | SOuter tuple ->
      check_tuple j tuple
  | SAbstraction tuple ->
      let _, _, bound = oib tuple in
      check_tuple (freshen j bound) tuple
  | STuple tuples ->
      List.fold_left check_tuple j tuples

and check_patterns j ps =
  List.fold_left check_pattern j ps

(* ------------------------------------------------------------------------- *)

(* Checking values. *)

let rec check_value (j : judgement) (v : value) : unit =
  match v with
  | VVar _
  | VBool _ ->
      ()
  | VTagTuple (tag, tuple, guard) ->
      Layout.iter (check_value j) tuple; (* proceed bottom-up *)
      let j = locate j guard in
      let goal _ = substitute (mapof tuple) (Annotation.content guard) in
      valid
	(require1 j (OGuard tag) goal)
	[]

and check_values j vs =
  List.iter (check_value j) vs

(* ------------------------------------------------------------------------- *)

(* Checking expressions. *)

let vvar x =
  VVar x

let vvars xs =
  List.map vvar xs

let rec check (j : judgement) (e : expression) : unit =
  let j = locate j e in
  match Annotation.content e with
  | EValue vs ->
      check_values j vs;
      valid j vs
  | EFresh (xs, e) ->
      check (fresh j xs) e
  | ELetCall (xs, f, vs, e) ->
      check_values j vs;
      valid (require1 j (OPre f) (precondition f)) vs;
      let j = introduce j xs in
      let j = assume1 j (postcondition f vs (vvars xs)) in
      (* the following postcondition comes for free *)
      let j = assume1 j (subset (supportofvs (vvars xs)) (supportofvs vs)) in
      check j e
  | ELetWhere (xs, claim, e1, e2) ->
      let goal vs = substitute (zip xs vs) claim in
      check (require1 j OClaim goal) e1;
      check (assume1 (introduce j xs) claim) e2
  | EAssert (StaticAssertion, claim, e) ->
      let goal _ = claim in
      valid (require1 j OClaim goal) [];
      check (assume1 j claim) e
  | EAssert (DynamicAssertion, claim, e) ->
      check (assume1 j claim) e
  | ECase (vs, branches) ->
      check_values j vs;
      List.iter (fun (ps, e) ->
	check (equate (check_patterns j ps) ps vs) e
      ) branches
  | EAbsurd ->
      absurd j
  | EFail
  | ENextCase ->
      ()

(* ------------------------------------------------------------------------- *)

(* Examining a function definition. *)

let check_fundef (f : valfun) (xs, pre, ys, post, e : fundef) =
  check (create f xs pre ys post) e

(* ------------------------------------------------------------------------- *)

(* Examining the program. *)

let () =
  Valfun.AtomMap.iter check_fundef Anf.fundefs;
  ExtendedSetConstraint.stats();
  Error.signaled()

(* TEMPORARY proprifier tout ca. Ne pas compter les obligations de preuve
   quand le but est true! *)

