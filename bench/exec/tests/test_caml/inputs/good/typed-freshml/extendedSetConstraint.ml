open Source
open Typed

(* ------------------------------------------------------------------------- *)

(* Facilities for building set expressions and constraints. *)

(* These smart constructors perform some simplifications on the fly. The goal
   is not to gain efficiency, but to construct simpler constraints, which is
   useful because these constraints may be displayed to the user. *)

let union xs =
  let xs = List.filter (function SEEmpty -> false | _ -> true) xs in
  match xs with
  | [] ->
      SEEmpty
  | [ x ] ->
      x
  | _ :: _ :: _ ->
      SEAssocOp (OpUnion, xs)

let intersection xs =
  match xs with
  | [] ->
      assert false
  | [ x ] ->
      x
  | _ :: _ :: _ ->
      SEAssocOp (OpIntersection, xs)

let difference xs =
  match xs with
  | [] ->
      assert false
  | SEEmpty :: _ ->
      SEEmpty
  | x :: xs ->
      let xs = List.filter (function SEEmpty -> false | _ -> true) xs in
      match xs with
      | [] ->
	  x
      | _ ->
	  SEAssocOp (OpDifference, x :: xs)

let subset s1 s2 =
  match s1 with
  | SEEmpty ->
      FTrue
  | _ ->
      FSetBinOp (s1, OpSubset, s2)

let equal s1 s2 =
  match s1, s2 with
  | SEEmpty, SEEmpty ->
      FTrue
  | _ ->
      FSetBinOp (s1, OpEqual, s2)

let notequal s1 s2 =
  FSetBinOp (s1, OpNotEqual, s2)

let nonempty s =
  match s with
  | SEEmpty ->
      FFalse
  | _ ->
      notequal s SEEmpty

let disjoint s1 s2 =
  match s1, s2 with
  | SEEmpty, _
  | _, SEEmpty ->
      FTrue
  | _ ->
      FSetBinOp (s1, OpDisjoint, s2)

let conjunction cs =
  if List.exists (function FFalse -> true | _ -> false) cs then
    FFalse
  else
    let cs = List.filter (function FTrue -> false | _ -> true) cs in
    match cs with
    | [] ->
	FTrue
    | [ c ] ->
	c
    | _ ->
	FBoolAssocOp (OpConjunction, cs)

let iff cs =
  match cs with
  | []
  | [ _ ] ->
      assert false
  | _ ->
      FBoolAssocOp (OpEquivalence, cs)

(* ------------------------------------------------------------------------- *)

(* Building applications of set functions. If the value argument is
   not a variable, then the application is rewritten. *)

(* [apply] reduces the application of the set function [f] to the
   value [v]. *)

let rec apply (f, v : set_function * value) : set_expression =
  match f, v with
  | SFSupport, _ ->
      supportofv v
  | SFOuter, _ ->
      outerofv v
  | SFInner, _ ->
      innerofv v
  | SFBound, _ ->
      boundofv v

(* ------------------------------------------------------------------------- *)

(* Here is the definition of the [support] set function. *)

and support tuple =
  match tuple with
  | SComponent (_, _, v) ->
      supportofv v
  | SInner tuple
  | SOuter tuple ->
      support tuple
  | SAbstraction tuple ->
      let outer, inner, bound = oib tuple in
      union [ outer; difference [ inner; bound ]]
  | STuple tuples ->
      union (List.map support tuples)

and supportof x =
  supportofv (VVar x)

and supportofv v =

  (* If the type of the value [v] is such that all values of this
     type have empty support, then return an empty set expression.
     The strength of this check is that it is applicable also to
     variables. *)

  if Lemmas.has_empty_support (Anf.typeofv v) then
    SEEmpty

  (* Otherwise, examine the structure of [v]. *)

  else
    match v with
    | VVar x ->
	SEApp (SFSupport, x)
    | VBool _ ->
	assert false (* already dealt with above *)
    | VTagTuple (_, tuple, _) ->
	support tuple

and supportofvs vs =
  union (List.map supportofv vs)

(* ------------------------------------------------------------------------- *)

(* Here is the definition of the [outer], [inner], [bound] set functions. *)

(* One invariant is that the terms outer(x), inner(x), and bound(x)
   are created only when x has a user-defined type -- not when x has
   atom type, because, in that case, both outer(x) and inner(x) are
   empty and bound(x) is support(x). *)

(* TEMPORARY prevent the user from using outer, inner, or bound at atoms. *)
(* TEMPORARY well, outer and inner are empty at atoms, and bound is support.
             why not axiomatize that? *)

and oib tuple = 
  match tuple with
  | SComponent (_, typ, v) ->
      begin match Lazy.force typ with
      | TData _ ->
	  outerofv v, innerofv v, boundofv v
      | TAtom
      | TAtomSet ->
	  SEEmpty, SEEmpty, supportofv v
      | TBool ->
	  SEEmpty, SEEmpty, SEEmpty
      end
  | SInner tuple ->
      SEEmpty, support tuple, SEEmpty
  | SOuter tuple ->
      support tuple, SEEmpty, SEEmpty
  | SAbstraction _ ->
      assert false (* abstractions cannot be nested *)
  | STuple tuples ->
      let os, is, bs =
	List.fold_left (fun (os, is, bs) tuple ->
	  let o, i, b = oib tuple in
	  o :: os, i :: is, b :: bs
        ) ([], [], []) tuples
      in
      union os, union is, union bs

and outerof x =
  SEApp (SFOuter, x)

and outerofv v =
  if Lemmas.has_empty_outer (Anf.typeofv v) then
    SEEmpty
  else
    match v with
    | VVar x ->
	outerof x
    | VBool _ ->
	assert false
    | VTagTuple (_, tuple, _) ->
	let outer, _, _ = oib tuple in
	outer

and innerof x =
  SEApp (SFInner, x)

and innerofv v =
  if Lemmas.has_empty_inner (Anf.typeofv v) then
    SEEmpty
  else
    match v with
    | VVar x ->
	innerof x
    | VBool _ ->
	assert false
    | VTagTuple (_, tuple, _) ->
	let _, inner, _ = oib tuple in
	inner

and boundof x =
  SEApp (SFBound, x)

and boundofv v =
  if Lemmas.has_empty_bound (Anf.typeofv v) then
    SEEmpty
  else
    match v with
    | VVar x ->
	boundof x
    | VBool _ ->
	SEEmpty
    | VTagTuple (_, tuple, _) ->
	let _, _, bound = oib tuple in
	bound

(* ------------------------------------------------------------------------- *)

(* We must axiomatize the fact that [free(x) == bound(x) U inner(x) U
   outer(x)] if [x] has pattern type. This is stronger than a
   conjunction of individual axioms of the form [bound(x) <= free(x)],
   etc. *)

let fbio_axiom x : contrainte =
   let v = VVar x in
   equal
     (supportofv v)
     (union [ outerofv v; innerofv v; boundofv v ])

(* ------------------------------------------------------------------------- *)

(* Substitution of values for variables within constraints. *)

class subst (subst : value Var.AtomMap.t) = object

  inherit map

  (* Substituting values for variables within entities. *)

  method seapp (f, y) =
    begin try
      apply (f, Var.AtomMap.find y subst)
    with Not_found ->
      SEApp (f, y)
    end

  (* Substituting values for variables within Boolean formulae. *)

  method fboolvar x =
    try
      match Var.AtomMap.find x subst with
      | VVar y ->
	  FBoolVar y
      | VBool true ->
	  FTrue
      | VBool false ->
	  FFalse
      | _ ->
	  assert false (* no other value form has type [bool] *)
    with Not_found ->
      FBoolVar x

end

let substitute subst =
  (new subst subst)#contrainte

(* ------------------------------------------------------------------------- *)

(* Entailment problems. *)

(* A goal is either falsity or a conjunction of constraints, each of
   which is associated with an error reporting function. A problem is
   a triple of a set of value equations (hypotheses), a conjunction of
   unannotated constraints (more hypotheses) and a goal. *)

type reporter =
    string -> unit

type goal =
  | GFalsity of reporter
  | GConjunction of (reporter * contrainte) list

type problem =
  Unify.state * contrainte * goal

(* ------------------------------------------------------------------------- *)

(* Translating extended set constraints down to ordinary set constraints. *)

open Print

let successes, failures =
  ref 0, ref 0

module Entailment (P : sig

  (* An entailment problem. *)

  val facts: Unify.state
  val hypotheses: contrainte
  val goal: goal

  (* How to display value variables. *)

  val print: var printer

end) = struct

(* ------------------------------------------------------------------------- *)

(* Instantiate [Show] and define how entailment problems should be printed. *)

module Show =
   Show.Make (P)

open Printf
open Show

let print_goal b = function
  | GFalsity _ ->
      bprintf b "false"
  | GConjunction acs ->
      List.iter (fun (_, c) ->
	print_toplevel_constraint b c
      ) acs

let print_failed_problem hypotheses goal b =
  bprintf b "I am unable to discharge the following proof obligation.\n\n\
             The hypotheses are:\n%a\n\
             The goal is:\n%a\n"
    print_toplevel_constraint hypotheses
    print_goal goal

let goal_count = function
  | GFalsity _ ->
      1
  | GConjunction acs ->
      List.length acs

(* ------------------------------------------------------------------------- *)

  (* Preparation for translation of set expressions and constraints. *)

  (* Let us map entities down to set variables. *)

  module EntityMap =
    AtomAllocator.Make (struct
      type thing = set_entity
      module Map =
	Map.Make(struct
	  type t = set_entity
	  let compare = Order.lexicographic_pair Pervasives.compare Var.Atom.compare
	end)
      let print = print_set_entity
    end) (SetConstraint.SetVar)

  (* We maintain a queue of entities whose defining equations are
     awaiting emission. *)

  let queue : set_entity Queue.t =
    Queue.create()

  (* We maintain a set of variables whose [fbio] axioms are awaiting
     emission. *)

  let fbio_vars : Var.AtomSet.t ref =
    ref Var.AtomSet.empty

  (* We also translate values of [bool] type down to Boolean
     formulae. *)

  module BooleanMap = AtomAllocator.Make (struct
    type thing = var
    module Map = Var.AtomMap
    let print = P.print
  end) (Boolean.Var)

  (* [translate_entity entity] maps the entity [entity] down to a set
     variable, updating the table and the queue if required. *)

  let rec translate_entity (f, x as entity) =
    let known = EntityMap.known entity in
    let setvar = EntityMap.image entity in
    if not known then begin
      Queue.add entity queue;
      match f with
      | SFSupport ->
	  ()
      | SFBound
      | SFInner
      | SFOuter ->
	  fbio_vars := Var.AtomSet.add x !fbio_vars
    end;
    setvar

(* ------------------------------------------------------------------------- *)

  (* Translate set expressions and set constraints. *)

  let translate_boolean_variable x =
    SetConstraint.boolean (BooleanMap.image x)

  let interpret_boolean_value (v : value) : contrainte =
    match v with
    | VVar x ->
	FBoolVar x
    | VBool true ->
	FTrue
    | VBool false ->
	FFalse
    | _ ->
	assert false (* no other value form has type [bool] *)

  let leftassoc op xs =
    match xs with
    | [] ->
	assert false
    | x :: xs ->
	List.fold_left op x xs

  let rightassoc op xs =
    leftassoc (fun x y -> op y x) (List.rev xs)

  let translate_setsetset_operator = function
    | OpUnion ->
	leftassoc SetConstraint.union
    | OpIntersection ->
	leftassoc SetConstraint.inter
    | OpDifference ->
	leftassoc SetConstraint.diff

  let rec translate_set_expression = function
    | SEEmpty ->
	SetConstraint.empty
    | SEApp entity ->
	SetConstraint.var (translate_entity entity)
    | SEAssocOp (op, ss) ->
	translate_setsetset_operator op (List.map translate_set_expression ss)
    | SEConditional (c, s1, s2) ->
	SetConstraint.conditional
	  (translate_constraint c)
	  (translate_set_expression s1)
	  (translate_set_expression s2)

  and translate_setsetbool_operator = function
    | OpSubset ->
	SetConstraint.subset
    | OpEqual ->
	SetConstraint.equal
    | OpNotEqual ->
	SetConstraint.notequal
    | OpDisjoint ->
	SetConstraint.disjoint

  and translate_boolboolbool_operator = function
    | OpConjunction ->
	leftassoc Propositional.conjunction
    | OpDisjunction ->
	leftassoc Propositional.disjunction
    | OpImplication ->
	rightassoc Propositional.implication
    | OpEquivalence ->
	leftassoc Propositional.iff

  and translate_constraint = function
    | FTrue ->
	Propositional.truth
    | FFalse ->
	Propositional.falsity
    | FBoolVar x ->
	translate_boolean_variable x
    | FNot f ->
	Propositional.negation (translate_constraint f)
    | FBoolAssocOp (op, cs) ->
	translate_boolboolbool_operator op (List.map translate_constraint cs)
    | FSetBinOp (s1, op, s2) ->
	translate_setsetbool_operator op
	  (translate_set_expression s1)
	  (translate_set_expression s2)

  let translate_goal = function
    | GFalsity _ ->
	Propositional.falsity
    | GConjunction acs ->
	Propositional.conjunction_map (fun (_, c) ->
	  translate_constraint c
        ) acs

(* ------------------------------------------------------------------------- *)

  (* We are now in a position to translate the entailment problem. We
     initially translate the hypotheses and goal. This populates the
     queue with requests for translating entities, which we then deal
     with iteratively, until the queue is exhausted.

     The value equations [facts] are taken into account in two ways.
     First, if [x] is related to [v], then every entity [f(x)] that is
     built is related to [f(v)]. Second, if [x] is related to [v] and
     if [x] has type [bool], then the equation [x = v] is translated
     directly to a Boolean formula. *)

  let outcome : bool =
    match Unify.exploit P.facts with
    | Unify.Inconsistent ->
	true
    | Unify.Consistent related ->

	(* In the code that follows, we maintain two versions of the
	   hypotheses: one in the source language, one in the target
	   language. The former serves for printing; the latter is
	   passed to the decision procedure. *)

        let hypotheses =
	  P.hypotheses, translate_constraint P.hypotheses
	and goal =
	  P.goal, translate_goal P.goal
	in

	let emit c (h, th) =
	  conjunction [ c; h ],
	  Propositional.conjunction (translate_constraint c) th
	in

	(* Emit equations that reflect the semantics of the built-in
	   set functions. *)

	let hypotheses =
	  Misc.qfold (fun hypotheses (f, x) ->

	    (* If [x] is related to a value [v], equate [f(x)] with
	       the expansion of [f(v)]. *)

	    (* Note that the call to [translate_constraint] can feed
	       new requests for translating entities into the queue. *)

	    match related x with
	    | None ->
		hypotheses
	    | Some v ->
		emit (equal (SEApp (f, x)) (apply (f, v))) hypotheses

          ) hypotheses queue
	in

	(* Emit all [fbio] axioms. *)
	
	let hypotheses =
	  Var.AtomSet.fold (fun x hypotheses ->

	    emit (fbio_axiom x) hypotheses

          ) !fbio_vars hypotheses
	in

	(* Reflect all known equations at type [bool]. *)

	let hypotheses =
	  BooleanMap.fold (fun x _ hypotheses ->
	    assert (Anf.typeof x = TBool);

	    match related x with
	    | None ->
		hypotheses
	    | Some v ->
		emit (iff [ FBoolVar x; interpret_boolean_value v ]) hypotheses

	  ) hypotheses
	in

	(* Forward the request to the ordinary set constraint solver.
	   If that fails and if there is a single reporter function,
	   then display the problem in the syntax of the source
	   language. (The case where there are multiple reporter
	   functions is dealt with below.) *)

	let h, th = hypotheses
	and g, tg = goal in

	if SetConstraint.entailment th tg then begin
	  successes := !successes + goal_count P.goal;
	  true
	end
	else
	  match P.goal with
	  | GFalsity reporter
	  | GConjunction [ reporter, _ ] ->
	      failures := !failures + 1;
	      let report = Print.ws (print_failed_problem h g) in
	      reporter report;
	      false
	  | GConjunction acs ->
	      assert (acs <> []);
	      false

end

(* ------------------------------------------------------------------------- *)

(* Turn this functor into a function. *)

let entailment printv (facts, hypotheses, goal) : bool =
  match goal with
  | GConjunction [] ->
      true
  | _ ->
      let module E = Entailment (struct
	let facts = facts
	let hypotheses = hypotheses
	let goal = goal
	let print = printv
      end) in
      E.outcome

(* Add a wrapper that implements individual retries when a
   multiple-goal request fails. *)

let entailment printv problem : bool =
  entailment printv problem ||
  match problem with
  | _, _, GFalsity _
  | _, _, GConjunction [ _ ] ->
      false
  | facts, hypotheses, GConjunction acs ->
      assert (acs <> []);
      List.fold_left (fun accu ac ->
	let outcome = entailment printv (facts, hypotheses, GConjunction [ ac ]) in
	accu && outcome (* intentional use of strict conjunction, so that all subgoals are retried *)
      ) true acs

(* Add a wrapper that decomposes conjunctions in the goal, so as
   to make the individual retry process more fine-grained. *)

let decompose goal =
  match goal with
  | GFalsity _ ->
      goal
  | GConjunction acs ->
      GConjunction (
	List.flatten (List.map (fun (reporter, c) ->
	  match c with
	  | FTrue ->
	      []
	  | FBoolAssocOp (OpConjunction, cs) ->
	      List.map (fun c -> (reporter, c)) cs
	  | _ ->
	      [ reporter, c ]
	) acs)
      )

let entailment printv (facts, hypotheses, goal) =
  entailment printv (facts, hypotheses, decompose goal)

(* Add a wrapper that drops the outcome. The check's only observable
   effect is to display an error message when it fails. *)

let entailment printv problem : unit =
  let (_ : bool) = entailment printv problem in
  ()

(* ------------------------------------------------------------------------- *)

(* Satistics. *)

open Printf

let stats () =
  if !successes > 0 then
    fprintf stderr "Successfully discharged %d proof obligations.\n%!" !successes;
  if !failures > 0 then
    fprintf stderr "Failed to fulfill %d proof obligations.\n%!" !failures;
  Cnf.stats()

