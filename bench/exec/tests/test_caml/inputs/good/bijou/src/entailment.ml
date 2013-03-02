open Annotation
open Source
open SourceConstraint
open Facts
open SymbolTable
open Typedefs

(* ------------------------------------------------------------------------- *)

(* Entailment problems. *)

(* A goal is either falsity or a conjunction of constraints, each of
   which is associated with an error reporting function. *)

(* A problem is a quadruple of a type environment (variables in
   scope), a set of value equations (hypotheses), a conjunction of
   unannotated constraints (more hypotheses) and a goal. *)

type reporter =
    string -> unit

type goal =
  | GFalsity of reporter
  | GConjunction of (reporter * contrainte) list

type problem =
  Types.environment * Value.equations * contrainte * goal

(* ------------------------------------------------------------------------- *)

(* Lifting a transformation of constraints into a transformation of goals. *)

let goal_map f goal =
  match goal with
  | GFalsity _ ->
      goal
  | GConjunction acs ->
      GConjunction (List.map (fun (reporter, c) ->
	reporter, f c
      ) acs)

(* ------------------------------------------------------------------------- *)

(* Evaluation of the built-in set functions. *)

(* We focus on a single sort at a time, because this makes things
   simpler, obviating the need to manipulate sets of sorts. *)

(* We require a type environment that maps every variable to a type.
   This, in particular, means that all variables must denote values;
   they cannot denote value tuples. (Variables that denote value
   tuples must have been substituted out in an earlier step.) *)

module Eval
   (Focus : sig val sort: sorte end)
   (E : sig val env: Types.environment end)
= struct

(* Determining whether a sort or a list of sorts is or contains the
   focus sort. *)

let is_focus sort =
  Sorte.Atom.equal Focus.sort sort

let has_focus sorts =
  List.exists is_focus sorts

let mode_of sorts =
  if has_focus sorts then MBinding else MNormal

(* Determining the type of a value. *)

let typeof v =
  Value.typeof E.env v

(* ------------------------------------------------------------------------- *)

(* [apply_v mode sf v] evaluates the application of the set function [sf]
   to the value [v]. *)

(* The [mode] parameter is the mode of the type of [v] with respect to
   the focus sort. It is an optional parameter, and is required only
   when [v] has type [atom], where it helps decide whether [bound(v)]
   should evaluate to [free(v)] or to [empty]. This parameter is
   supplied by [apply_v] to itself during recursive calls, so that the
   interpretation of the [atom] type varies according to the
   context. External callers should just supply [None]. *)

let rec apply_v mode sf v =

  (* If the type of the value [v] is such that all values of this type have an
     empty image under [sf], then return an empty set expression. The strength
     of this check is that it is applicable also to variables. *)

  let typ = typeof v in
  let facts = Facts.typ Focus.sort (typeof v) in
  let fact = Facts.project sf facts in
  if not fact.can then
    empty

  (* Otherwise, examine the structure of [v]. *)

  else
    match v with
    | VVar x ->

	(* If [v] is a variable, examine its type. When the [mode]
	   parameter is absent, the set functions [inner], [outer],
	   and [bound] are defined only at algebraic data types. When
	   [mode] is present, they are defined also at type [atom]; in
	   that case, they are converted either to applications of
	   [support] or to [empty]. *)

	begin match Location.content typ with
	| TAtom sort
	| TAtomSet sort ->
	    assert (Sorte.Atom.equal sort Focus.sort); (* due to initial emptiness check *)
	    begin match sf, mode with
	    | SFSupport, _
	    | SFBound, Some MBinding ->
		app_at Focus.sort SFSupport x
	    | SFBound, Some MNormal ->
		empty
	    | SFBound, None ->
		assert false (* illegal call *)
	    | SFInner, _
	    | SFOuter, _ ->
		assert false (* due to initial emptiness check *)
	    end
	| TBool ->
	    assert false (* due to initial emptiness check *)
	| TData _ ->
	    app_at Focus.sort sf x
	end

    | VBool _ ->
	assert false (* due to initial emptiness check *)

    | VData (tag, vs) ->
	apply_vt sf (Value.tag_structure tag vs)

(* [apply_vt sf tuple] evaluates the application of the set function
   [sf] to the value tuple [tuple]. *)

and apply_vt sf tuple =
  match sf with
  | SFSupport ->
      support_vt tuple
  | SFInner ->
      inner_vt tuple
  | SFOuter ->
      outer_vt tuple
  | SFBound ->
      bound_vt tuple

(* ------------------------------------------------------------------------- *)

(* The [support] set function. *)

and support_vt tuple =
  match tuple with
  | VTComponent v ->
      apply_v None (* TEMPORARY incorrect *) SFSupport v
  | VTAbstraction (sorts, tuple) when has_focus sorts ->
      union [ outer_vt tuple; difference [ inner_vt tuple; bound_vt tuple ]]
  | VTInner (_, tuple)
  | VTOuter (_, tuple)
  | VTAbstraction (_, tuple) ->
      support_vt tuple
  | VTTuple tuples ->
      union (List.map support_vt tuples)

(* ------------------------------------------------------------------------- *)

(* The [inner] set function. *)

and inner_vt tuple =
  match tuple with
  | VTComponent v ->
      apply_v None (* TEMPORARY incorrect *) SFInner v
  | VTInner (sorts, tuple) when has_focus sorts ->
      support_vt tuple
  | VTOuter (sorts, _) when has_focus sorts ->
      empty
  | VTInner (_, tuple)
  | VTOuter (_, tuple)
  | VTAbstraction (_, tuple) ->
      inner_vt tuple
  | VTTuple tuples ->
      union (List.map inner_vt tuples)

(* ------------------------------------------------------------------------- *)

(* The [outer] set function. *)

and outer_vt tuple =
  match tuple with
  | VTComponent v ->
      apply_v None (* TEMPORARY incorrect *) SFOuter v
  | VTInner (sorts, _) when has_focus sorts ->
      empty
  | VTOuter (sorts, tuple) when has_focus sorts ->
      support_vt tuple
  | VTInner (_, tuple)
  | VTOuter (_, tuple)
  | VTAbstraction (_, tuple) ->
      outer_vt tuple
  | VTTuple tuples ->
      union (List.map outer_vt tuples)

(* ------------------------------------------------------------------------- *)

(* The [bound] set function. *)

and bound_vt tuple =
  match tuple with
  | VTComponent v ->
      apply_v None (* TEMPORARY incorrect *) SFBound v
  | VTInner (sorts, _) 
  | VTOuter (sorts, _) when has_focus sorts ->
      empty
  | VTInner (_, tuple)
  | VTOuter (_, tuple)
  | VTAbstraction (_, tuple) ->
      bound_vt tuple
  | VTTuple tuples ->
      union (List.map bound_vt tuples)

end

(* ------------------------------------------------------------------------- *)

(* Turn the above functor into a pair of functions. *)

let apply_v_at env sort sf v : set_expression =
  let module E = Eval (struct let sort = sort end) (struct let env = env end) in
  E.apply_v None sf v

let apply_vt_at env sort sf tuple : set_expression =
  let module E = Eval (struct let sort = sort end) (struct let env = env end) in
  E.apply_vt sf tuple

(* The notation [sf(x)], where [sf] is a set function, can be viewed
   as sugar for the union of the set expressions [sf@s(x)], where [s]
   ranges over all sorts. *)

let apply_v env sf v =
  union (
    SortTable.fold (fun sort () accu ->
      apply_v_at env sort sf v :: accu
    ) []
  )

let apply_vt env sf tuple =
  union (
    SortTable.fold (fun sort () accu ->
      apply_vt_at env sort sf tuple :: accu
    ) []
  )

(* ------------------------------------------------------------------------- *)

(* The constraint that is passed to us by the client can contain applications
   of set functions to arbitrary values or value tuples. Rewrite it so that
   all applications that remain are (i) to a variable; (ii) at a specific sort;
   (iii) not known to denote an empty set. *)

(* The meat of this code is the [SApp] case; the rest is boilerplate. *)

let rec rewrite_set_expression env s =
  match Location.content s with
  | SEmpty
  | SUniverse
  | SSort _ ->
      s
  | SApp (sf, None, tuple) ->
      apply_vt env sf tuple
  | SApp (sf, Some sort, tuple) ->
      apply_vt_at env sort sf tuple
  | SAssocOp (op, ss) ->
      sop op (List.map (rewrite_set_expression env) ss)
  | SConditional (c, s1, s2) ->
      conditional
        (rewrite_constraint env c)
        (rewrite_set_expression env s1)
        (rewrite_set_expression env s2)

and rewrite_constraint env c =
  match Location.content c with
  | FTrue
  | FFalse
  | FBoolVar _ ->
      c
  | FNot c ->
      negation (rewrite_constraint env c)
  | FBoolAssocOp (op, cs) ->
      cop op (List.map (rewrite_constraint env) cs)
  | FSetBinOp (s1, op, s2) ->
      acop op (rewrite_set_expression env s1) (rewrite_set_expression env s2)

let rewrite_goal env goal =
  goal_map (rewrite_constraint env) goal

(* ------------------------------------------------------------------------- *)

(* In order to make constraints more readable, we infer an upper
   approximation of which sorts of atoms can appear in a set
   expression, and use this information to simplify set expressions
   and constraints. *)

(* Note that this simplification process must not be applied to the
   sort axioms, otherwise they would become trivial! *)

module Approx = struct

  open Sorte.AtomSet

  let rec approx s =
    match Location.content s with
    | SEmpty ->
	empty
    | SUniverse ->
	SortTable.domain
    | SSort sort
    | SApp (_, Some sort, _) ->
	singleton sort
    | SApp (_, None, _) ->
	assert false
	  (* no longer possible after constraints have been rewritten *)
    | SAssocOp (OpUnion, ss) ->
	Misc.fold_map_left union approx empty ss
    | SAssocOp (OpIntersection, ss) ->
	Misc.fold_map_left inter approx empty ss
    | SAssocOp (OpDifference, s :: _) ->
	approx s
    | SAssocOp (OpDifference, []) ->
	assert false
    | SConditional (_, s1, s2) ->
	union (approx s1) (approx s2)

end

open Approx

let rec simplify_set_expression filter s =
  match Location.content s with
  | SEmpty ->
      s
  | SUniverse ->
      assert false
	(* never used outside the sort axioms *)
  | SSort sort
  | SApp (_, Some sort, _) ->
      if Sorte.AtomSet.mem sort filter then s else empty
  | SApp (_, None, _) ->
      assert false
	(* no longer possible after constraints have been rewritten *)
  | SAssocOp (op, ss) ->
      sop op (List.map (simplify_set_expression filter) ss)
  | SConditional (c, s1, s2) ->
      conditional
        (simplify_constraint c)
	(simplify_set_expression filter s1)
        (simplify_set_expression filter s2)

and simplify_constraint c =
  match Location.content c with
  | FTrue
  | FFalse
  | FBoolVar _ ->
      c
  | FNot c ->
      negation (simplify_constraint c)
  | FBoolAssocOp (op, cs) ->
      cop op (List.map simplify_constraint cs)
  | FSetBinOp (s1, op, s2) ->
      let sorts1 = approx s1
      and sorts2 = approx s2 in
      let filter1, filter2 =
	match op with
	| OpSubset ->
	    (* No need for the right-hand side to mention sorts that cannot
	       appear on the left-hand side. *)
	    sorts1, sorts1
	| OpEqual
	| OpNotEqual ->
	    (* No change. *)
	    sorts1, sorts2
	| OpDisjoint ->
	    (* No need for either side to mention sorts that cannot appear
	       on the other side. *)
            sorts2, sorts1
      in
      acop op (simplify_set_expression filter1 s1) (simplify_set_expression filter2 s2)

let simplify_goal goal =
  goal_map simplify_constraint goal

(* ------------------------------------------------------------------------- *)

(* Axioms that explicitly express (initially implicit) knowledge about
   sorts, values (based on their types), and values (based on value
   equations). *)

(* ------------------------------------------------------------------------- *)

(* Sorts form a partition of the universe. *)

let sort_axioms =

  (* The union of all sorts is the universe. *)

  let sorts =
    SortTable.fold (fun sort () accu ->
      sort :: accu
    ) []
  in
  let universality =
    equal
      (union (List.map univ_at sorts))
      univ
  in

  (* Sorts are pairwise disjoint. *)

  let disjointness =
    List.map (fun (sort1, sort2) ->
      disjoint
	(univ_at sort1)
	(univ_at sort2)
    ) (Misc.pairs sorts)
  in

  conjunction (universality :: disjointness)

(* ------------------------------------------------------------------------- *)

(* What can be said of a variable, based purely on its type? *)

let type_based_axioms env =
  conjunction (Var.AtomMap.fold (fun x typ accu ->
    match Location.content typ with

    | TData t ->

	(* [x] inhabits an arbitrary type [t]. For every sort [sort] and
	   each set function [sf], if the image of every inhabitant of
	   [t] under the set function [sf@sort] is known to be nonempty,
	   then an instance of that fact at [x] can be exploited. *)

	(* Note that, if [sf@s(x)] is known to be empty, it is rewritten to
	   the empty set, so that we do not need to emit an axiom about it. *)

	let accu =
	  SortTable.fold (fun sort () accu ->
	    let fact = Facts.datatype sort t in
	    List.fold_right (fun sf accu ->
	      if (Facts.project sf fact).must then
		nonempty (app_at sort sf x) :: accu
	      else
		accu
	    ) [ SFSupport; SFInner; SFOuter; SFBound ] accu
	  ) accu
	in

	(* [x] inhabits the algebraic data type [t]. For each sort [s]
	   such that [t] binds [s], we have [free@s(x) == bound@s(x) U
	   inner@s(x) U outer@s(x)]. *)

	let def = DatatypeTable.def t in
	let accu =
	  List.fold_right (fun sort accu ->
	    let app sf = app_at sort sf x in
	    equal
	      (app SFSupport)
	      (union [ app SFBound; app SFInner; app SFOuter ])
	    :: accu
	  ) def.datatype_sorts accu
	in

	(* [x] inhabits the algebraic data type [t]. For each user-supplied
	   lemma that concerns [t], an instance of the lemma at [x] can be
	   exploited. *)

	let accu =
	  List.fold_right (fun lemma accu ->
	    let y, u, c = open_lemma lemma in
	    if Datatype.Atom.equal t u then
	      substitute_constraint (Var.AtomMap.singleton y (VTComponent (VVar x))) c
	      :: accu
	    else
	      accu
	  ) lemmata accu
	in

	accu

    | TAtom sort ->

	(* [x] inhabits the type [atom sort]. As a result, [support@sort(x)] is
	   known to be nonempty. *)

	nonempty (app_at sort SFSupport x) :: accu

    | TBool
    | TAtomSet _ ->
	accu

  ) env [])

(* ------------------------------------------------------------------------- *)

(* What are the consequences of equations between Boolean values? *)

let interpret_boolean_value v =
  Location.none (
    match v with
    | VVar x ->
	FBoolVar x
    | VBool true ->
	FTrue
    | VBool false ->
	FFalse
    | _ ->
	assert false (* no other value form has type [bool] *)
  )

let interpret_boolean_equation v1 v2 =
  iff [ interpret_boolean_value v1; interpret_boolean_value v2 ]

(* ------------------------------------------------------------------------- *)

(* What are the consequences of equations between values of arbitrary type? *)

let value_equation_axioms env (v1, v2) =

  (* This auxiliary function applies a list of set functions, in turn,
     to both sides of the value equation, yielding a conjunction of
     equations between set expressions. *)

  let apply sfs =
    conjunction (
      List.map (fun sf ->
        equal (app_v sf v1) (app_v sf v2)
      ) sfs
    )
  in

  (* Examine the common type of [v1] and [v2]. If it is [bool],
     transcribe the value equation into a constraint. Otherwise,
     determine which set functions are applicable at this
     type. [support] is applicable at every type. [inner], [outer],
     and [bound] are applicable at every algebraic data type. *)

  let typ = Value.typeof env v1 (* for instance *) in
  match Location.content typ with
  | TBool ->
      interpret_boolean_equation v1 v2
  | TAtom _
  | TAtomSet _ ->
      apply [ SFSupport ]
  | TData _ ->
      apply [ SFSupport; SFInner; SFOuter; SFBound ]

let value_equation_axioms env (ovvs : (value * value) list option) =
  match ovvs with
  | None ->
      falsity (* the value equations are inconsistent *)
  | Some vvs ->
      conjunction (List.map (value_equation_axioms env) vvs)

(* ------------------------------------------------------------------------- *)

(* Satistics. *)

open Printf
open Print

let successes, failures =
  ref 0, ref 0

let obligations verb n =
  if n > 1 then
    fprintf stderr "%s %d proof obligations.\n%!" verb n
  else if n = 1 then
    fprintf stderr "%s one proof obligation.\n%!" verb

let stats () =
  obligations "Successfully discharged" !successes;
  obligations "Failed to fulfill" !failures;
  Cnf.stats()

(* ------------------------------------------------------------------------- *)

(* Pretty-printers for goals. *)

module Check (P : sig

  val print: var printer

end) = struct

let print_toplevel_constraint =
  print_toplevel_constraint P.print

let print_goal b = function
  | GFalsity _ ->
      bprintf b "false"
  | GConjunction acs ->
      List.iter (fun (_, c) ->
	print_toplevel_constraint b c
      ) acs

let print_problem header hypotheses goal b =
  bprintf b "%s\
             The hypotheses are:\n%a\n\
             The goal is:\n%a\n"
    header
    print_toplevel_constraint hypotheses
    print_goal goal

let print_failed_problem =
  print_problem "I am unable to discharge the following proof obligation.\n\n"

let print_successful_problem =
  print_problem "I was able to discharge the following proof obligation.\n\n"

(* ------------------------------------------------------------------------- *)

(* A sound (and incomplete) translation of typed constraints down to set
   constraints, as offered by [SetConstraint]. *)

(* ------------------------------------------------------------------------- *)

(* Sorts are mapped down to set variables. *)

module SortMap =
  AtomAllocator.Make (struct
    type thing = sorte
    module Map = Sorte.AtomMap
    let print = SortTable.print
  end) (SetConstraint.SetVar)

(* ------------------------------------------------------------------------- *)

(* Applications of set functions to variables are mapped down to set
   variables. *)

module AppMap =
  AtomAllocator.Make (struct
    type thing = set_function * var
    module Map =
      Map.Make(struct
	type t = thing
	let compare (sf1, x1) (sf2, x2) =
	  Order.lexicographic2 Pervasives.compare Var.Atom.compare sf1 x1 sf2 x2
	    (* comparing set functions using [Pervasives.compare] *)
      end)
    let print b (sf, x) =
      bprintf b "%s(%a)"
	(print_set_function sf)
	P.print x
  end) (SetConstraint.SetVar)

(* ------------------------------------------------------------------------- *)

(* Variables of type [bool] are mapped down to Boolean variables. *)

module BooleanMap = AtomAllocator.Make (struct
  type thing = var
  module Map = Var.AtomMap
  let print = P.print
end) (Boolean.Var)

(* ------------------------------------------------------------------------- *)

(* Instantiate the lower layer solver. *)

module SC =
  SetConstraint.Make (struct

    let xor b1 b2 =
      (b1 && not b2) || (not b1 && b2)

    let print_set_variable b s =
      assert (xor (SortMap.in_codomain s) (AppMap.in_codomain s));
      if SortMap.in_codomain s then
	SortMap.print b s
      else
	AppMap.print b s

    let print_boolean_variable =
      BooleanMap.print

  end)

(* ------------------------------------------------------------------------- *)

(* N-ary, associative operators are translated down to binary operators. *)

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

let translate_setsetbool_operator = function
  | OpSubset ->
      SetConstraint.subset
  | OpEqual ->
      SetConstraint.equal
  | OpNotEqual ->
      SetConstraint.notequal
  | OpDisjoint ->
      SetConstraint.disjoint

let translate_boolboolbool_operator = function
  | OpConjunction ->
      leftassoc Propositional.conjunction
  | OpDisjunction ->
      leftassoc Propositional.disjunction
  | OpImplication ->
      rightassoc Propositional.implication
  | OpEquivalence ->
      leftassoc Propositional.iff

(* ------------------------------------------------------------------------- *)

(* Translating set expressions, constraints, and goals. *)

let rec translate_set_expression s =
  match Location.content s with
  | SEmpty ->
      SetConstraint.empty
  | SSort sort ->
      SetConstraint.var (SortMap.image sort)
  | SUniverse ->
      SetConstraint.univ
  | SApp (sf, None, VTComponent (VVar x)) ->
      SetConstraint.var (AppMap.image (sf, x))
  | SApp (_, None, _) ->
      assert false (* application of set function to non-variable *)
  | SApp (sf, Some sort, x) ->
      translate_set_expression (
        intersection [
          Location.none (SApp (sf, None, x));
          univ_at sort
        ]
      )
  | SAssocOp (op, ss) ->
      translate_setsetset_operator op (List.map translate_set_expression ss)
  | SConditional (c, s1, s2) ->
      SetConstraint.conditional
	(translate_constraint c)
	(translate_set_expression s1)
	(translate_set_expression s2)

and translate_constraint c =
  match Location.content c with
  | FTrue ->
      Propositional.truth
  | FFalse ->
      Propositional.falsity
  | FBoolVar x ->
      SetConstraint.boolean (BooleanMap.image x)
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

(* Solving entailment problems. *)

let check (env, eqs, hypotheses, goal) : bool =

  (* Make all axioms but the sort axioms explicit. *)

  let hypotheses =
    conjunction [
      type_based_axioms env;
      value_equation_axioms env (Value.decode eqs);
      hypotheses
    ]
  in

  (* Rewrite the hypotheses and goal in order to evaluate all
     applications of set functions. *)

  let hypotheses = rewrite_constraint env hypotheses
  and goal = rewrite_goal env goal in

  (* Simplify the hypotheses and goal by using the knowledge
     that sorts are disjoint. *)

  let hypotheses = simplify_constraint hypotheses
  and goal = simplify_goal goal in

  (* Include the sort axioms. These need not be rewritten, and must
     not be simplified, which is why I do not include them earlier. *)

  let hypotheses =
    conjunction [
      sort_axioms;
      hypotheses
    ]
  in

  (* Pass the question down to the lower layer. *)

  if SC.entailment
       (translate_constraint hypotheses)
       (translate_goal goal)

  (* Success. *)

  then begin
    if false then (* toggle *)
      w (print_successful_problem hypotheses goal);
    successes := !successes + (
      match goal with
      | GFalsity _ ->
	  1
      | GConjunction acs ->
	  List.length acs
    );
    true
  end

  (* Failure. If there is a single reporter function, then display the
     problem in the syntax of the source language. (The case where
     there are multiple reporter functions is dealt with below.) *)

  else begin
    match goal with
    | GFalsity reporter
    | GConjunction [ reporter, _ ] ->
	failures := !failures + 1;
	reporter (ws (print_failed_problem hypotheses goal));
	false
    | GConjunction acs ->
	assert (acs <> []);
	false
  end

(* ------------------------------------------------------------------------- *)

(* A wrapper implements individual retries when a multiple-goal
   request fails. *)

let check problem =
  check problem ||
  match problem with
  | _, _, _, GFalsity _
  | _, _, _, GConjunction [ _ ] ->
      false
  | env, eqs, hypotheses, GConjunction acs ->
      assert (acs <> []);
      List.fold_left (fun accu ac ->
	check (env, eqs, hypotheses, GConjunction [ ac ]) && accu
	  (* intentional use of strict conjunction, so that all subgoals are retried *)
      ) true acs

(* ------------------------------------------------------------------------- *)

(* A wrapper decomposes conjunctions in the goal, so as to make the
   individual retry process more fine-grained. It also bypasses the
   entailment check if the goal is [true]. *)

let decompose goal =
  match goal with
  | GFalsity _ ->
      goal
  | GConjunction acs ->
      GConjunction (
	Misc.flat_map (fun (reporter, c) ->
	  match c with
	  | { content = FTrue } ->
	      []
	  | { content = FBoolAssocOp (OpConjunction, cs) } ->
	      List.map (fun c -> (reporter, c)) cs
	  | _ ->
	      [ reporter, c ]
	) acs
      )

let check (env, eqs, hypotheses, goal) =
  match decompose goal with
  | GConjunction [] ->
      true
  | goal ->
      check (env, eqs, hypotheses, goal)

end

(* ------------------------------------------------------------------------- *)

(* A last wrapper turns the above functor into a function, and drops
   the Boolean outcome. The check's only observable effect is to
   display an error message when it fails. *)

let check print problem : unit =
  let module C = Check (struct let print = print end) in
  let (_ : bool) = C.check problem in
  ()

