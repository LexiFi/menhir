(* ------------------------------------------------------------------------- *)

(* We do not distinguish internally between set variables and Boolean
   variables, even though we present an interface where the two
   categories are distinguished.

   Both kinds of variables are in fact implemented as variables of the
   kind expected by the module [Boolean], so that no translation of
   variables is required. *)

(* Set variables. *)

module SetVar =
  Boolean.Var

type set_variable =
    SetVar.Atom.t

(* Boolean variables. *)

module BoolVar =
  Boolean.Var

type boolean_variable =
    Boolean.variable

(* ------------------------------------------------------------------------- *)

(* Set expressions are propositional formulae above atomic set
   expressions.

   An atomic set expression is either a set variable or a constraint.

   [ASCon] is the constructor that injects constraints into set
   expressions. Its interpretation is given by the characteristic
   function -- that is, it is the universal set when the constraint
   holds, and the empty set otherwise.

   The constraint carried by [ACCon] can, without loss of generality, be
   required to be atomic. This is because, modulo the characteristic
   interpretation, Boolean connectives inside constraints have the same
   meaning as Boolean connectives inside set expressions. *)

type atomic_set_expression =
  | ASVar of set_variable
  | ASCon of atomic_constraint

and set_expression =
    atomic_set_expression Propositional.formula

(* Constraints are propositional formulae above atomic constraints.

   An atomic constraint is either a Boolean variable or a set
   expression.

   [ACPos] is the constructor that injects set expressions into
   constraints. It is interpreted as a positive constraint, that is, a
   set emptiness assertion: it is true when the set is empty and false
   otherwise. *)

and atomic_constraint =
  | ACVar of boolean_variable
  | ACPos of set_expression

(* Note that the composition of [ACPos] and [ACCon], which maps set
   expressions to set expressions, is not interpreted as the identity (hence
   not redundant). It maps the empty set to the universal set, and every
   non-empty set to the empty set. It can be thought of as a set-valued test
   for emptiness or as a rounding operator. *)

(* A constraint is a propositional formula built on top of atomic
   constraints. This means that the functions offered by module
   [Propositional] can be used to build constraints. *)

type contrainte =
   atomic_constraint Propositional.formula

(* ------------------------------------------------------------------------- *)

(* Building set expressions. *)

let empty =
  Propositional.falsity

let univ =
  Propositional.truth

let var x =
  Propositional.leaf (ASVar x)

let union =
  Propositional.disjunction

let inter =
  Propositional.conjunction

let diff =
  Propositional.nonimplication

let complement =
  Propositional.negation

let characteristic_atomic (ac : atomic_constraint) : set_expression =
  Propositional.leaf (ASCon ac)

let characteristic (c : contrainte) : set_expression =
  Propositional.substitute characteristic_atomic c

let conditional (c : contrainte) s1 s2 =
  Propositional.conditional (characteristic c) s1 s2

(* ------------------------------------------------------------------------- *)

(* Building atomic constraints. *)

let boolean b =
  Propositional.leaf (ACVar b)

let positive s =
  match s with
  | Propositional.FTrue ->
      Propositional.falsity (* [univ] is not empty *)
  | Propositional.FFalse ->
      Propositional.truth (* [empty] is empty *)
  | _ ->
      Propositional.leaf (ACPos s)

let subset s1 s2 =
  positive (diff s1 s2)

let equal s1 s2 =
  positive (union (diff s1 s2) (diff s2 s1))

let notequal s1 s2 =
  Propositional.negation (equal s1 s2)

let disjoint s1 s2 =
  positive (inter s1 s2)

(* ------------------------------------------------------------------------- *)

(* Printers for debugging. *)

open Printf

let print_set_variable b x =
  bprintf b "%s" (SetVar.Atom.basename x)

let print_boolean_variable b x =
  bprintf b "%s%d" (BoolVar.Atom.basename x) (BoolVar.Atom.identity x)

let rec print_atomic_set_expression b = function
  | ASVar x ->
      print_set_variable b x
  | ASCon ac ->
      bprintf b "char: %a" print_atomic_constraint ac
	(* flag an instance of the characteristic interpretation *)

and print_set_expression b s =
  Propositional.print print_atomic_set_expression b s

and print_atomic_constraint b = function
  | ACVar x ->
      print_boolean_variable b x
  | ACPos s ->
      bprintf b "%a == empty" print_set_expression s

let print_contrainte =
  Propositional.print print_atomic_constraint

(* ------------------------------------------------------------------------- *)

(* Eliminating positive constraints nested within set expressions -- that is,
   compositions of [ACPos] and [ASCon] -- that is, uses of the rounding
   operator. *)

(* To get rid of these, we replace positive constraints with fresh Boolean
   variables, so that, after this pass is over, the only constraints that can
   be nested within set expressions are Boolean variables -- that is, [ASCon]
   can be applied only to a term of the form [ACVar _]. This in turn means
   that, after Boolean variables are eliminated, no constraints at all will be
   nested within set expressions. *)

let explode (c : contrainte) : contrainte =

  (* Allocate a cell to hold the defining equations for the fresh
     Boolean variables. *)

  let conjuncts : contrainte ref =
    ref Propositional.truth
  in

  (* Define a function that emits new defining equations. *)

  let equate (b : boolean_variable) (c : contrainte) =
    conjuncts :=
      Propositional.conjunction
	!conjuncts
	(Propositional.iff (boolean b) c)
  in

  (* Define the transformation. *)

  let rec explode_atomic_constraint (nested : bool) ac : atomic_constraint =
    match ac with
    | ACVar _ ->
	ac
    | ACPos s ->
	let s = explode_set_expression s in
	if nested then
	  let b = BoolVar.Atom.freshb "<explode>" in
	  equate b (positive s);
	  ACVar b
	else
	  ACPos s

  and explode_atomic_set_expression ase : atomic_set_expression =
    match ase with
    | ASVar _ ->
	ase
    | ASCon ac ->
	ASCon (explode_atomic_constraint true ac)

  and explode_set_expression s : set_expression =
    Propositional.map explode_atomic_set_expression s

  and explode_constraint c : contrainte =
    Propositional.map (explode_atomic_constraint false) c

  in

  (* Run the transformation. *)

  let c = explode_constraint c in
  Propositional.conjunction c !conjuncts

(* ------------------------------------------------------------------------- *)

(* Collecting all of the Boolean variables in a constraint. *)

let rec fbv_s (s : set_expression) accu =
  Propositional.fold (fun ase accu ->
    match ase with
    | ASVar _ ->
	accu
    | ASCon ac ->
	fbv_ac ac accu
  ) s accu

and fbv_ac (ac : atomic_constraint) accu =
  match ac with
  | ACVar b ->
      BoolVar.AtomSet.add b accu
  | ACPos s ->
      fbv_s s accu

let fbv (c : contrainte) =
  Propositional.fold fbv_ac c BoolVar.AtomSet.empty

(* ------------------------------------------------------------------------- *)

(* Elimination of the Boolean variables. *)

(* Boolean variables pose a theoretical problem: in their presence,
   negative set constraints are no longer independent. For this
   reason, we first eliminate them, by enumerating all possible
   assignments to them. *)

(* Of course, we do not want to naively enumerate all assignments to
   the Boolean variables, as there is an exponential number of
   them. Instead, in order to diminish the number of Boolean
   assignments that must be considered, we extract, out of the
   original constraint [c], a Boolean formula [f], such that [c]
   implies [f]. We then enumerate the solutions of [f]. Any solution
   of [c] must be an extension of some solution of [f], which means
   that it is sufficient to consider only those assignments that
   satisfy [f]. *)

type formula =
    boolean_variable Propositional.formula

let rec extract polarity (ac : atomic_constraint) : formula =
  match ac with
  | ACVar b ->
      Propositional.leaf b
  | ACPos _ ->
      if polarity then Propositional.truth else Propositional.falsity

let extract (c : contrainte) : formula =
  Propositional.polarized_substitute extract true c

(* ------------------------------------------------------------------------- *)

(* Elimination of the Boolean variables, continued. *)

(* After Boolean variables are eliminated, set expressions and constraints
   contain set variables only (no Boolean variables), and constraints can no
   longer be nested within expressions. *)

type pure_set_expression =
    set_variable Propositional.formula

type pure_atomic_constraint =
  | SPos of pure_set_expression 

type pure_constraint =
    pure_atomic_constraint Propositional.formula

(* More printers. *)

let print_pure_set_expression =
  Propositional.print print_set_variable

let print_pure_atomic_constraint b = function
  | SPos s ->
      bprintf b "%a == empty" print_pure_set_expression s

let print_pure_constraint =
  Propositional.print print_pure_atomic_constraint

(* ------------------------------------------------------------------------- *)

(* Elimination of the Boolean variables, continued. *)

(* Boolean-grounding of constraints. *)

let ground_b assignment (b : boolean_variable) : 'a Propositional.formula =
  try
    if BoolVar.AtomMap.find b assignment then Propositional.truth else Propositional.falsity
  with Not_found ->
    assert false (* all Boolean variables are grounded *)

let ground_s assignment (s : set_expression) : pure_set_expression =
  Propositional.substitute (function
    | ASVar v ->
	Propositional.leaf v
    | ASCon (ACVar b) ->
	ground_b assignment b
    | ASCon (ACPos _) ->
	assert false (* no constraints within set expressions *)
  ) s

let ground_ac assignment (ac : atomic_constraint) : pure_constraint =
  match ac with
  | ACVar b ->
      ground_b assignment b
  | ACPos s ->
      Propositional.leaf (SPos (ground_s assignment s))

let ground assignment (c : contrainte) : pure_constraint =
  Propositional.substitute (ground_ac assignment) c

(* ------------------------------------------------------------------------- *)

(* Elimination of the Boolean variables, continued. *)

(* [eliminate c] eliminates all Boolean variables by turning a [c]
   into a disjunction, indexed over all possible Boolean assignments,
   of Boolean-grounded versions of [c]. *)

(* TEMPORARY this could lead to explosion in contexts where there are
   many Boolean variables whose value is unconstrained. One approach
   would be to first *not* ground the Boolean variables, yielding an
   incomplete decision procedure; and to revert to grounding variables
   only when the first attempt fails to establish unsatisfiability. *)

let eliminate c =
  Propositional.disjunction_map (fun assignment ->
    ground assignment c
  ) (Boolean.solutions (fbv c) (extract c))

(* ------------------------------------------------------------------------- *)

(* Constraints in disjunctive normal form. *)

(* A conjunction of positive set constraints reduces to a single
   positive constraint: [s1 == empty /\ s2 == empty] is equivalent to
   [s1 U s2 == empty]. Dually, a disjunction of negative set
   constraints reduces to a single negative set constraint. *)

(* Because of these simplifications, every conjunction in a DNF can be made to
   contain at most one positive set constraint, together with an arbitrary
   number of negative set constraints. Similarly, every disjunction in a DNF
   can be made to contain at most one negative set constraint, together with
   an arbitrary number of conjunctions. *)

(* These simplifications are important, because fewer conjuncts (or disjuncts)
   means that the conversion to DNF is less explosive, and also means, in the
   end, that fewer calls to the external SAT solver are made. In high-level
   terms, they allow us to establish several (positive) postconditions with a
   single call to the SAT solver. *)

type conjunction = {
    positive_conjunct: pure_set_expression option;
    negative_conjuncts: pure_set_expression list
  }

type disjunction = {
    negative_disjunct: pure_set_expression option;
    other_disjuncts: conjunction list
  }

(* ------------------------------------------------------------------------- *)

(* Conversion to disjunctive normal form. *)

let empty_disjunction =
  { negative_disjunct = None; other_disjuncts = [] }

let positive_singleton_conjunction s =
  { positive_conjunct = Some s; negative_conjuncts = [] }

let negative_singleton_conjunction s =
  { positive_conjunct = None; negative_conjuncts = [ s ] }

let negative_singleton_disjunction s =
  { negative_disjunct = Some s; other_disjuncts = [] }

let other_singleton_disjunction c =
  { negative_disjunct = None; other_disjuncts = [ c ] }

let insert_other_disjunct c d =
  { d with other_disjuncts = c :: d.other_disjuncts }

let true_disjunction =
  negative_singleton_disjunction univ

let conjunction_of_conjunctions c1 c2 = {
  positive_conjunct = Option.concat union c1.positive_conjunct c2.positive_conjunct;
    (* [s1 == empty /\ s2 == empty] is equivalent to [s1 U s2 == empty]. *)
  negative_conjuncts = c1.negative_conjuncts @ c2.negative_conjuncts
}

let disjunction_of_disjunctions c1 c2 = {
  negative_disjunct = Option.concat union c1.negative_disjunct c2.negative_disjunct;
    (* [s1 != empty \/ s2 != empty] is equivalent to [s1 U s2 != empty]. *)
  other_disjuncts = c1.other_disjuncts @ c2.other_disjuncts
}

let fold_disjuncts f d accu =
  let accu =
    Option.fold (fun s accu ->
      f (negative_singleton_conjunction s) accu
    ) d.negative_disjunct accu
  in
  let accu =
    List.fold_right f d.other_disjuncts accu
  in
  accu

module C = struct

  open Propositional

  let rec convert = function
    | FTrue -> (* can only appear at the root *)
	true_disjunction
    | FFalse -> (* can only appear at the root *)
	empty_disjunction
    | FLeaf (SPos s) ->
	other_singleton_disjunction (positive_singleton_conjunction s)
    | FNot (FLeaf (SPos s)) ->
	negative_singleton_disjunction s
    | FNot _ ->
	assert false (* constraint is in NNF *)
    | FAnd (c1, c2) ->
	let d1 = convert c1
	and d2 = convert c2 in
	fold_disjuncts (fun disjunct1 accu ->
	  fold_disjuncts (fun disjunct2 accu ->
	    insert_other_disjunct (conjunction_of_conjunctions disjunct1 disjunct2) accu
	  ) d2 accu
	) d1 empty_disjunction
    | FOr (c1, c2) ->
	disjunction_of_disjunctions (convert c1) (convert c2)

  let convert f =
    convert (nnf f)

end

(* ------------------------------------------------------------------------- *)

(* Satisfiability. *)

(* A simple constraint, of the form [P], [N], or [P /\ N] is
   satisfiable over the algebra of sets if and only if it is
   satisfiable over the algebra of Booleans. *)

let satisfiable_simple_constraint po no =
  Boolean.satisfiable (
    Propositional.conjunction (
      match po with
      | None ->
	  Propositional.truth
      | Some p ->
	  (* [p == empty] translates to [not p] at the Boolean level. *)
	  Propositional.negation p

    )(
      match no with
      | None ->
	  Propositional.truth
      | Some n ->
	  (* [n != empty] translates to [n] at the Boolean level. *)
	  n
    )
  )

(* Because negative constraints are independent, a conjunction [P /\
   N1 ... /\ Nk], where [k > 0], is satisfiable if and only if each of
   [P /\ N1], ..., [P /\ Nk] is satisfiable. *)

(* TEMPORARY this enumeration can be made much faster, in practice, if
   we know which [Nj] is most likely to cause unsatisfiability, and
   try that one first. When trying to prove an entailment judgement,
   the negation of the goal is that [Nj]. Mark it and keep track of
   it? *)

let satisfiable_conjunction c =
  match c.negative_conjuncts with
  | [] ->
      satisfiable_simple_constraint c.positive_conjunct None
  | _ :: _ ->
      List.fold_right (fun n satisfiable ->
	satisfiable &&
	satisfiable_simple_constraint c.positive_conjunct (Some n)
      ) c.negative_conjuncts true

(* A disjunction is satisfiable if and only if at least one its disjuncts is
   satisfiable. *)

let satisfiable_disjunction d =
  fold_disjuncts (fun c satisfiable ->
    satisfiable || satisfiable_conjunction c
  ) d false

(* ------------------------------------------------------------------------- *)

(* The public interface to satisfiability. *)

(* To determine whether a constraint is satisfiable, we first eliminate
   positive set constraints nested inside set expressions, which possibly
   introduces new Boolean variables; then, we eliminate all Boolean variables;
   then, we convert the constraint to DNF; last, we check for satisfiability
   using the above functions. *)

let satisfiable c =
  satisfiable_disjunction (C.convert (eliminate (explode c)))

let valid c =
  not (satisfiable (Propositional.negation c))

let entailment c1 c2 =
  valid (Propositional.implication c1 c2)

