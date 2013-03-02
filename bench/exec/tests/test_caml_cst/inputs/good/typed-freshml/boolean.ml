(* ------------------------------------------------------------------------- *)

(* Boolean variables. *)

module Var =
  Cnf.Var

type variable =
    Var.Atom.t

(* ------------------------------------------------------------------------- *)

(* Boolean formulae. *)

type formula =
    variable Propositional.formula

(* ------------------------------------------------------------------------- *)

(* Conversion to CNF. *)

(* One could limit the duplication of subformulae by introducing a
   fresh variable, together with an implication, wherever a large
   subformula is about to be duplicated. See, for instance, Daniel
   Sheridan's paper. *)

open Propositional
open Cnf

let rec cnf f : problem =
  match f with
  | FFalse ->
      falsity
  | FTrue ->
      truth
  | FLeaf v ->
      singleton (positive v)
  | FNot (FLeaf v) ->
      singleton (negative v)
  | FNot _ ->
      assert false (* formula is in NNF *)
  | FAnd (f1, f2) ->
      conjunction (cnf f1) (cnf f2)
  | FOr (f1, f2) ->
      disjunction (cnf f1) (cnf f2)

(* ------------------------------------------------------------------------- *)

(* Solving. *)

type solution =
    bool Var.AtomMap.t

let solve comment mode = function
  | FTrue ->
      [ Var.AtomMap.empty ]
  | FFalse ->
      []
  | f ->
      Cnf.solve comment mode (cnf (nnf f))

let satisfiable f =
  match solve "" Cnf.ModeOneSolution f with
  | [] ->
      false
  | _ :: _ ->
      true

(* The Boolean solver provides us with a primary assignment -- one
   that assigns a value to every variable that occurs in [f]. We
   extend it to an assignment over [domain] by enumerating all
   possible values of the remaining variables. *)

let solutions domain f : solution list =
  List.fold_left (fun accu primary ->

    (* Determine which variables appear in the primary solution,
       and, a contrario, which variables do not appear. *)

    let present = Var.AtomMap.domain primary in
    assert (Var.AtomSet.subset present domain);
    let remainder = Var.AtomSet.diff domain present in

    (* Enumerate all assignments of the remaining variables. *)

    let rec build remainder assignment accu =
      if Var.AtomSet.is_empty remainder then
	(* The assignment is complete. *)
	assignment :: accu
      else
	(* The assignment is incomplete. Enumerate two possible
	   values of some remaining variable. *)
	let b = Var.AtomSet.choose remainder in
	let remainder = Var.AtomSet.remove b remainder in
	let assignment1 = Var.AtomMap.add b true assignment
	and assignment2 = Var.AtomMap.add b false assignment in
	build remainder assignment1 (build remainder assignment2 accu)
    in
    build remainder primary accu

  ) [] (solve "" Cnf.ModeAllSolutions f)

