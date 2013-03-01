open Printf
open Source
open SourceConstraint
open Entailment
open SymbolTable

(* This module checks that the user-supplied lemmata hold. It exploits
   the constraint solver, which itself exploits the lemmata. This is
   sound, because we are careful to produce proof obligations where
   the lemmata can be exploited only at sub-terms of the original
   term. This amounts to an inductive proof, where all lemmata are
   possibly mutually inductive. *)

(* ------------------------------------------------------------------------- *)

(* Checking an individual lemma at an individual constructor. *)

let check x statement (tag, params) =
  let tuple, guard = open_guarded_tuple params in

  (* Manufacture a generic value whose head constructor is [tag] and
     whose sub-terms are fresh variables. *)

  let env, vs = Types.manufacture tuple in
  let v = VData (tag, vs) in

  (* Instantiate the guard for this value. This yields a
     hypothesis. *)

  let px b x = bprintf b "%s%d" (Location.content (Var.Atom.basename x)) (Var.Atom.identity x) in (* TEMPORARY *)
  if false then
    Print.w (fun b -> (* TEMPORARY *)
      bprintf b "THE INITIAL GUARD IS %a\n"
	(print_toplevel_constraint px) guard
    );

  let subst, _ = Value.tuple_structure tuple vs in
  let guard = substitute_constraint subst guard in

  if false then
    Print.w (fun b -> (* TEMPORARY *)
      bprintf b "THE SUBSTITUTION IS:\n%a\n"
	(print_substitution px)
	subst;
      bprintf b "THE SUBSTITUTED GUARD IS %a\n"
	(print_toplevel_constraint px) guard
    );

  (* Instantiate the lemma's statement for this value. This yields a
     goal. *)

  let goal =
    substitute_constraint
      (Var.AtomMap.singleton x (VTComponent v)) statement
  in

  if false then
    Print.w (fun b -> (* TEMPORARY *)
      bprintf b "THE GOAL IS %a\n%!"
	(print_toplevel_constraint px) goal
    );

  (* Define a variable printer. *)

  let m =
    Var.AtomIdMap.add_set (Var.AtomMap.domain env) Var.AtomIdMap.empty
  in
  let print b x =
    bprintf b "%s" (Location.content (Var.AtomIdMap.find x m))
  in

  (* Define how to display an error message if the check fails. *)

  let reporter msg =
    Error.signalv statement
      (sprintf "%sThe reason why I am attempting to prove this assertion is...\n\
	        It is required to prove case %s of this lemma.\n"
		    msg
		    (DataconTable.name tag))
  in

  (* Run the check. *)

  check print (env, Value.truth, guard, GConjunction [ reporter, goal ])

(* ------------------------------------------------------------------------- *)

(* Checking an individual lemma. *)

let check lemma =
  let x, t, statement = open_lemma lemma in
  let def = DatatypeTable.def t in
  List.iter (check x statement) def.datatype_constructors

(* ------------------------------------------------------------------------- *)

(* Checking all lemmata. *)

let () =
  List.iter check lemmata;
  Error.signaled()

