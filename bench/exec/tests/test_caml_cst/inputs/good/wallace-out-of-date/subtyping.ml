(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/subtyping.ml,v 1.22.2.5 1999/02/18 00:06:29 francois Exp $ *)
(*

Checking whether a type scheme is subsumed by another type scheme.

*)

open Errors
open Types
open Entailment

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This is an implementation of the scheme subsumption algorithm, as described in my PhD thesis. Both type schemes
must be canonized (i.e. they must contain no GLUBs) and must verify the small terms invariant.

The right-hand scheme is called rigid, because its variables behave like constants. Rigid variables are not explicitly
marked as such; instead, an additional parameter is passed to solve_constraint, which tells whether the rigid variable
is the left-hand or the right-hand one. (Marking variables is not possible, because new variables might be generated
transparently by row expansion.) The left-hand scheme is called flexible.

*)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

As the algorithm runs, flexible variables receive new upper or lower bounds which are rigid variables. These bounds
cannot be stored in the flexible constraint graph, because we would then confuse flexible and rigid bounds. An
extra structure is used to store them. One could also choose to use extra fields in the type_variable structure.

*)

let state =
  ref VarMap.empty
;;

let rigid_bounds v =
  try
    VarMap.find v !state
  with Not_found ->
    empty_variable_set, empty_variable_set
;;

let add_state sign v1 v2 =
  if sign then begin

    (* v1 is flexible, v2 is rigid. Add v2 to v1's upper rigid bounds. *)

    let (lo1, hi1) = rigid_bounds v1 in
    state := VarMap.add v1 (lo1, Set7.add v2 hi1) !state

  end
  else begin
    let (lo2, hi2) = rigid_bounds v2 in
    state := VarMap.add v2 (Set7.add v1 lo2, hi2) !state
  end
;;

let state_has sign v1 v2 =
  try
    if sign then
      let _, hi1 = VarMap.find v1 !state in
      Set7.mem v2 hi1
    else
      let lo2, _ = VarMap.find v2 !state in
      Set7.mem v1 lo2
  with Not_found ->
    false
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This is the core of the algorithm. It consists only of two simple, symmetrical cases.

Note that we make a series of entailment calls with the same set of hypotheses; it might be (exponentially?) faster
to share their history sets. This is not done at the moment.

*)

let rec solve_constraint sign v1 v2 = match (sign, v1, v2) with

  (* The sign indicates which variable is flexible and which one is rigid. *)

  true, v, rv ->

    (* Test whether this constraint already exists. *)

    if not (state_has true v rv) then begin

      (* Add rv as an upper bound to all flexible variables below and including v. *)

      add_state true v rv;
      Set7.iter (fun v -> add_state true v rv) v.loset;

      (* Make sure that constraints on rigid variables below v are true. *)

      Set7.iter (fun rv' -> entailment rv' rv) (fst (rigid_bounds v));

      (* Solve the constraint on constructed terms. *)

      signed_subconstraints (fun sign leaf1 leaf2 ->
	solve_constraint sign (leaf_to_variable leaf1) (leaf_to_variable leaf2)
      ) v.lo rv.lo

    end

| false, rv, v ->

    if not (state_has false rv v) then begin

      add_state false rv v;
      Set7.iter (fun v -> add_state false rv v) v.hiset;

      Set7.iter (fun rv' -> entailment rv rv') (snd (rigid_bounds v));

      signed_subconstraints (fun sign leaf1 leaf2 ->
	solve_constraint (not sign) (leaf_to_variable leaf1) (leaf_to_variable leaf2)
      ) rv.hi v.hi

    end  
    
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This exception is raised when the right-hand scheme has fewer context entries than the left-hand one (i.e. it is
trying to conceal the fact that some identifier is needed by the expression).

*)

exception MissingContextEntry of identifier

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The main entry point.

*)

let subsumption scheme rscheme =

  Errors.trace "Subtyping.subsumption" (fun () ->

    let Scheme(context, body, effect) = scheme
    and Scheme(rcontext, rbody, reffect) = rscheme in

    (* Solve the constraint (context => body * effect) <= (rcontext => rbody * reffect).
       An Inconsistent exception shall be raised if the constraints can't be solved. *)

    try

      solve_constraint true body rbody;
      solve_constraint true effect reffect;

      Set7.smart_subset (fun () (_, ty) (_, rty) ->
	solve_constraint false rty ty
      ) (fun (identifier, _) ->
	raise (MissingContextEntry identifier)
      ) () context rcontext;

      (* Reset the state immediately, rather than at the beginning of the next run, to allow collection by the GC. *)

      state := VarMap.empty
    with error ->
      state := VarMap.empty;
      raise error

  )
;;

