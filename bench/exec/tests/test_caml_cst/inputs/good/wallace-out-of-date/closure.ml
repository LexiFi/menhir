(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/closure.ml,v 1.24.4.9 1999/04/05 16:45:37 francois Exp $ *)
(*

Closure operations on constraint graphs.

*)

open Errors
open Types

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The incremental closure code.

*)

let rec merge_leaf_constraint leaf1 leaf2 =
  match_leaves merge_clean_leaf_constraint leaf1 leaf2

and merge_link alpha1 alpha2 =
  match_variables merge_clean_link merge_clean_leaf_constraint alpha1 alpha2

and merge_clean_leaf_constraint leaf1 leaf2 =
  match leaf1, leaf2 with
    TLUB vset, _ ->
      Set7.iter (fun alpha1 -> merge_clean_leaf_upper_bound alpha1 leaf2) vset
  | _, TGLB vset ->
      Set7.iter (fun alpha2 -> merge_clean_leaf_lower_bound leaf1 alpha2) vset
  | TVar alpha1, TVar alpha2 ->
      merge_clean_link alpha1 alpha2
  | _, TLUB _
  | TGLB _, _ ->
      raise (CantHappen "Non-natural GLUB in Closure.merge_leaf_constraint.")

and merge_clean_leaf_upper_bound alpha1 = function
    TVar alpha2 ->
      merge_clean_link alpha1 alpha2
  | TGLB vset ->
      Set7.iter (fun alpha2 -> merge_clean_link alpha1 alpha2) vset
  | TLUB _ ->
      raise (CantHappen "Non-natural LUB in Closure.merge_leaf_upper_bound.");

and merge_clean_leaf_lower_bound leaf1 alpha2 =
  match leaf1 with
    TVar alpha1 ->
      merge_clean_link alpha1 alpha2
  | TLUB vset ->
      Set7.iter (fun alpha1 -> merge_clean_link alpha1 alpha2) vset
  | TGLB _ ->
      raise (CantHappen "Non-natural GLB in Closure.merge_leaf_lower_bound.");

and merge_clean_link alpha1 alpha2 =
  if alpha1 != alpha2 then
    try

      (* If this link is already present in the set, stop here. *)

      if not (Set7.mem alpha1 alpha2.loset) then begin

	let alphaloset1 = Set7.add alpha1 alpha1.loset in
	let alphahiset2 = Set7.add alpha2 alpha2.hiset in

	(* Update the upper bounds, and the guards, of all variables below and including alpha1. *)

	Set7.iter (fun v ->
	  v.hiset <- Set7.union alphahiset2 v.hiset;
	  v.hi <- terms_GLB alpha2.hi v.hi;
	  v.guards <- Set7.union alpha2.guards v.guards
	) alphaloset1;

	(* Update the lower bounds of all variables above and including alpha2. *)

	Set7.iter (fun v ->
	  v.lo <- terms_LUB alpha1.lo v.lo;
	  v.loset <- Set7.union alphaloset1 v.loset
	) alphahiset2;

	(* Check whether any guards are triggered. *)

	let head2 = head_constructor_of_term alpha2.lo in
	Set7.iter (fun (head1, v1, v2) ->
	  if head_constructors_ordered head1 head2 then
	    merge_link v1 v2
        ) alpha2.guards;

	(* Propagate the new constraint obtained between alpha1's constructed lower bound and alpha2's constructed
	   upper bound. *)

	try
	  subconstraints merge_leaf_constraint alpha1.lo alpha2.hi
	with Inconsistent cl ->
	  raise (Inconsistent ((SmallTerm alpha1.lo, SmallTerm alpha2.hi) :: cl))

      end

    with Inconsistent cl ->
      raise (Inconsistent ((LeafTerm (TVar alpha1), LeafTerm (TVar alpha2)) :: cl))

let merge_small_upper_bound alpha bound =
  Errors.trace "Closure.merge_small_upper_bound" (fun () ->
    if alpha.link <> None then
      raise (CantHappen "Attempting to set an upper bound on a linked variable.");

    alpha.hi <- terms_GLB alpha.hi bound;
    Set7.iter (fun v -> v.hi <- terms_GLB v.hi bound) alpha.loset;
    subconstraints merge_leaf_constraint alpha.lo bound
  )
      
let merge_small_lower_bound bound alpha =
  Errors.trace "Closure.merge_small_lower_bound" (fun () ->
    if alpha.link <> None then
      raise (CantHappen "Attempting to set a lower bound on a linked variable.");

    alpha.lo <- terms_LUB alpha.lo bound;
    Set7.iter (fun v -> v.lo <- terms_LUB v.lo bound) alpha.hiset;
    subconstraints merge_leaf_constraint bound alpha.hi
  )

