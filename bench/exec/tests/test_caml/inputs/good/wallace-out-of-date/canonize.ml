(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/canonize.ml,v 1.12.2.9 1999/04/01 17:08:58 francois Exp $ *)
(*

Turning a constraint set into an equivalent one with no GLUBs at all.

The code supports decoupling, that is, replacing a variable alpha with two fresh variables gamma and lambda linked
by a constraint gamma <= lambda. Every positive [resp. negative] occurrence of alpha becomes lambda [resp.
gamma]. This is useful to convert type schemes entered by the user into a form where variables with both signs are
prohibited. Decoupling a variable is useful only if it actually has both signs; otherwise, we end up generating
extra variables and constraints to no avail, because a step of garbage collection brings us back to our starting
point. For efficiency, we don't allow the caller to specify which variables should be decoupled; we decouple variables
which have both signs.

*)

open Types

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Mappings from sets of pre-existing variables to fresh variables. We have one mapping for LUBs and one for GLBs.

*)

module VarSetMap = Map.Make (struct
  type t = type_variable Set7.t
  let compare = Set7.compare
end)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

We need to keep track of the pre-existing variables' original bounds throughout the algorithm. However, we will be
updating them in place, which destroys their old value. The solution chosen here is to record the original bounds
in the "representative" field. (Once again, this is dirty, but it's local to this file.) We obtain constant time
access to the original bounds without using more memory. Performance tests show that the speed gain with respect
to using a separate logarithmic-time structure is marginal.

Define the functions that look up original bounds. If a variable has already been traversed, then its bounds have been
rewritten and the original bounds are found in the "representative" field. Otherwise, the bounds haven't been
rewritten yet.

*)

let lookup_original_lo v =
  if Traverse.is_marked v then fst (Obj.magic v.representative : small_term * small_term)
  else v.lo
;;

let lookup_original_hi v =
  if Traverse.is_marked v then snd (Obj.magic v.representative : small_term * small_term)
  else v.hi
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The actual conversion code.

*)

let canonize remove_bipolar (Scheme(context, body, effect) as scheme) =

  (* Create empty mappings. *)

  let lub_mapping = ref VarSetMap.empty
  and glb_mapping = ref VarSetMap.empty in

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Functions to add a new minimal or maximal variable and close the variable graph by transitivity.

*)

  let rec add_minimal_variable gamma vset =

    (* Determine the basic relationships between gamma and the variables it stands for, that is, the elements of
       vset. We must also add their transitive consequences. *)

    let above_gamma = Set7.fold (fun alpha above_gamma ->

      (* Add alpha itself to the upper bounds of gamma. *)

      let above_gamma = Set7.add alpha above_gamma in

      (* Add all of alpha's upper bounds in the current set, i.e. both alphas and lambdas. *)

      Set7.union alpha.hiset above_gamma

    ) vset empty_variable_set in

    (* Now, we know exactly which variables should be above gamma. Create the links, both ways. *)

    Set7.iter (fun v ->
      v.loset <- Set7.add gamma v.loset
    ) above_gamma;

    gamma.hiset <- above_gamma;

    (* Any guarded constraints carried by these variables should be copied onto gamma. This is, again,
       transitivity. *)

    gamma.guards <- Set7.fold (fun v guards ->
      Set7.union v.guards guards
    ) above_gamma empty_guard_set

  and add_maximal_variable lambda vset =

    let below_lambda = Set7.fold (fun alpha below_lambda ->

      let below_lambda = Set7.add alpha below_lambda in

      Set7.union alpha.loset below_lambda

    ) vset empty_variable_set in

    Set7.iter (fun v ->
      v.hiset <- Set7.add lambda v.hiset
    ) below_lambda;

    lambda.loset <- below_lambda

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Creating a fresh variable to stand for a GLUB also involves adding appropriate constraints.

[vset] is the set of variables being replaced. The function must create a fresh variable, add it to the mapping
and add the appropriate constraints to the current constraint set. Recursive calls to rewrite might also modify
the current constraint set; we have to be careful not to stomp on them.

Note that the constraints added to the set (disregarding the new variables' constructed bounds) are of the form
gamma < alpha, alpha < lambda, or gamma < lambda. If we required the set to be simply closed, we would find that
other constraints are necessary, but those would be swept away by garbage collection. So, it is better not to
generate them.

*)

  and create_glb vset =

    (* Create a fresh variable and add it to the mapping. *)

    let gamma = freshV (Set7.choose vset) in
    glb_mapping := VarSetMap.add vset gamma !glb_mapping;

    (* Add the basic relationships with the variables it stands for, that is, the elements of vset, and their
       transitive consequences. *)

    add_minimal_variable gamma vset;

    (* Compute gamma's constructed upper bound. The recursive call to rewrite might add other new variables to the
       mapping. We have already added gamma to the mapping so we can rest assured that we won't loop. Note that
       since we use the original bounds, we are guaranteed not to call rewrite on a term that has already been
       rewritten. *)

    let bound = Set7.fold (fun alpha bound ->
      terms_GLB (lookup_original_hi alpha) bound
    ) vset gamma.hi in
    gamma.hi <- rewrite false bound;

    (* Return the freshly created variable *)

    gamma

  and create_lub vset =

    let lambda = freshV (Set7.choose vset) in
    lub_mapping := VarSetMap.add vset lambda !lub_mapping;

    add_maximal_variable lambda vset;

    let bound = Set7.fold (fun alpha bound ->
      terms_LUB (lookup_original_lo alpha) bound
    ) vset lambda.lo in
    lambda.lo <- rewrite true bound;

    lambda

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Rewrite rewrites a type term using the mappings, possibly extending the mappings as it goes.

*)

  and map_positive_vset vset =
    let lambda = try
      VarSetMap.find vset !lub_mapping
    with Not_found ->
      create_lub vset
      
    in TVar lambda

  and map_negative_vset vset =
    let gamma = try
      VarSetMap.find vset !glb_mapping
    with Not_found ->
      create_glb vset

    in TVar gamma

  and behavior sign = function

    (TVar alpha) as term ->

      (* Pass alpha to do_variable so that alpha's bounds are rewritten. *)

      do_variable alpha;

      (* If this variable has both signs, and if bipolar variables are to be eliminated, split it. *)

      if (remove_bipolar & (alpha.sign = Bipolar)) then begin

      	let vset = Set7.add alpha empty_variable_set in
      	if sign then map_positive_vset vset
      	else map_negative_vset vset

      end
      else
	term

  | TLUB vset ->

      Set7.iter do_variable vset;
      map_positive_vset vset

  | TGLB vset ->

      Set7.iter do_variable vset;
      map_negative_vset vset

  and rewrite sign term =
    Walk.walk behavior sign term

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Do_variable makes sure that the supplied variable (seen as node in the contraint graph) has been traversed.
It must be called on all variables to ensure that the whole graph is traversed.

Note that it is critical that each variable be traversed at most once here, since it ensures that we don't call
rewrite on a bound that has already been rewritten.

*)

  and do_variable v =

    if not (Traverse.is_marked v) then begin
      Traverse.mark v;

      (* We are going to modify v's bounds. Save them first so they can still be retrieved if necessary. *)

      v.representative <- Obj.magic (v.lo, v.hi);

      v.lo <- rewrite true v.lo;
      Set7.iter do_variable v.loset;
      Set7.iter do_variable v.hiset;
      v.hi <- rewrite false v.hi;
      Set7.iter (fun (_, v1, v2) -> do_variable v1; do_variable v2) v.guards
    end in

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The bulk of the conversion is done here.

*)

  Traverse.start();

  (* Rewrite the context and the body. *)

  Set7.iter (fun (_, v) -> do_variable v) context;
  do_variable body;
  do_variable effect;

  (* Reset the "representative" field to its normal value. This is doubly necessary; first, the outside world
     (more precisely, the scheme copy operation) expects this field to point to the variable itself; second,
     resetting the value of this field allows O'Caml's garbage collector to destroy the original bounds. *)

  Walk.iter_scheme (fun v -> v.representative <- v) scheme
;;
