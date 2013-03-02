(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/subst.ml,v 1.16.4.7 1999/02/18 20:07:19 francois Exp $ *)
(*

Substitutions on types.

*)

open Types

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Multi-variable substitutions. These are performed using the "representative" field in the type_variable structure,
so the substitution is not explicitly passed to the call. These fields are set by the minimization algorithm.

*)

let behavior _ = function
  TVar v when v.representative != v ->
    TVar v.representative
| leaf ->
    leaf

let multi_typ term =
  Walk.walk behavior true term

let multi_guards guards =
  Set7.fold (fun (head, v1, v2) accu ->
    Set7.add (head, v1.representative, v2.representative) accu
  ) guards empty_guard_set

let must_be_kept v =
  v.representative == v

let filter vset =
  Set7.fold (fun elem accu -> if must_be_kept elem then Set7.add elem accu else accu) vset empty_variable_set

let multi_type_scheme (Scheme(context, body, effect) as scheme) =

  (* Loop over variables. For each variable that must remain, update its bounds. *)

  Walk.iter_scheme (fun v ->
    if must_be_kept v then begin

      v.lo <- multi_typ v.lo;
      v.loset <- filter v.loset;
      v.hiset <- filter v.hiset;
      v.hi <- multi_typ v.hi;
      v.guards <- multi_guards v.guards

    end
  ) scheme;

  (* Update the scheme's entry points. *)

  Scheme(
    Set7.endo_map (function (identifier, v) -> (identifier, v.representative)) context,
    body.representative,
    effect.representative
  )
;;

