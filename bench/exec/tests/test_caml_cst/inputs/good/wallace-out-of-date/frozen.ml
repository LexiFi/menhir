(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/frozen.ml,v 1.19.2.14 1999/04/05 16:45:38 francois Exp $ *)
(*

Converting common data structures to and from frozen form, to allow input/output operations.

*)

open Rowsig
open Types
open Env

let rec freeze_var v =
  if not (Traverse.is_marked v) then begin
    Traverse.mark v;

    freeze_type v.lo;
    Set7.iter freeze_var v.loset;
    Set7.freeze v.loset;
    Set7.iter freeze_var v.hiset;
    Set7.freeze v.hiset;
    freeze_type v.hi;
    Set7.iter (fun (_, v1, v2) -> freeze_var v1; freeze_var v2) v.guards;
    Set7.freeze v.guards;

    freeze_span v.span;
    freeze_link v.link
  end

and freeze_span = function
    Remainder lset ->
      RowSet.freeze lset
  | _ ->
      ()

and freeze_link = function
    Some row_term ->
      freeze_row row_term
  | None ->
      ()

and freeze_leaf = function
| TVar v ->
    freeze_var v
| TLUB vset ->
    Set7.iter freeze_var vset;
    Set7.freeze vset
| TGLB vset ->
    Set7.iter freeze_var vset;
    Set7.freeze vset

and freeze_type = function
  TBottom
| TTop
| TRTop
| TRMissing
| TVAbsent
| TRBottom
| TVTop ->
    ()
| TAbstract(_, args) ->
    List.iter freeze_leaf args
| TRPresent ty ->
    freeze_leaf ty
| TRMaybe ty ->
    freeze_leaf ty
| TVPresent ty ->
    freeze_leaf ty

and freeze_row row =
  freeze_leaf row.remainder;
  RowMap.iter freeze_leaf row.entries;
  RowMap.freeze row.entries
;;

let rec unfreeze_var v =
  if not (Traverse.is_marked v) then begin
    Traverse.mark v;

    unfreeze_type v.lo;
    Set7.unfreeze v.loset compare_variables;
    Set7.iter unfreeze_var v.loset;
    Set7.unfreeze v.hiset compare_variables;
    Set7.iter unfreeze_var v.hiset;
    unfreeze_type v.hi;
    Set7.unfreeze v.guards compare_guards;
    Set7.iter (fun (_, v1, v2) -> unfreeze_var v1; unfreeze_var v2) v.guards;

    unfreeze_span v.span;
    unfreeze_link v.link
  end

and unfreeze_span = function
    Remainder lset ->
      RowSet.unfreeze lset
  | _ ->
      ()

and unfreeze_link = function
    Some row_term ->
      unfreeze_row row_term
  | None ->
      ()

and unfreeze_leaf = function
| TVar v ->
    unfreeze_var v
| TLUB vset ->
    Set7.unfreeze vset compare_variables;
    Set7.iter unfreeze_var vset
| TGLB vset ->
    Set7.unfreeze vset compare_variables;
    Set7.iter unfreeze_var vset

and unfreeze_type = function
  TBottom
| TTop
| TRTop
| TRMissing
| TVAbsent
| TRBottom
| TVTop ->
    ()
| TAbstract(_, args) ->
    List.iter unfreeze_leaf args
| TRPresent ty ->
    unfreeze_leaf ty
| TRMaybe ty ->
    unfreeze_leaf ty
| TVPresent ty ->
    unfreeze_leaf ty

and unfreeze_row row =
  RowMap.unfreeze row.entries;
  RowMap.iter unfreeze_leaf row.entries;
  unfreeze_leaf row.remainder
;;

let freeze_context context =
  Set7.iter (fun (_, term) -> freeze_var term) context;
  Set7.freeze context
;;

let unfreeze_context context =
  Set7.unfreeze context Types.compare_labeled_pairs;
  Set7.iter (fun (_, term) -> unfreeze_var term) context
;;

let freeze_scheme (Scheme(context, body, effect)) =
  Traverse.start();
  freeze_context context; freeze_var body; freeze_var effect
;;

let unfreeze_scheme (Scheme(context, body, effect)) =
  Traverse.start();
  unfreeze_context context; unfreeze_var body; unfreeze_var effect
;;

let freeze_environment_element = function
  Lambda id ->
    ()
| Let(_, scheme) ->
    freeze_scheme scheme
;;

let unfreeze_environment_element = function
  Lambda id ->
    ()
| Let(_, scheme) ->
    unfreeze_scheme scheme
;;

let freeze_environment env =
  List.iter freeze_environment_element env
;;

let unfreeze_environment env =
  List.iter unfreeze_environment_element env
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Dumping an environment to disk.

*)

let dump env =
  let channel = open_out "dump" in
  freeze_environment env;
  output_value channel env;
  unfreeze_environment env;
  close_out channel;
  print_endline "Type environment dumped to file \"dump\".";
  flush stdout
;;

let undump () =
  let channel = open_in "dump" in
  let env = (input_value channel : Env.environment) in
  unfreeze_environment env;
  env
;;
