(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/type_expr.ml,v 1.28.2.14 1999/04/05 16:45:40 francois Exp $ *)
(*

Converting type expressions to types.

*)

open Errors
open Rowsig
open Types
open Closure
open Small
open Env

type type_expression =
    TEBottom
  | TETop
  | TEVar of string
  | TERecord of (string * type_expression) list * type_expression
  | TEVariant of (string * type_expression) list * type_expression
  | TEAbstract of string * (type_expression list)
  | TEAbsent
  | TEPresent of type_expression
  | TEMissing
  | TEMaybe of type_expression

and context_expression =
  (string * type_expression) list

and constrained_type_expression =
  type_expression * type_expression * ((type_expression list) * (type_expression list)) list

and type_scheme_expression =
  context_expression * type_expression * type_expression * ((type_expression list) * (type_expression list)) list

exception MalformedTypeExpression of string
exception ArityMismatch of string * int * int

(* ----------------------------------------------------------------------------------------------------------------- *)

let empty_row_map = RowMap.empty()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

We need to map variable names to variables.

*)

module StringMap = Map.Make (struct
  type t = string
  let compare = Pervasives.compare
end)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This exception is raised if the user-supplied types are not well-kinded.

*)

exception IllKinded

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Converting type expressions to types.

*)

let mapping = ref StringMap.empty;;

let rec type_expression_to_leaf_term sign ((expectedS, expectedK) as expectedSK) = function
    
    TEVar name -> TVar (
      try
	let v = StringMap.find name !mapping in
	if (compare_spans expectedS v.span <> 0) or (expectedK <> kind_of_variable v) then
	  raise IllKinded;
	v
      with Not_found ->
	let v = freshSK expectedS expectedK in
	mapping := StringMap.add name v !mapping;
	v
    )
  | expr ->
      (if sign then spos else sneg) expectedS
	(type_expression_to_small_term sign expectedSK expr)

and type_expression_to_small_term sign ((expectedS, expectedK) as expectedSK) texpr =
  match texpr, expectedS, expectedK with
    TEVar _, _, _ ->
      raise (CantHappen "TEVar in Type_expr.type_expression_to_small_term.")
  | TEBottom, _, KRegular ->
      TBottom
  | TETop, _, KRegular ->
      TTop
  | TERecord (entries, remainder), Field, KRegular ->
      let entries = row_entries_expression_to_row_entries sign KRecord entries in
      let signature = RowMap.domain entries in
      let remainder = type_expression_to_leaf_term sign (Remainder signature, KRecord) remainder in
      make_trecord entries remainder
  | TEVariant (entries, remainder), Field, KRegular ->
      let entries = row_entries_expression_to_row_entries sign KVariant entries in
      let signature = RowMap.domain entries in
      let remainder = type_expression_to_leaf_term sign (Remainder signature, KVariant) remainder in
      make_tvariant entries remainder
  | TEAbstract (name, elist), _, KRegular ->
      let stamp = Abstract.get_stamp name in
      let definition = Abstract.get_variance stamp in

      let rec loop variances args = match (variances, args) with
	[], [] ->
	  []
      | variance :: variances, arg :: args ->
	  (type_expression_to_leaf_term (if variance then sign else not sign) expectedSK arg) ::
	    (loop variances args)
	| _, _ ->
	    raise (ArityMismatch (name, List.length definition, List.length elist)) in

    	TAbstract(stamp, loop definition elist)
  | TEAbsent, _, KVariant ->
      TVAbsent
  | TEPresent expr, _, KVariant ->
      TVPresent (type_expression_to_leaf_term sign (expectedS, KRegular) expr)
  | TEPresent expr, _, KRecord ->
      TRPresent (type_expression_to_leaf_term sign (expectedS, KRegular) expr)
  | TEMaybe expr, _, KRecord ->
      TRMaybe (type_expression_to_leaf_term sign (expectedS, KRegular) expr)
  | (TEMissing | TEAbsent), _, KRecord ->
      TRMissing
  | _, _, _ ->
      raise IllKinded

and row_entries_expression_to_row_entries sign kind = function
  [] ->
    empty_row_map
| (name, expr) :: rest ->
    try
      RowMap.add name (type_expression_to_leaf_term sign (Field, kind) expr)
	(row_entries_expression_to_row_entries sign kind rest)
    with RowMap.StrictAdd ->
      raise (MalformedTypeExpression ("Duplicate row entry: " ^ name))
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Converting constraint set expressions to constraint graphs.

*)

let do_ceset ceset =
  List.iter (fun (exprlist1, exprlist2) ->
    List.iter (fun expr1 ->
      List.iter (fun expr2 ->

	(* Because we distinguish leaf terms from small terms, we are forced to perform a large switch here.
	   (We could convert both sides to leaf terms, but that would cause unnecessary garbage collection
	   when they are already type variables.) *)

	(* TEMPORARY It is totally incorrect to expect these types to have SK = (Field, KRegular)!!!
	   In fact, top-down kind checking is nonsensical. A unification-based kind computation must
	   be used. *)

	match (expr1, expr2) with
	  TEVar _, TEVar _ ->
	    let leaf1 = type_expression_to_leaf_term true (* irrelevant *) (Field, KRegular) expr1
	    and leaf2 = type_expression_to_leaf_term false (* irrelevant *) (Field, KRegular) expr2 in
	    merge_leaf_constraint leaf1 leaf2
	| TEVar _, _ ->
	    let leaf1 = type_expression_to_leaf_term true (* irrelevant *) (Field, KRegular) expr1
	    and term2 = type_expression_to_small_term false (Field, KRegular) expr2 in
	    merge_small_upper_bound (leaf_to_variable leaf1) term2
	| _, TEVar _ ->
	    let term1 = type_expression_to_small_term true (Field, KRegular) expr1
	    and leaf2 = type_expression_to_leaf_term false (* irrelevant *) (Field, KRegular) expr2 in
	    merge_small_lower_bound term1 (leaf_to_variable leaf2)
	| _, _ ->
	    let term1 = type_expression_to_small_term true (Field, KRegular) expr1
	    and term2 = type_expression_to_small_term false (Field, KRegular) expr2 in
	    subconstraints merge_leaf_constraint term1 term2

      ) exprlist2
    ) exprlist1
  ) ceset
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Converting contexts.

*)

let rec context_expr_to_context environment = function
  [] ->
    empty_context
| (name, expr) :: rest ->
    if List.mem_assoc name rest then
      raise (MalformedTypeExpression ("variable " ^ name ^ " appears twice in context."));

    let context = context_expr_to_context environment rest in
    try
      let identifier = match lookup_name name environment with
        Lambda identifier ->
	  identifier
      | Let _ ->
      	  raise (MalformedTypeExpression ("variable " ^ name ^ " is let-bound instead of lambda-bound.")) in

      let leaf = type_expression_to_leaf_term false (Field, KRegular) expr in
      Set7.add (identifier, leaf_to_variable leaf) context
    with
      Not_found ->
        raise (MalformedTypeExpression ("variable " ^ name ^ " is unbound."))
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Converting type schemes.

*)

let ctype_expression_to_ctype environment (body_expr, effect_expr, ceset) =

  Errors.trace "Type_expr.ctype_expression_to_ctype" (fun () ->

    do_ceset ceset;
    let body = type_expression_to_leaf_term false (Field, KRegular) body_expr
    and effect = type_expression_to_leaf_term false (Field, KRegular) effect_expr in

    mapping := StringMap.empty;
    Ctype(leaf_to_variable body, leaf_to_variable effect)

  )
;;

let scheme_expression_to_scheme environment (context_expr, body_expr, effect_expr, ceset) =

  Errors.trace "Type_expr.scheme_expression_to_scheme" (fun () ->

    do_ceset ceset;
    let context = context_expr_to_context environment context_expr in
    let body = type_expression_to_leaf_term true (Field, KRegular) body_expr in
    let effect = type_expression_to_leaf_term true (Field, KRegular) effect_expr in

    mapping := StringMap.empty;
    Scheme(context, leaf_to_variable body, leaf_to_variable effect)

  )
;;
