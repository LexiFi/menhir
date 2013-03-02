open Printf
open Print
open Source

module Make (P : sig

  val print: var printer

end) = struct

  let print_set_function = function
    | SFSupport ->
	"free"
    | SFOuter ->
	"outer"
    | SFInner ->
	"inner"
    | SFBound ->
	"bound"

  let print_set_entity b (f, x) =
    bprintf b "%s(%a)" (print_set_function f) P.print x

  let print_setsetset_operator op b =
    bprintf b "%s" (
      match op with
      | OpUnion ->
	  " U "
      | OpIntersection ->
	  " @ "
      | OpDifference ->
	  " \\ "
    )

  let rec print_atomic_set_expression b s =
    match s with
    | SEEmpty ->
	bprintf b "empty"
    | SEApp entity ->
	print_set_entity b entity
    | SEAssocOp _ ->
	bprintf b "(%a)" print_set_expression s
    | SEConditional (c, s1, SEEmpty) ->
	bprintf b "if %a then %a end" print_constraint c print_set_expression s1
    | SEConditional (c, s1, s2) ->
	bprintf b "if %a then %a else %a end" print_constraint c print_set_expression s1 print_set_expression s2

  and print_set_expression b s =
    match s with
    | SEEmpty
    | SEApp _
    | SEConditional _ ->
	print_atomic_set_expression b s
    | SEAssocOp (op, ss) ->
	seplist (print_setsetset_operator op) print_atomic_set_expression b ss

  and print_setsetbool_operator = function
    | OpSubset ->
	"<="
    | OpEqual ->
	"=="
    | OpNotEqual ->
	"!="
    | OpDisjoint ->
	"#"

  and print_boolboolbool_operator op b =
    bprintf b "%s" (
      match op with
      | OpConjunction ->
	  " and "
      | OpDisjunction ->
	  " or "
      | OpImplication ->
	  " -> "
      | OpEquivalence ->
	  " <-> "
    )

  and print_atomic_constraint b c =
    match c with
    | FTrue ->
	bprintf b "true"
    | FFalse ->
	bprintf b "false"
    | FBoolVar x ->
	P.print b x
    | FNot c ->
	bprintf b "not %a" print_atomic_constraint c
    | FBoolAssocOp _ ->
	bprintf b "(%a)" print_constraint c
    | FSetBinOp (s1, op, s2) ->
	bprintf b "%a %s %a" print_set_expression s1 (print_setsetbool_operator op) print_set_expression s2

  and print_constraint b c =
    match c with
    | FTrue
    | FFalse
    | FBoolVar _
    | FNot _
    | FSetBinOp _ ->
	print_atomic_constraint b c
    | FBoolAssocOp (op, cs) ->
	seplist (print_boolboolbool_operator op) print_atomic_constraint b cs

  let rec print_toplevel_constraint b c =
    match c with
    | FTrue ->
	()
    | FBoolAssocOp (OpConjunction, cs) ->
	list print_toplevel_constraint b cs
    | _ ->
	bprintf b "%a%t" print_constraint c nl

end
