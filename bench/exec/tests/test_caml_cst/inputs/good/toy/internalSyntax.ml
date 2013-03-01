(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/toy/internalSyntax.ml,v 1.6 2000/05/12 09:17:37 fpottier Exp $ *)

(* This module defines the syntax of language expressions, as shown internally to the type checker.
   For simplicity, it is much smaller than the external syntax. *)

(* Program terms. *)

type pattern =
  | KWildcard
  | KVar of string
  | KUnit

type expression =

  | VVar of string
  | VLambda of pattern * expression
  | VApp of expression * expression
  | VLet of pattern * expression * expression

  | VRecordAccess of string
  | VRecordUpdate of string
  | VRecordRestrict of string
  | VRecordModify of string
  | VRecordTest of string

  | VConstruct of string
  | VMatch of string

(* A term pretty-printer is provided to help debug the compiler. *)

open Format

let _ =
  set_max_boxes max_int

let shield do_it content =
  if do_it then
    print_string "(";
  content();
  if do_it then
    print_string ")"

let priority = function
  | VVar _
  | VMatch _
  | VRecordAccess _
  | VRecordRestrict _
  | VRecordUpdate _
  | VRecordModify _
  | VRecordTest _ ->
      0
  | VApp _ | VConstruct _ ->
      1
  | VLet _ | VLambda _ ->
      2

let print_pat = function
  | KWildcard -> "_"
  | KVar x -> x
  | KUnit -> "()"

let rec print level e =
  open_box 0;
  let priority = priority e in
  shield (priority >= level) (fun () ->
    match e with
    | VVar name ->
	print_string name
    | VLambda (pat, e) ->
	printf "fun %s ->" (print_pat pat);
	print_space(); print (priority+1) e
    | VApp(e1, e2) ->
	print (priority+1) e1; print_space(); print priority e2
    | VLet(pat, e1, e2) ->
	printf "let %s =" (print_pat pat);
	print_space();
	print (priority+1) e1;
	print_space(); print_string "in"; print_space();
	print (priority+1) e2
    | VRecordAccess label ->
	printf ".%s" label
    | VRecordRestrict label ->
	printf "\\%s" label
    | VRecordUpdate label ->
	printf ".%s<-" label
    | VRecordModify label ->
	printf "~%s" label
    | VRecordTest label ->
	printf "?%s" label
    | VConstruct label ->
	print_string label
    | VMatch label ->
	printf "%s~" label
  );
  close_box()

let print e =
  print max_int e;
  print_flush()

