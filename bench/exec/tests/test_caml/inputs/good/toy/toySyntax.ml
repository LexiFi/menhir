(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/toy/toySyntax.ml,v 1.12 2000/05/12 09:17:37 fpottier Exp $ *)

(* This module defines the syntax of language expressions, as accepted by our toy language. *)

type expression =

  (* The $\lambda$-calculus with \texttt{let}, i.e. core ML. *)

  | EVar of string
  | EFun of matching
  | EApp of expression * expression
  | ELet of bool * bindings * expression

  (* Records. *)

  | ERecord of (string * expression) list
  | ERecordAccess of string
  | ERecordUpdate of string
  | ERecordRestrict of string
  | ERecordModify of string
  | ERecordTest of string

  (* Constants and data constructors. *)

  | EConstant of constant
  | EConstruct of string

  (* Sequences, branches. *)

  | ESeq of expression * expression
  | EIf of expression * expression * expression

  (* Arrays. *)

  | EVector of expression list

  (* Exceptions. *)

  | ETry of expression * bindings

and pattern =
  | PWildcard
  | PVar of string
  | PConstant of constant
  | PPair of pattern * pattern
  | PRecord of pattern ToyParserUtil.StringMap.t
  | PConstruct of string * pattern
  | PAlias of pattern * string
  | POr of pattern * pattern
  | PRef of pattern

(* Constants. Their value has an impact on typing when they appear as part of a pattern. *)

and constant =
  | ConstInt of int
  | ConstUnit
  | ConstBool of bool
  | ConstFloat of float
  | ConstChar of char
  | ConstString of string

and matching =
  (pattern list * expression) list

and bindings =
  (pattern * expression) list

(* Toplevel phrases. *)

type phrase =
  | PhraseDirective of string
  | PhraseExpr of expression
  | PhraseLet of bool * bindings
(*i  | PhraseValue of string * type_scheme_expression i*)
  | PhraseInclude of string
(*i  | PhraseAbstractType of string * (bool list) i*)

(* Sets of constants are useful when compiling pattern matchings on constants. *)

module ConstantSet = Set.Make (struct
  type t = constant
  let compare = Pervasives.compare
end)

