(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/toy/toyParserUtil.ml,v 1.2 2000/02/11 16:16:35 fpottier Exp $ *)

(* This module makes some definitions which allow the parser to communicate with the logically following modules. *)

(* Sets and maps over strings. *)

module OrderedString = struct
  type t = string
  let compare = Pervasives.compare
end

module StringMap = FixMap.Make(SetBasedMap.Make(Baltree.Height))(OrderedString)
module StringSet = StringMap.Domain

(* This exception is raised when a record pattern contains a duplicate label. *)

exception DuplicateLabel of string

