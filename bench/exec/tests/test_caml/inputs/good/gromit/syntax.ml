(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/gromit/syntax.ml,v 1.6 2000/02/11 16:15:37 fpottier Exp $ *)

(* This module provides abstract syntax for Gromit's type algebra definition files. *)

(* A signature consists of a series of lattice definitions, plus a priority list. *)

type signature = lattice list * string list

(* A lattice consists of a name, followed by a series of lattice components. *)

and lattice =
  | Lattice of string * lattice_component list

(* A lattice component is one of a constructor definition, a label definition, or an ordering definition.
   \begin{itemize}
   \item A constructor definition includes the constructor's name, as well as its arity, plus its printed
         appearance.
   \item A label definition includes a variance, a flag which tells whether the label's argument is to be a row,
         a lattice name (which indicates the label's argument's kind), and the label name.
   \item An ordering definition is an ordered pair of constructor names.
   \end{itemize} *)

and lattice_component =
  | ComponentConstructor of string * arity * appearance
  | ComponentLabel of variance * sort * string * string
  | ComponentOrdering of string * string

and sort =
  | SortRegular
  | SortRow

(* An arity is a list of labels. *)

and arity = string list

(* An appearance is a list of labels and tokens, plus one token, representing the head constructor alone.
   Labels may be parenthesized. *)

and appearance = label_or_token list * string

and label_or_token =
  | Label of string * bool
  | Token of string

(* A variance annotation is one of ``covariant'' or ``contravariant''. *)

and variance =
  | Covariant
  | Contravariant

(* This exception is used by the parser to give accurate error reports. *)

exception SyntaxError of string

