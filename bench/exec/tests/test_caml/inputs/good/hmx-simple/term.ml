(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/term.ml,v 1.1.4.1 2003/01/27 12:42:48 fpottier Exp $ *)

(* This tiny signature describes the operations which [Herbrand] expects are supported by row labels. *)

module type OrderedPrintable = sig

  type t

  val compare: t -> t -> int
  val print: t -> string

end

(* This signature documents the operations which [Herbrand] expects are supported by the free term algebra. *)

module type Algebra = sig

  (* The type of terms. *)

  type 'a term

  (* Abstract operations on terms. *)

  exception Iter2

  val arity: 'a term -> int
  val map: ('a -> 'b) -> 'a term -> 'b term
  val fork: ('a -> 'b * 'c) -> 'a term -> 'b term * 'c term
  val iter: ('a -> unit) -> 'a term -> unit
  val fold: ('a -> 'b -> 'b) -> 'a term -> 'b -> 'b
  val iter2: ('a -> 'b -> unit) -> 'a term -> 'b term -> unit

  (* The type of labels, used to name every argument of every type constructor. *)

  type label

  (* [print term] returns a list of labeled sub-terms and
     tokens. [parenthesize label subterm] tells whether the given
     [subterm] must be parenthesized, if found at the given [label]
     within a larger term. [safe label] tells whether a label occurs
     between two tokens, i.e. subterms at this label \emph{never} need
     to be parenthesized. *)

  val print: 'a term -> (label * 'a) Tree.element list
  val parenthesize: label -> 'a term -> bool
  val safe: label -> bool

end

