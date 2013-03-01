(* This module provides some type and function definitions
   that help deal with the keywords that we recognize within
   semantic actions. *)

(* The user can request position information either at type
   [int] (a simple offset) or at type [Lexing.position]. *)

type flavor =
  | FlavorOffset
  | FlavorPosition

(* The user can request position information about the
   start or end of a symbol. *)

type where =
  | WhereStart
  | WhereEnd

(* The user can request position information about a production's
   left-hand side or about one of the symbols in its right-hand
   side, which he can refer to by position or by name. *)

type subject =
  | Left
  | RightDollar of int
  | RightNamed of string

(* Keywords inside semantic actions. They allow access to semantic
   values or to position information. *)

type keyword =
  | Dollar of int
  | Position of subject * where * flavor
  | PreviousError
  | SyntaxError

(* This maps a [Position] keyword to the name of the variable that the
   keyword is replaced with. *)

val posvar: subject -> where -> flavor -> string

(* Sets of keywords. *)
module KeywordSet : 
  sig 
    include Set.S

    (* This converts a list of keywords with positions into a set of keywords. *)
    val from_list: elt list -> t

  end with type elt = keyword

