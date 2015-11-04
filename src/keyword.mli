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
   side, which he can refer to by position or by name.

   A positional reference of the form [$i] is a syntactic sugar for the
   name [_i]. This surface syntax is first parsed as a [parsed_subject]
   and desugared as a [subject] during keywords rewriting into actual
   OCaml identifiers. (See {!Lexer.transform_keywords}) *)
type parsed_subject =
  | PLeft
  | PRightDollar of int
  | PRightNamed of string

and subject =
  | Left
  | RightNamed of string

(* Keywords inside semantic actions. They allow access to semantic
   values or to position information.

   As said previously, a positional reference is a syntactic sugar
   which appears in a [parsed_keyword] but is desugared in the
   actual [keyword] representation. *)
type parsed_keyword =
  | PDollar of int
  | PPosition of parsed_subject * where * flavor
  | PSyntaxError

and keyword =
  | Position of subject * where * flavor
  | SyntaxError

(* This maps a [Position] keyword to the name of the variable that the
   keyword is replaced with. *)

val posvar: subject -> where -> flavor -> string

(* Sets of keywords. *)
module KeywordSet : sig

  include Set.S with type elt = keyword

  val map: (keyword -> keyword) -> t -> t

end

