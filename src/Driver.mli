(* The module [Driver] serves to offer a unified API to the parser,
   which could be produced by either ocamlyacc or Menhir. *)

val grammar :
  (Lexing.lexbuf  -> Parser.token) -> Lexing.lexbuf -> Syntax.partial_grammar
