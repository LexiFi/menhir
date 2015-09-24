(* The module [Driver] serves to offer a unified API to the parser,
   which could be produced by either ocamlyacc or Menhir. *)

(* This is the Menhir-specific driver. We wish to handle syntax errors
   in a more ambitious manner, so as to help our end users understand
   their mistakes. *)

module F = FancyParserMessages (* TEMPORARY *)

let grammar lexer lexbuf =
  try
    Parser.grammar lexer lexbuf
  with Parsing.Parse_error ->
    Error.error (Positions.lexbuf lexbuf) "Syntax error."

