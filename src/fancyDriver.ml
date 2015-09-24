(* The module [Driver] serves to offer a unified API to the parser,
   which could be produced by either ocamlyacc or Menhir. *)

(* This is the Menhir-specific driver. We wish to handle syntax errors
   in a more ambitious manner, so as to help our end users understand
   their mistakes. *)

(* TEMPORARY a lot of code is copied from [Engine]. Can we avoid it? *)

(* A short name for the incremental parser API. *)

module I =
  Parser.MenhirInterpreter

(* The loop which drives the parser. At each iteration, we analyze a
   result produced by the parser, and act in an appropriate manner. *)

(* [lexbuf] is the lexing buffer. [result] is the last result produced
   by the parser. *)

let rec loop lexer lexbuf (result : 'a I.result) : 'a =
  let open Lexing in
  match result with
  | I.InputNeeded _ ->
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         result. Then, repeat. *)
      let token = lexer lexbuf in
      let result = I.offer result (token, lexbuf.lex_start_p, lexbuf.lex_curr_p) in
      loop lexer lexbuf result
  | I.Shifting _
  | I.AboutToReduce _ ->
      let result = I.resume result in
      loop lexer lexbuf result
  | I.HandlingError _env ->
      (* The parser has suspended itself because of a syntax error. Stop. *)
      (* TEMPORARY *)
      Printf.fprintf stderr
        "At offset %d: syntax error.\n%!"
        (lexeme_start lexbuf);
      exit 1
  | I.Accepted v ->
      v
  | I.Rejected ->
      (* The parser rejects this input. This cannot happen because
         we stop as soon as the parser reports [HandlingError]. *)
      assert false

(* The entry point. *)

module F = FancyParserMessages (* TEMPORARY *)

let grammar lexer lexbuf =
  loop lexer lexbuf (Parser.Incremental.grammar())

