open Lexing

(* A short name for the incremental parser API. *)

module I =
  Parser.MenhirInterpreter

(* -------------------------------------------------------------------------- *)

(* The above loop is shown for explanatory purposes, but can in fact be
   replaced with the following code, which exploits the functions
   [lexer_lexbuf_to_supplier] and [loop_handle] offered by Menhir. *)

let succeed (v : int) =
  (* The parser has succeeded and produced a semantic value. Print it. *)
  Printf.printf "%d\n%!" v

let fail lexbuf (_ : int I.checkpoint) =
  (* The parser has suspended itself because of a syntax error. Stop. *)
  Printf.fprintf stderr
    "At offset %d: syntax error.\n%!"
    (lexeme_start lexbuf)

let loop lexbuf result =
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  I.loop_handle succeed (fail lexbuf) supplier result

(* -------------------------------------------------------------------------- *)

(* Initialize the lexer, and catch any exception raised by the lexer. *)

let process (line : string) =
  let lexbuf = from_string line in
  try
    loop lexbuf (Parser.Incremental.main lexbuf.lex_curr_p)
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg

(* -------------------------------------------------------------------------- *)

(* The rest of the code is as in the [calc] demo. *)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some line ->
      process line

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel

let () =
  repeat (from_channel stdin)

