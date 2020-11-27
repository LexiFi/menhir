open Lexing
open Printf
module E = MenhirLib.ErrorReports
module G = MenhirLib.General
module I = Parser.MenhirInterpreter
module L = MenhirLib.LexerUtil

(* [stack checkpoint] extracts the parser's stack out of a checkpoint
   that has been returned by the parser after encountering a syntax
   error. *)

let stack checkpoint : I.stack =
  match checkpoint with
  | I.HandlingError env ->
      I.stack env
  | _ ->
      assert false

(* [state checkpoint] extracts the number of the current state out of a
   parser checkpoint. *)

let state checkpoint : int =
  match Lazy.force (stack checkpoint) with
  | G.Nil ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory
         and should be fixed in the future. *)
      0
  | G.Cons (Element (s, _, _, _), _) ->
      I.number s

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)

let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 30

(* [succeed v] is invoked when the parser has succeeded and produced a
   semantic value [v]. *)

let succeed (v : int) =
  printf "%d\n%!" v

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)

let fail text buffer (checkpoint : int I.checkpoint) =
  eprintf
    "%sSyntax error %s.\n%s%!"
    (L.range (E.last buffer))
    (E.show (show text) buffer)
    (ParserMessages.message (state checkpoint))

(* [process text] runs the parser. *)

let process (text : string) =
  (* Allocate and initialize a lexing buffer. *)
  let lexbuf = Lexing.from_string text in
  let lexbuf = L.init "<standard input>" lexbuf in
  (* Wrap the lexer and lexbuf together into a supplier, that is, a
     function of type [unit -> token * position * position]. *)
  let supplier = I.lexer_lexbuf_to_supplier Lexer.token lexbuf in
  (* Equip the supplier with a two-place buffer that records the positions
     of the last two tokens. This is useful when a syntax error occurs, as
     these are the token just before and just after the error. *)
  let buffer, supplier = E.wrap_supplier supplier in
  (* Fetch the parser's initial checkpoint. *)
  let checkpoint = Parser.Incremental.main lexbuf.lex_curr_p in
  (* Run the parser, and handle the lexer's errors. *)
  try
    I.loop_handle succeed (fail text buffer) supplier checkpoint
  with
  | Lexer.Error msg ->
      eprintf "%s%!" msg

(* -------------------------------------------------------------------------- *)

(* The rest of the code is as in the other demos. *)

let process (optional_line : string option) =
  match optional_line with
  | None ->
      ()
  | Some text ->
      process text

let rec repeat channel =
  (* Attempt to read one line. *)
  let optional_line, continue = Lexer.line channel in
  process optional_line;
  if continue then
    repeat channel

let () =
  repeat (from_channel stdin)
