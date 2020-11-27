open Lexing
open Printf
module E = MenhirLib.ErrorReports
module I = Parser.MenhirInterpreter
module L = MenhirLib.LexerUtil

(* [env checkpoint] extracts a parser environment out of a checkpoint,
   which must be of the form [HandlingError env]. *)

let env checkpoint =
  match checkpoint with
  | I.HandlingError env ->
      env
  | _ ->
      assert false

(* [state checkpoint] extracts the number of the current state out of a
   checkpoint. *)

let state checkpoint : int =
  match I.top (env checkpoint) with
  | Some (I.Element (s, _, _, _)) ->
      I.number s
  | None ->
      (* Hmm... The parser is in its initial state. The incremental API
         currently lacks a way of finding out the number of the initial
         state. It is usually 0, so we return 0. This is unsatisfactory
         and should be fixed in the future. *)
      0

(* [show text (pos1, pos2)] displays a range of the input text [text]
   delimited by the positions [pos1] and [pos2]. *)

let show text positions =
  E.extract text positions
  |> E.sanitize
  |> E.compress
  |> E.shorten 20 (* max width 43 *)

(* [get text checkpoint i] extracts and shows the range of the input text that
   corresponds to the [i]-th stack cell. The top stack cell is numbered zero. *)

let get text checkpoint i =
  match I.get i (env checkpoint) with
  | Some (I.Element (_, _, pos1, pos2)) ->
      show text (pos1, pos2)
  | None ->
      (* The index is out of range. This should not happen if [$i]
         keywords are correctly inside the syntax error message
         database. The integer [i] should always be a valid offset
         into the known suffix of the stack. *)
      "???"

(* [succeed v] is invoked when the parser has succeeded and produced a
   semantic value [v]. *)

let succeed (v : int) =
  printf "%d\n%!" v

(* [fail text buffer checkpoint] is invoked when parser has encountered a
   syntax error. *)

let fail text buffer (checkpoint : int I.checkpoint) =
  (* Indicate where in the input file the error occurred. *)
  let location = L.range (E.last buffer) in
  (* Show the tokens just before and just after the error. *)
  let indication = sprintf "Syntax error %s.\n" (E.show (show text) buffer) in
  (* Fetch an error message from the database. *)
  let message = ParserMessages.message (state checkpoint) in
  (* Expand away the $i keywords that might appear in the message. *)
  let message = E.expand (get text checkpoint) message in
  (* Show these three components. *)
  eprintf "%s%s%s%!" location indication message

(* [process text] runs the parser. *)

let process filename =
  (* Read the file; allocate and initialize a lexing buffer. *)
  let text, lexbuf = L.read filename in
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

(* In this demo, instead of reading one line at a time as in the other demos,
   we read all of our input in one go. This allows multi-line arithmetic
   expressions. We take a file name on the command line. *)

let () =
  process Sys.argv.(1)
