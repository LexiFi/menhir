open MenhirLib.General
open Parser.MenhirInterpreter

(* Instantiate [MenhirLib.Printers] for our parser. This requires providing a
   few printing functions -- see [CalcPrinters]. *)

module P =
  MenhirLib.Printers.Make
    (Parser.MenhirInterpreter)
    (CalcPrinters)

(* Instantiate [MenhirLib.ErrorReporting] for our parser. This requires
   providing a few functions -- see [CalcErrorReporting]. *)

module E =
  MenhirLib.ErrorReporting.Make
    (Parser.MenhirInterpreter)
    (CalcErrorReporting)

let print_explanation explanation =
  (* not printing positions in the past, as they are hard to show in a
     meaningful way in text mode *)
  P.print_item (E.item explanation)

(* Initialize the lexer, and catch any exception raised by the lexer. *)

let process (line : string) =
  let lexbuf = Lexing.from_string line in
  try
    let v = E.entry (Parser.Incremental.main()) Lexer.token lexbuf in
    Printf.printf "%d\n%!" v
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | E.Error ((startp, _), explanations) ->
      Printf.fprintf stderr
        "At line %d, column %d: syntax error.\n"
        startp.Lexing.pos_lnum
        startp.Lexing.pos_cnum;
      List.iter print_explanation explanations;
      flush stderr

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
  repeat (Lexing.from_channel stdin)

