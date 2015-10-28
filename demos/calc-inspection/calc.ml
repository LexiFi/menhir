open MenhirLib.General
open Parser.MenhirInterpreter

(* Instantiate [MenhirLib.Printers] for our parser. This requires providing a
   few printing functions -- see [CalcPrinters]. *)

module P =
  MenhirLib.Printers.Make
    (Parser.MenhirInterpreter)
    (CalcPrinters)

(* Instantiate [ErrorReporting] for our parser. This requires
   providing a few functions -- see [CalcErrorReporting]. *)

module E =
  ErrorReporting.Make
    (Parser.MenhirInterpreter)
    (CalcErrorReporting)

(* Define a printer for explanations. We treat an explanation as if it
   were just an item: that is, we ignore the position information that
   is provided in the explanation. Indeed, this information is hard to
   show in text mode. *)

let print_explanation explanation =
  P.print_item (E.item explanation)

let print_explanations startp explanations =
  Printf.fprintf stderr
    "At line %d, column %d: syntax error.\n"
    startp.Lexing.pos_lnum
    startp.Lexing.pos_cnum;
  List.iter print_explanation explanations;
  flush stderr

(* The rest of the code is as in the [calc] demo. *)

let process (line : string) =
  let lexbuf = Lexing.from_string line in
  try
    let v = E.entry (Parser.Incremental.main()) Lexer.token lexbuf in
    Printf.printf "%d\n%!" v
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg
  | E.Error ((startp, _), explanations) ->
     print_explanations startp explanations

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

