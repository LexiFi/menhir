open MenhirLib.General
open Parser.MenhirInterpreter

(* --------------------------------------------------------------------------- *)

(* In order to submit artificial tokens to the parser, we need a function that
   converts a terminal symbol to a (dummy) token. Unfortunately, we cannot (in
   general) auto-generate this code, because it requires making up semantic
   values of arbitrary OCaml types. *)

let terminal2token (type a) (symbol : a terminal) : token =
  let open Parser in
  match symbol with
  | T_TIMES ->
      TIMES
  | T_RPAREN ->
      RPAREN
  | T_PLUS ->
      PLUS
  | T_MINUS ->
      MINUS
  | T_LPAREN ->
      LPAREN
  | T_INT ->
      INT 0
  | T_EOL ->
      EOL
  | T_DIV ->
      DIV
  | T_error ->
      assert false

(* In order to print syntax error messages and/or debugging information, we
   need a symbol printer. *)

let print_symbol symbol =
  match symbol with
  | X (T T_TIMES) ->
      "*"
  | X (T T_RPAREN) ->
      ")"
  | X (T T_PLUS) ->
      "+"
  | X (T T_MINUS) ->
      "-"
  | X (T T_LPAREN) ->
      "("
  | X (T T_INT) ->
      "INT"
  | X (N N_expr) ->
      "expr"
  | X (N N_main) ->
      "main"
  | X (T T_EOL) ->
      "EOL"
  | X (T T_DIV) ->
      "/"
  | X (T T_error) ->
      "error"

(* In order to print a view of the stack that includes semantic values,
   we need an element printer. (If we don't need this feature, then
   [print_symbol] above suffices.) *)

let print_element e : string =
  match e with
  | Element (s, v, _, _) ->
      match incoming_symbol s with
      | T T_TIMES ->
          "*"
      | T T_RPAREN ->
          ")"
      | T T_PLUS ->
          "+"
      | T T_MINUS ->
          "-"
      | T T_LPAREN ->
          "("
      | T T_INT ->
          string_of_int v
      | N N_expr ->
          string_of_int v
      | N N_main ->
          string_of_int v
      | T T_EOL ->
          ""
      | T T_DIV ->
          "/"
      | T T_error ->
          "error"

(* --------------------------------------------------------------------------- *)

(* TEMPORARY comment *)

module P =
  MenhirLib.Printers.Make (Parser.MenhirInterpreter) (struct
    let print s = Printf.fprintf stderr "%s" s
    let print_symbol s = print (print_symbol s)
    let print_element = Some (fun s -> print (print_element s))
  end)

module E =
  MenhirLib.ErrorReporting.Make(Parser.MenhirInterpreter) (struct
    let terminal2token = terminal2token
  end)

let print_explanation explanation =
  (* TEMPORARY not satisfactory at all *)
  P.print_item explanation.MenhirLib.ErrorReporting.item

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

