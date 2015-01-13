(* Introduce a short name for the incremental parser API. *)

module I =
  Parser.MenhirInterpreter

(* TEMPORARY *)

module Essai = (I : sig 
  include MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
  include MenhirLib.IncrementalEngine.INSPECTION
    with type 'a lr1state := 'a lr1state
    with type production := production
end)

(* A measure of the stack height. Used as a primitive way of
   testing the [view] function. *)

let height env =
  I.length (I.view env)

(* Printing a symbol. *)

let print_symbol symbol =
  let open I in
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

module P =
  Printers.Make(I)(I) (struct
    let arrow = " -> "
    let dot = "."
    let space = " "
    let print_symbol = print_symbol
  end)

(* Printing an element. *)

let print_element e : string =
  match e with
  | I.Element (s, v, _, _) ->
      let open I in
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

(* Debugging. *)

let dump env =
  Printf.fprintf stderr "Stack height: %d\n%!" (height env);
  Printf.fprintf stderr "Stack view:\n%s\n%!" (P.print_env print_element env);
  begin match Lazy.force (I.view env) with
  | I.Nil ->
      ()
  | I.Cons (I.Element (current, _, _, _), _) ->
      Printf.fprintf stderr "Current state: %d\n%!" (Obj.magic current);
      let items = I.items current in
      Printf.fprintf stderr "#Items: %d\n%!" (List.length items);
      List.iter (fun item ->
        Printf.fprintf stderr "%s\n%!" (P.print_item item)
      ) items
  end;
  print_newline()

(* Define the loop which drives the parser. At each iteration,
   we analyze a result produced by the parser, and act in an
   appropriate manner. *)

let rec loop linebuf (result : int I.result) =
  match result with
  | I.InputNeeded env ->
      dump env;
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         result. Then, repeat. *)
      let token = Lexer.token linebuf in
      let startp = linebuf.Lexing.lex_start_p
      and endp = linebuf.Lexing.lex_curr_p in
      let result = I.offer result (token, startp, endp) in
      loop linebuf result
  | I.AboutToReduce (env, prod) ->
      dump env;
      let result = I.resume result in
      loop linebuf result
  | I.HandlingError env ->
      (* The parser has suspended itself because of a syntax error. Stop. *)
      Printf.fprintf stderr
        "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start linebuf)
  | I.Accepted v ->
      (* The parser has succeeded and produced a semantic value. Print it. *)
      Printf.printf "%d\n%!" v
  | I.Rejected ->
      (* The parser rejects this input. This cannot happen, here, because
         we stop as soon as the parser reports [HandlingError]. *)
      assert false

(* Initialize the lexer, and catch any exception raised by the lexer. *)

let process (line : string) =
  let linebuf = Lexing.from_string line in
  try
    loop linebuf (Parser.Incremental.main())
  with
  | Lexer.Error msg ->
      Printf.fprintf stderr "%s%!" msg

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

