(* A short name for the incremental parser API. *)

module I =
  Parser.MenhirInterpreter

(* A custom symbol printer. *)

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

(* A custom element printer. *)

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

(* TEMPORARY comment *)

module P =
  Printers.Make(I) (struct
    let print s = Printf.fprintf stderr "%s" s
    let print_symbol s = print (print_symbol s)
    let print_element = Some (fun s -> print (print_element s))
  end)

(* Debugging. *)

let dump env =
  P.print_stack (I.stack env);
  Printf.fprintf stderr "\n%!";
  P.print_current_state env;
  Printf.fprintf stderr "\n%!"

(* The loop which drives the parser. At each iteration, we analyze a
   result produced by the parser, and act in an appropriate manner. *)

(* [lexbuf] is the lexing buffer. [result] is the last result produced
   by the parser. *)

let rec loop lexbuf (result : int I.result) =
  match result with
  | I.InputNeeded env ->
      dump env;
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         result. Then, repeat. *)
      let token = Lexer.token lexbuf in
      let startp = lexbuf.Lexing.lex_start_p
      and endp = lexbuf.Lexing.lex_curr_p in
      let result = I.offer result (token, startp, endp) in
      loop lexbuf result
  | I.AboutToReduce (env, prod) ->
      dump env;
      let result = I.resume result in
      loop lexbuf result
  | I.HandlingError env ->
      (* The parser has suspended itself because of a syntax error. Stop. *)
      Printf.fprintf stderr
        "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start lexbuf)
  | I.Accepted v ->
      (* The parser has succeeded and produced a semantic value. Print it. *)
      Printf.printf "%d\n%!" v
  | I.Rejected ->
      (* The parser rejects this input. This cannot happen, here, because
         we stop as soon as the parser reports [HandlingError]. *)
      assert false

(* Initialize the lexer, and catch any exception raised by the lexer. *)

let process (line : string) =
  let lexbuf = Lexing.from_string line in
  try
    loop lexbuf (Parser.Incremental.main())
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

