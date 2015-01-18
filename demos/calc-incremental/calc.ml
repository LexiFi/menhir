(* A short name for the incremental parser API. *)

module I =
  Parser.MenhirInterpreter

(* The loop which drives the parser. At each iteration, we analyze a
   result produced by the parser, and act in an appropriate manner. *)

(* [lexbuf] is the lexing buffer. [result] is the last result produced
   by the parser. *)

let rec loop lexbuf (result : int I.result) =
  match result with
  | I.InputNeeded env ->
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         result. Then, repeat. *)
      let token = Lexer.token lexbuf in
      let startp = lexbuf.Lexing.lex_start_p
      and endp = lexbuf.Lexing.lex_curr_p in
      let result = I.offer result (token, startp, endp) in
      loop lexbuf result
  | I.Shifting _
  | I.AboutToReduce _ ->
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

