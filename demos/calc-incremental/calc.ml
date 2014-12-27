(* Introduce a short name for the incremental parser API. *)

module I =
  Parser.MenhirInterpreter

(* The length of a stream. *)

let rec length xs =
  match Lazy.force xs with
  | I.Nil ->
      0
  | I.Cons (_, xs) ->
      1 + length xs

(* A measure of the stack height. Used as a primitive way of
   testing the [view] function. *)

let height env =
  length (I.view env)

(* Define the loop which drives the parser. At each iteration,
   we analyze a result produced by the parser, and act in an
   appropriate manner. *)

let rec loop linebuf (result : int I.result) =
  match result with
  | I.InputNeeded env ->
      (* TEMPORARY *)
      if false then
        Printf.fprintf stderr "Stack height: %d\n%!" (height env); 
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         result. Then, repeat. *)
      let token = Lexer.token linebuf in
      let startp = linebuf.Lexing.lex_start_p
      and endp = linebuf.Lexing.lex_curr_p in
      let result = I.offer env (token, startp, endp) in
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
    loop linebuf (Parser.main_incremental())
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

