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

(* Folding over a stream. *)

let rec foldr f xs accu =
  match Lazy.force xs with
  | I.Nil ->
      accu
  | I.Cons (x, xs) ->
      f x (foldr f xs accu)

(* A measure of the stack height. Used as a primitive way of
   testing the [view] function. *)

let height env =
  length (I.view env)

(* Printing an element. *)

let print_element e : string =
  match e with
  | I.Element (s, v, _, _) ->
      let open Parser.Inspection in
      match symbol s with
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

(* Printing a stack. *)

let print env : string =
  let b = Buffer.create 80 in
  foldr (fun e () ->
    Buffer.add_string b (print_element e);
    Buffer.add_char b ' ';
  ) (I.view env) ();
  Buffer.contents b

(* Define the loop which drives the parser. At each iteration,
   we analyze a result produced by the parser, and act in an
   appropriate manner. *)

let rec loop linebuf (result : int I.result) =
  match result with
  | I.InputNeeded env ->
      (* TEMPORARY *)
      if true then begin
        Printf.fprintf stderr "Stack height: %d\n%!" (height env);
        Printf.fprintf stderr "Stack view:\n%s\n%!" (print env);
        begin match Lazy.force (I.view env) with
        | I.Nil ->
            ()
        | I.Cons (I.Element (current, _, _, _), _) ->
            Printf.fprintf stderr "Current state: %d\n%!" (Obj.magic current);
            let items : (I.production * int) list = Parser.Inspection.items current in
            Printf.fprintf stderr "#Items: %d\n%!" (List.length items);
            List.iter (fun (prod, index) ->
              let _lhs : Parser.Inspection.xsymbol = Parser.Inspection.lhs prod in
              let _rhs : Parser.Inspection.xsymbol list = Parser.Inspection.rhs prod in
              (* TEMPORARY print item *)
              ()
            ) items
        end
      end;
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         result. Then, repeat. *)
      let token = Lexer.token linebuf in
      let startp = linebuf.Lexing.lex_start_p
      and endp = linebuf.Lexing.lex_curr_p in
      let result = I.offer result (token, startp, endp) in
      loop linebuf result
  | I.AboutToReduce _ ->
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

