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

(* Printing a symbol. *)

let print_symbol symbol =
  let open Parser.Inspection in
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

(* Printing a list of symbols. An optional dot is printed at offset
   [dot] into the list [symbols], if this offset lies between [0] and
   the length of the list (included). *)

let rec print_symbols print_symbol print_dot dot symbols =
  let rec loop dot symbols =
    if dot = 0 then begin
      print_dot();
      loop (-1) symbols
    end
    else begin
      match symbols with
      | [] ->
          ()
      | symbol :: symbols ->
          print_symbol symbol;
          loop (dot - 1) symbols
    end
  in
  loop dot symbols

(* Printing a production. *)

let print_production print_symbol print_arrow print_dot dot (lhs, rhs) =
  print_symbol lhs;
  print_arrow();
  print_symbols print_symbol print_dot dot rhs

(* Printing an item. *)

let print_item print_symbol print_arrow print_dot (prod, dot) =
  let open Parser.Inspection in
  print_production print_symbol print_arrow print_dot dot (lhs prod, rhs prod)

let print_production print_symbol print_arrow prod =
  let print_dot () = () in
  print_item print_symbol print_arrow print_dot (prod, -1)

(* B. *)

let wrap b f x =
  Buffer.add_string b (f x)

let with_buffer print_symbol print_arrow print_dot f x =
  let b = Buffer.create 128 in
  f (wrap b print_symbol) (wrap b print_arrow) (wrap b print_dot) x;
  Buffer.contents b

let print_item print_symbol print_arrow print_dot item =
  with_buffer print_symbol print_arrow print_dot print_item item

let print_arrow () =
  " ->"

let print_dot () =
  " ."

let print_item =
  print_item print_symbol print_arrow print_dot

(* Printing an element. *)

let print_element e : string =
  match e with
  | I.Element (s, v, _, _) ->
      let open Parser.Inspection in
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

let dump env =
  Printf.fprintf stderr "Stack height: %d\n%!" (height env);
  Printf.fprintf stderr "Stack view:\n%s\n%!" (print env);
  begin match Lazy.force (I.view env) with
  | I.Nil ->
      ()
  | I.Cons (I.Element (current, _, _, _), _) ->
      Printf.fprintf stderr "Current state: %d\n%!" (Obj.magic current);
      let items : Parser.Inspection.item list = Parser.Inspection.items current in
      Printf.fprintf stderr "#Items: %d\n%!" (List.length items);
      List.iter (fun item ->
        Printf.fprintf stderr "%s\n" (print_item item)
      ) items
  end

let rec loop linebuf (result : int I.result) =
  match result with
  | I.InputNeeded env ->
      (* TEMPORARY *)
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

