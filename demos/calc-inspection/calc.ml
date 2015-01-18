open MenhirLib.General
open Parser.MenhirInterpreter

(* --------------------------------------------------------------------------- *)

(* In order to submit artificial tokens to the parser, we need a function that
   converts a terminal symbol to a (dummy) token. Unfortunately, we cannot (in
   general) auto-generate this code, because it requires making up semantic
   values of arbitrary OCaml types. *)

let dummy_token (type a) (symbol : a terminal) : token =
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
  Printers.Make(Parser.MenhirInterpreter) (struct
    let print s = Printf.fprintf stderr "%s" s
    let print_symbol s = print (print_symbol s)
    let print_element = Some (fun s -> print (print_element s))
  end)

(* The past of an LR(0) item is the first part of the right-hand side,
   up to the point. We represent it as a reversed list, right to left.
   Thus, the past corresponds to a prefix of the stack. *)

let past (prod, index) =
  let rhs = rhs prod in
  List.rev (MenhirLib.General.take index rhs)

(* The LR(0) items that form the core of an LR(1) state have compatible
   pasts. If we pick the one with the longest past, we obtain the past
   of this state, i.e., the longest statically known prefix of the stack
   in this state. *)

let past items =
  let (max_index, max_past) =
    List.fold_left (fun ((max_index, max_past) as accu) ((_, index) as item) ->
      if max_index < index then
        index, past item
      else
        accu
    ) (0, []) items
  in
  max_past

let items_current env =
  (* Get the current state. *)
  match Lazy.force (stack env) with
  | Nil ->
      (* If we get here, then the stack is empty, which means the parser
         is an initial state. This cannot happen. *)
      invalid_arg "items_current"
  | Cons (Element (current, _, _, _), _) ->
      (* Extract the current state out of the top stack element, and
         convert it to a set of LR(0) items. Returning a set of items
         instead of an ['a lr1state] is convenient; returning [current]
         would require wrapping it in an existential type. *)
      items current

let shift_item (prod, index) t =
  let rhs = rhs prod in
  let length = List.length rhs in
  assert (0 < index && index <= length);
  if index = length then
    (* This item cannot justify a shift transition. *)
    []
  else
    let symbol = List.nth rhs index in
    if xfirst symbol t then
      (* This item can justify a shift transition along [t]. *)
      [ drop index rhs ]
    else
      (* This item cannot justify a shift transition along [t]. *)
      []

let shift_items items t futures =
  List.fold_left (fun futures item ->
    shift_item item t @ futures
  ) futures items

let rec investigate_terminal checkpoint t futures =
  match checkpoint with
  | Shifting (env, _, _) ->
      shift_items (items_current env) t futures
  | AboutToReduce (_, prod) ->
      investigate_terminal (resume checkpoint) t futures
  | HandlingError _ ->
      futures
  | InputNeeded _
  | Accepted _
  | Rejected ->
      assert false (* cannot happen *)

let investigate checkpoint =
  (* Print what we have recognized so far. *)
  Printf.fprintf stderr "Past:\n%!";
  let env = match checkpoint with InputNeeded env -> env | _ -> assert false in
  P.print_symbols (List.rev (past (items_current env)));
  Printf.fprintf stderr "\n%!";
  (* Let us analyse which tokens are accepted in this state. *)
  let futures =
    foreach_terminal_but_error (fun symbol futures ->
      match symbol with
      | X (N _) -> assert false
      | X (T t) ->
          (* Build a dummy token for the terminal symbol [t]. *)
          let token = (dummy_token t, Lexing.dummy_pos, Lexing.dummy_pos) in
          (* Submit it to the parser. *)
          let checkpoint = offer checkpoint token in
          investigate_terminal checkpoint t futures
    ) []
  in
  let futures = uniq compare_words (List.sort compare_words futures) in
  Printf.fprintf stderr "Futures:\n%!";
  List.iter (fun future ->
    P.print_symbols future;
    Printf.fprintf stderr "\n%!"
  ) futures

(* The loop which drives the parser. At each iteration, we analyze a
   result produced by the parser, and act in an appropriate manner. *)

(* [lexbuf] is the lexing buffer. [result] is the last result produced
   by the parser. [checkpoint] is the last [InputNeeded] that was
   produced by the parser. *)

let rec loop lexbuf (checkpoint : int result) (result : int result) =
  match result with
  | InputNeeded env ->
      let checkpoint = result in
      (* The parser needs a token. Request one from the lexer,
         and offer it to the parser, which will produce a new
         result. Then, repeat. *)
      let token = Lexer.token lexbuf in
      let startp = lexbuf.Lexing.lex_start_p
      and endp = lexbuf.Lexing.lex_curr_p in
      let result = offer result (token, startp, endp) in
      loop lexbuf checkpoint result
  | Shifting _
  | AboutToReduce _ ->
      let result = resume result in
      loop lexbuf checkpoint result
  | HandlingError env ->
      (* The parser has suspended itself because of a syntax error. Stop. *)
      Printf.fprintf stderr
        "At offset %d: syntax error.\n%!"
        (Lexing.lexeme_start lexbuf);
      investigate checkpoint
  | Accepted v ->
      (* The parser has succeeded and produced a semantic value. Print it. *)
      Printf.printf "%d\n%!" v
  | Rejected ->
      (* The parser rejects this input. This cannot happen, here, because
         we stop as soon as the parser reports [HandlingError]. *)
      assert false

(* Initialize the lexer, and catch any exception raised by the lexer. *)

let process (line : string) =
  let lexbuf = Lexing.from_string line in
  try
    let result = Parser.Incremental.main() in
    (* The parser cannot accept or reject before it asks for the very first
       character of input. (Indeed, we statically reject a symbol that
       generates the empty language or the singleton language {epsilon}.)
       So, [result] must be [InputNeeded _]. *)
    assert (match result with InputNeeded _ -> true | _ -> false);
    loop lexbuf result result
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

