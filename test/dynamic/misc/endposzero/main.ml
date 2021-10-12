open Lexing
open Printf
open Parser

let tokens =
  ref [ A; B; C ]

let lexer lexbuf =
  match !tokens with
  | [] ->
      raise End_of_file
  | t :: ts ->
      (* This horrible code copies curr_p into start_p and increments
         curr_p, so the first token has positions 0-1, the next token
         has positions 1-2, and so on. *)
      let lex_curr_p = lexbuf.lex_curr_p in
      let pos_cnum = lex_curr_p.pos_cnum + 1 in
      lexbuf.lex_start_p <- lex_curr_p;
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_cnum };
      tokens := ts;
      t

let () =
  let lexbuf = Lexing.from_string "" in
  let result = main lexer lexbuf in
  if result = 1 then
    (* Happy. *)
    printf "The position is 1, as expected.\n"
  else begin
    printf "Incorrect position: got %d, expected 1.\n" result;
    assert false
  end
