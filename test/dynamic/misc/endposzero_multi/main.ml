open Lexing
open Printf
open Parser

let tokens =
  ref [ B; C; B; C ]

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

let pair (x, y) =
  sprintf "(%d, %d)" x y

let () =
  let lexbuf = Lexing.from_string "" in
  let result = main lexer lexbuf in
  let expected = (0, 2) in
  if result = expected then
    (* Happy. *)
    printf "The positions are %s, as expected.\n%!" (pair expected)
  else begin
    printf "Incorrect position: got %s, expected %s.\n%!"
      (pair result) (pair expected);
    assert false
  end
