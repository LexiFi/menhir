open Lexing
open Parser

let tokens =
  ref [ A; D ]

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

exception Unexpected

let () =
  let lexbuf = Lexing.from_string "" in
  try
    main lexer lexbuf;
    (* Menhir inserts code to check that the semantic action associated
       with an error production aborts the parser. This check is expected
       to fail here, so the call [main lexer lexbuf] is expected to *not*
       terminate normally. *)
    raise Unexpected
  with Assert_failure _ ->
    (* This is expected. *)
    Printf.eprintf "An assertion failure has occurred, as expected.\n%!"
