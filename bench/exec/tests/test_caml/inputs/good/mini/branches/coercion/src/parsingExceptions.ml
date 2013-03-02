(* $Id$ *)

(** This module handles parsing errors. *)

open Positions
exception Unclosed of string * string * position * position
exception Other of Lexing.lexbuf

let handle_parsing_error = function
    Unclosed (s, e, l1, l2) ->
      ("Unclosed '"^s^"' opened at "^(string_of_pos l1), [ l1; l2 ])
  | Other lexer -> 
      ("Syntax error at "^(string_of_cpos lexer) , [ cpos lexer ])
  | e -> raise e
