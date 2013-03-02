(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/mini.ml,v 1.3 2004/04/13 16:21:25 fpottier Exp $ *)

module MiniTypeInferencer =
  Infer.Make(MiniSolver)(MiniAlgebra)(MiniPrimitives)

open Printf
open Sig
open Misc

let load filename =
  try
    let lexer = Lexing.from_channel (open_in filename) in
    try
      let bs = MiniParser.program MiniLexer.token lexer in
      let c = List.fold_right MiniTypeInferencer.bind bs CDump in
      if !debug then begin
	Printf.fprintf stderr "%s\n" (MiniSolver.print_constraint c);
	flush stderr
      end;
      MiniSolver.solve c

    with
    | MiniLexer.Error (msg, c1, c2) ->
	fprintf stderr "%s near characters %d-%d\n" msg c1 c2
    | Parsing.Parse_error ->
	fprintf stderr "Unknown parse error near character %d\n" lexer.Lexing.lex_curr_pos
  with
  | Sys_error msg ->
      fprintf stderr "%s\n" msg

let _ =
  Arg.parse [] load
    "Usage: mini <file> ... <file>\n  Parses and typechecks the files whose names appear on the command line.\n";
  flush stdout;
  flush stderr

