(* The front-end. *)

(* ------------------------------------------------------------------------- *)

(* Reading a grammar from a file. *)

let load_partial_grammar filename =
  let validExt = if Settings.coq then ".vy" else ".mly" in
  if not (Filename.check_suffix filename validExt) then
    Error.error [] (Printf.sprintf
      "argument file names should end in %s. \"%s\" is not accepted."
      validExt filename);
  Error.set_filename filename
  try

    let contents = IO.read_whole_file filename in
    Error.file_contents := Some contents;
    let lexbuf = Lexing.from_string contents in
    lexbuf.Lexing.lex_curr_p <-
	{ 
	  Lexing.pos_fname = filename; 
	  Lexing.pos_lnum  = 1;
	  Lexing.pos_bol   = 0; 
	  Lexing.pos_cnum  = 0
	};
    let grammar =
      { (Parser.grammar Lexer.main lexbuf) with ConcreteSyntax.pg_filename = filename }
    in
    Error.file_contents := None;
    grammar

  with Sys_error msg ->
    Error.error [] msg

(* ------------------------------------------------------------------------- *)

(* Read all of the grammar files that are named on the command line. *)

let partial_grammars = 
  List.map load_partial_grammar Settings.filenames

let () =
  Time.tick "Lexing and parsing"

(* ------------------------------------------------------------------------- *)

(* If several grammar files were specified, merge them. *)

let parameterized_grammar = 
  PartialGrammar.join_partial_grammars partial_grammars

(* ------------------------------------------------------------------------- *)

(* Expand away all applications of parameterized nonterminal symbols, so as to
   obtain a grammar without parameterized nonterminal symbols. *)

let grammar = 
  ParameterizedGrammar.expand parameterized_grammar

let () =
  Time.tick "Joining and expanding"

