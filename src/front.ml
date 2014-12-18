(* The front-end. This module performs a series of toplevel side effects. *)

(* ------------------------------------------------------------------------- *)

(* Reading a grammar from a file. *)

let load_partial_grammar filename =
  let validExt = if Settings.coq then ".vy" else ".mly" in
  if not (Filename.check_suffix filename validExt) then
    Error.error [] (Printf.sprintf
      "argument file names should end in %s. \"%s\" is not accepted."
      validExt filename);
  Error.set_filename filename;
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

(* ------------------------------------------------------------------------- *)

(* If [--only-tokens] was specified on the command line, produce
   the definition of the [token] type and stop. *)

let () =
  TokenType.produce_tokentypes grammar

(* ------------------------------------------------------------------------- *)

(* Perform reachability analysis. *)

let grammar =
  Reachability.trim grammar

let () =
  Time.tick "Trimming"

(* ------------------------------------------------------------------------- *)

(* If [--depend] was specified on the command line, perform dependency
   analysis and stop. *)

let () =
  match Settings.depend with
  | Settings.OMRaw
  | Settings.OMPostprocess ->
      Infer.depend grammar (* never returns *)
  | Settings.OMNone ->
      ()

(* [--depend] is really a strange hack: we are expected to be able to predict
   the dependencies exhibited by the [.ml] and [.mli] files produced by
   Menhir, *without* actually producing these files, because we cannot yet
   produce them. (If [--infer] is enabled, producing these files would require
   consulting certain [.cmi] files that may not exist yet.) *)

(* Remarks. 1- If [make] was not brain-dead, we would not have to do this in
   the first place. A good compilation manager should be able to mix the
   production of targets and the dependency analysis. Apparently, by this
   definition, [ocamlbuild] is brain-dead too: it uses [menhir --raw-depend].

   2- We are able to do this because we can produce mock [.ml] and [.mli]
   files that exhibit the same dependencies as the real ones. This allows us
   to run [ocamldep] on these mock files and obtain correct depdendency
   information. Traditionally, our mock [.mli] file was in fact the final
   [.mli] file, and our mock [.ml] file contained just the semantic actions
   and nothing else. (But see 4- below.)

   3- If both [--depend] and [--infer] are enabled, we must effectively ignore
   [--infer] and perform the dependency computation without attempting to
   perform type inference. Indeed, as mentioned above, that would require
   consulting certain [.cmi] files that may not exist yet.

   4- As of 2014/12/17, we would like to produce as part of the [.mli] file
   a type [nonterminal] which lists all nonterminal symbols together with
   their OCaml type. This typically requires [--infer]. Thus, this implies
   that the mock [.mli] file can no longer be the same as the final [.mli]
   file; the mock cannot contain the type [nonterminal]. As a result, maybe
   the dependencies (computed based on the mock) will be incomplete. Which
   would be a problem (failure to build). I am not sure at the moment how
   to address this problem. *)

(* ------------------------------------------------------------------------- *)

(* If [--infer] was specified on the command line, perform type inference.
   The OCaml type of every nonterminal is then known. *)

let grammar =
  if Settings.infer then
    let grammar = Infer.infer grammar in
    Time.tick "Inferring types for nonterminals";
    grammar
  else
    grammar

(* ------------------------------------------------------------------------- *)

(* If [--no-inline] was specified on the command line, skip the
   inlining of non terminal definitions marked with %inline. *)

let grammar =
  if Settings.inline then begin
    let grammar, inlined = 
      NonTerminalDefinitionInlining.inline grammar
    in
    if not Settings.infer && inlined && not Settings.coq then
      Error.warning []
	"you are using the standard library and/or the %inline keyword. We\n\
	 recommend switching on --infer in order to avoid obscure type error messages.";
    Time.tick "Inlining";
    grammar
  end
  else 
    grammar

(* ------------------------------------------------------------------------- *)

(* If [--only-preprocess] or [--only-preprocess-drop] was specified on the
   command line, print the grammar and stop. Otherwise, continue. *)

let () =
  match Settings.preprocess_mode with
  | Settings.PMOnlyPreprocess mode ->
      UnparameterizedPrinter.print mode stdout grammar;
      exit 0
  | Settings.PMNormal ->
      ()

