(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* The front-end. This module performs a series of toplevel side effects. *)

(* ------------------------------------------------------------------------- *)

(* Reading a grammar from a file. *)

let load_partial_grammar filename =
  let validExt = if Settings.coq then ".vy" else ".mly" in
  if not (Filename.check_suffix filename validExt) then
    Error.error []
      "argument file names should end in %s. \"%s\" is not accepted."
      validExt filename;
  InputFile.new_input_file filename;
  try

    let contents = IO.read_whole_file filename in
    InputFile.with_file_contents contents (fun () ->
      let open Lexing in
      let lexbuf = Lexing.from_string contents in
      lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };
      (* the grammar: *)
      { (Driver.grammar Lexer.main lexbuf)
        with Syntax.pg_filename = filename }
    )

  with Sys_error msg ->
    Error.error [] "%s" msg

(* ------------------------------------------------------------------------- *)

(* Read all of the grammar files that are named on the command line. *)

let partial_grammars =
  List.map load_partial_grammar Settings.filenames

let () =
  Time.tick "Lexing and parsing"

(* ------------------------------------------------------------------------- *)

(* Eliminate anonymous rules. *)

let partial_grammars =
  List.map Anonymous.transform_partial_grammar partial_grammars

(* ------------------------------------------------------------------------- *)

(* If several grammar files were specified, merge them. *)

let parameterized_grammar =
  PartialGrammar.join_partial_grammars partial_grammars

(* ------------------------------------------------------------------------- *)

(* Check that the grammar is well-sorted; infer the sort of every symbol. *)

let sorts =
  SortInference.infer parameterized_grammar

(* ------------------------------------------------------------------------- *)

(* Expand away all applications of parameterized nonterminal symbols, so as
   to obtain a grammar without parameterized nonterminal symbols. *)

let grammar =
  CheckSafeParameterizedGrammar.check parameterized_grammar;
  Drop.drop (SelectiveExpansion.expand sorts parameterized_grammar)

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

(* If [--depend] or [--raw-depend] was specified on the command line,
   perform dependency analysis and stop. *)

let () =
  match Settings.depend with
  | Settings.OMRaw
  | Settings.OMPostprocess ->
      Infer.depend grammar (* never returns *)
  | Settings.OMNone ->
      ()

(* The purpose of [--depend] and [--raw-depend] is to support [--infer].
   Indeed, [--infer] is implemented by producing a mock [.ml] file (which
   contains just the semantic actions) and invoking [ocamlc]. This requires
   certain [.cmi] files to exist. So, [--(raw-)depend] is a way for us to
   announce which [.cmi] files we need. It is implemented by producing the
   mock [.ml] file and running [ocamldep] on it. We also produce a mock
   [.mli] file, even though in principle it should be unnecessary -- see
   comment in [nonterminalType.mli]. *)

(* ------------------------------------------------------------------------- *)

(* If some flags imply that we will NOT produce an OCaml parser, then there
   is no need to perform type inference, so we act as if --infer was absent.
   This saves time and dependency nightmares. *)

let skipping_parser_generation =
  Settings.coq ||
  Settings.compile_errors <> None ||
  Settings.interpret_error ||
  Settings.list_errors ||
  Settings.compare_errors <> None ||
  Settings.update_errors <> None ||
  Settings.echo_errors <> None ||
  false
    (* maybe also: [preprocess_mode <> PMNormal] *)

(* ------------------------------------------------------------------------- *)

(* If [--infer] was specified on the command line, perform type inference.
   The OCaml type of every nonterminal is then known. *)

let grammar =
  if Settings.infer && not skipping_parser_generation then
    let grammar = Infer.infer grammar in
    Time.tick "Inferring types for nonterminals";
    grammar
  else
    grammar

(* ------------------------------------------------------------------------- *)

(* Expand away some of the position keywords. *)

let grammar =
  KeywordExpansion.expand_grammar grammar

(* ------------------------------------------------------------------------- *)

(* If [--no-inline] was specified on the command line, skip the
   inlining of non terminal definitions marked with %inline. *)

let grammar =
  if Settings.inline then begin
    let grammar, inlined =
      NonTerminalDefinitionInlining.inline grammar
    in
    if not Settings.infer && inlined && not skipping_parser_generation then
      Error.warning []
        "you are using the standard library and/or the %%inline keyword. We\n\
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
