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

open Printf
open Grammar
open StackLang

let debug =
  false

(* -------------------------------------------------------------------------- *)

(* The possible outcomes of an execution. *)

type outcome =
  | Accepted
  | Rejected
  | Overshoot

let print_outcome = function
  | Accepted ->
      "accepts this sentence"
  | Rejected ->
      "rejects this sentence"
  | Overshoot ->
      "reads past the end of the input stream"

(* -------------------------------------------------------------------------- *)

(* [run interpret sentence] uses the interpreter [interpret] to parse the
   sentence [sentence]. *)

let run interpret sentence : outcome =
  let lexer, lexbuf = Interpret.stream sentence in
  match interpret Settings.trace lexer lexbuf with
  | Some _cst ->
      Accepted
  | None ->
      Rejected
  | exception Interpret.EndOfStream ->
      Overshoot

(* -------------------------------------------------------------------------- *)

(* [test program nt sentence] tests the program [program] with the start
   symbol [nt] and the sentence [sentence]. *)

let test program nt sentence =
  (* In debug mode, show the sentence that we are about to test. *)
  if debug then
    eprintf "StackLangTester: About to test this sentence:\n  %s%!"
      (Sentence.print `Abstract (Some nt, sentence));
  (* These are the two interpreters that we compare. *)
  let reference = ReferenceInterpreter.interpret nt in
  let label = Lr1.NodeMap.find (Lr1.entry_of_nt nt) program.entry in
  let candidate = StackLangInterpreter.interpret program label in
  (* Run each of them. *)
  if debug then eprintf "StackLangTester: Running the reference interpreter...";
  let reference = run reference sentence in
  if debug then eprintf "\n%!";
  if debug then eprintf "StackLangTester: Running the StackLang program...";
  let candidate = run candidate sentence in
  if debug then eprintf "\n%!";
  (* Compare the results. *)
  if reference <> candidate then begin
    eprintf "StackLangTester: Error: reference and candidate disagree.\n%s"
      (Sentence.print `Abstract (Some nt, sentence));
    eprintf "The reference interpreter %s, whereas\n"
      (print_outcome reference);
    eprintf "the StackLang program %s.\n"
      (print_outcome candidate);
    exit 1
  end

(* -------------------------------------------------------------------------- *)

(* [test program nt sentence] tests the program [program] with the start
   symbol [nt] and with a number of sentences. *)

(* We sample sentences of increasing sizes, picking at most [m] sentences of
   each size, until a total of [n] sentences is reached or a certain size
   threshold is reached. The latter condition is necessary because there is no
   guarantee that the grammar is capable of generating sentences of arbitrary
   sizes. *)

let m, n, threshold = 100, 1000, 100

let test program nt =
  (* Sample sentences of increasing sizes, picking at most [m] sentences
     of each size, until a total of [n] sentences is reached or the size
     threshold is reached. *)
  let count, size = ref 0, ref 0 in
  while !count < n && !size < threshold do
    for _ = 1 to m do
      let sentence = RandomSentenceGenerator.nonterminal nt !size in
      test program nt sentence
    done;
    count := !count + m;
    incr size
  done;
  (* Log a success message. *)
  if debug then
    eprintf "StackLangTester: Tested %d sentences of length up to %d.\n" !count (!size - 1)

(* -------------------------------------------------------------------------- *)

(* [test program nt sentence] tests the program [program]. *)

let test program =
  (* For each start symbol [nt], test this entry point. *)
  Lr1.entry |> ProductionMap.iter begin fun _prod s ->
    let nt = Lr1.nt_of_entry s in
    test program nt
  end

(* -------------------------------------------------------------------------- *)

(* As a temporary measure, if the grammar uses the [error] token,
   then we skip this test, because the new code back-end does not
   yet have any error-handling code. *)

let test program =
  if not grammar_uses_error_token then
    test program
