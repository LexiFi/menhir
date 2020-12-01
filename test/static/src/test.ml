(* This script produces a file [dune.auto], which describes the tests we
   would like dune to execute. It is used to produce [../good/dune.auto]
   and [../bad/dune.auto]. *)

(* Note: no test in bad/ should have the same name as a test in good/. *)

(* -------------------------------------------------------------------------- *)

open Sys
open Array
open List
open Printf
open Auxiliary
open PrintSExp
open DuneRule

(* -------------------------------------------------------------------------- *)

(* Settings. *)

module Settings = struct

let extra : string list ref =
  ref []

let kind : string ref =
  ref "good"

let source : string ref =
  ref "."

let usage =
  sprintf "Usage: %s <options> <directory>\n" argv.(0)

let spec = Arg.align [
  "--extra-flags", Arg.String (fun flag -> extra := flag :: !extra),
                     "<string> specify extra flags for Menhir";
  "--kind",        Arg.String ((:=) kind),
                     "<string> what kind of test is this? good/bad/merge";
]

let () =
  Arg.parse spec (fun d -> source := d) usage

let extra : string list =
  rev !extra

let kind =
  !kind

let source =
  !source

end

(* -------------------------------------------------------------------------- *)

(* Test files and groups of test files. *)

let id basenames =
  (* A name for a nonempty group of test files. *)
  hd basenames

let mly basename =
  basename ^ ".mly"

let mlys =
  map mly

(* -------------------------------------------------------------------------- *)

(* Test inputs and outputs. *)

(* A test input is a list of basenames, without the .mly extension.
   These files must be passed together to menhir. *)

type input =
  | NegativeTest of filename list
  | PositiveTest of filename list

type inputs = input list

(* -------------------------------------------------------------------------- *)

(* Calling conventions for Menhir. *)

(* A --base option is needed for groups of several files. *)

let base basenames =
  if length basenames > 1 then
    let id = id basenames in
    [A"--base"; A id]
  else
    []

(* The extra flags passed to Menhir are those found in a local .flags file,
   if there is one, plus those passed to us via --extra-flags. *)

let extra id =
  let flags_file = id ^ ".flags" in
  if file_exists flags_file then
    A(sprintf "%%{read-lines:%s}" flags_file) :: atoms Settings.extra
  else
    atoms Settings.extra

(* The Menhir command, for use inside a rule, with an optional timeout. *)

(* If this is a positive test and if the [threshold] parameter is [Some _],
   then we assume that a [timeout] command exists on the system, and we use it
   to limit Menhir's execution time. This is normally not necessary, but can
   be useful when testing experimental extensions of Menhir. *)

let threshold =
  None
  (* or: [Some seconds] *)

let menhir_args base flags =
  base @ flags @ A"%{deps}" :: []

let menhir (impose_timeout : bool) base flags =
  match impose_timeout, threshold with
  | true, Some threshold ->
      (* We must use a [system] action. *)
      system
        "timeout %d %%{bin:menhir}%s || \
         (status=$?; if (( $status == 124 )) ; then \
           echo 'TIMEOUT after %d seconds.' ; fi; exit $status)"
          threshold
          (show_list (menhir_args base flags))
          threshold
  | _, _ ->
      (* We can use a [run] action. *)
      L(A"run" :: A"menhir" :: menhir_args base flags)

(* Constructing and printing a rule to run Menhir.

   [positive]   positive or negative test?
   [basenames]  base names of the .mly files
   [outputs]    names of the files created by this command
   [flags]      flags for Menhir

   There can be several output files in the list [outputs], in
   which case all of them are declared to [dune] as targets.

   The first file named in this list is used as the target of
   the redirection of Menhir's output channels.

   If this is a positive test, then a timeout is imposed. *)

let run positive basenames outputs flags =
  let output = hd outputs in
  (* Run Menhir. *)
  print (rule
    outputs
    (mlys basenames)
    (redirect_both output (
      possibly_expecting_failure positive (
        menhir positive (base basenames) flags
  ))))

(* -------------------------------------------------------------------------- *)

(* Running a negative test. *)

(* The file %.flags   (if it exists) stores flags for Menhir.
   The file %.exp     stores the expected output. *)

(* The file %.out     stores the output of Menhir. *)

let process_negative_test basenames : unit =
  (* Run menhir. *)
  let id = id basenames in
  let output = id ^ ".out" in
  let expected = id ^ ".exp" in
  let flags = extra id in
  run false basenames [output] flags;
  (* Check that the output coincides with what was expected. *)
  print (phony id (diff expected output))

(* -------------------------------------------------------------------------- *)

(* Running a positive test. *)

(* The file %.flags   (if it exists) stores flags for Menhir.
   The file %.opp.exp stores its expected output.
   The file %.exp     stores its expected output. *)

(* The file %.opp.out stores the output of menhir --only-preprocess.
   The file %.out     stores the output of menhir.
   The file %.out.timings stores performance data.

   Because the performance data is not perfectly reproducible,
   it is not compared against a reference. *)

let process_positive_test basenames : unit =
  let id = id basenames in
  let flags = extra id in
  (* Run menhir --only-preprocess. *)
  let output = id ^ ".opp.out" in
  let expected = id ^ ".opp.exp" in
  run true basenames [output] (atoms [
    "--only-preprocess";
  ] @ flags);
  (* Check that the output coincides with what was expected. *)
  print (phony id (diff expected output));
  (* Run menhir. *)
  let output = id ^ ".out" in
  let automaton = id ^ ".automaton" in
  let automaton_resolved = id ^ ".automaton.resolved" in
  let conflicts = id ^ ".conflicts" in
  let timings = id ^ ".timings" in
  let targets = [output;automaton;automaton_resolved;conflicts;timings] in
  run true basenames targets (atoms [
    "--dump";
    "--dump-resolved";
    "--explain";
    "-lg"; "2";
    "-la"; "2";
    "-lc"; "2";
    "--timings-to"; timings;
  ] @ flags);
  (* Check that the output coincides with what was expected. *)
  print (phony id (diff (id ^ ".exp") output));
  (* Check the .automaton and .conflicts files. *)
  print (phony id (diff (id ^ ".automaton.exp") automaton));
  print (phony id (diff (id ^ ".automaton.resolved.exp") automaton_resolved));
  print (phony id (diff (id ^ ".conflicts.exp") conflicts))

(* -------------------------------------------------------------------------- *)

(* Running a test. *)

let process input =
  match input with
  | NegativeTest basenames ->
      process_negative_test basenames
  | PositiveTest basenames ->
      process_positive_test basenames

let id input =
  match input with
  | NegativeTest basenames
  | PositiveTest basenames ->
      id basenames

(* -------------------------------------------------------------------------- *)

(* [run] runs a bunch of tests in parallel. *)

let run (inputs : inputs) =
  iter process inputs;
  let ids = map id inputs in
  let ids = sort_uniq Stdlib.compare ids in
  print (alias "test" ids)

(* -------------------------------------------------------------------------- *)

(* Printing a header. *)

let header () =
  print_endline
    ";; This file has been auto-generated. Please do not edit it.\n\
     ;; Instead, edit [test.ml] and run [make depend].\n"

(* -------------------------------------------------------------------------- *)

(* Handling tests in good/ and bad/. *)

(* Menhir can accept several .mly files at once. By convention, if several
   files have the same name up to a numeric suffix, then they belong in a
   single group and should be fed together to Menhir. *)

let tag basenames =
  match Settings.kind with
  | "good" ->
      PositiveTest basenames
  | "bad" ->
      NegativeTest basenames
  | _ ->
      assert false

let good_or_bad () =
     readdir Settings.source
  |> to_list
  |> filter (has_suffix ".mly")
  |> map Filename.chop_extension
  |> sort Stdlib.compare
  |> groups equal_up_to_numeric_suffix
  |> map tag
  |> run

(* -------------------------------------------------------------------------- *)

(* Handling tests in merge/. *)

(* The number of tests is currently hardcoded here, and they have a fixed
   naming convention. *)

let n = 10

let merge mly lhs rhs out err exp =
  print (rule [] [] (
    redirect_stdout out (
    redirect_stderr err (
      not_expecting_failure (
        (* We must use a [system] action because we pipe Menhir's output through
           sed in order to remove the auto-generated comments. [set -o pipefail]
           forces the pipeline to fail if Menhir fails. *)
        system
          "set -o pipefail &&        \\\n        \
           %%{bin:menhir} %%{dep:%s} \\\n        \
           --merge-errors %%{dep:%s} \\\n        \
           --merge-errors %%{dep:%s} \\\n        \
          | sed -e '/^##/d'"
            mly lhs rhs
  )))));
  print (phony "test" (diff exp out))

let merge () =
     List.init n (fun i -> i + 1)
  |> List.iter (fun i ->
     let mly = "parser.mly"
     and lhs = sprintf "lhs%02d.messages" i
     and rhs = sprintf "rhs%02d.messages" i
     and out = sprintf "merged%02d.out" i
     and err = sprintf "merged%02d.err" i
     and exp = sprintf "merged%02d.exp" i in
     merge mly lhs rhs out err exp
  )

(* -------------------------------------------------------------------------- *)

(* Main. *)

let () =
  header()

let () =
  match Settings.kind with
  | "good"
  | "bad" ->
      good_or_bad()
  | "merge" ->
      merge()
  | _ ->
      eprintf "--kind must be followed with good, bad, or merge.\n";
      exit 1
