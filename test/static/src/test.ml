(* This script produces a file [dune.auto], which describes the tests wexclude
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

(* A list of tests which we know cannot be compiled using the code back-end,
   e.g., because the code is too large to be processed by the OCaml compiler. *)

let exclude = [
  (* a pathological grammar with very long productions: *)
  "partest";
  (* large grammars: *)
  "cca_cpp";
  "cca_verilog";
  "mezzo_canonical";
  "sysver";
  "verilog";
]

(* The extension used to identify the copy of the grammar where the semantic
   actions have been removed. *)

let stripped =
  "_stripped"

(* OCaml does not tolerate a dot in a filename, so we normalize this. *)

let normalize c =
  if c = '.' then '_' else c

let normalize s =
  String.map normalize s

(* We distinguish two targets, [test] and [quick]. The latter is a subset of
   the former. [quicktest] runs Menhir only, whereas [test] runs Menhir and
   the OCaml compiler. *)

let test =
  "test"

let quick =
  "quick"

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

let menhir impose_timeout base flags =
  match impose_timeout, threshold with
  | `WithTimeout, Some threshold ->
      (* We must use a [system] action. *)
      system
        "timeout %d %%{bin:menhir}%s || \
         (status=$?; if (( $status == 124 )) ; then \
           echo 'TIMEOUT after %d seconds.' ; fi; exit $status)"
          threshold
          (show_list (menhir_args base flags))
          threshold
  | `NoTimeout, _
  | _, None ->
      (* We can use a [run] action. *)
      L(A"run" :: A"menhir" :: menhir_args base flags)

(* Constructing and printing a rule to run Menhir.

   [positive]   positive or negative test?
   [redirect]   what redirection is desired?
   [basenames]  base names of the .mly files
   [outputs]    names of the files created by this command
   [flags]      flags for Menhir

   There can be several output files in the list [outputs], in
   which case all of them are declared to [dune] as targets.

   If this is a positive test, then a timeout is imposed. *)

type destination =
  | File of string

let redirect (out, err : destination * destination) =
  match out, err with
  | File out, File err when out = err ->
      redirect_both out
  | File out, File err ->
      fun action ->
        action |> redirect_stdout out |> redirect_stderr err

let run positive out err basenames outputs flags =
  (* Impose a timeout if necessary. *)
  let impose_timeout =
    match positive with
    | `Positive -> `WithTimeout
    | `Negative -> `NoTimeout
  in
  (* Run Menhir. *)
  print (rule
    outputs
    (mlys basenames)
    (redirect (out, err) (
      possibly_expecting_failure positive (
        menhir impose_timeout (base basenames) flags
  ))))

(* -------------------------------------------------------------------------- *)

(* Running the OCaml compiler. *)

(* We enable all warnings and make them errors,
   as we want to be aware of them. *)

(* Warning 24 (invalid module names) must be disabled. *)

let ocamlc source =
  L (atoms [
    "run"; "ocamlc";
    "-w"; "A-24";
    "-warn-error"; "A";
    "-c"; source;
  ])

let compile ocamlbase =
  (* Compile the .mli file first. *)
  let source = ocamlbase ^ ".mli"
  and cmi = ocamlbase ^ ".cmi"
  and out = ocamlbase ^ ".mli.dtimings"
  and log = ocamlbase ^ ".mli.log" in
  print (rule
    [cmi; log]
    [source]
    (redirect_stdout out
    (redirect_stderr log
      (ocamlc source)))
  );
  (* Then, compile the .ml file. *)
  let source = ocamlbase ^ ".ml"
  and cmo = ocamlbase ^ ".cmo"
  and out = ocamlbase ^ ".ml.dtimings"
  and log = ocamlbase ^ ".ml.log" in
  print (rule
    [cmo; log]
    [source; cmi]
    (redirect_stdout out
    (redirect_stderr log
      (ocamlc source)))
  );
  (* Compare the log file with an expected result. *)
  (* If the file [expected] does not exist, then Dune >= 2.8.0 behaves as if
     this file exists and is empty, which is fine; we expect the compiler log
     to be empty. *)
  let expected = log ^ ".exp" in
  print (phony test (diff expected log))

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
  let out, err = File output, File output in
  run `Negative out err basenames [output] flags;
  (* Check that the output coincides with what was expected. *)
  print (phony test (diff expected output))

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

(* The file %.stripped.mly stores the output of menhir --only-preprocess-uu. *)

let process_positive_test basenames : unit =
  let id = id basenames in
  let flags = extra id in

  (* Run menhir --only-preprocess. *)
  let output = id ^ ".opp.out" in
  let expected = id ^ ".opp.exp" in
  let out, err = File output, File output in
  run `Positive out err basenames [output] (atoms [
    "--only-preprocess";
  ] @ flags);
  (* Check that the output coincides with what was expected. *)
  print (phony quick (diff expected output));

  (* Run menhir in --table mode. We cannot use the (new) code back-end, in
     general, because it requires type inference, which we cannot do here
     because the semantic actions contain references to external modules. We
     use the code back-end, below, after stripping the semantic actions. *)
  let output = id ^ ".out" in
  let automaton = id ^ ".automaton" in
  let automaton_resolved = id ^ ".automaton.resolved" in
  let conflicts = id ^ ".conflicts" in
  let timings = id ^ ".timings" in
  let targets = [output;automaton;automaton_resolved;conflicts;timings] in
  let out, err = File output, File output in
  run `Positive out err basenames targets (atoms [
    "--table";
    "--dump";
    "--dump-resolved";
    "--explain";
    "-lg"; "2";
    "-la"; "2";
    "-lc"; "2";
    "--timings-to"; timings;
  ] @ flags);
  (* Check that the output coincides with what was expected. *)
  print (phony quick (diff (id ^ ".exp") output));
  (* Check the .automaton and .conflicts files. *)
  print (phony quick (diff (id ^ ".automaton.exp") automaton));
  print (phony quick (diff (id ^ ".automaton.resolved.exp") automaton_resolved));
  print (phony quick (diff (id ^ ".conflicts.exp") conflicts));

  if not (List.mem id exclude) then begin
  let ocamlbase = normalize (id ^ stripped) in

  (* Run menhir --only-preprocess-uu to remove the semantic actions. *)
  let output = ocamlbase ^ ".mly" in
  let ignored = ocamlbase ^ ".ignored" in
  let out, err = File output, File ignored in
  run `Positive out err basenames [output] (atoms [
    "--only-preprocess-uu";
  ] @ flags);

  (* Run menhir again to compile *.stripped.mly using the new code back-end.
     [--infer] is required. Take this opportunity to pass [--stacklang-test]
     and [--stacklang-dump] and other flags. *)
  let basenames = [ocamlbase ^ ""] in
  let log = ocamlbase ^ ".log"
  and ml = ocamlbase ^ ".ml"
  and mli = ocamlbase ^ ".mli"
  and stacklang = ocamlbase ^ ".stacklang"
  and timings = ocamlbase ^ ".timings"
  and scount = ocamlbase ^ ".scount"
  and dcount = ocamlbase ^ ".dcount" in
  let outputs = [ log; mli; ml; stacklang; timings; scount; dcount ] in
  let out, err = File log, File log in
  run `Positive out err basenames outputs (atoms [
    "--infer";
    "--stacklang-test";
    "--stacklang-dump";
    "-lg"; "2";
    "-la"; "2";
    "-lc"; "2";
    "--timings-to"; timings;
  ] @ flags);

  (* Force the previous commands to be run as part of quicktest. *)
  print (conjunction quick [stacklang]);

  (* Compile the file produced by --only-preprocess-uu. Since the semantic
     actions have been removed, this file should be well-typed and
     stand-alone. Thus, we check that the code produced by the new code
     back-end is accepted by OCaml. *)
  compile ocamlbase

  end

(* -------------------------------------------------------------------------- *)

(* Running a test. *)

let process input =
  match input with
  | NegativeTest basenames ->
      process_negative_test basenames
  | PositiveTest basenames ->
      process_positive_test basenames

(* -------------------------------------------------------------------------- *)

(* [run] runs a bunch of tests in parallel. *)

let run (inputs : inputs) =
  printf ";; %d distinct tests.\n\n" (List.length inputs);
  iter process inputs;
  if Settings.kind = "good" then
    print (alias test [quick])

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

(* We ignore the file %.stripped.mly, which is produced out of %.mly as part
   of the test. *)

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
  |> filter (fun f -> has_suffix ".mly" f && not (has_suffix (stripped ^ ".mly") f))
  |> map Filename.chop_extension
  |> sort Stdlib.compare
  |> groups equal_up_to_numeric_suffix
  |> map tag
  |> run

(* -------------------------------------------------------------------------- *)

(* Handling tests in merge/. *)

(* The number of tests is currently hardcoded here, and they have a fixed
   naming convention. *)

(* We wish to we pipe Menhir's output through sed in order to remove the
   auto-generated comments. We need this process to fail if Menhir fails.
   We could use a pipe and [set -o pipefail], but this is specific to
   bash. Instead, we go through a temporary file. *)

let n = 10

let merge mly lhs rhs out err xout xerr =
  let tmp = sprintf "%s.tmp" out in
  print (rule [] [] (
    redirect_stdout tmp (
    redirect_stderr err (
      not_expecting_failure (
        system
          "%%{bin:menhir} %%{dep:%s} \\\n        \
           --merge-errors %%{dep:%s} \\\n        \
           --merge-errors %%{dep:%s}"
            mly lhs rhs
  )))));
  print (rule [] [] (
    redirect_stdin tmp (
    redirect_stdout out (
      system "sed -e '/^##/d'"
  ))));
  print (phony test (diff xout out));
  print (phony test (diff xerr err))

let merge () =
     List.init n (fun i -> i + 1)
  |> List.iter (fun i ->
     let mly = "parser.mly"
     and lhs = sprintf "lhs%02d.messages" i
     and rhs = sprintf "rhs%02d.messages" i
     and out = sprintf "merged%02d.out" i
     and err = sprintf "merged%02d.err" i
     and xout = sprintf "merged%02d.xout" i
     and xerr = sprintf "merged%02d.xerr" i in
     merge mly lhs rhs out err xout xerr
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
