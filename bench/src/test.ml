open Sys
open Array
open List
open Filename
open Printf
open Auxiliary

(* TEMPORARY:
   -- set the verbosity on the command line
   -- allow running just one test?
   -- allow recreating all expected output files (just remove them and run)
 *)

(* -------------------------------------------------------------------------- *)

(* Logging. *)

(* 0 is minimal verbosity;
   1 shows some progress messages;
   2 is maximal verbosity. *)

let verbosity =
  1

let log level format =
  kprintf (fun s ->
    if level <= verbosity then
      print_string s
  ) format

(* Extend [fail] to display an information message along the way.
   The message is immediately emitted by the worker, depending on
   the verbosity level, whereas the failure message is sent back
   to the master. *)

let fail id format =
  log 1 "[FAIL] %s\n%!" id;
  fail format

(* When issuing an external command, log it along the way. *)

let command cmd =
  log 2 "%s\n%!" cmd;
  command cmd

(* -------------------------------------------------------------------------- *)

(* Paths. *)

let root =
  (* Move up to the root of the Menhir repository. *)
  absolute_directory "../.."

let src =
  root ^ "/src"

let good =
  root ^ "/bench/good"

let good_slash filename =
  good ^ "/" ^ filename

let bad =
  root ^ "/bench/bad"

let bad_slash filename =
  bad ^ "/" ^ filename

(* We use the stage 2 executable (i.e., Menhir compiled by Menhir)
   because it has better syntax error messages and we want to test
   them. *)
(* The standard library is the one in [src], viewed from [test/bad]
   or [test/good], so we use the relative path [../../src]. *)
let menhir =
  src ^ "/_stage2/menhir.native --stdlib ../../src"

(* -------------------------------------------------------------------------- *)

(* Test files and groups of test files. *)

let id basenames =
  (* A name for a nonempty group of test files. *)
  hd basenames

let thisfile basenames =
  if length basenames > 1 then "these input files" else "this input file"

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

let print_input = function
  | NegativeTest basenames ->
      id basenames
  | PositiveTest basenames ->
      id basenames

type outcome =
  | OK
  | Fail of string (* message *)

let print_outcome = function
  | OK ->
      ""
  | Fail msg ->
      msg

type output =
  input * outcome

type outputs = output list

let print_output (input, outcome) =
  printf "\n[FAIL] %s\n%s"
    (print_input input)
    (print_outcome outcome)

(* -------------------------------------------------------------------------- *)

(* Auxiliary functions. *)

let check_expected directory id result expected =
  (* Check that the file [expected] exists. If it does not exist, create
     it by renaming [result] to [expected]. Nevertheless, fail, and invite
     the user to review the newly created file. *)
  if not (file_exists (directory ^ "/" ^ expected)) then begin
    let cmd = sep ["cd"; directory; "&&"; "mv"; result; expected] in
    if command cmd = 0 then
      let cmd = sep ["more"; directory ^ "/" ^ expected] in
      fail id "The file %s did not exist.\n\
               I have just created it. Please review it.\n%s\n"
        expected cmd
    else
      fail id "The file %s does not exist.\n" expected
  end

(* -------------------------------------------------------------------------- *)

(* Running a negative test. *)

let process_negative_test basenames : unit =

  (* Display an information message. *)
  let id = id basenames in
  log 2 "Testing %s...\n%!" id;

  (* A --base option is needed for groups of several files. *)
  let base = if length basenames > 1 then sprintf "--base %s" id else "" in

  (* The output is stored in this file. *)
  let result = id ^ ".result" in

  (* Run Menhir in the directory bad/. *)
  let cmd = sep (
    "cd" :: bad :: "&&" ::
    menhir :: base :: mlys basenames @ sprintf ">%s" result :: "2>&1" :: []
  ) in
  if command cmd = 0 then
    fail id "menhir should not accept %s.\n" (thisfile basenames);

  (* Check that the file [expected] exists. *)
  let expected = id ^ ".expected" in
  check_expected bad id result expected;

  (* Check that the output coincides with what was expected. *)
  let cmd = sep ("cd" :: bad :: "&&" :: "diff" :: expected :: result :: []) in
  if command (silent cmd) <> 0 then
    fail id "menhir correctly rejects %s, with incorrect output.\n(%s)\n"
      (thisfile basenames)
      cmd;

  (* Succeed. *)
  log 1 "[OK] %s\n%!" id

(* -------------------------------------------------------------------------- *)

(* Running a positive test. *)

(*
  Conventions:
  The file %.flags   (if it exists) stores extra flags for Menhir.
  The file %.opp.out stores the output of menhir --only-preprocess.
  The file %.opp.exp stores its expected output.
  The file %.out     stores the output of menhir.
  The file %.exp     stores its expected output.
 *)

let process_positive_test basenames : unit =

  (* Display an information message. *)
  let id = id basenames in
  log 2 "Testing %s...\n%!" id;

  (* A --base option is needed for groups of several files. *)
  let base = if length basenames > 1 then sprintf "--base %s" id else "" in

  (* Extra flags. *)
  let flags = id ^ ".flags" in
  let flags =
    if file_exists (good_slash flags) then sprintf "`cat %s`" flags else ""
  in

  (* Run menhir --only-preprocess. *)
  let oppout = id ^ ".opp.out" in
  let cmd = sep (
    "cd" :: good :: "&&" ::
    menhir :: "--only-preprocess" :: base :: flags
           :: mlys basenames @ sprintf ">%s" oppout :: "2>&1" :: []
  ) in
  if command cmd <> 0 then begin
    let cmd = sep ["more"; good_slash oppout] in
    fail id "menhir rejects %s.\n%s\n" (thisfile basenames) cmd
  end;

  (* Check that the file [oppexp] exists. *)
  let oppexp = id ^ ".opp.exp" in
  check_expected good id oppout oppexp;

  (* Check that the output coincides with what was expected. *)
  let cmd = sep ("cd" :: good :: "&&" :: "diff" :: oppexp :: oppout :: []) in
  if command (silent cmd) <> 0 then
    fail id "menhir --only-preprocess accepts %s,\nbut produces incorrect output.\n(%s)\n"
      (thisfile basenames)
      cmd;

  (* Succeed. *)
  log 1 "[OK] %s\n%!" id

(* -------------------------------------------------------------------------- *)

(* Running a test. *)

let process input : output =
  try
    begin match input with
    | NegativeTest basenames ->
        process_negative_test basenames
    | PositiveTest basenames ->
        process_positive_test basenames
    end;
    input, OK
  with Failure msg ->
    input, Fail msg

(* -------------------------------------------------------------------------- *)

(* [run] runs a bunch of tests in parallel. *)

let run (inputs : inputs) : outputs =
  Functory.Cores.set_number_of_cores (get_number_of_cores ());
  (* Functory.Control.set_debug true; *)
  flush stdout; flush stderr;
  let outputs = Functory.Cores.map ~f:process inputs in
  outputs

(* -------------------------------------------------------------------------- *)

(* Main. *)

(* Menhir can accept several .mly files at once. By convention, if several
   files have the same name up to a numeric suffix, then they belong in a
   single group and should be fed together to Menhir. *)

let inputs directory : filename list list =
     readdir directory
  |> to_list
  |> filter (has_suffix ".mly")
  |> map chop_extension
  |> sort compare
  |> groups equal_up_to_numeric_suffix

let positive : inputs =
     inputs good
  |> map (fun basenames -> PositiveTest basenames)

let negative : inputs =
     inputs bad
  |> map (fun basenames -> NegativeTest basenames)

let inputs =
  positive @ negative

let outputs : outputs =
  printf "Preparing to run %d tests...\n%!" (length inputs);
  run inputs

let successful, failed =
  partition (fun (_, o) -> o = OK) outputs

let () =
  printf "%d out of %d tests are successful.\n"
    (length successful) (length inputs);
  failed |> iter (fun (input, outcome) ->
    printf "\n[FAIL] %s\n%s" (print_input input) (print_outcome outcome)
  )
