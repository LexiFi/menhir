open Sys
open Array
open List
open Filename
open Printf
open Auxiliary

(* -------------------------------------------------------------------------- *)

(* Settings. *)

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

(* -------------------------------------------------------------------------- *)

(* Paths. *)

let root =
  (* Move up to the root of the Menhir repository. *)
  absolute_directory "../.."

let src =
  root ^ "/src"

let bad =
  root ^ "/bench/bad"

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

(* Tests. *)

type input =
  (* A negative test input is a list of basenames, without the .mly extension.
     These files must be passed together to menhir. *)
  | NegativeTest of filename list

let print_input = function
  | NegativeTest basenames ->
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

type inputs = input list
type outputs = output list

let prepare (bits : string list) : command =
  let cmd = sep bits in
  log 2 "%s\n%!" cmd;
  cmd

let process_negative_test basenames : unit =
  (* Informational message. *)
  let id = id basenames in
  log 1 "Testing %s...\n%!" id;

  (* A --base option is needed for groups of several files. *)
  let base = if length basenames > 1 then sprintf "--base %s" id else "" in

  (* The output is stored in this file. *)
  let result = id ^ ".result" in

  (* Run Menhir in the directory bad/. *)
  let cmd = prepare (
    "cd" :: bad :: "&&" ::
    menhir :: base :: mlys basenames @ sprintf ">%s" result :: "2>&1" :: []
  ) in

  if command cmd = 0 then begin
    log 1 "[FAIL] %s\n%!" id;
    fail "menhir should not accept %s.\n" (thisfile basenames)
  end;

  (* Check that the output coincides with what was expected. *)
  let expected = id ^ ".expected" in
  let cmd = prepare (
    "cd" :: bad :: "&&" ::
    "diff" :: expected :: result :: []
  ) in
  if succeeds cmd then
    log 1 "[OK] %s\n%!" id
  else begin
    log 1 "[FAIL] %s\n%!" id;
    fail "menhir correctly rejects %s, with incorrect output.\n(%s)\n"
      (thisfile basenames)
      cmd
  end

let process input : output =
  try
    begin match input with
    | NegativeTest basenames ->
        process_negative_test basenames
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

let negative : inputs =
     readdir bad
  |> to_list
  |> filter (has_suffix ".mly")
  |> map chop_extension
  |> sort compare
  |> groups equal_up_to_numeric_suffix
  |> map (fun basenames -> NegativeTest basenames)

let inputs =
  negative

let outputs : outputs =
  printf "Preparing to run %d tests...\n%!" (length inputs);
  run negative

let successful, failed =
  partition (fun (_, o) -> o = OK) outputs

let () =
  printf "%d out of %d tests are successful.\n"
    (length successful) (length inputs);
  failed |> iter (fun (input, outcome) ->
    printf "\n[FAIL] %s\n%s" (print_input input) (print_outcome outcome)
  )
