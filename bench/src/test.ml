open Sys
open Array
open List
open Filename
open Printf
open Auxiliary

(* -------------------------------------------------------------------------- *)

(* Paths. *)

let root =
  (* Move up to the root of the Menhir repository. *)
  absolute_directory "../.."

let src =
  root ^ "/src"

let bad =
  root ^ "/bench/bad"

let menhir =
  src ^ "/_stage1/menhir.native --stdlib " ^ src

(* -------------------------------------------------------------------------- *)

(* Test files and groups of test files. *)

let id basenames =
  (* A name for a group of test files. *)
  if length basenames = 1 then
    hd basenames
  else
    sprintf "%s[0-9] (%d files)"
      (chop_numeric_suffix (hd basenames))
      (length basenames)

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
      hd basenames

type outcome =
  | Success
  | Failure of string (* message *)

type output =
  input * outcome

type inputs = input list
type outputs = output list

let process input : output =
  match input with

  (* A negative test. *)
  | NegativeTest basenames ->

      (* Informational message. *)
      let id = id basenames in
      printf "Testing %s...\n%!" id;

      (* A --base option is needed for groups of several files. *)
      let base =
        if length basenames > 1 then
          sprintf "--base %s" (chop_numeric_suffix (hd basenames))
        else
          ""
      in

      (* Run Menhir. *)
      let command = sep (menhir :: base :: mlys basenames) in

      if succeeds command then begin
        printf "[FAIL] %s\n%!" id;
        let msg = sprintf "menhir should not accept %s." (thisfile basenames) in
        input, Failure msg
      end
      else begin
        printf "[OK] %s\n%!" id;
        input, Success
      end

(* -------------------------------------------------------------------------- *)

(* [run] runs a bunch of tests in parallel. *)

let run (inputs : inputs) : outputs =
  Functory.Cores.set_number_of_cores (get_number_of_cores ());
  (* Functory.Control.set_debug true; *)
  flush stdout; flush stderr;
  let outputs = Functory.Cores.map process inputs in
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
  partition (fun (_, o) -> o = Success) outputs

let () =
  printf "%d out of %d tests are successful.\n"
    (length successful) (length inputs);
  failed |> iter (fun (input, outcome) ->
    printf "[FAIL] %s\n" (print_input input)
  )
