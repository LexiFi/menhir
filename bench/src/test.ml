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

(* Tests. *)

type input =
  | NegativeTest of string (* basename *)

let print_input = function
  | NegativeTest basename ->
      basename

type outcome =
  | Success
  | Failure of string (* message *)

type output =
  input * outcome

type inputs = input list
type outputs = output list

let process input : output =
  match input with
  | NegativeTest basename ->
      printf "Testing %s...\n%!" basename;
      let command = sprintf "%s %s.mly" menhir basename in
      if succeeds command then begin
        printf "[FAIL] %s\n%!" basename;
        input, Failure (sprintf "menhir should not accept %s.mly." basename)
      end
      else begin
        printf "[OK] %s\n%!" basename;
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

let negative : inputs =
     readdir bad
  |> to_list
  |> filter (has_suffix ".mly")
  |> map chop_extension
  |> sort compare
  |> map (fun basename -> NegativeTest basename)

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
