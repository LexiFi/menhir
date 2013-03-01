(* This module performs proof search and displays its outcome. *)

open Printf
open Print
open Data
open Proof
open Bool3

module D =
  Driver.Make (Proof)

let () =
  D.prove Front.goals

let conjunction =
  max

let () =
  let global_status =
    ref BTrue
  in
  iter_initial (fun goal ->
    let status = status goal in
    global_status := conjunction !global_status status;
    w (fun b ->
      bprintf b "Goal %a : %a\n"
	Goal.print goal
	Bool3.print status
    )
  );
  exit (match !global_status with
  | BTrue ->
      0
  | BFalse ->
      1
  | BUndetermined ->
      assert false
  )

(* TEMPORARY set up test suite in Makefile with timings and success reports *)

