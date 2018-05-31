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

(* ---------------------------------------------------------------------------- *)

(* A mechanism to turn all display (logging, warnings, errors) on and off. *)

let enabled =
  ref true

let enable () =
  enabled := true

let disable () =
  enabled := false

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

let log kind verbosity msg =
  if kind >= verbosity && !enabled then
    Printf.fprintf stderr "%t%!" msg

let logG =
  log Settings.logG

let logA =
  log Settings.logA

let logC =
  log Settings.logC

(* ---------------------------------------------------------------------------- *)

(* Errors and warnings. *)

let errors =
  ref false

let print_positions f positions =
  List.iter (fun position ->
    fprintf f "%s:\n" (Positions.string_of_pos position)
  ) positions

let display continuation header positions format =
  let kprintf = if !enabled then Printf.kfprintf else Printf.ikfprintf in
  kprintf continuation stderr
    ("%a" ^^ header ^^ format ^^ "\n%!")
    print_positions positions

let error positions format =
  display
    (fun _ -> exit 1)
    "Error: "
    positions format

let signal positions format =
  display
    (fun _ -> errors := true)
    "Error: "
    positions format

let warning positions format =
  display
    (fun _ -> ())
    "Warning: "
    positions format

let errors () =
  !errors

let errorp v =
  error [ Positions.position v ]

let grammar_warning =
  if Settings.strict then signal else warning
