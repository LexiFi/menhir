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

(* Logging and log levels. *)

let log kind verbosity msg =
  if kind >= verbosity then
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

let display continuation header positions format =
  List.iter (fun position ->
    fprintf stderr "%s:\n" (Positions.string_of_pos position)
  ) positions;
  Printf.kfprintf
    continuation
    stderr
    (header ^^ format ^^ "\n%!")

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
