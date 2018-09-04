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

(* The new OCaml type inference protocol means that Menhir is called twice, first
   with [--infer-write-query], then with [--infer-read-reply]. This means that any
   information messages or warnings issued before OCaml type inference takes place
   are duplicated, unless we do something about it. To address this issue, when
   [--infer-read-reply] is set, we disable all output until the point where we
   read the inferred [.mli] file. Then, we enable it again and continue. *)

(* An alternative idea would be to disable all output when [--infer-write-query]
   is set. However, we would then have no output at all if this command fails. *)

let () =
  Settings.(match infer with
  | IMReadReply _ ->
      disable()
  | _ ->
      ()
  )

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

let exit () =
  if errors() then
    exit 1

let errorp v =
  error [ Positions.position v ]

let grammar_warning =
  if Settings.strict then signal else warning
