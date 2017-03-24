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

(* This module helps report errors. *)

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

val logG: int -> (out_channel -> unit) -> unit
val logA: int -> (out_channel -> unit) -> unit
val logC: int -> (out_channel -> unit) -> unit

(* ---------------------------------------------------------------------------- *)

(* Errors and warnings. *)

(* [error ps format ...] displays the list of positions [ps], followed with the
   error message [format ...], and exits. The strings "Error: " and "\n" are
   automatically added at the beginning and end of the error message. The
   message should begin with a lowercase letter and end with a dot. *)

val error: Positions.positions -> ('a, out_channel, unit, 'b) format4 -> 'a

(* [errorp] is like [error], but uses the position range carried by [v]. *)

val errorp: _ Positions.located -> ('a, out_channel, unit, 'b) format4 -> 'a

(* [signal] is like [error], except it does not exit immediately. It sets a
   flag which can be tested using [errors]. *)

val signal: Positions.positions -> ('a, out_channel, unit, unit) format4 -> 'a

(* [errors] returns [true] if [signal] was previously called. Together
   [signal] and [errors] allow reporting multiple errors before aborting. *)

val errors: unit -> bool

(* [warning] is like [signal], except it does not set a flag. *)

val warning: Positions.positions -> ('a, out_channel, unit, unit) format4 -> 'a

(* Certain warnings about the grammar can optionally be treated as errors.
   The following function emits a warning or error message, via [warning] or
   [signal]. It does not stop the program; the client must at some point call
   [errors] and stop the program if any errors have been reported. *)

val grammar_warning: Positions.positions -> ('a, out_channel, unit, unit) format4 -> 'a
