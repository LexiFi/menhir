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

open StackLang

(* [successors yield block] applies the function [yield] in turn to every
   label that is the target of a [jump] instruction in the block [block]. *)

val successors: (label -> unit) -> block -> unit

(* [wf program] checks that the program [program] contains no references to
   undefined registers. This check is in principle unnecessary, but can be a
   useful debugging aid. *)

val wf: program -> unit

(* [inline program] transforms the program [program] by removing every
   unreachable block and by inlining away every (non-entry) label whose
   in-degree is 1. *)

val inline: program -> program

(* [measure program] computes instruction counts for the program [program].
   [print_measure m] prints this information. It is intended to be used for
   debugging and engineering purposes. *)

type measure
val measure: program -> measure
val print: measure -> unit

val get_args_map: block RegisterMap.t -> register list RegisterMap.t

val optimize: program -> program