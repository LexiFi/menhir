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

(* This module offers an imperative API for building a StackLang program. *)

open StackLang

(* -------------------------------------------------------------------------- *)

(** A program is built by invoking the functor [Build]. The following data
   must be provided: *)
module Build (L : sig
  (** A type of code labels (not necessarily strings). *)
  type label

  (** An injection of labels into strings. *)
  val print: label -> string

  (** A way of iterating over all labels. *)
  val iter: (label -> unit) -> unit

  (** A mapping of labels to code. The function call [code label] is expected
     to use the imperative API below to build the code block that corresponds
     to label [label]. *)
  val code: label -> unit

  (** A family of entry labels. *)
  val entry: label Lr1.NodeMap.t

  val states: cell_info array Lr1.NodeMap.t
  
end) : sig

  (** A StackLang program. *)
  val program: program

end

(* -------------------------------------------------------------------------- *)

(* The following imperative API can be used by the function [code] above. *)


(**
Set the type of the whole block
*)
val set_stack_type: cell_info array -> unit
val set_final_type: IL.typ -> unit
val set_needed: register list -> unit

(* Each of the functions in the first group extends a code block that is
   currently under construction. Each of the functions in the second group
   ends the construction of the block. Each of the functions in the third
   group generates a case analysis construct, whose branches can then be
   independently constructed. *)

(* Group 1: Instructions with exactly one successor. *)

val need: registers -> unit
val need_list: register list -> unit
val push: value -> unit
val pop: pattern -> unit
val def: pattern -> value -> unit
val prim: register -> primitive -> unit
val trace: string -> unit
val comment: string -> unit

(* [move dst src] generates a move instruction from register [src] to
   register [dst]. It is a short-hand for [def (PReg dst) (VReg src)]. *)
val move: register -> register -> unit

(* Group 2: Instructions with zero successor. *)

val die: unit -> unit
val return: register -> unit
val jump: label -> unit

(* Group 3: Case analysis instructions. *)

(* [case_token src cases] generates a case analysis instruction on a token,
   which is held in the register [src]. The user-provided function [cases] is
   provided with two functions, [branch] and [default], which allow generating
   an ordinary branch (guarded by a pattern) and generating a default branch.
   The default branch is implicitly discarded if the ordinary branches alone
   form an exhaustive case analysis. *)

val case_token:
  register ->
  (
    (* branch:  *) (tokpat -> (unit -> unit) -> unit) ->
    (* default: *) ((unit -> unit) -> unit) ->
    unit
  ) ->
  unit

(* [case_tag src cases] generates a case analysis instruction on a tag, which
   is held in the register [src]. The user-provided function [cases] is
   provided with one function, [branch], which allows generating a branch
   (guarded by a pattern). *)

val case_tag:
  register ->
  (
    (* branch:  *) (tagpat -> (unit -> unit) -> unit) ->
    unit
  ) ->
  unit
