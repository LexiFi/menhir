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

  val print : label -> string
  (** An injection of labels into strings. *)

  val iter : (label -> unit) -> unit
  (** A way of iterating over all labels. *)

  val code : label -> unit
  (** A mapping of labels to code. The function call [code label] is expected
     to use the imperative API below to build the code block that corresponds
     to label [label]. *)

  val entry : string StringMap.t
  (** A family of entry labels. *)

  val states : state_info TagMap.t
  (** A map of represented states to their typing information. *)
end) : sig
  val program : program
  (** A StackLang program. *)
end

(* -------------------------------------------------------------------------- *)

(* The following imperative API can be used by the function [code] above. *)

val routine_stack_type : cell_info array -> unit
(** [routine_stack_type typ] sets the stack type of the current routine to [typ].
    It is mandatory to call it once. *)

val routine_final_type : Stretch.ocamltype -> unit
(** [routine_final_type typ] sets the final type of the current routine to [typ].
    It can be called once or never, depending on whether the routine has a
    determined final type. *)

(* Each of the functions in the first group extends a code block that is
   currently under construction. Each of the functions in the second group
   ends the construction of the block. Each of the functions in the third
   group generates a case analysis construct, whose branches can then be
   independently constructed. *)

(* Group 1: Instructions with exactly one successor. *)

val push : value -> cell_info -> unit

val pop : pattern -> unit

val def : pattern -> value -> unit

val prim : register -> primitive -> unit

val trace : trace -> unit

val comment : string -> unit

(* [move dst src] generates a move instruction from register [src] to
   register [dst]. It is a short-hand for [def (PReg dst) (VReg src)]. *)
val move : register -> register -> unit

(* Group 2: Instructions with zero successor. *)

val die : unit -> unit

val return : value -> unit

val jump : label -> unit

(* Group 3: Case analysis instructions. *)

(* [case_token src cases] generates a case analysis instruction on a token,
   which is held in the register [src]. The user-provided function [cases] is
   provided with two functions, [branch] and [default], which allow generating
   an ordinary branch (guarded by a pattern) and generating a default branch.
   The default branch is implicitly discarded if the ordinary branches alone
   form an exhaustive case analysis. *)

val case_token :
     register
  -> (   ((* branch:  *) tokpat -> (unit -> unit) -> unit)
      -> (* default: *) ((unit -> unit) -> unit)
      -> unit )
  -> unit

(* [case_tag src cases] generates a case analysis instruction on a tag, which
   is held in the register [src]. The user-provided function [cases] is
   provided with one function, [branch], which allows generating a branch
   (guarded by a pattern). *)

val case_tag :
     register
  -> (((* branch:  *) tagpat -> (unit -> unit) -> unit) -> unit)
  -> unit
