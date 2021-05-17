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

type error =
  { context : label
        (** The name of the routine during which the error occurs. *)
  ; culprit : block  (** The block responsible for the error. *)
  ; message : string  (** An arbirary string that explains the error *)
  ; state_relevance : bool
        (** indiquate whether printing the states
            with their type information is relevant to understanding the error. *)
  }

exception StackLangError of error

val wf : program -> unit
(** [wf program] checks that the program [program] contains no references to
   undefined registers. This check is in principle unnecessary, but can be a
   useful debugging aid. Raises [StackLangError] if the check fails.*)

val wt : program -> unit
(** [wt program] checks that no impossible pop is performed and that ITypedBlock
    have correct [stack_type] annotations. Raises [StackLangError] if it is not
    the case. *)

type measure

val measure : program -> measure
(** [measure program] computes instruction counts for the program [program].
   [print_measure m] prints this information. It is intended to be used for
   debugging and engineering purposes. *)

val print : measure -> unit
(** [print m] print the measure [m] *)

val test : unit -> unit
