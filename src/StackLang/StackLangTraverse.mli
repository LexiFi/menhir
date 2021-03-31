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

(** [wf program] checks that the program [program] contains no references to
   undefined registers. This check is in principle unnecessary, but can be a
   useful debugging aid. *)
val wf: program -> unit

(** [wt program] checks that no impossible pop is performed and that ITypedBlock
    have correct [stack_type] annotations. *)
val wt: program -> unit

type measure

(** [measure program] computes instruction counts for the program [program].
   [print_measure m] prints this information. It is intended to be used for
   debugging and engineering purposes. *)
val measure: program -> measure
val print: measure -> unit

val get_args_map: block RegisterMap.t -> register list RegisterMap.t
