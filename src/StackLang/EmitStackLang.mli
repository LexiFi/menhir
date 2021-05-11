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

(* This module creates a StackLang program, based on information about the
   grammar and about the LR(1) automaton that is extracted from the modules
   Grammar, Lr1, and Default. *)

module Run () : sig
  val program : StackLang.program

  val entry : Lr1.node -> StackLang.label
end

(* These are registers that must be defined (with dummy values) in the initial
   runtime environment in StackLangInterpreter. This is not very pretty, but
   works. It is used as part of our testing infrastructure for StackLang. *)

val required : StackLang.register list
