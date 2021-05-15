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

open Grammar

(**This module performs a static analysis of the LR(1) automaton in order to
   determine which states might possibly be held in the known suffix of the
   stack at every state.

   We assume that the known suffix of the stack, a sequence of symbols, has
   already been computed at every state. All that is needed, actually, is
   the size of the known suffix, given by the function [stack_height]. This
   size information must be consistent: the size at a state [s] must be no
   greater than the minimum of the sizes at the predecessors of [s], plus
   one. *)
module Run (S : sig

  (**[stack_height s] is the height of the known suffix of the stack
     at state [s]. *)
  val stack_height: Lr1.node -> int

  (**[production_height prod] is the height of the known suffix of the stack
     at a state where production [prod] can be reduced. *)
  val production_height: Production.index -> int

  (**[goto_height nt] is the height of the known suffix of the stack at a
     state where an edge labeled [nt] has just been followed. *)
  val goto_height: Nonterminal.t -> int

end) : sig

  (**A property is a description of the known suffix of the stack at state
     [s]. It is represented as an array. By convention, the top of the stack
     is the end of the array. Each array element is a set of states that may
     appear in this stack cell. *)
  type property =
    Lr1.NodeSet.t array

  (**[print] prints a property. *)
  val print: property -> string

  (**[stack_states s] is the known suffix of the stack at state [s]. *)
  val stack_states: Lr1.node -> property

  (**[production_states prod] is the known suffix of the stack at a state
     where production [prod] can be reduced. In the short invariant, the
     length of this suffix is [Production.length prod]. In the long
     invariant, its length can be greater. *)
  val production_states: Production.index -> property

  (**[goto_states nt] is the known suffix of the stack at a state where an
     edge labeled [nt] has just been followed. If [long] is false, then the
     length of this suffix is [1]. If [long] is true, then its length can be
     greater. *)
  val goto_states: Nonterminal.t -> property

  (**[dump prefix f] dumps the result of the analysis to the output
     channel [f], in an unspecified format. The string [prefix] is
     emitted at the beginning of every line of output. *)
  val dump: string -> out_channel -> unit

end
