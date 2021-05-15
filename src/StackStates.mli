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
   the size of the known suffix. This size information must be consistent:
   the size at a state [s] must be no greater than the minimum of the sizes
   at the predecessors of [s], plus one. *)
module Run (S : sig

  (**[stack_symbols s] is the known suffix of the stack at state [s]. It
     is represented as an array of symbols. By convention, the top of
     the stack is the end of the array. *)
  val stack_symbols: Lr1.node -> Symbol.t array

end) : sig

  (**A property is a description of the known suffix of the stack at state
     [s]. It is represented as an array of symbols. By convention, the top
     of the stack is the end of the array. Each array element is a set of
     states that may appear in this stack cell. *)
  type property =
    Lr1.NodeSet.t array

  (**[print] prints a property. *)
  val print: property -> string

  (**[stack_states s] is the known suffix of the stack at state [s]. *)
  val stack_states: Lr1.node -> property

  (**[production_states s] is the known suffix of the stack at a point
     where production [prod] is about to be reduced. *)
  val production_states: Production.index -> property

  (**[dump prefix f] dumps the result of the analysis to the output
     channel [f], in an unspecified format. The string [prefix] is
     emitted at the beginning of every line of output. *)
  val dump: string -> out_channel -> unit

end