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

module type STACK_SYMBOLS = sig

  (**[stack_height s] is [Array.length (stack_symbols s)]. *)
  val stack_height: Lr1.node -> int

  (**[stack_symbols s] is the known suffix of the stack at state [s]. It
     is represented as an array of symbols. By convention, the top of
     the stack is the end of the array. *)
  val stack_symbols: Lr1.node -> Symbol.t array

  (**[production_symbols s] is the known suffix of the stack at a point
     where production [prod] is about to be reduced. *)
  val production_symbols: Production.index -> Symbol.t array

end

(**This module computes the known suffix of the stack, a sequence of
   symbols, in each of the automaton's states. The length of this
   sequence can be predicted based on the LR(0) items present in this
   state: it is the maximum position of the bullet over all items. *)
module Run () : STACK_SYMBOLS

(**This module computes the known suffix of the stack, a sequence of
   symbols, in each of the automaton's states. The length of this
   sequence is determined by an analysis of the paths in the LR(0)
   automaton. At each state, the sequence computed by [Run] is always
   a suffix of the sequence computed by [Long]. *)
module Long () : STACK_SYMBOLS

(* The "long invariant" was used in Menhir until 2012/08/25. However, the
   extra information that it contains, compared to the "short invariant",
   was useless; computing it was a waste of time. As of 2012/08/25, the
   short invariant has been used. As of 2021/05/14, the long invariant
   is re-introduced, for possible use in the new code back-end. *)

(**This utility function prints a sequence of symbols. Every symbol is
   preceded with a space. *)
val print_symbols: Symbol.t array -> string
