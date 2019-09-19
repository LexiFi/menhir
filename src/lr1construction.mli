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

(* This module constructs an LR(1) automaton for the grammar described by the
   module [Grammar]. *)

(* In this construction, precedence declarations are not taken into account.
   Thus, conflicts are not resolved; no transitions or reductions are removed
   in order to resolve conflicts. As a result, every node is reachable from
   some entry node. *)

open Grammar

module Run () : sig

  (* An abstract type of nodes, that is, states in the LR(1) automaton. *)

  type node

  (* The number of nodes. *)

  val n: int

  (* Nodes are numbered from 0 to [n-1]. *)

  val number: node -> int
  val node: int -> node

  (* Each node represents an LR(1) state, that is, a set of LR(1) items. *)

  val state: node -> Lr0.lr1state

  (* To each start production corresponds an entry node. *)

  val entry : node ProductionMap.t

  (* Each node carries outgoing transitions towards other nodes. *)

  val transitions: node -> node SymbolMap.t

end
