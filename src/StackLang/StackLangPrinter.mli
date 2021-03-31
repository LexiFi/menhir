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

(* This module offers a pretty-printer for StackLang. *)

val print_value: out_channel -> StackLang.value -> unit

val print_substitution: out_channel -> StackLang.substitution -> unit

val print: out_channel -> StackLang.program -> unit

val print_block: out_channel -> StackLang.block -> unit

val print_known_cells: out_channel -> StackLang.cell_info array -> unit

val print_states: out_channel -> StackLang.state_info StackLang.TagMap.t -> unit