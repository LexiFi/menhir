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

(* This module offers a pretty-printer for  *)

open StackLang

val print: out_channel -> program -> unit
val to_string: program -> string

val print_value: out_channel -> value -> unit
val value_to_string: value -> string

val print_bindings: out_channel -> bindings -> unit
val bindings_to_string: bindings -> string

val print_tblock: out_channel -> typed_block -> unit
val tblock_to_string: typed_block -> string

val print_block: out_channel -> block -> unit
val block_to_string: block -> string

val print_known_cells: out_channel -> cell_info array -> unit
val known_cells_to_string: cell_info array -> register

val print_states: out_channel -> state_info TagMap.t -> unit
val states_to_string: state_info TagMap.t -> string

val print_pattern: out_channel -> pattern -> unit
val pattern_to_string: pattern -> string
