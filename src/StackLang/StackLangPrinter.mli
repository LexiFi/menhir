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

val print_value: out_channel -> value -> unit

val print_substitution: out_channel -> substitution -> unit

val print: out_channel -> program -> unit

val print_block: out_channel -> block -> unit

val print_known_cells: out_channel -> cell_info array -> unit

val print_states: out_channel -> state_info TagMap.t -> unit

val pattern_to_string: pattern -> string

val value_to_string: value -> string

val known_cells_to_string: cell_info array -> register