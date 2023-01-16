(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

(* The functor [Read] reads a .cmly file. If the file is unreadable,
   the exception [Error] is raised. Otherwise, the functor builds a
   module of type [Cmly_api.GRAMMAR], which gives access to a description
   of the grammar and automaton. *)

exception Error of string

val read_channel : in_channel -> Cmly_format.grammar

module Lift (X : sig val grammar : Cmly_format.grammar end) : Cmly_api.GRAMMAR

module Read (X : sig val filename : string end) : Cmly_api.GRAMMAR
