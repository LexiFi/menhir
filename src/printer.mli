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

(* A pretty-printer for [IL]. *)

module Make (X : sig

  (* This is the channel that is being written to. *)

  val f: out_channel

  (* This controls the way we print OCaml stretches (types and
     semantic actions). We either surround them with #line directives
     (for better error reports if the generated code is ill-typed) or
     don't (for better readability). The value is either [None] -- do
     not provide #line directives -- or [Some filename] -- do provide
     them. [filename] is the name of the file that is being written. *)

  val locate_stretches: string option

end) : sig

  val program: IL.program -> unit

  val expr: IL.expr -> unit

  val interface: IL.interface -> unit

end

(* Common instantiations. In the following two functions, [locate_stretches]
   is [None], so no #line directives are printed. *)

val     print_expr: out_channel -> IL.expr -> unit
val string_of_expr:                IL.expr -> string
