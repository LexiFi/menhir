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

(** [inline g] traverses the rules of [g] and inlines the non terminal
    definitions that are marked with [%inline]. It returns a pair of the transformed
    grammar and a flag that tells whether any inlining was actually done. *)
val inline: UnparameterizedSyntax.grammar -> UnparameterizedSyntax.grammar * bool
