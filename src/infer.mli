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

(* [ntvar symbol] is the name of the type variable associated with a
   nonterminal symbol. *)

val ntvar: string -> string

(* [infer grammar] analyzes the grammar [grammar] and returns a new
   grammar, augmented with a [%type] declaration for every nonterminal
   symbol. The [ocamlc] compiler is used to infer types. *)

val infer: UnparameterizedSyntax.grammar -> UnparameterizedSyntax.grammar

(* [depend grammar] prints (on the standard output channel) the
   OCaml dependencies induced by the semantic actions.
   Then, it exits the program. *)

val depend: UnparameterizedSyntax.grammar -> 'a

