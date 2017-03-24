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

(* This turns a grammar where nonterminal symbols can be parameterized
   into a grammar where nonterminal symbols are not parameterized. The
   transformation is a textual expansion process, whose termination is
   guaranteed by a simple type system.

   Expansion creates new nonterminal symbols whose names contain
   parentheses and commas. These names can be printed directly in
   informational messages (error messages, conflict reports,
   descriptions of the automaton, etc.). However, they must be
   sanitized via [Misc.normalize] when printed in a context where a
   valid identifier is expected. *)

val expand : Syntax.grammar -> UnparameterizedSyntax.grammar
