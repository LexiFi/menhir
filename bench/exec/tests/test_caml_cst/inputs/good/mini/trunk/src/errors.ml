(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: errors.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module handles error messages. *)
open MultiEquation
open CoreAlgebra
open Printf
open Positions
open MiniTypingExceptions
open MiniSolver
open Unifier
open MiniAst

let error_channel = ref stderr

let set_error_channel c = 
  error_channel := c

let msg m p =
  fprintf !error_channel m (Positions.string_of_pos p)
    
let handle ?dont_exit e = (
  try
    e (); if dont_exit = None then exit 0
  with 

    | MiniLexer.Error (msg, c1, c2) ->
	fprintf !error_channel "%s near characters %d-%d.\n" msg c1 c2

    | NonExhaustiveMatch (pos, pat) -> 
	msg ("%s:\n This match is not exhaustive, the following pattern " ^^
	"is not matched:\n")
	  pos;
	assert false

    | CannotGeneralize (p, v) ->
	msg "%s:\n Cannot generalize `%s'.\n" p 
	  (Print.print_variable false v)

    | NonDistinctVariables (p, vs) ->
	let lvs = Misc.print_separated_list ";" 
	  (Print.print_variable false) vs in
	  msg ("%s:\n The following variables have been unified: [%s].\n")
	    p lvs 
	    
    | KindError p ->
	msg "%s:\n  Kind error.\n" p 

    | TypingError p -> 
	msg "%s:\n  Typing error.\n" p 

    | UnboundTypeConstructor (p, TName t) ->
	msg "%s:\n Unbound type constructor `%s'.\n" p t

    | UnboundIdentifier (p, k) ->
	msg "%s:\n Unbound identifier `%s'.\n" p k

    | CannotUnify (p, t1, t2) ->
	msg "%s:\n `%s' cannot be unified with `%s'.\n" p 
	  (Print.print_term false t1) 
	  (Print.print_term false t2);  

    | NonLinearPattern (p, SName x) ->
	msg "%s:\n The variable '%s' occurs several times.\n" p x

    | NotEnoughPatternArgts p ->
	msg "%s:\n Not enough pattern arguments.\n" p

    | InvalidDisjunctionPattern p ->
	msg 
	  "%s:\n A disjunction pattern must bind exactly the same variables.\n"
	  p

    | PartialDataConstructorApplication (p, d, u) ->
	msg
	  "%s:\n This data constructor needs %d arguments not %d.\n" 
	  p d u

    | InvalidNumberOfTypeVariable p ->
	msg "%s:\n Invalid number of local type variables in pattern.\n" p

    | MultipleLabels (p, LName n) ->
	msg "%s:\n Multiple definition of label %s.\n" p n

    | InvalidTypeVariableIdentifier (p, TName v) -> 
	msg "%s:\n `%s' type constructor is used as a type variable.\n" p v

    | RecursiveDefMustBeVariable p ->
	msg ("%s:\n The left-hand side of a recursive definition must be "^^
	     "a variable.\n") p

    | InvalidDataConstructorDefinition (p, DName k) ->
	msg "%s:\n The type of the data constructor '%s' is incorrect.\n"
	  p k

    | ParsingExceptions.Unclosed (b, e, p1, p2) ->
	msg "%s:\n Unclosed %s %s (may begin at %s).\n"
	  p2 b e (Positions.string_of_pos p1)
	
    | ParsingExceptions.Other l ->
	msg "%s:\n Parse error.\n"  (Positions.cpos l));

  if dont_exit = None then exit 1



