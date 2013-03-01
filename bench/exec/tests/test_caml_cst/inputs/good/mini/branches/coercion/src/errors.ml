(* $Id$ *)

(** This module handles error messages. *)
open Sig
open Printf
open Positions
open MiniTypingExceptions

let error_channel = ref stderr

let msg m p =
  fprintf (!error_channel) m (Positions.string_of_pos p)
    
let handle ?dont_exit e = (
  try
    e (); if dont_exit = None then exit 0
  with 

    | MiniLexer.Error (msg, c1, c2) ->
	fprintf stderr "%s near characters %d-%d.\n" msg c1 c2

    | NonExhaustiveMatch (pos, pat) -> 
	msg ("%s:\n This match is not exhaustive, the following pattern " ^^
	"is not matched:\n")
	  pos;
	assert false

    | CannotGeneralize (p, v) ->
	msg "%s:\n Cannot generalize `%s'.\n" p 
	  (MiniTermPrinter.print_variable false v)

    | NonDistinctVariables (p, vs) ->
	let lvs = Misc.print_separated_list ";" 
	  (MiniTermPrinter.print_variable false) vs in
	    msg ("%s:\n The following variables have been unified: [%s].\n")
	      p lvs 

    | KindError p ->
	msg "%s:\n  Kind error.\n" p 

    | TypingError p -> 
	msg "%s:\n  Typing error.\n" p 

    | UnboundConstructor (p, t) ->
	msg "%s:\n Unbound type identifier `%s'.\n" p t

    | UnboundIdentifier (p, k) ->
	msg "%s:\n Unbound identifier `%s'.\n" p k

    | InvalidTypeConstructorUse (p, v, k, a) -> 
	msg ("%s:\n The `%s' constructor is used with %d arguments" ^^
	    " instead of %d.\n")
	  p v k a; 

    | CannotUnify (p, t1, t2) ->
	msg "%s:\n `%s' cannot be unified with `%s'.\n" p 
	  (MiniTermPrinter.print_term false t1) 
	  (MiniTermPrinter.print_term false t2);  

    | NonLinearPattern (p, x) ->
	msg "%s:\n The variable '%s' occurs several times.\n" p x

    | NotEnoughPatternArgts p ->
	msg "%s:\n Not enough pattern arguments.\n" p

    | InvalidNumberOfTypeVariable p ->
	msg "%s:\n Invalid number of local type variables in pattern.\n" p

    | MultipleLabels (p, n) ->
	msg "%s:\n Multiple definition of label %s.\n" p n

    | InvalidTypeVariableIdentifier (p, v) -> 
	msg "%s:\n `%s' type constructor is used as a type variable.\n" p v

    | RecursiveDefMustBeVariable p ->
	msg ("%s:\n The left-hand side of a recursive definition must be "^^
	     "a variable.\n") p
    | NonRigidInEqt (p, t) ->
	msg ("%s:\n Trying to use non rigid '%s' in equation theory."^^
	       "\n Is the matched expression well annotated ?\n")
	  p 
	  (MiniTermPrinter.print_term false t) 

    | ParsingExceptions.Unclosed (b, e, p1, p2) ->
	msg "%s:\n Unclosed %s %s (may begin at %s).\n"
	  p2 b e (Positions.string_of_pos p1)
	
    | ParsingExceptions.Other l ->
	msg "%s:\n Parse error.\n"  (Positions.cpos l)

    | InvalidCoercion (p, t1, t2) ->
	msg ("%s:\n '%s' cannot be coerced into '%s'.\n")
	  p 
	  (MiniTermPrinter.print_term false t1) 
	  (MiniTermPrinter.print_term false t2));

(*    | _ -> 
	fprintf stderr ("Unexpected error.\n")); *)

  if dont_exit = None then exit 1



