 (* $Id$ *)

(** This module handles error messages. *)
open Sig
open Printf
open Positions
open MiniTypingExceptions

let msg m p =
  eprintf m (Positions.string_of_pos p)
    
let handle e = (
  try
    e (); exit 0
  with 

    | MiniLexer.Error (msg, c1, c2) ->
        fprintf stderr "%s near characters %d-%d.\n" msg c1 c2

    | NonExhaustiveMatch (pos, pat) -> 
        msg ("%s:\n This match is not exhaustive, the following pattern " ^^
        "is not matched:\n")
          pos;
        assert false

    | NotClosedTypeAnnotation (p, t) ->
        msg "%s:\n Unclosed annotation for pattern variable : %s\n" p 
          (MiniTermPrinter.print_term false t)
        
    | CannotGeneralize (p, v) ->
        msg "%s:\n Cannot generalize `%s'.\n" p 
          (MiniTermPrinter.print_variable false v)

    | CannotInferShape (p, v) ->
        msg "%s:\n Attempt to extrude rigid variable `%s'.\n" p 
          (MiniTermPrinter.print_variable false v)

    | InvalidAnnotationForLambda (p, t) ->
        msg "%s:\n The annotation %s is not valid for a lambda abstraction.\n" p 
          (MiniTermPrinter.print_term false t)

    | InvalidAnnotationForDataConstructor (p, dname, t) ->
        msg "%s:\n The annotation %s is not valid for the data constructor %s.\n" p 
          (MiniTermPrinter.print_term false t)
          dname
                    
    | InvalidAnnotationForPrimApp (p, t) ->
        msg "%s:\n The annotation %s is not valid for a primitive application.\n" p 
          (MiniTermPrinter.print_term false t)
                 
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
        
    | UnboundDataConstructor (p, t) ->
        msg "%s:\n Unbound type constructor identifier `%s'.\n" p t    

    | UnboundIdentifier (p, k) ->
        msg "%s:\n Unbound identifier `%s'.\n" p k

    | InvalidTypeConstructorUse (p, v, k, a) -> 
        msg ("%s:\n The `%s' constructor is used with %d arguments" ^^
            " instead of %d.\n")
          p v k a; 
    
    | CyclicStructureInferred (opt_name, var_root) -> 
        let pos_list = MiniTermPrinter.extract_pos_of_cyclic_variable var_root in
        let p = match pos_list with
          | [] -> Positions.undefined_position
          | pos1::pos_others -> pos1
          in
        msg "%s:\n Cyclic structure appeared `%s'%s.\n" p 
          (MiniTermPrinter.print_rec_variable var_root)
          (match opt_name with
            | None -> ""
            | Some name -> " on " ^ name
          )
              
    | CannotUnify (p, t1, t2) ->
        msg "%s:\n rigid variable `%s' cannot be unified with `%s'.\n" p 
          (MiniTermPrinter.print_term false t1) 
          (MiniTermPrinter.print_term false t2);  

    | CannotEqualize (p, t1, t2) ->
       msg "%s:\n Cannot equalize polymorphic terms `%s' and `%s'.\n" p 
          (MiniTermPrinter.print_term false t1) 
          (MiniTermPrinter.print_term false t2); 
          
    | NotAbleToUnifyMonoAndPoly (p, t1, t2) ->
        msg "%s:\n The polymorphic shape of `%s' cannot be \
                   infered to match `%s'.\n" p 
          (MiniTermPrinter.print_term false t1) 
          (MiniTermPrinter.print_term false t2);  

    | NonLinearPattern (p, x) ->
        msg "%s:\n The variable '%s' occurs several times.\n" p x

    | NotEnoughPatternArgts p ->
        msg "%s:\n Invalid number of arguments for type constructor in pattern.\n" p

    | InvalidNumberOfTypeVariable p ->
        msg "%s:\n Invalid number of local type variables in pattern.\n" p
    
    | MatchingDataConstructorWithTGen p ->
        msg "%s:\n The matched expression is annoted with a generally quantified type,\
             and this cannot match a type construction.\n" p
          
    | MultipleLabels (p, n) ->
        msg "%s:\n Multiple definition of label %s.\n" p n

    | InvalidTypeVariableIdentifier (p, v) -> 
        msg "%s:\n `%s' type constructor is used as a type variable.\n" p v

    | RecursiveDefMustBeVariable p ->
        msg ("%s:\n The left-hand side of a recursive definition must be "^^
             "a variable.\n") p

    | InvalidDataConstructorDefinition (p, k, case) ->
        let text = match case with
          | InvalidDataConstructorResult 
              -> "Only variables should appear in the result type"
          | InvalidDataConstructorQuantified 
              -> "Variables of the result type should be exactly those universally quantified"
          | InvalidDataConstructorNotDistinct 
              -> "Variables that appear in the result type should be distinct from each other"
          | InvalidDataConstructorBadKind
              -> "Kind of argument should be *, + or -, and kind of result should be *"
          | InvalidDataConstructorBadResult
              -> "The return type should be constructed with the type of the definition"
          | InvalidDataConstructorBadVariance v
              -> sprintf "Variance of variable %s is not respected in the arguments"
                         (MiniTermPrinter.print_variable false v);  
              in
        msg "%s:\n The type of the data constructor '%s' is incorrect.\n %s.\n"
          p k text
                              
    | ParsingExceptions.Unclosed (b, e, p1, p2) ->
        msg "%s:\n Unclosed %s %s (may begin at %s).\n"
          p2 b e (Positions.string_of_pos p1)
        
    | ParsingExceptions.Other l ->
        msg "%s:\n Parse error.\n"  (Positions.cpos l));
    exit 1



