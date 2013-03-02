(* $Id$ *)

open Positions
open Sig

module Make (MultiEquation: MultiEquation) : 
  TypingExceptions with module MultiEquation = MultiEquation =
struct
  module MultiEquation = MultiEquation
  open MultiEquation
  open MultiEquation.Algebra.Ast

  (* FIXME: it lacks some commentaries. *)

  (** {2 Exceptions} *)

  (** [UnboundTypeIdentifier] is raised when an unbound type identifier
      is found. *)
  exception UnboundTypeIdentifier of position * tname
    
  (** [InvalidTypeVariableIdentifier] is raised when a type variable is 
      overwriting a type constructor. *)
  exception InvalidTypeVariableIdentifier of position * tname
    
  (** [UnboundDataConstructor] is raised when a constructor identifier is
      used although it has not been defined. *)
  exception UnboundDataConstructor of position * tname 
    
  (** [InvalidDataConstructorDefinition] is raised when a data constructor
      scheme is not legal. *)
  exception InvalidDataConstructorDefinition of position * dname

  (** [UnboundTypeVariable] is raised when a variable identifier is
      used although it has not been defined. *)
  exception UnboundTypeVariable of position * tname 
    
  (** [MultipleLabels] is raised when the user has built a record 
      with two fields with the same name. *)
  exception MultipleLabels of position * string
    
  (** [NonLinearPattern] is raised when at least two occurrences of a variable 
      appear in a pattern. *)
  exception NonLinearPattern of position * name
    
  exception NotEnoughPatternArgts of position

  exception InvalidNumberOfTypeVariable of position

  exception InvalidTypeConstructorUse of position * string * int * int
    
  (** This exception is raised by [unify] when a system of equations
      is found to be unsatisfiable. *)
  exception Inconsistency

  exception NonDistinctVariables of position * (variable list)
    
  exception CannotUnify of position * crterm * crterm
    
  exception CannotUnifyHeadWithTerm of position * string * crterm
    
  exception TypingError of Positions.position
    
  exception NonDistinctVariables of position * (variable list) 
    
  (** This exception is raised when a match is not complete. *)
  exception NonExhaustiveMatch of position * pattern
    
  exception CannotGeneralize of position * variable

  exception UnboundIdentifier of position * string

  exception UnboundConstructor of position * string

  exception KindError of position

  (** [RecursiveDefMustBeVariable] is raised in case of bad formed 
      recursive value definition. *)
  exception RecursiveDefMustBeVariable of position 

  exception InvalidCoercion of position * crterm * crterm    

  exception NonRigidInEqt of position * crterm 

end
