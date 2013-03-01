(* $Id$ *)

(** The abstract syntax of programs *)

open Positions
open MiniMultiEquation
open Sig


type program = binding list

and binding =
  | BindValue of position * value_definition list
  | BindRecValue of position * value_definition list
  | TypeDec of position * type_declaration list

and expression =
    
  (** Core ML. *)
  | EVar of position * name 
  | ELambda of position * pattern * expression
  | EAppAnnoted of position * expression * expression * existential_crterm
  | EBinding of position * binding * expression
  | EPrimApp of position * primitive * expression list
  
  (** Type annotations. *)
  | EForall of position * variable list * expression
  | EExists of position * variable list * expression
  | ETypeConstraint of position * expression * existential_crterm
  | EImpredicativity of position * expression * variable list * crterm * crterm
  
  (** Algebraic datatypes. *) 
  | EDCon of position * dname
  | EMatchAnnoted of position * expression * existential_crterm * (clause list) 

  (** Records. *)
  | ERecordEmpty of position
  | ERecordAccess of position * expression * lname
  | ERecordExtend of position * record_binding list * expression
  | ERecordUpdate of position * expression * lname * expression

  (** Misc. *)
  | EError of position * pattern
  | EAssertFalse of position


  
  
  
    
(** Program identifiers. *)
and name =
    MiniPst.name
      
(** Type variable names. *)
and tname =
    MiniPst.tname

(** Data constructors. *)
and dname =
    MiniPst.dname

(** Record labels. *)
and lname =
    MiniPst.lname 
    
(** Constant. *)
and primitive = 
	MiniPst.primitive

	    
(** Pattern matching clause. *)
and clause =
    position * pattern * expression

and record_binding =
    name * expression

and type_declaration =
    position * MiniKindInferencer.t * variance list * variable * tname * type_definition
    
and existential_crterm = variable list * crterm 
   

      
(** Type definitions. 
    
    {L [DAlgebraic]}
    Algebraic datatypes are defined by that way:
    [type id : kind = forall a1 ... an. K1 : typ | ... | KN : typ].
    For example, here is the definition of polymorphic lists:
    [type list : * -> * = forall a.
        Nil : list a
      | Cons : a -> list a -> list a
    ]
    
    {L [DAbbrev]}
    Abbreviation for types are simple macros.
*)
and type_definition = MiniEst.type_definition
(*
  | DAlgebraic of (position * dname * crterm) list
  | DAbbrev of crterm
*)

(** A value definition consists of a list of explicit universal
    quantifiers, a pattern, and an expression. *)
and value_definition =
    position * variable list * pattern * expression * existential_crterm

and pattern =
  | PZero of position
  | PVar of position * name
  | PWildcard of position 
  | PAlias of position * name * pattern
  | PTypeConstraint of position * pattern * existential_crterm
  | PPrimitive of position * primitive
  | PData of position * tname list * dname * pattern list
  | PAnd of position * pattern list
  | POr of position * pattern list





