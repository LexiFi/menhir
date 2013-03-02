(* $Id$ *)

(** The abstract syntax of programs *)

open Positions

(** A [program] in the Mini language is a sequence of toplevel bindings. *)
type program = 
    binding list

(** A [binding] is 
    {ul {- a set of value definition ({!MiniAst.value_definition}):
           [ let v1 = exp1 and v2 = exp2 and ... and vn = expn v ]}
        {- a set of mutually recursive definitions:
           [ letrec v1 = exp1 and v2 = exp2 and ... and vn = expn v ]}
        {- a type definition ({!MiniAst.type_definition}).}}
*)
and binding =
  | BindValue of position * value_definition list
  | BindRecValue of position * value_definition list
  | TypeDec of position * type_declaration list

(** An [expression] in Mini. Here is a description of the abstract
    syntax tree's nodes:
    
    {3 Core ML}

    {L [EVar]}
    A variable is an identifier in the language \[azAZ\]\[azAZ_\]*.

    {L [ELambda]}
    Lambda expressions follow the syntax [\pattern. exp]. For
    example, [\x. x + 1] is the successor function. 

    {L [EApp]}
    Application is done by concatenation as in [id 0]. 

    {L [EBinding]}
    One can define binding locally using the {b let} keyword. 
    For instance: [let id = \x.x in id 0]. There is a syntactic
    sugar to expand: [let identifier pat1 ... patn = exp] into
    [let identifier = \pat1. ... \patn. exp]. Furthermore, a 
    set of rigid universally quantified type variable can be 
    introduced in the typing scope using the syntax:
    [let forall a1 ... an. id = exp]. 

    {L [EPrimApp]}
    The Mini language has builtins like integers or predefined
    algebraic datatypes. See {!primitive}.
      
    {L [EForall]}
    Universally quantified type variables can be introduced using
    the syntax: [forall a.exp]. Such type variables can be used
    as rigid variables in type annotations, as in: 
    [let id = forall a. (\x. x : a -> a)]
    
    {L [EExist]}
    Existentially quantified type variables can be introduced using
    the syntax: [exists a.exp]. Such type variables can be used
    as rigid variables in type annotations, as in: 
    [let id = exists a. (\x. x : a -> a)]
    
    {L [ETypeConstraint]}
    Expression can be annotated by types. As an example:
    [let id = \x. x : int -> int].

    {3 Algebraic Datatypes}

    {L [EDCon]}
    Data constructors are used as usual function expression except
    that they must be fully applied. [Cons 0 Nil] is the application
    of the data constructor [Cons] to [0] and [Nil].
    
    {L [EDMatch]}
    A value whose type is an algebraic datatype can be matched against
    a set of [clause]'s patterns. The syntax of matching in Mini is
    [match exp with clause1 | clause2 | ... | clausen end]. For instance,
    the following function computes the length of a list:
    [let rec len l = 
      match exp with
        Nil => 0
      | Cons x xs => 1 + len xs
      end
    ]
    
    {3 Record}

    {L [ERecordEmpty]}
    The syntax [{}] defines an empty record.

    {L [ERecordAccess]}
    The syntax [expression.l] is an access the label [l] of the
    record expression.

    {L [ERecordExtend]}
    A record can be defined by extension as in 
    [{ l1 = exp1 and l2 = exp2 and ... and ln = expn }].

    {L [ERecordUpdate]}
    Record provides extension using the syntax
    [expression.l <- expression].

    {3 Misc}

    {L [EError]}
    This node is used internally to define dead code branches.

    {L [EAssertFalse]}
    The user can use the syntax [assert false] to express 
    assumed dead code branches.

*)
and expression =
    
  (** Core ML. *)
  | EVar of position * name 
  | ELambda of position * pattern * expression
  | EApp of position * expression * expression
  | EBinding of position * binding * expression
  | EPrimApp of position * primitive * expression list
  | EForall of position * tname list * expression
  | EExists of position * tname list * expression
  
  (** Type annotations. *)
  | ETypeConstraint of position * expression * shape
  
  (** Algebraic datatypes. *) 
  | EDCon of position * dname * expression list
  | EMatch of position * expression * (clause list)

  (** Records. *)
  | ERecordEmpty of position
  | ERecordAccess of position * expression * lname
  | ERecordExtend of position * record_binding list * expression
  | ERecordUpdate of position * expression * lname * expression

  (** Misc. *)
  | EError of position * pattern
  | EAssertFalse of position

  (** Coercion. *)
  | ECoerce of position * expression * tname list * typ * typ

(** Program identifiers. *)
and name =
    string
      
(** Type variable names. *)
and tname =
    string

(** Data constructors. *)
and dname =
    string

(** Record labels. *)
and lname =
    string 
  
(** Constant. *)
and primitive =
  | PIntegerConstant of int	(** Integer constant. *)
  | PCharConstant of char	(** Character constant. *)
  | PUnit			(** Unit constant. *)

(** Pattern matching clause. *)
and clause =
    position * pattern * expression

and record_binding =
    name * expression

and type_declaration =
    position * kind * tname * type_definition

(** Type definitions. 

    {L [DAlgebraic]}
    Algebraic datatypes are defined by that way:
    [type id : kind = forall a1 ... an. K1 : typ | ... | KN : typ].
    For example, here is the definition of polymorphic lists:
    [type list : * -> * = forall a.
        Nil : list a
      | Cons : a -> list a -> list a
    ]
*)
and type_definition =
  | DAlgebraic of tname list * (position * dname * tname list * typ) list
  | DAbbrev of typ

(** A value definition consists of a list of explicit universal
    quantifiers, a pattern, and an expression. *)
and value_definition =
    position * tname list * pattern * expression

and pattern =
  | PZero of position
  | PVar of position * name
  | PWildcard of position 
  | PAlias of position * name * pattern
  | PTypeConstraint of position * pattern * shape
  | PPrimitive of position * primitive
  | PData of position * tname list * dname * pattern list
  | PAnd of position * pattern list
  | POr of position * pattern list

and kind = 
  | KStar
  | KTimes of kind * kind
  | KArrow of kind * kind
  | KEmptyRow 

and typ =
  | TypVar of position * tname
  | TypApp of position * typ * typ list 
  | TypRowCons of position * (tname * typ) list * typ
  | TypRowUniform of position * typ

and shape = 
    tname list * typ

