(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/syntax.ml,v 1.29 2002/05/29 16:22:06 fpottier Exp $ *)

(* This module defines the abstract syntax produced by the parser. *)

open Id
open KindUnification

type channel = (* channels decorate type abstractions and applications *)
    string

type formals =
    (channel * id * kind) list

type actuals =
    (channel * typ) list

and typ =
  | TypVar of id
  | TypApp of typ * channel * typ
  | TypAbs of channel * id * kind * typ
  | TypArrow of typ * typ
  | TypRecord of typ
  | TypRowCons of (id * actuals option) * typ
  | TypRowEmpty
  | TypTuple of typ list
  | TypInteger

type quantifiers =
    (id * kind) list

type scheme =
  quantifiers * typ

type attribute =
  | Binds                                           (* binds a variable *)
  | BindsAndDefines                                 (* binds a variable and defines a record field *)

type pattern =
  | PWildcard
  | PVar of attribute * id
  | PAlias of attribute * id * pattern
  | PRecord of (id * pattern) list * pattern
  | PRecordEmpty
  | PTuple of pattern list
  | PData of id * pattern list
  | PInteger of int
  | PTypeConstraint of pattern * typ

type value_definition =
    quantifiers * pattern * expression

and type_definition =
    id * typ

and binding =
  | BindValue of value_definition list              (* polymorphic, ML-style generic [let] binding *)
  | BindRecValue of value_definition list           (* mutually recursive [let] binding *)
  | BindType of type_definition list                (* local type abbreviation *)
  | BindImport of attribute * id list * expression  (* defines a number of identifiers by reading them from a record *)

and expression =
  | EVar of id
  | EAbs of pattern * expression
  | EApp of expression * expression
  | EBinding of binding * expression
  | EMatch of expression * (pattern * expression) list
  | ERecordEmpty
  | ERecordAccess of id * expression
  | ERecordExtend of binding list * expression
  | ERecordUpdate of expression * id * expression
  | ETuple of expression list
  | EDisplay of expression
  | EInteger of int
  | EData of id
  | EExists of quantifiers * expression
  | ETypeConstraint of expression * typ

type case =                                    (* one branch of a concrete type definition: *)
    id * typ list                              (* data constructor name and arguments *)

type declaration =
  | DeclField of id * bool * formals * scheme  (* record field declaration; Boolean flag is [mutable] *)
  | DeclData of id * formals * case list       (* concrete type definition *)

type program =
  declaration list * expression

