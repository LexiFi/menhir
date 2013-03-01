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

(* $Id: miniAst.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** The abstract syntax of programs *)

open Positions

type program = 
    binding list

and binding =
  | BindValue of position * value_definition list
  | BindRecValue of position * value_definition list
  | TypeDec of position * type_declaration list

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
  | ETypeConstraint of position * expression * typ
  
  (** Algebraic datatypes. *) 
  | EDCon of position * dname * expression list
  | EMatch of position * expression * (clause list)

  (** Records. *)
  | ERecordEmpty of position
  | ERecordAccess of position * expression * lname
  | ERecordExtend of position * record_binding list * expression
  | ERecordUpdate of position * expression * lname * expression

  (** Misc. *)
  | EAssertFalse of position

(** Program identifiers. *)
and name = Constraint.sname =
  | SName of string
      
(** Type variable names. *)
and tname =
    MultiEquation.tname

(** Data constructors. *)
and dname =
    DName of string

(** Record labels. *)
and lname =
    CoreAlgebra.lname
  
(** Constant. *)
and primitive =
  | PIntegerConstant of int	(** Integer constant. *)
  | PCharConstant of char	(** Character constant. *)
  | PUnit			(** Unit constant. *)

(** Pattern matching clause. *)
and clause =
    position * pattern * expression

and record_binding =
    lname * expression

and type_declaration =
    position * kind * tname * type_definition

and type_definition =
  | DAlgebraic of (position * dname * tname list * typ) list

(** A value definition consists of a list of explicit universal
    quantifiers, a pattern, and an expression. *)
and value_definition =
    position * tname list * pattern * expression

and pattern =
  | PVar of position * name
  | PWildcard of position 
  | PAlias of position * name * pattern
  | PTypeConstraint of position * pattern * typ
  | PPrimitive of position * primitive
  | PData of position * dname * pattern list
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
  | TypRowCons of position * (lname * typ) list * typ
  | TypRowUniform of position * typ


