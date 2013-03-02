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

(* $Id: miniTypes.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module transforms types from the user's syntax to the
    internal representation of the inference engine. *)

open Positions
open Misc
open MiniKindInferencer
open Constraint
open MiniAlgebra
open CoreAlgebra
open MultiEquation
open MiniTypingEnvironment
open MiniTypingExceptions
open MiniAst

let rec extract_type = function
    
  | ETypeConstraint (_, e, typ) ->
      typ, e

  | ELambda (pos, PTypeConstraint (pos', p, typ1), e2) ->
      let typ2, e2 = extract_type e2 in
	TypApp (pos', TypVar (pos', TName "->"), [typ1; typ2]), 
	ELambda (pos, p, e2)

  | _ ->
      raise Not_found

type recursive_value_definition_kind =
  | Implicit of name * expression
  | Explicit of name * typ * expression
  | NotPVar
      
(** [explicit_or_implicit] examines a value definition and determines whether
    it carries an explicit type annotation. It optionally checks that the 
    left-hand side is a variable. *)
let rec explicit_or_implicit p e =
  match p with
    | PTypeConstraint (pos, p, typ) ->
	explicit_or_implicit p (ETypeConstraint (pos, e, typ))
	  
    | PVar (_, name) -> (
	try
	  let typ, e = extract_type e in
	    Explicit (name, typ, e)
	with Not_found ->
	  Implicit (name, e)
      )

    | _ -> NotPVar
	
(** {2 From user's syntax to internal term representation} *)
	
let variables_of_typ = 
  let rec vtyp acu = function
    | TypVar (_, TName x) -> 
	StringSet.add x acu

    | TypApp (_, t, ts) -> 
	List.fold_left vtyp (vtyp acu t) ts

    | TypRowCons (_, attributes, t) -> 
	List.fold_left vtyp (vtyp acu t) (assoc_proj2 attributes)

    | TypRowUniform (_, x) -> 
	vtyp acu x
  in
    vtyp StringSet.empty 

let arrow tenv = 
  arrow (typcon_variable tenv)

let rec type_of_args t =
  let rec chop acu = function
    | TypApp (_, TypVar (_, TName "->"), [ t1; t2 ]) -> 
	chop (t1 :: acu) t2

    | t -> 
	acu 
  in List.rev (chop [] t)

let arity t =
  List.length (type_of_args t)
       
let tycon tenv t =
  app (lookup_type_variable tenv t) 
    
let rec intern' pos tenv ty : crterm = 
  match ty with
      
    | TypVar (pos, name) -> 
	as_fun tenv name

    | TypApp (pos, t, typs) ->
	let iargs = List.map (intern' pos tenv) typs in
	  app (intern' pos tenv t) iargs 

    | TypRowCons (pos, attributes, t) ->
	let typed_labels = 
	  List.map (fun (l, t) -> l, intern' pos tenv t) attributes
	in
	  n_rowcons typed_labels (intern' pos tenv t)

    | TypRowUniform (pos, t) ->
	uniform (intern' pos tenv t)

(** [intern tenv typ] converts the type expression [typ] to a type.
    The environment [tenv] maps type identifiers to types. *)
let rec intern pos tenv ty = 
  let kind_env = as_kind_env tenv in
  let _ = MiniKindInferencer.check pos kind_env ty star in 
    intern' pos tenv ty 

let intern_let_env pos tenv rs fs = 
  let fqs, rtenv = fresh_flexible_vars pos tenv fs in
  let rqs, rtenv' = fresh_rigid_vars pos tenv rs in
    rqs, fqs, add_type_variables (rtenv @ rtenv') tenv 

(** [intern_scheme tenv name qs typ] produces a type scheme
    that binds [name] to [forall qs.typ]. *)
let intern_scheme pos tenv name qs typ =
  let fqs, rtenv = fresh_flexible_vars pos tenv qs in
    Scheme (pos, [], fqs, CTrue pos, 
	    StringMap.singleton name 
	      ((intern pos (add_type_variables rtenv tenv) typ),
	       pos))


