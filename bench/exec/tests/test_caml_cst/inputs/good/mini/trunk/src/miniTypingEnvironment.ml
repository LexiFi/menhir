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

(* $Id: miniTypingEnvironment.ml 421 2006-12-22 09:27:42Z regisgia $ *)


(** This module implements a typing environment useful for type 
    inference. *)

open Positions
open Misc
  
module TypingExceptions = MiniTypingExceptions
module Constraint = Constraint
module KindInferencer = MiniKindInferencer
module Algebra = MiniAlgebra
open KindInferencer
open Constraint
open Algebra
open CoreAlgebra
open MiniAst
open TypingExceptions
open MultiEquation

(** {2 Typing environment} *)

(* Use a basic implementation. *)
open Env

(** [typing_info] denotes information collected during the user-defined
    type constructor analysis. *)

(* The following information is stored for each type constructor:
   - its kind ;
   - its associated term (a type variable actually) ;
   - if it is an algebraic datatype, the list of its datatype 
   constructors. *)
type algebraic_datatype = 
    (dname * variable) list

type type_info = 
    KindInferencer.t * variable * algebraic_datatype option ref
      
let as_type_constructor ((_, v, _) as x) = 
  if (UnionFind.find v).kind = Constant then
    x
  else
    raise Not_found

let as_type_variable (_, v, _) = 
  v

(* The following information is stored for each datatype constructor:
   - its type variables ;
   - its arity ;
   - its type ; *)
type data_constructor = int * variable list * crterm

(** [environment] denotes typing information associated to identifiers. *)
type environment = 
    {
      type_info        : (tname, type_info) Env.t;
      data_constructor : (dname, data_constructor) Env.t;
    }

let empty_environment = 
  {
    type_info        = Env.empty;
    data_constructor = Env.empty
  }

let union_type_variables env1 env2 =
  { env1 with type_info = Env.concat env1.type_info env2.type_info }
    
let add_type_variable env t (k, v) = 
  { env with type_info = Env.add env.type_info t (k, v, ref None) }

let add_type_variables var_env env = 
  { env with type_info = 
      List.fold_left (fun env (x, k) -> Env.add env x k) env.type_info var_env }

let add_type_constructor env t x = 
  { env with type_info = Env.add env.type_info t x }

let add_data_constructor env t x = 
  { env with data_constructor = Env.add env.data_constructor t x }

(** [lookup_typcon ?pos env t] retrieves typing information about
    the type constructor [t]. *)
let lookup_typcon ?pos env t =
  try 
    as_type_constructor (Env.lookup env.type_info t)
  with Not_found -> 
    raise (UnboundTypeIdentifier ((pos_or_undef pos), t))

(** [find_typcon env t] looks for typing information related to
    the type constructor [t] in [env]. *)
let find_typcon env t =
  just_try (fun () -> as_type_constructor (Env.lookup env.type_info t))

(** [lookup_type_variable env v] looks for typing information related to
    the type variable [v] in [env]. *)
let lookup_type_variable ?pos env k = 
  try
    TVariable (as_type_variable (Env.lookup env.type_info k))
  with Not_found -> 
    raise (UnboundTypeVariable ((pos_or_undef pos), k))

(* The kind inferencer wants a view on the environment that
   concerns only the kinds. *)
let as_kind_env env = 
  let env = ref env in
  let read id = 
    try 
      match Env.lookup (!env).type_info id with
	| (k, _, _) -> k
    with Not_found -> 
      raise (UnboundTypeConstructor (undefined_position, id))
  in
  let update i k = 
    env := add_type_variable (!env) i (k, variable Flexible ())
  in
    (read : tname -> KindInferencer.t), 
  (update : tname -> KindInferencer.t -> unit)

let fold_type_info f init env = 
  Env.fold_left f init env.type_info

(* Some accessors. *)
let typcon_kind env t = 
  proj1_3 (lookup_typcon env t)

let typcon_variable env t = 
  TVariable (proj2_3 (lookup_typcon env t))

let as_fun tenv name = 
  match find_typcon tenv name with
    | None -> lookup_type_variable tenv name
    | Some (_, v, _) -> TVariable v

let as_env env varlist = 
  List.fold_left 
    (fun env (n, v) -> add_type_variable env n (fresh_kind (), v))
    empty_environment varlist

(** [is_typcon env t] check if there exists a type constructor whose
    name is [t]. *)
let is_typcon env t = 
  (find_typcon env t) <> None
    
(** [filter_tycon_name tenv names] checks that [names] does not contain
    a name already used by a type constructor. *)
let filter_tycon_name tenv =
  List.filter (notf (function v -> match variable_name v with
			 None -> false 
		       | Some name -> is_typcon tenv name))

let add_type_and_kind_variables denv tenv =
  add_type_variables 
    (List.map (fun (n, v) -> (n, (fresh_kind (), v, ref None))) denv)
    tenv

(** [tycon_name_conflict tyconv_env env] checks if a type constructor is not
    overwritten by a type variable. *)
let tycon_name_conflict pos env (fqs, denv) =
  try
    let (n, _) = List.find (fun (x, _) -> is_typcon env x) denv in
      raise (InvalidTypeVariableIdentifier (pos, n))
  with Not_found ->
    (fqs, List.map (function (n, TVariable v) -> (n, v) | _ -> assert false) denv)


(** [lookup_datacon env k] looks for typing information related to
    the data constructor [k] in [env]. *)
let lookup_datacon ?pos env k = 
  try
    Env.lookup env.data_constructor k
  with Not_found -> 
    raise (UnboundDataConstructor ((pos_or_undef pos), k))

let rigid_args rt = 
  List.fold_left (fun acu -> 
	    function 
		TVariable v -> 
		  if (UnionFind.find v).kind = Rigid then v :: acu
		  else acu
	      | _ -> acu) []
    (tycon_args rt)


let fresh_datacon_scheme pos tenv k =
  let (_, kvars, kt) = lookup_datacon tenv k in 
  let fresh_kvars =
    let mkvar ?name v = variable Flexible ?name () in
      List.map mkvar kvars
  in
  let fresh_kvars_assoc = List.combine kvars fresh_kvars 
  in
    (fresh_kvars, change_arterm_vars fresh_kvars_assoc kt)

let is_regular_datacon_scheme tenv kvars kt =
  let rt = result_type (as_fun tenv) kt in
  let rigid_args = rigid_args rt in
    (* Check that all the tycon arguments are distinct rigid variables. *)
    List.for_all (fun v -> List.memq v kvars) rigid_args 
    && List.length rigid_args == List.length kvars

(** [find_algebraic_datatypes env k] looks for all the data
    constructor that are related to the data constructor [k]. *)
let rec find_algebraic_datatypes env k = 
  let ts = Env.filter env.type_info
    (fun (_, _, r) -> match !r with
	 Some l -> List.mem k l
       | _ -> false)
  in
    match ts with
	[ (_,_, r) ] -> unSome (!r)
      | _ -> assert false

(** [fresh_vars kind pos env vars] allocates fresh variables from a
    list of names [vars], checking name clashes with type constructors. *)
let fresh_vars kind pos env vars =
  let vs = variable_list_from_names (fun v -> (kind, Some v)) vars in
  let (fqs, denv) = tycon_name_conflict pos env vs in
    (fqs, 
     (List.map (fun (n, v) -> (n, (fresh_kind (), v, ref None))) denv))

(** [fresh_flexible_vars] is a specialized allocator for flexible
    variables. *)
let fresh_flexible_vars = 
  fresh_vars Flexible

(** [fresh_rigid_vars] is a specialized allocator for rigid variables. *)
let fresh_rigid_vars = 
  fresh_vars Rigid

let fresh_unnamed_rigid_vars pos env vars = 
  let rqs, denv = variable_list Rigid vars in
    rqs, 
  List.map (function (n, TVariable v) -> (n, (fresh_kind (), v, ref None))
	    | _ -> assert false)
    denv
    
