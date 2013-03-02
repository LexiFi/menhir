(* $Id: coreInferenceInternals.ml 53 2007-10-01 14:48:22Z yann.regisgianas $ *)

(* Pangolin, a functional programming language for correct programs.
   Copyright (C) 2007, Yann Régis-Gianas, François Pottier.
   Contact: yann.regisgianas@gmail.com

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

(** This module implements an internalization of term types and
    formula types of the module [CoreSyntax]. It also implements
    an externalization to the raw syntax of the [XCoreSyntax] 
    module. *)

(** The syntax of the surface language into which type annotations
    are written. *)

module I = CoreSyntax

(** The syntax of the inferred types is taken in the explicit 
    language, raw version. *)

module X = XCoreSyntax.Raw

(** The internal representation is implemented by one unification variable. 
    Such a variable is an entry point into the graph induced by the
    multi-equations, the answers to the constraint solving process. *)

type v = MultiEquation.variable
type t = MultiEquation.crterm

(** {1 Internalize} 
    
    We want to internalize term types and formula types into a 
    common formalism in order to propagate typing constraint 
    directly from the logical world to the computational world
    and conversely. 

    Indeed, in the theory, we have a lift operator that should be 
    inserted at each injection of values into the formulae. The 
    problem is the existence of a lifting operator at the level
    of types which is not expressible in terms of constraints. 

    Thus, we ignore the lift operator totally and postpone its 
    introduction to the type checking step. As mentionned
    in [CoreSyntax], the term types are a subset of formula types
    which simplifies a lot the treatment of internalization and 
    externalization. The shortcut removes some expressiveness: a 
    function at the logical level cannot be treated as a pair. This
    limitation is not of the biggest importance from the programmer's
    point of view. *)

(** First, we need a way to add some constants into our 
    algebra. *)

let fresh k name = 
  MultiEquation.variable k ~name:(MultiEquation.TName name) ()
    
let fresh_constant name =
  let name = PIdentifier.mk_type_id (Position.dummy, name) in
    fresh MultiEquation.Constant name

let fresh_from_atom k a = 
  fresh k (PIdentifier.fresh (I.Var.Atom.basename a))

(** There is a constant type variable for each primitive construction. *)

let internal_prop_constant     = fresh_constant "prop"
let internal_product_constant  = fresh_constant "*"
let internal_arrow_constant    = fresh_constant "->"
let internal_larrow_constant   = fresh_constant "-->"
let internal_int_constant      = fresh_constant "int"
let internal_bool_constant     = fresh_constant "bool"
let internal_unit_constant     = fresh_constant "unit"

let internal_primitive_constants = [
  internal_int_constant,  X.TInt;
  internal_bool_constant, X.TBool;
  internal_unit_constant, X.TUnit
]

let internal_formula_primitive_constants = [
  internal_prop_constant,  X.FTProp;
]

let internal_primitive_constants_variables = 
  fst (List.split internal_primitive_constants) 
  @ [ internal_prop_constant;
      internal_product_constant;
      internal_arrow_constant;
      internal_larrow_constant
    ]

exception NotPrimitive

let as_primitive v = 
    try 
      snd (List.find (fun (v', p) -> MultiEquation.are_equivalent v v') 
	     internal_primitive_constants)
    with Not_found -> assert false

let as_formula_primitive v = 
    try 
      snd (List.find (fun (v', p) -> MultiEquation.are_equivalent v v') 
	     internal_formula_primitive_constants)
    with Not_found -> X.FTPrimitive (as_primitive v)
  
let is_arrow v = MultiEquation.are_equivalent v internal_arrow_constant 
let is_larrow v = MultiEquation.are_equivalent v internal_larrow_constant 
let is_prod v  = MultiEquation.are_equivalent v internal_product_constant 

(** For the type variables that are introduced all along the source,
    we use an environment which associates one unification variable
    to each atom. Depending on the quantifier that has introduced
    the type variable, we generate a flexible or a rigid variable. *)

type env = (I.Var.Atom.t * MultiEquation.variable) list

let empty_env = []

let introduce_type_variable k name id env = 
  (name, fresh k id) :: env
    
let lookup_type_variable x env = 
  try 
    snd (List.find (fun (y, v) -> I.Var.Atom.equal x y) env)
  with Not_found -> 
    (* This case is impossible thanks to \alpha-Caml. *)
    assert false

(** [internalize_app_type t [a1; ...; an] generates a n-ary application
    of the [a_k]s to [t]. *)

let internalize_app_type symbol args = 
  let symbol = CoreAlgebra.TVariable symbol in
    List.fold_left (fun acu x -> 
	      CoreAlgebra.TTerm (CoreAlgebra.App (acu, x))) symbol args

let rec unapp = function
  | CoreAlgebra.Var x -> 
      (match MultiEquation.variable_structure x with
	 | Some t ->
	     unapp t 
	 | None ->
	     (x, []))

  | CoreAlgebra.App (f, x) ->
      (match MultiEquation.variable_structure f with
	 | Some t ->
	     let (v, ts) = unapp t in
	       (v, ts @ [ x ])
	 | None ->
	     (f, [ x ]))

  | _ -> assert false

(** Some shortcuts to internalize usual type constructions. *)

let mk_binop op ty1 ty2 =
  internalize_app_type op [ty1; ty2]

let mk_arrow ty1 ty2 = 
  mk_binop internal_arrow_constant ty1 ty2

let mk_larrow ty1 ty2 = 
  mk_binop internal_larrow_constant ty1 ty2

let mk_prod tys = 
  match tys with
    | [] -> CoreAlgebra.TVariable internal_unit_constant
    | [ ty ] -> ty
    | tys -> internalize_app_type internal_product_constant tys

let mk_n_arrow tys ty =
  mk_arrow (mk_prod tys) ty

let mk_n_larrow tys ty =
  mk_larrow (mk_prod tys) ty

(** Internalize a primitive type. *)

let internalize_primitive_type = function
  | I.TInt ->
      CoreAlgebra.TVariable (internal_int_constant)
	
  | I.TUnit ->
      CoreAlgebra.TVariable (internal_unit_constant)
	
  | I.TBool ->
      CoreAlgebra.TVariable (internal_bool_constant)

(** Internalize a term type. *)

let rec internalize_term_type env = function
  | I.TVar x -> 
      CoreAlgebra.TVariable (lookup_type_variable x env)

  | I.TPrimitive p ->
      internalize_primitive_type p

  | I.TArrow (ty1, ty2) ->
      mk_arrow (internalize_term_type env ty1) (internalize_term_type env ty2)

  | I.TProd tys ->
      mk_prod (List.map (internalize_term_type env) tys)

  | I.TApp (tycon, tys) ->
      let tycon = lookup_type_variable tycon env in
	internalize_app_type tycon (List.map (internalize_term_type env) tys)

(** Internalize a formula type. *)

let rec internalize_formula_type env = function

  | I.FTProp -> 
      CoreAlgebra.TVariable internal_prop_constant

  | I.FTVar x -> 
      CoreAlgebra.TVariable (lookup_type_variable x env)

  | I.FTArrow (ty1, ty2) ->
      mk_larrow 
	(internalize_formula_type env ty1) 
	(internalize_formula_type env ty2)  

  | I.FTCArrow (ty1, ty2) ->
      mk_arrow 
	(internalize_formula_type env ty1) 
	(internalize_formula_type env ty2)  

  | I.FTProd tys ->      
      mk_prod (List.map (internalize_formula_type env) tys)

  | I.FTApp (tycon, tys) ->
      let tycon = lookup_type_variable tycon env in
	internalize_app_type tycon (List.map (internalize_formula_type env) tys)

  | I.FTPrimitive p ->
      internalize_primitive_type p

(** {1 Externalisation} 

    Given an entry into the graph induced by the multi-equations, we
    produce a term type or a formula type in the explicit raw syntax.
    
    Sometimes, we want to produce a type scheme or a formula type scheme. 
    To know exactly what type variables are generalized AND bounded in
    the caller's context, the user provides a rank. *)

(** [name_from_int i] turns the integer [i] into a type variable name. *)

let rec name_from_int i : string =
  if i < 26 then
    String.make 1 (Char.chr (0x61 + i))
  else
    String.concat "" [ name_from_int (i / 26 - 1) ; name_from_int (i mod 26) ]

(** [gi] is the last consumed number. *)
let gi =
   ref (-1)

(** [ghistory] is a mapping from variables to variable names. *)
let ghistory =
  ref []

let lhistory = 
  ref []

(** [reset()] clears the global namespace, which is implemented
   by [gi] and [ghistory]. *)
let reset () =
  gi := -1;
  ghistory := []; 
  lhistory := []

let local_var is_local n =
  if is_local && not (List.mem n !lhistory) then (
    lhistory := n :: !lhistory
  )

let new_name is_local x = 
  try 
    snd (List.find (fun (y, t) -> UnionFind.equivalent x y) !ghistory)
  with Not_found ->
    let n = name_from_int (incr gi; !gi) in
    let n = PIdentifier.mk_type_id (Position.dummy, n) in
      ghistory := (x, n) :: !ghistory;
      local_var is_local n;
      n

let rec type_scheme k v : X.type_scheme = 
  reset ();
  let t = var k v in
    X.TScheme (List.rev !lhistory, t)
      
and rigid_var desc k v =
  match MultiEquation.variable_name v with
    | Some (MultiEquation.TName f) ->
	if desc.MultiEquation.kind = MultiEquation.Constant then
	  try 
	    X.TPrimitive (as_primitive v)
	  with _ -> X.TVar f
	else
	  (match IntRank.as_generalized_rank desc.MultiEquation.rank with
	     | Some k' ->
		 X.TVar (new_name (k' > k) v)
	     | None -> 
		 X.TVar f)

    | None ->
	if desc.MultiEquation.kind = MultiEquation.Constant then
	  X.TPrimitive (as_primitive v)
	else
	  (match IntRank.as_generalized_rank desc.MultiEquation.rank with
	     | Some k' ->
		 X.TVar (new_name (k' > k) v)
	     | None -> assert false)

and flexible_var desc k v = 
  match IntRank.as_generalized_rank desc.MultiEquation.rank with
    | Some k' ->
	X.TVar (new_name (k' > k) v)

    | None -> 
	X.TVar (new_name true v)

and var k v =
  let desc = UnionFind.find v in
  match MultiEquation.variable_structure v with
    | None -> 
	if MultiEquation.is_rigid v then 
	  rigid_var desc k v 
	else 
	  flexible_var desc k v
	  
    | Some t -> 
	term k t 
	  
and term k = function
  | CoreAlgebra.Var v -> 
      var k v

  | t -> 
      let (tycon, ts) = unapp t in
	if is_arrow tycon then
	  match ts with
	    | [ ty1; ty2 ] -> X.TArrow (var k ty1, var k ty2)
	    | _ -> assert false
	else if is_prod tycon then
	  X.TProd (List.map (var k) ts)
	else 
	  let xtycon = 
	    match var k tycon with X.TVar x -> x | _ -> assert false
	  in
	    X.TApp (xtycon, List.map (var k) ts)

let term_type' k t = 
  term k (MultiEquation.explode t)

let term_type k t = 
  term k (MultiEquation.explode (CoreAlgebra.TVariable t))

let rec formula_type_scheme k v = 
  lhistory := [];
  let t = var k v in
    X.FTScheme (List.rev !lhistory, t)

and rigid_var desc k v =
  match MultiEquation.variable_name v with
    | Some (MultiEquation.TName f) ->
	if desc.MultiEquation.kind = MultiEquation.Constant then
	  try 
	    as_formula_primitive v
	  with _ -> X.FTVar f
	else
	  (match IntRank.as_generalized_rank desc.MultiEquation.rank with
	     | Some k' ->
		 X.FTVar (new_name (k' > k) v)
	     | None -> 
		 X.FTVar f)

    | None ->
	if desc.MultiEquation.kind = MultiEquation.Constant then
	  as_formula_primitive v
	else
	  (match IntRank.as_generalized_rank desc.MultiEquation.rank with
	     | Some k' ->
		 X.FTVar (new_name (k' > k) v)
	     | None -> assert false)
      
and flexible_var desc k v = 
  match IntRank.as_generalized_rank desc.MultiEquation.rank with
    | Some k' ->
	X.FTVar (new_name (k' > k) v)

    | None -> 
	X.FTVar (new_name false v)

and var k v =
  let desc = UnionFind.find v in
  match MultiEquation.variable_structure v with
    | None -> 
	if MultiEquation.is_rigid v then 
	  rigid_var desc k v 
	else 
	  flexible_var desc k v
	  
    | Some t -> 
	term k t 
	  
and term k = function
  | CoreAlgebra.Var v -> 
      var k v

  | t -> 
      let (tycon, ts) = unapp t in
	if is_arrow tycon then
	  match ts with
	    | [ ty1; ty2 ] -> X.FTCArrow (var k ty1, var k ty2)
	    | _ -> assert false

	else if is_larrow tycon then
	  match ts with
	    | [ ty1; ty2 ] -> X.FTArrow (var k ty1, var k ty2)
	    | _ -> assert false

	else if is_prod tycon then
	  X.FTProd (List.map (var k) ts)
	else 
	  let xtycon = 
	    match var k tycon with X.FTVar x -> x | _ -> assert false
	  in
	    X.FTApp (xtycon, List.map (var k) ts)

let formula_type k t = 
  term k (MultiEquation.explode (CoreAlgebra.TVariable t))

let formula_type' k t = 
  term k (MultiEquation.explode t)

