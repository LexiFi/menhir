(* $Id$ *)

(** This module handles types from the user's syntax to the
    internal representation of the inference engine.
*)

open Positions
open Misc
open Sig
open MiniKindInferencer
open MiniConstraint
open MiniAlgebra
open MiniMultiEquation
open MiniTypingEnvironment
open MiniTypingExceptions
open MiniAst

(** [extract_type] examines an expression and looks for a sufficiently 
    explicit type annotation. If it finds one, it returns the type annotation, 
    together with the expression (deprived of its annotation). 
    Otherwise, it raises  [Not_found]. *)
let rec extract_type = function
    
  | ETypeConstraint (_, e, (vs, typ)) ->
      vs, typ, e

  | ECoerce (_, e, vs, _, typ) ->
      vs, typ, e
	
  | ELambda (pos, PTypeConstraint (pos', p, (vs1, typ1)), e2) ->
      let vs2, typ2, e2 = extract_type e2 in
	(vs1 @ vs2), 
	TypApp (pos', TypVar (pos', "->"), [typ1; typ2]), 
	ELambda (pos, p, e2)
	
  | _ ->
      raise Not_found

type recursive_value_definition_kind =
  | Implicit of name * expression
  | Explicit of name * tname list * typ * expression
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
	  let vs, typ, e = extract_type e in
	    Explicit (name, vs, typ, e)
	with Not_found ->
	  Implicit (name, e)
      )

    | _ -> NotPVar
	
(** {2 From user's syntax to internal term representation} *)
	
let tycon_arity env t = 
  kind_arity (as_kind_env env) (typcon_kind env t)

let rec typ_fold f init = function
  | TypVar (_, x) -> f init x
  | TypApp (_, t1, t2) -> List.fold_left (typ_fold f) (typ_fold f init t1) t2
  | TypRowCons (_, attributes, t) ->
      List.fold_left (typ_fold f) (typ_fold f init t) (assoc_proj2 attributes)
  | TypRowUniform (_, x) ->
      typ_fold f init x

let rec typ_map f t = 
  match t with
  | TypVar (pos, x) -> 
      TypVar (pos, x)
  | TypApp (pos, t1, ts) -> 
      TypApp (pos, typ_map f (f t1), List.map (fun t -> typ_map f (f t)) ts)
  | TypRowCons (pos, attributes, tn) ->
      TypRowCons (pos, List.map (fun (n, t) -> (n, typ_map f (f t))) attributes, 
		  typ_map f (f tn))
  | TypRowUniform (pos, x) ->
      TypRowUniform (pos, typ_map f (f x))

let variables_of_typ = 
  let rec vtyp acu = function
    | TypVar (_, x) -> StringSet.add x acu
    | TypApp (_, t, ts) -> List.fold_left vtyp (vtyp acu t) ts
    | TypRowCons (_, attributes, t) -> 
	List.fold_left vtyp (vtyp acu t) (assoc_proj2 attributes)
    | TypRowUniform (_, x) -> 
	vtyp acu x
  in
    vtyp StringSet.empty 

let arrow tenv = 
  arrow (typcon_variable tenv)

let rec type_of_result t = 
  match t with
    | TypApp (_, TypVar (_, "->"), [ t1; t2]) -> t2
    |  t -> t
	 
let rec type_of_last_result t = 
  match t with
    | TypApp (_, TypVar (_, "->"), [ t1; t2 ]) -> type_of_last_result t2
    |  t -> t

let rec type_of_args t =
  let rec chop acu = function
    | TypApp (_, TypVar (_, "->"), [ t1; t2 ]) -> chop (t1 :: acu) t2
    | t -> acu 
  in List.rev (chop [] t)
       
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
  let kind = KindInferencer.check pos kind_env ty (mkstar kind_env) in 
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


let rec typ_fold2 f init t t' = 
  let fold = typ_fold2 f in

    match t, t' with

	TypVar (_, x), TypVar (_, y) -> 
	  f init t t'

      | TypApp (_, t1, t2), TypApp (_, t1', t2') -> 
	  assert (List.length t2 = List.length t2');
	  List.fold_left2 fold (fold init t1 t1') t2 t2'

      | TypRowCons (_, ls, t), TypRowCons (_, ls', t') ->
	  assert false

      | TypRowUniform (_, t), TypRowUniform (_, t') -> 
	  assert false

      | _ -> 
	  f init t t'

let rec typ_map2 f t t' = 
  let upos = Positions.undefined_position 
  and map = typ_map2 f in

  match t, t' with

	TypVar (_, x), TypVar (_, y) -> 
	  f t t'

      | TypApp (_, t1, t2), TypApp (_, t1', t2') -> 
	  assert (List.length t2 = List.length t2');
	  TypApp (upos, map t1 t1', List.map2 map t2 t2')

      | TypRowCons (_, ls, t), TypRowCons (_, ls', t') ->
	  assert false

      | TypRowUniform (_, t), TypRowUniform (_, t') -> 
	  assert false

      | _ -> 
	  f t t'

let rec ( |=| ) t1 t2 = 
  try 
    typ_fold2 (fun f t1 t2 ->
		 match t1, t2 with
		     TypVar (_, v), TypVar (_, v') -> f && v = v'
		   | _ -> false)
      true t1 t2
  with _ -> false

