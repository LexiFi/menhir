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

(* $Id: miniInfer.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module implements type inference. *)

open Positions
open Misc
open MiniKindInferencer
open Constraint
open MiniAlgebra
open CoreAlgebra
open MultiEquation
open MiniTypingEnvironment
open MiniTypingExceptions
open MiniTypes
open MiniAst

(** {2 Inference} *)

(** A fragment denotes the typing information acquired in a match branch. 
    [gamma] is the typing environment coming from the binding of pattern
    variables. [vars] are the fresh variables introduced to type the
    pattern. [tconstraint] is the constraint coming from the instantiation
    of the data constructor scheme. *)
type fragment = 
    {
      gamma       : (crterm * position) StringMap.t;
      vars        : variable list;
      tconstraint : tconstraint;
    }

(** The [empty_fragment] is used when nothing has been bound. *)
let empty_fragment = 
  {
    gamma       = StringMap.empty;
    vars        = [];
    tconstraint = CTrue undefined_position;
  }

(** Joining two fragments is straightforward except that the environments 
    must be disjoint (a pattern cannot bound a variable several times). *)    
let rec join_fragment pos f1 f2 = 
  {
    gamma = 
      (try 
	StringMap.strict_union f1.gamma f2.gamma
      with StringMap.Strict x -> raise (NonLinearPattern (pos, SName x)));
    vars        = f1.vars @ f2.vars;
    tconstraint = f1.tconstraint ^ f2.tconstraint;
  }
	      
(** [infer_pat_fragment p t] generates a fragment that represents the 
    information gained by a success when matching p. *)
and infer_pat_fragment tenv p t =
  let join pos = List.fold_left (join_fragment pos) empty_fragment in
  let rec infpat t = function

    (** Wildcard pattern does not generate any fragment. *)
    | PWildcard pos ->
	empty_fragment

    (** We refer to the algebra to know the type of a primitive. *)
    | PPrimitive (pos, p) ->
	{ empty_fragment with 
	    tconstraint = (t =?= type_of_primitive (as_fun tenv) p) pos
	}
	  
    (** Matching against a variable generates a fresh flexible variable,
	binds it to the [name] and forces the variable to be equal to [t]. *)
    | PVar (pos, SName name) ->
	let v = variable Flexible () in
	  { 
	    gamma       = StringMap.singleton name (TVariable v, pos);
	    tconstraint = (TVariable v =?= t) pos;
	    vars        = [ v ]
	  }

    (** A disjunction forces the bounded variables of the subpatterns to 
	be equal. For that purpose, we extract the types of the subpatterns'
	environments and we make them equal. *)
    | POr (pos, ps) ->
	let fps = List.map (infpat t) ps in
	  (try 
	    let rgamma = (List.hd fps).gamma in
	    let cs = 
	      List.fold_left (fun env_eqc fragment ->
			StringMap.mapi 
			  (fun k (t', _) ->
			     let (t, c) = StringMap.find k env_eqc in
			       (t, (t =?= t') pos ^ c)) 
			  fragment.gamma)
		(StringMap.mapi (fun k (t, _) -> (t, CTrue pos)) rgamma)
	      fps
	    in
	    let c = StringMap.fold (fun k (_, c) acu -> c ^ acu) cs (CTrue pos)
	    in
	      {
		gamma       = rgamma;
		tconstraint = c ^ conj (List.map (fun f -> f.tconstraint) fps);
		vars        = List.flatten (List.map (fun f -> f.vars) fps)
	      }
	  with Not_found -> 
	    raise (InvalidDisjunctionPattern pos))

    (** A conjunction pattern does join its subpatterns' fragments. *)
    | PAnd (pos, ps) ->
	join pos (List.map (infpat t) ps)

    (** [PAlias (x, p)] is equivalent to [PAnd (PVar x, p)]. *)
    | PAlias (pos, SName name, p) ->
	let fragment = infpat t p in 
	  { fragment with 
	      gamma       = StringMap.strict_add name (t, pos) fragment.gamma;
	      tconstraint = (SName name <? t) pos ^ fragment.tconstraint
	  }

    (** A type constraint is taken into account by the insertion of a type
	equality between [t] and the annotation. *)
    | PTypeConstraint (pos, p, typ) ->
	let fragment = infpat t p 
	and ityp = intern pos tenv typ in
	  { fragment with
	      tconstraint = (ityp =?= t) pos ^ fragment.tconstraint
	  }

    (** Matching against a data constructor generates the fragment that:
	- forces [t] to be the type of the constructed value ;
	- constraints the types of the subpatterns to be equal to the arguments
	of the data constructor. *)
    | PData (pos, k, ps) -> 
	let (alphas, kt) = fresh_datacon_scheme pos tenv k in
	let rt = result_type (as_fun tenv) kt
	and ats = arg_types (as_fun tenv) kt in
	  if (List.length ps <> List.length ats) then
	    raise (NotEnoughPatternArgts pos)
	  else 
	    let fragment = join pos (List.map2 infpat ats ps) in
	      { fragment with 
		  tconstraint = fragment.tconstraint ^ (t =?= rt) pos ;
		  vars        = alphas @ fragment.vars;
	      }
  in
    infpat t p
    
(** Constraint contexts. *)
type context =
    (crterm, variable) type_constraint -> (crterm, variable) type_constraint

(** [intern_data_constructor adt_name env_info dcon_info] returns
    env_info augmented with the data constructor's typing information 
    It also checks if its definition is legal. *)
let intern_data_constructor pos (TName adt_name) env_info dcon_info = 
  let (tenv, acu, lrqs, let_env) = env_info
  and (pos, DName dname, qs, typ) = dcon_info in
  let rqs, rtenv = fresh_unnamed_rigid_vars pos tenv qs in
  let tenv' = add_type_variables rtenv tenv in 
  let ityp = intern pos tenv' typ in
  let _ = 
    if not (is_regular_datacon_scheme tenv rqs ityp) then
      raise (InvalidDataConstructorDefinition (pos, DName dname)) 
  in
  let v = variable ~structure:ityp Flexible () in
    ((add_data_constructor tenv (DName dname) (arity typ, rqs, ityp)),
     (DName dname, v) :: acu, 
     (rqs @ lrqs),
     StringMap.add dname (ityp, pos) let_env)
	
(** [infer_vdef pos tenv (pos, qs, p, e)] returns the constraint
    related to a value definition. *)
let rec infer_vdef pos tenv (pos, qs, p, e) =
  let x = variable Flexible () in
  let tx = TVariable x in
  let rqs, rtenv = fresh_rigid_vars pos tenv qs in 
  let tenv' = add_type_variables rtenv tenv in
  let fragment = infer_pat_fragment tenv' p tx in 
    Scheme (pos, rqs, x :: fragment.vars, 
	    fragment.tconstraint ^ infer_expr tenv' e tx,
    fragment.gamma) 

(** [infer_binding tenv b] examines a binding [b], updates the
    typing environment if it binds new types or generates 
    constraints if it binds values. *)
and infer_binding tenv b =
  match b with
      
    | TypeDec (pos, tds) ->
	List.fold_left 
	  (fun (tenv, c) (pos', kind, name, def) -> 
	     (* Insert the type constructor into the environment. *)
	     let ikind = 
	       MiniKindInferencer.intern_kind (as_kind_env tenv) kind 
	     and ids_def = ref None
	     and ivar = variable ~name:name Constant () in
	     let tenv = add_type_constructor tenv name (ikind, ivar, ids_def)
	     and c = fun c' -> 
	       CLet ([Scheme (pos, [ivar], [], c', StringMap.empty)],
		     CTrue pos)
	     in
	       match def with
		   (* Algebraic datatype definition. *)
		 | DAlgebraic ds ->
		     let (tenv, ids, rqs, let_env) = 
		       List.fold_left
			 (intern_data_constructor pos name)
			 (tenv, [], [], StringMap.empty)
			 ds
		     in
		       ids_def := Some ids;
		       (* Insert the data constructors in the constraint
			  context. *)
		       let c = fun c' -> 
			 c (CLet ([Scheme (pos', rqs, [], CTrue pos', 
					   let_env)],
				  c'))
		       in
			 (tenv, c)
	  )
	  (tenv, fun c -> c)
	  tds

    | BindValue (pos, vdefs) ->

	let schemes = List.map (infer_vdef pos tenv) vdefs in
	  tenv, (fun c -> CLet (schemes, c))

    | BindRecValue (pos, vdefs) ->

	(* The constraint context generated for 
	   [let rec forall X1 . x1 : T1 = e1 
	   and forall X2 . x2 = e2] is

	   let forall X1 (x1 : T1) in
	   let forall [X2] Z2 [
	   let x2 : Z2 in [ e2 : Z2 ]
	   ] ( x2 : Z2) in (
	   forall X1.[ e1 : T1 ] ^
	   [...]
	   )

	   In other words, we first assume that x1 has type scheme 
	   forall X1.T1.
	   Then, we typecheck the recursive definition x2 = e2, making sure 
	   that the type variable X2 remains rigid, and generalize its type. 
	   This yields a type scheme for x2, which is then used to check 
	   that e1 actually has type scheme forall X1.T1.

	   In the above example, there are only one explicitly typed and one
	   implicitly typed value definitions.

	   In the general case, there are multiple explicitly and implicitly
	   typed definitions, but the principle remains the same. We generate
	   a context of the form

	   let schemes1 in

	   let forall [rqs2] fqs2 [
	   let h2 in c2
	   ] h2 in (
	   c1 ^
	   [...]
	   )

	*)

	let schemes1, rqs2, fqs2, h2, c2, c1 =
	  List.fold_left 
	    (fun (schemes1, rqs2, fqs2, h2, c2, c1) (pos, qs, p, e) ->
	       (match p with
		   PVar _ -> ()
		 | _ -> raise (RecursiveDefMustBeVariable pos));
	       
	       (* Allocate variables for the quantifiers in the list
		  [qs], augment the type environment accordingly. *)

	       let rvs, rtenv = fresh_rigid_vars pos tenv qs in
	       let tenv' = add_type_variables rtenv tenv in

		 (* Check whether this is an explicitly or implicitly
		    typed definition. *)

		 match explicit_or_implicit p e with
		   | Implicit (SName name, e) ->

		       let v = variable Flexible () in
		       let (t : crterm) = TVariable v in
			 
			 schemes1,
		       rvs @ rqs2,
		       v :: fqs2,
		       StringMap.add name (t, pos) h2,
		       infer_expr tenv' e t ^ c2,
		       c1

		   | Explicit (SName name, typ, e) ->

		       intern_scheme pos tenv name qs typ :: schemes1,
		       rqs2,
		       fqs2,
		       h2,
		       c2,
		       fl rvs (infer_expr tenv' e (intern pos tenv' typ)) 
		       ^ c1

		   | _ -> assert false

	    ) ([], [], [], StringMap.empty, CTrue pos, CTrue pos) vdefs in
	  
	  tenv, 
	fun c -> CLet (schemes1,
		       CLet ([ Scheme (pos, rqs2, fqs2, 
				       CLet ([ monoscheme h2 ], c2), h2) ],
			     c1 ^ c)
		      )

(** [infer_expr tenv d e t] generates a constraint that guarantees that [e]
    has type [t]. It implements the constraint generation rules for
    expressions. It may use [d] as an equation theory to prove coercion 
    correctness. *)
and infer_expr tenv e (t : crterm) =
  match e with
      
    (** The [exists a. e] construction introduces [a] in the typing
	scope so as to be usable in annotations found in [e]. *)
    | EExists (pos, vs, e) ->
	let (fqs, denv) = fresh_flexible_vars pos tenv vs in 
	let tenv = add_type_variables denv tenv in
	  ex fqs (infer_expr tenv e t)

    (** [forall a. e] generates the constraint:
	let forall b [ (( e : b )) ].(_z : b) in _z < t
	which means the [e] must have a type at least as general as
	[t] assuming [a] is generic. *)
    | EForall (pos, vs, e) ->
	let (rqs, denv) = fresh_rigid_vars pos tenv vs in 
	let tenv = add_type_variables denv tenv in
	let beta = variable Flexible () in
	let gt = TVariable beta in
	  CLet ([Scheme (pos, rqs, [beta],
			 infer_expr tenv e gt, 
			 StringMap.singleton "_z" (gt, pos)) ],
		(SName "_z" <? t) pos)

    (** The type of a variable must be at least as general as [t]. *)
    | EVar (pos, name) ->
	(name <? t) pos

    (** To type a lambda abstraction, [t] must be an arrow type. 
	Furthermore, type variables introduce by the lambda pattern 
	cannot be generalized locally. *)
    | ELambda (pos, p, e) ->
        (* Allocate fresh type variables [x1] and [x2]. *)
        exists 
	  (* Allocate a fresh type variable for every variable 
	     defined by [p]. *)
	  (fun x1 -> exists 
	     (fun x2 ->
		let fragment = infer_pat_fragment tenv p x1 in
		  ex fragment.vars (
		    CLet (
		      (* Bind the variables of [p] via 
			 a monomorphic [let] constraint. *)
		      [ monoscheme fragment.gamma ],       
		      (* Require [x1] to be a valid type for [p]. *)
		      fragment.tconstraint
			(* Require [x2] to be a valid type for [e]. *)
		      ^ infer_expr tenv e x2     
		    )
		  ) ^
		    (t =?= arrow tenv x1 x2) pos         
		    (* Require the expected type [t] to be an arrow 
		       of [x1] to [x2]. *)
             )
          )

    (** Application requires the left hand side to be an arrow and 
	the right hand side to be compatible with the domain of this 
	arrow. *)
    | EApp (pos, e1, e2) ->
	exists (fun x ->
	     infer_expr tenv e1 (arrow tenv x t) ^ infer_expr tenv e2 x
	  )
	  
    (** A binding [b] defines a constraint context into which the 
	constraint of [e] must be injected. *)
    | EBinding (_, b, e) ->
	snd (infer_binding tenv b) (infer_expr tenv e t)
	  
    (** A type constraint inserts a type equality into the generated 
	constraint. *)
    | ETypeConstraint (pos, e, typ) ->
	let ityp = intern pos tenv typ in
	  (t =?= ityp) pos ^ infer_expr tenv e ityp

    (** The constraint of a [match] makes equal the type of the scrutinee
	and the type of every branch pattern. The body of each branch must
	be equal to [t]. *)
    | EMatch (pos, e, clauses) ->
	exists (fun x ->
	     infer_expr tenv e x ^
	       conj 
	       (List.map 
		  (fun (pos, p, e) -> 
		     let fragment = infer_pat_fragment tenv p x in
		       CLet ([ Scheme (pos, [], fragment.vars, 
				       fragment.tconstraint, 
				       fragment.gamma) ],
			     infer_expr tenv e t))
		  clauses))

    (** A data constructor application is similar to usual application 
	except that it must be fully applied. *)
    | EDCon (pos, (DName d as k), es) ->
	let arity, _, _ = lookup_datacon tenv k in
	let les = List.length es in
	  if les <> arity then 
	    raise (PartialDataConstructorApplication (pos, arity, les))
	  else 
	    exists_list es 
	      (fun xs -> 
		 let (kt, c) = 
		   List.fold_left (fun (kt, c) (e, x) ->
			     arrow tenv x kt, c ^ infer_expr tenv e x)
		     (t, CTrue pos)
		     (List.rev xs)
		 in
		   c ^ (SName d <? kt) pos)
  
    (** The application of a primitive refers to the algebra to check the 
	types. *)
    | EPrimApp (pos, c, args) -> 
	exists_list args 
	  (fun xs ->
	     let xts = snd (List.split xs) in
	     let ct = List.fold_left (fun acu x -> arrow tenv x acu) t xts in
	       (ct =?= type_of_primitive (as_fun tenv) c) pos ^
		 CConjunction (List.map (curry (infer_expr tenv)) xs))
 
    (** An empty record as type {v \abs v}. *)
    | ERecordEmpty (pos) -> 
	(t =?= uniform (abs (as_fun tenv))) pos
	  
    (** The record definition by extension. 
	We give a type to each bounded label.
	[t] must be a record that assigns a pre type to each label. *)
    | ERecordExtend (pos, bindings, exp) ->
	exists_list bindings 
	  (fun xs -> 
	     exists (fun x -> 
		  let labels = List.map extract_label_from_binding bindings 
		  and ts = assoc_proj2 xs in
		  let typed_labels = List.combine labels ts in
		    CConjunction 
		      ([
			 (t =?= record_type pos tenv typed_labels x) pos;
			 infer_expr tenv exp x
		       ] @
			 List.map (infer_label tenv) xs
		      ))
	  )

    (** Record update assigns the type of [e2] to the label [label] and
	does not change the reminder of the record. This is done by the use
	of three type variables:
	- [x] is the type of [label] in [e1] (No constraint, an update can
	create a field or change the field type). 
	- [pre x'] is the type of [label] in [e2]. 
	- [y] is the type of the other labels of [e1]. [e2]'s type is
 	  constrained to maintain them. *)
    | ERecordUpdate (pos, e1, label, e2) ->
	exists3 
	  (fun x x' y ->
	     let r = 
	       record_constructor (as_fun tenv)
		 (rowcons label (pre (as_fun tenv) x') y)
	     and r' = 
	       record_constructor (as_fun tenv)
		 (rowcons label x y)
	     in
	       CConjunction 
		 [ 
		   infer_expr tenv e1 r'; 
		   infer_expr tenv e2 x';
		   (t =?= r) pos
		 ]
	  )

    (** Accessing the label [label] of [e1] requires [e1]'s type to
    be a record in which [label] is assign a [pre x] type. *)
    | ERecordAccess (pos, e1, label) ->
	exists (fun x -> 
	     exists (fun y -> 
		  let r = 
		    record_constructor (as_fun tenv) 
		      (rowcons label (pre (as_fun tenv) x) y)
		  in
		    infer_expr tenv e1 r ^ (t =?= x) pos))

    (** [Assert false] is a special construction whose type: forall a. a. *)
    | EAssertFalse pos ->
	CTrue pos

and extract_label_from_binding (name, _) = 
  name

and record_type pos tenv xs x =
    let xs = List.map (fun (b, t) -> (b, pre (as_fun tenv) t)) xs in
      (match are_distinct (fst (List.split xs)) with
	   Some x -> raise (MultipleLabels (pos, x))
	 | _ -> ());
      record_constructor (as_fun tenv) (n_rowcons xs x) 

and extend_record_binding pos tenv l t =
  exists3 
    (fun x x' y -> 
       (n_arrows tenv [ rowcons l x y; x' ] (rowcons l x' y) =?= t) pos
    )
    
and infer_label tenv ((_, exp), t) =
  infer_expr tenv exp t
    
(** [infer e] determines whether the expression [e] is well-typed
    in the empty environment. *)
let infer tenv e =
  exists (infer_expr tenv e)
    
(** [bind b] generates a constraint context that describes the
    top-level binding [b]. *)
let bind env b =
  infer_binding env b  

let infer_program env = 
  List.fold_left (fun (env, acu) b ->
		    let env, f = bind env b in
		      env, (fun c -> acu (f c)))
    (env, fun c -> c)

let init_env () = 
  let builtins = 
    init_builtin_env (fun ?name () -> variable Rigid ?name:name ())
  in

    (* Add the builtin data constructors into the environment. *)
  let init_ds adt_name acu ds = 
    if ds = [] then None, acu 
    else 
      let (env, acu, lrqs, let_env) as r = 
	List.fold_left 
	  (fun acu (d, rqs, ty) -> 
	     intern_data_constructor undefined_position adt_name acu 
	       (undefined_position, d, rqs, ty)
	  ) acu ds
      in
	(Some acu, r)
  in
    
    (* For each builtin algebraic datatype, define a type constructor
       and related data constructors into the environment. *)
  let (init_env, acu, lrqs, let_env) = 
    List.fold_left 
      (fun (env, dvs, lrqs, let_env) (n, (kind, v, ds)) -> 
	   let r = ref None in
	   let env = add_type_constructor env n 
	     (MiniKindInferencer.intern_kind (as_kind_env env) kind,
	      variable ~name:n Constant (),
	      r) in
	   let (dvs, acu) = init_ds n (env, dvs, lrqs, let_env) ds in
	     r := dvs;
	     acu
      )
      (empty_environment, [], [], StringMap.empty)
      (List.rev builtins)
  in
  let vs = 
    fold_type_info (fun vs (n, (_, v, _)) -> v :: vs) [] init_env
  in
    (* The initial environment is implemented as a constraint context. *)
    (fun c -> 
       CLet ([ Scheme (undefined_position, vs, [], 
		       CLet ([ Scheme (undefined_position, lrqs, [],
				       CTrue undefined_position, 
				       let_env) ],
			     c),
		       StringMap.empty) ], 
	     CTrue undefined_position)),
  init_env

let remove_init_context = function
    CLet ([ Scheme (pos, rqs, fqs, CLet (_, c), h) ], CTrue pos') -> 
      c
  | _ -> assert false

let generate_constraint_task = "generate-constraint"
  
let generate_constraint b =
  let c, env = init_env () in
  c ((snd (infer_program env b)) (CDump Positions.undefined_position))

let register_tasks parse_program_task =
  Processing.register
    generate_constraint_task ([], ignore)
    [ [ parse_program_task ] ]
    (fun t -> generate_constraint (List.hd t)) 
    (const true)
    
 
