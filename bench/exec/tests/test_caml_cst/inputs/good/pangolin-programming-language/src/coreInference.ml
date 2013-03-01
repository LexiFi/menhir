(* $Id: coreInference.ml 44 2007-10-01 14:35:31Z yann.regisgianas $ *)

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

(** This module offers type inference for the syntax of [CoreSyntax]. 
    
    Inference a la Hindley Milner is implemented in a modern way: 
    we generate typing constraints and solve them following the style of
    the ATTAPL book. Yet, we improve the process by enriching 
    the syntax of constraints with explicit term. This allows the
    generation of the explicit version of the term in parallel 
    to the constraint solving. *)

(** The source language. *)

open CoreSyntax

(** The constraint language is imported. *)

open Constraint
open MultiEquation

(** The following module enables the injection of the syntactic
    categories of the output language into the constraint world. *)

open XCoreInConstraint
open CoreInferenceEnv
open CoreInferenceInternals

let mk_flex _ = 
  MultiEquation.variable MultiEquation.Flexible ()

let pre_id = PIdentifier.fresh_predicate_id (Position.dummy, "pre")

(** Associate an identifier [x] in the explicit language to a type
    [t] in the header of a [def] constraint. *)

let cbind_var h (x, t) =
  PIdentifier.Map.add x (t, mk_xcore_var x, PIdentifier.position x) h

(** Associate an identifier [x] in the explicit language to a 
    constraint [c] that generates a term in the explicit language. *)

let cbind_term h (x, c) v =
  PIdentifier.Map.add x (CoreAlgebra.TVariable v, c, PIdentifier.position x) h

(** Associate a list of identifiers [ids] of the implicit language to
    a list of flexible variables [gs], 
    a list of identifiers [xs] in the explicit language, 
    a list of internal bindings [xts],
    a list of internal types [tvs]. *)

let introduce_identifiers_typing env ids = 
  let (env, xs) = introduce_identifiers env ids in
  let (gs, xts) = variable_list Flexible xs in
  let tvs = snd (List.split xts) in
   (env, (gs, xs, xts, tvs))

(** [scheme vs c [ (x_1, v_1) ... (x_n, v_n) ] 
                 [ (y_1, c_1) ... (y_m, c_m) ] ]
    
    generates a constraint scheme of the form:

    forall vs vs'. [ c ] 
       (x_1 : (<x_1>, v_1), ..., x_n : (<x_n>, v_n),
        y_1 : (c_1, v'_1, ..., y_m : (c_m, v'_m)))

    where the vs' are chosen fresh. *)

let scheme pos vs c xs cs = 
  let vs' = List.map (fun _ -> MultiEquation.variable Flexible ()) cs in
    Scheme (pos, [], vs @ vs', c,
	    List.fold_left cbind_var 
	      (List.fold_left2 cbind_term PIdentifier.Map.empty cs vs')
	      xs)

(** [monoscheme pos xts] introduces an internal binding in the
    header of a [def] constraint. *)

let monoscheme pos xts = 
  Scheme (pos, [], [], CTrue pos,
	  List.fold_left (fun h (x, ty) -> cbind_var h (x, ty))
	    PIdentifier.Map.empty xts)

let rec formula_gen_ex' ?(pos = Position.dummy) vs f = 
  let tvs = List.map (fun v -> CoreAlgebra.TVariable v) vs in
  let v = mk_flex () in
  let tv = CoreAlgebra.TVariable v in
  let v' = mk_flex () in
  let qid = PIdentifier.fresh_value_id (pos, "_q") in
  let fid = PIdentifier.fresh_value_id (pos, "_f") in
  let h = PIdentifier.Map.empty in
  let c = f (CoreAlgebra.TVariable v') in
  let h = cbind_term h (fid, c) v' in
  let h = cbind_var h (qid, CoreAlgebra.TVariable v) in
    CLet ([ Scheme (pos, [], v :: v' :: vs, (tv =?= mk_prod tvs) pos, h) ], 
	  mk_xcore_fforall_tys pos qid fid)

and formula_gen_ex ?(pos = Position.dummy) vs c = 
  formula_gen_ex' ~pos vs 
    (fun v -> 
       let tvs = List.map (fun v -> CoreAlgebra.TVariable v) vs in
	 (v =?= mk_prod tvs) pos ^ c) 

and formula_gen_exists_list pos xs f = 
  let vs = List.map mk_flex xs in
  let tvs = List.map (fun v -> CoreAlgebra.TVariable v) vs in
    formula_gen_ex' ~pos vs (f (List.combine xs tvs))

and formula_gen_exists pos f =
  let v = mk_flex () in
    formula_gen_ex' ~pos [ v ] (f (CoreAlgebra.TVariable v))

(** Internalize a formula type scheme. *)

let internalize_formula_type_scheme env rigid (FTScheme abs) = 
  let (ts, ty) = open_formula_scheme_abs abs in
  let env = 
    if rigid then introduce_rigid_type_variables ts env 
    else introduce_flexible_type_variables ts env
  in
  let vts = List.map (lookup_rigid_type_variable env) ts in
  let ity = internalize_formula_type (types_env env) ty in
    (env, vts, ity)

(** Internalize a term type scheme. *)

let internalize_term_type_scheme env (TScheme abs) = 
  let (ts, ty) = open_scheme_abs abs in
  let env = introduce_rigid_type_variables ts env in 
  let vts = List.map (lookup_rigid_type_variable env) ts in
  let ity = internalize_term_type (types_env env) ty in
    (env, vts, ity)

(** [cbind_constant_type_scheme env k s] introduces a constant [k] 
    whose type scheme in the implicit language is [s]. *)

let cbind_constant_type_scheme env k s = 
  let pos = PIdentifier.position k in
  let (env, vts, ity) = internalize_term_type_scheme env s in
    (env,
     (fun c -> CLet ([ Scheme (pos, vts, [], CTrue pos, 
			       cbind_var PIdentifier.Map.empty (k, ity)) ], 
		     c)))

(** [cbind_constant_formula_type_scheme env p s] introduces a formula
    identifier [p] whose formula type scheme in the implicit language 
    is [s]. *)

let cbind_constant_formula_type_scheme env k s = 
  let pos = PIdentifier.position k in
  let (env, vts, ity) = internalize_formula_type_scheme env true s in
    (env,
     (fun c -> CLet ([ Scheme (pos, vts, [], CTrue pos, 
			       cbind_var PIdentifier.Map.empty (k, ity)) ], 
		     c)))

let instanciate_formula_type_scheme pos env s ty' =
  let (env, vts, ity) = 
    internalize_formula_type_scheme env false s 
  in
    ex vts ((ity =?= ty') pos)

(** [bindings pos env bs] generates a typing context that corresponds
    to a monomorphic binding in the implicit language (for
    lambda-abstractions). *)

let gen_bindings internalize_annotation ex pos env bs = 

  (* Introduce enough flexible variables and explicit identifiers in 
     the environment to handle the translation and the inference of
     arguments' types. *)

  let ids, tys = List.split bs in
  let (env, (gs, xs, xts, tvs)) = introduce_identifiers_typing env ids in

  (* Internalize the argument's types and equalize them with the 
     introduced flexible variables. *)

  let tenv = types_env env in
  let itys = List.map (internalize_annotation tenv) tys in 
  let eqs = 
    conj (List.map2 (fun g ity -> 
		       (CoreAlgebra.TVariable g =?= ity) pos) gs itys) 
  in
    
    (env, xs, itys, 
     (fun c -> ex gs (CLet ([ scheme pos [] eqs xts [] ], c))))

let bindings = gen_bindings internalize_term_type ex

let fbindings = gen_bindings internalize_formula_type formula_gen_ex 

let fbindings_nogen = gen_bindings internalize_formula_type ex

(** Handle values definition that takes the form:

                 (x_1, ..., x_n) where F = t
    
    Here is the different steps :

    1. Introduce a type variable g_i for each x_i.

    2. Generate the typing constraint C for t having the type g_1 x ... g_n.

    3. Introduce y_i, an identifier in the explicit translation of x_i.

    4. Introduce d, an identifier for the explicit term that comes from
       the resolution of C.

    5. In an environment that binds the (x_i) to monomorphic (g_i), 
       generate the typing constraint D of F :
       def (x_i : (y_i, g_i)) in (( F : prop ))

    6. Introduce f, an identifier for the explicit formula that comes from
       the resolution of D.

    Finally, we produce:

    - the typing context 
      
      def (g_i) (x_i : (y_i, g_i)) (d : C) (f : D) in []

    - the typing environment that binds x_i to their type scheme g_i. 

    - the let binding in the explicit language (<x_i> : <g_i>) 
      (<x_i> will produce y_i and <g_i> will produce the inferred type scheme)

    - the explicit value definition <C>

    - the explicit formula <F> *)

let rec value_definition pos env (ids, f, t) = 
  let (env, (gs, xs, xts, tvs)) = introduce_identifiers_typing env ids in
  let c = term env (mk_prod tvs) t in
  let d = PIdentifier.fresh_value_id (t.tpos, "_vdef") in
  let g = CLet ([ monoscheme pos xts ], 
		optional_proposition_abstracted_by pos env [tvs] f) in
  let h = PIdentifier.fresh_value_id (t.tpos, "_vassertion") in
  let s = scheme t.tpos gs (CTrue t.tpos) xts [ (d, c); (h, g) ] in
    ((fun c -> CLet ([ s ], c)), env, 
     mk_xcore_logic_bindings t.tpos xs h,
     (mk_xcore_term t.tpos d))

(** Same thing as [value_definition] except that the identifiers [ids] 
    are bound into [t]. *)

and recvalue_definition pos env (ids, f, t) = 
  let (env, (gs, xs, xts, tvs)) = introduce_identifiers_typing env ids in
  let c = CLet ([ monoscheme pos xts ], 
		term env (mk_prod tvs) t) 
  in
  let d = PIdentifier.fresh_value_id (t.tpos, "_vdef") in
  let g = CLet ([ monoscheme pos xts ], 
		optional_proposition_abstracted_by pos env [tvs] f) in
  let h = PIdentifier.fresh_value_id (t.tpos, "_vassertion") in
  let s = scheme t.tpos gs (CTrue t.tpos) xts [ (d, c); (h, g) ] in
    ((fun c -> CLet ([ s ], c)), env, 
     mk_xcore_logic_bindings t.tpos xs h,
     (mk_xcore_term t.tpos d))

(** Generate a typing constraint for a term [t] to have a type 
    [ty]. *)

and term env ty t = 
  let pos = t.tpos in
    match t.tvalue with

      | EId x ->
	  
	  (* The term is a variable.
	     
	     We generate the typing constraint that forces the type scheme
	     of [x] to be instanciated to [ty]. The constraint produces the
	     corresponding variable in the explicit language. *)
	  
	  let y = explicit_identifier_for x env in
	    (y <? ty) pos ^ mk_xcore_var y
	      
      | EApp (f, largs, args) ->

	  (* The term is an application. 

	     We generate the typing constraint that forces [f] to be
	     a function that is waiting for a tuple whose number of
	     components is equal to the lenght of [args]. We generate
	     a corresponding application in the explicit language. *)

	  let v = MultiEquation.variable MultiEquation.Flexible () in
	  let tv = CoreAlgebra.TVariable v in
	    ex [v] 
	      (let name = PIdentifier.fresh_value_id (pos, "_c") in
	       let h = PIdentifier.Map.empty in
	       let h = cbind_var h (name, tv) in
		 (tv =?= ty) pos ^
		 CLet ([ Scheme (pos, [], [], CTrue pos, h) ],
		       exists_list ~pos args 
			 (fun ctys ->
			    exists_list ~pos largs
			      (fun ltys -> 
				 let tys = snd (List.split ctys) in
				 let ltys = snd (List.split ltys) in
				 let f_ty = mk_n_arrow tys ty in
				   mk_xcore_app pos 
				     name
				     (term env f_ty f) 
				     (List.map2 (formula env) ltys largs)
				     (List.map2 (term env) tys args)))))

      | EKApp (f, args) ->

	  (* The term is an application. 

	     We generate the typing constraint that forces [f] to be
	     a function that is waiting for a tuple whose number of
	     components is equal to the lenght of [args]. We generate
	     a corresponding application in the explicit language. *)

	  exists_list ~pos args 
	    (fun ctys ->
	       let tys = snd (List.split ctys) in
	       let f_ty = mk_n_arrow tys ty in
		 mk_xcore_kapp pos 
		   (term env f_ty { tpos = pos; tvalue = EId f })
		   (List.map2 (term env) tys args))
	    
      | ELam abs ->

	  (* The term is a lambda-abstraction. 

	     We generate the typing constraint that forces [ty] to be
	     an arrow. We generate the corresponding lambda-abstraction
	     in the explicit language. *)

	  let (largs, (ins, pre), outs, body) = open_fun_abs abs in
	  let (outs, post) = open_function_output_abs outs in

	  (* Internalize the arguments' type annotations. *)

	  let (env, lxs, lxs_itys, ctx_lxs) = fbindings_nogen pos env largs in
	  let (env, xs, ins_itys, ctx_ins) = bindings pos env ins in
	  let (env', ys, outs_itys, ctx_outs) = bindings pos env outs in

	  let out_ity = mk_prod outs_itys in
	  let f_ty = mk_n_arrow ins_itys out_ity in

	  let pre_c = 
	    optional_proposition_abstracted_by pos env [lxs_itys; ins_itys] pre
	  in
	  let post_c = 
	    optional_proposition_abstracted_by pos env 
	      [lxs_itys; ins_itys; [out_ity] ] post
	  in
	    ctx_lxs 
	      (ctx_ins 
		 (ctx_outs (
		    
		    (f_ty =?= ty) pos 
		      
		    ^ 
		      mk_xcore_lam pos 
		      lxs
		      xs pre_c
		      ys post_c
		      (term env out_ity body))))
		 
      | EPrimitive p ->

	  (* The term is a primitive. *)

	  primitive env pos ty p

      | EForallTys abs ->

	  (* The term is a universal quantification over types.

	     We introduce the rigid type variables [a_1 ... a_n] 
	     into the environment and we generate the constraint 
	     [\forall a_1 ... a_n. C]. *)

	  let (vs, t) = open_eforalltys_abs abs in
	  let env = introduce_rigid_type_variables vs env in 
	  let rvs = 
	    List.map (CoreInferenceEnv.lookup_rigid_type_variable env) vs 
	  in 

	    fl rvs (term env ty t)

      | EAnnot (t, ty') ->
	  
	  (* The term is annotated by a type. 

	     We take this annotation into account and we remove the
	     annotation in the explicit version of the term. *)

	  let ity = 
	    internalize_term_type (types_env env) ty' 
	  in

	    (ity =?= ty) pos ^ term env ty t
	    
      | EExistsTys abs ->

	  (* The term introduces a set of flexible variables. 

	     We remove the flexible variables in the explicit version. *)

	  let (vs, t) = open_eexiststys_abs abs in

	  let env = 
	    CoreInferenceEnv.introduce_flexible_type_variables vs env 
	  in
	  let rvs = 
	    List.map 
	      (CoreInferenceEnv.lookup_flexible_type_variable env) vs 
	  in 
	    ex rvs (term env ty t)

      | ELet abs ->
	  
	  (* The term is a local definition. *)

	  let (ids, f, defs, t') = open_let_abs abs in
	  let (ctx, env, lt, t) = value_definition pos env (ids, f, defs) in
	    ctx (mk_xcore_let pos lt t (term env ty t'))

      | ELetRec abs ->

	  (* The term is a local definition. *)

	  let (ids, f, defs, t') = open_letrec_abs abs in
	  let (ctx, env, lt, t) = 
	    recvalue_definition pos env (ids, f, defs) 
	  in
	    ctx (mk_xcore_letrec pos lt t (term env ty t'))
	    
      | ECase (s, cs) ->
	  
	  (* The term is a pattern matching. *)

	  let v = MultiEquation.variable MultiEquation.Flexible () in
	  let tv = CoreAlgebra.TVariable v in
	    ex [v] 
	      (let name = PIdentifier.fresh_value_id (pos, "_c") in
	       let h = PIdentifier.Map.empty in
	       let h = cbind_term h (name, term env tv s) v in
		 CLet ([ Scheme (pos, [], [], CTrue pos, h) ],
		       mk_xcore_case pos name
			 (List.map (clause env tv ty) cs)))
	    
      | EProd [ t ] ->

	  (* The term is 1-uple which is isomorphic to a simple term. *)

	  term env ty t
	    	    
      | EProd ts ->

	  (* The term is an n-uple. *)

	  exists_list ~pos ts 
	    (fun tvs ->
	       let tys = List.map snd tvs in
		 (mk_prod tys =?= ty ) pos ^
		   mk_xcore_prod pos 
		   (List.map (fun (t, ty) -> term env ty t) tvs))

      | EIf (c, t, f) ->

	  (* The term is a conditional. *)
	  
	  mk_xcore_if pos
	    (term env (internalize_primitive_type TBool) c) 
	    (term env ty t)
	    (term env ty f)
	   
      | EAbsurd -> 

	  (* The term is dead code. 

	     As it is never executed, any type is accepted. *)

	  mk_xcore_eabsurd pos

      | EAssert (f, t) -> 

	  (* The term is a logic assertion. *)

	  mk_xcore_assert pos (proposition env f) (term env ty t)

 
      | ELetLogic abs ->
	  
	  (* The term is let-logic assertion. *)

	  let (bs, f, t) = open_let_logic_abs abs in

	  let (env, xs, ins_itys, ctx_ins) = fbindings_nogen pos env bs in

	     ctx_ins 
	       (mk_xcore_let_logic pos xs 
		  (formula env (iprop_type env) f)
		  (term env ty t))

      | EDeferred ->

	  (* The term is deferred, no typing constraint is generated. *)

	  mk_xcore_deferred_term pos

(** Generate a constraint for a clause [(p, t)] that ensures that the
    pattern [p] has type [v] and the body [t] has type [ty]. *)

and clause env v ty abs =
  let (p, t) = open_clause_abs abs in
  let pos = t.tpos in
  let (env, c, vs, h) = pattern pos ([], PIdentifier.Map.empty) env v p in
  let v = MultiEquation.variable MultiEquation.Flexible () in
  let pname = PIdentifier.fresh_value_id (pos, "_p") in
    ex (v :: vs)
      (CLet ([ Scheme (pos, [], [], CTrue pos, h) ], 
	     (let h = cbind_term PIdentifier.Map.empty (pname, c) v in
		CLet ([ Scheme (pos, [], [], CTrue pos, h) ], 
		      mk_xcore_clause pos pname (term env ty t)))))

(** [pattern pos (vs, h) env ty p] augments the typing environment [env] and 
    the constraint's context with the free variables of [p] and generates
    a constraint to ensure that [p] has type [ty]. [vs] are the
    flexible variables that are necessary to built the constraint. *)

and pattern pos (vs, h) env ty = function

  | PVar x -> 

      (* The pattern is a variable. *)

      let (env, (gs, xs, xts, tvs)) = introduce_identifiers_typing env [ x ] in
      let v = Misc.destruct_single gs 
      and tv = Misc.destruct_single tvs 
      and x = Misc.destruct_single xs in
      let c = (tv =?= ty) pos ^ mk_xcore_pvar pos x in
      let h = cbind_var h (x, tv) in
	(env, c, v :: vs, h)

  | PApp (k, ps) -> 

      (* The pattern is a dataconstructor's application. *)

      (* Retrieve the contribution of subpatterns. *)

      let (env, vs, cs, args, h) = 
	List.fold_left 
	  (fun (env, vs, cs, args, h) p ->
	     let v = MultiEquation.variable MultiEquation.Flexible () in
	     let tv = CoreAlgebra.TVariable v in
	     let (env, c, vs, h) = pattern pos (vs, h) env tv p in
	       (env, v :: vs, c :: cs, tv :: args, h))
	  (env, vs, [], [], h) ps
      in
	
      (* Look up the identifier for 'k' in the explicit language. *)

      let xk = explicit_identifier_for k env in

      (* There are two kinds of data constructors: the constant
	 and the n-ary constructors. 

	 One could argue that 0-ary constructor are constants. Yet,
	 it is syntactically simpler to write [K] instead of [K ()] 
	 if [K] is a constant. Thus, we have a special case to express
	 that the type of a constant is [ty] and not [() -> ty]. *)

      let ty = 
	match args with
	  | []   -> ty
	  | args -> mk_n_arrow (List.rev args) ty
      in

	(env, (xk <? ty) pos ^ mk_xcore_pkapp pos xk (List.rev cs), vs, h)

(** Generate the typing constraint for primitive constructs. *)
	
and primitive env pos ty p =
  let mk_primitive_type p = 
    internalize_primitive_type p
  in
    match p with

      | PInt x ->
	  
	  (* The primitive is an integer. *)

	  (ty =?= mk_primitive_type TInt) pos 

	  ^ mk_xcore_primitive pos (XCoreSyntax.Raw.PInt x)

      | PFalse ->

	  (* The primitive is [false]. *)

	  (ty =?= mk_primitive_type TBool) pos 

	  ^ mk_xcore_primitive pos XCoreSyntax.Raw.PFalse

      | PTrue ->

	  (* The primitive is [true]. *)

	  (ty =?= mk_primitive_type TBool) pos 

	  ^ mk_xcore_primitive pos XCoreSyntax.Raw.PTrue

(** Generate the typing constraint for a formula. *)

and iprop_type env = 
  internalize_formula_type (types_env env) FTProp

and formula env ty f =
  let pos = f.fpos in
  let n = PIdentifier.fresh_value_id (f.fpos, "type") in
  let v = MultiEquation.variable MultiEquation.Flexible () in
  let tv = CoreAlgebra.TVariable v in
    formula_gen_ex [v] 
      ((ty =?= tv) pos ^
	 let c = nude_formula env tv f in 
	 let h = cbind_term PIdentifier.Map.empty (n, c) v in
	   CLet ([ Scheme (pos, [], [], CTrue pos, h) ], 
		 mk_xcore_fannot f.fpos n))

and nude_formula env ty f = 
  let pos = f.fpos in
    match f.fvalue with

      | FDeferred ->

	  (* The formula is deferred. *)

	  mk_xcore_fdeferred pos

      | FTrue ->
	  
	  (* The formula is [True]. *)

	  (ty =?= iprop_type env) pos
	    
	  ^ mk_xcore_formula pos XCoreSyntax.Raw.FTrue

      | FFalse -> 

	  (* The formula is [False]. *)

	  (ty =?= iprop_type env) pos
	    
	  ^ mk_xcore_formula pos XCoreSyntax.Raw.FFalse

      | FVar x ->
	  
	  (* The formula is a variable [x]. *)

	  let y = explicit_identifier_for x env in
	    (y <? ty) pos ^ mk_xcore_fvar y
	  
      | FLam abs ->

	  (* The formula is a lambda-abstraction. *)

	  let (ins, body) = open_lfun_abs abs in

	  let (env, xs, ins_itys, ctx_ins) = fbindings pos env ins in

	    formula_gen_exists pos 
	      (fun o v -> 
		 let f_ty = mk_n_larrow ins_itys o in
		   ctx_ins 
		     ((f_ty =?= ty) pos 
			
		      ^ (v =?= ty) pos

		      ^ mk_xcore_flam pos xs (formula env o body)))

      | FApp (f, args) ->

	  (* The formula is an application. *)

	  formula_gen_exists_list pos args 
	    (fun ctys v ->
	       (v =?= ty) pos ^
	       let tys = snd (List.split ctys) in
	       let f_ty = mk_n_larrow tys ty in
		 mk_xcore_fapp pos 
		   (formula env f_ty f) 
		   (List.map2 (formula env) tys args))

      | FForallTys abs ->

	  (* The term is a universal quantification over types.

	     We introduce the rigid type variables [a_1 ... a_n] 
	     into the environment and we generate the constraint 
	     [\forall a_1 ... a_n. C]. *)

	  let (vs, t) = open_fforalltys_abs abs in
	  let env = introduce_rigid_type_variables vs env in 
	  let rvs = 
	    List.map (CoreInferenceEnv.lookup_rigid_type_variable env) vs 
	  in 
	    fl rvs (formula env ty t)
  
      | FExistsTys abs -> 

	  (* The term introduces a set of flexible variables. 

	     We remove the flexible variables in the explicit version. *)

	  let (vs, t) = open_fexiststys_abs abs in

	  let env = 
	    CoreInferenceEnv.introduce_flexible_type_variables vs env 
	  in
	  let rvs = 
	    List.map 
	      (CoreInferenceEnv.lookup_flexible_type_variable env) vs 
	  in 
	    formula_gen_ex rvs (formula env ty t)

      | FForall abs ->

	  (* The formula is a universal quantification. *)

	  let (ins, tgs, body) = open_lforall_abs abs in

	  let (env, xs, ins_itys, ctx_ins) = fbindings pos env ins in

	  let tgs = List.map (fun tg -> explicit_identifier_for tg env) tgs in

	    ctx_ins 
	      ((iprop_type env =?= ty) pos 
	       ^ 
	       mk_xcore_forall pos xs tgs (formula env (iprop_type env) body))

      | FExists abs ->

	  (* The formula is an existential quantification. *)

	  let (ins, body) = open_lexists_abs abs in

	  let (env, xs, ins_itys, ctx_ins) = fbindings pos env ins in

	    ctx_ins 
	      ((iprop_type env =?= ty) pos 
	       ^ 
	       mk_xcore_exists pos xs (formula env (iprop_type env) body))

      | FEq (f1, f2) ->

	  (* The formula is an equality between two logical objects. *)

	  formula_gen_exists pos
	    (fun o v ->
	       (v =?= ty) pos ^
	       let oname = PIdentifier.fresh_value_id (pos, "_eq") in
	       let h = PIdentifier.Map.empty in
	       let h = cbind_var h (oname, o) in
		 CLet ([ Scheme (pos, [], [], CTrue pos, h) ], 
		       (ty =?= iprop_type env) pos
		       ^ mk_xcore_feq pos oname 
			 (formula env o f1) (formula env o f2)))

      | FKApp (k, args) ->

	  (* The formula is a dataconstructor application. *)

	  formula_gen_exists_list pos args 
	    (fun ctys v ->
	       (v =?= ty) pos ^
	       let tys = snd (List.split ctys) in
	       let f_ty = mk_n_arrow tys ty in
	       let k_var = { fpos = pos; fvalue = FVar k } in
		 mk_xcore_fkapp pos 
		   (formula env f_ty k_var) 
		   (List.map2 (formula env) tys args))

      | FProd ts ->
	  
	  (* The formula is a tuple. *)

	  formula_gen_exists_list pos ts 
	    (fun tvs v ->
	       (v =?= ty) pos ^
	       let tys = List.map snd tvs in
		 (mk_prod tys =?= ty ) pos ^
		   mk_xcore_fprod pos 
		   (List.map (fun (t, ty) -> formula env ty t) tvs))


      | FPrimitive p ->

	  (* The formula is a primitive. *)

	  formula_primitive pos env ty p

      | FAnnot (f, ty') ->
	  
	  (* The formula is annotated by a type. 

	     We take this annotation into account and we remove the
	     annotation in the explicit version of the term. *)

	  let ity = 
	    internalize_formula_type (types_env env) ty'
	  in

	    (ity =?= ty) pos ^ formula env ty f

and formula_primitive pos env ty = function

  | Pre -> 

      (* The formula primitive is the precondition operator. *)

      mk_xcore_formula_primitive pos Pre ^ (pre_id <? ty) pos

  
  | Post ->

      (* The formula primitive is the postcondition operator. *)

      mk_xcore_formula_primitive pos Post

      ^ instanciate_formula_type_scheme pos env formula_type_scheme_of_post ty

  | (PAnd | POr | PImply | PEquiv) as p ->

      (* The formula primitive is a logical binary connective. *)

      mk_xcore_formula_primitive pos p

      ^ (internalize_formula_type (types_env env) 
	   formula_type_of_logical_connective 
	 =?= ty) pos

  | PNot ->

      (* The formula is the negation operator. *)

      mk_xcore_formula_primitive pos PNot 

      ^ (internalize_formula_type (types_env env) formula_type_of_not 
	 =?= ty) pos


  | (PLessThan | PLessEqualThan | PGreaterThan | PGreaterEqualThan) as p ->

      (* The formula primitive is a logical binary connective. *)

      mk_xcore_formula_primitive pos p

      ^ (internalize_formula_type (types_env env) 
	   formula_type_of_integer_relation
	 =?= ty) pos

  | (PAdd | PSub | PMult | PDiv) as p ->

      (* The formula primitive is a logical binary connective. *)

      mk_xcore_formula_primitive pos p

      ^ (internalize_formula_type (types_env env) 
	   formula_type_of_integer_function
	 =?= ty) pos

  | PEPrimitive p ->

      (* The formula is a computational value. *)

      mk_xcore_formula_peprimitive pos (primitive env pos ty p)

and proposition env f = 
  formula env (iprop_type env) f

and optional_proposition pos env = function

  | ImplicitFormula pos -> 
      mk_xcore_implicit_formula pos 

  | ExplicitFormula f ->
      mk_xcore_explicit_formula pos (proposition env f)

and optional_proposition_abstracted_by pos env tvss = function

  | ImplicitFormula pos ->
      mk_xcore_implicit_formula pos 

  | ExplicitFormula f ->
      let ty = 
	List.fold_left (fun ty tvs -> mk_n_larrow tvs ty)
	  (iprop_type env) (List.rev tvss)
      in
	mk_xcore_explicit_formula pos (formula env ty f)

(** Augment the environment and the constraint's context with
   the declaration of a datatype constructor [k] whose type
   is [ty] and type parameters are [ts]. We also generate 
   the corresponding declaration in the explicit language. *)

let algebraic_data_constructor ts (env, ctx, ds) (k, ty) =
  let s = TScheme (create_scheme_abs (ts, ty)) in
  let (env, _) = introduce_identifier env k in
  let k' = explicit_identifier_for k env in
  let (env, ctx') = cbind_constant_type_scheme env k' s in
  let xty = 
    (* FIXME: the rank may be uncorrect. Check that. *)
    term_type' 1 (internalize_term_type (types_env env) ty)
  in
    (env, (fun c -> ctx (ctx' c)), (k', xty) :: ds)

(** Transform a kind in the surface language into its isomorphic
   representation in the explicit language. *)

let rec kind = function
  | KStar -> XCoreSyntax.Raw.KStar
  | KArrow ks -> XCoreSyntax.Raw.KArrow (List.map kind ks)

(** Augment the environment with a type definition by introducing
    a new constant constraint variable, a new type identifier in
    the explicit language and take the type definition into account. *)

let type_definition pos env (t, k, tdef) =
  let env, tx = introduce_type_constant t env in
  let tv = lookup_type_constant env t in
  let (tdef, env, ctx) = 
    match tdef with
      | DAlgebraic (ts, ds) ->
	  let env = introduce_rigid_type_variables ts env in
	  let txs = 
	    List.map (fun x -> explicit_type_identifier_for x env) ts 
	  in
	  let (env, ctx, ds) = 
	    List.fold_left (algebraic_data_constructor ts) 
	      (env, Misc.id, []) ds
	  in
	    (XCoreSyntax.Raw.DAlgebraic (txs, ds), env, ctx)

      | DeferredType ->
	  (XCoreSyntax.Raw.DeferredType, env, (fun x -> x))

  in
    ((fun c -> fl [tv] (ctx c)), env, tx, kind k, tdef)

(** Augment the environment with a predicate definition by introducing
    a predicate identifier in the explicit language and by translating
    the predicate definition. *)      

let rec predicate_definition pos env (p, pdef) = 

    match pdef with

      | PDAbbrev f ->

	  let (env, (gs, xs, xts, tvs)) = 
	    introduce_identifiers_typing env [p] in
	  let x = Misc.destruct_single xs in
	  let c = formula env (mk_prod tvs) f in
	  let d = PIdentifier.fresh_predicate_id (f.fpos, "_vdef") in
	  let s = scheme f.fpos gs (CTrue f.fpos) xts [ (d, c) ] in
	    ((fun c -> CLet ([ s ], c)), env, x, mk_xcore_pdabbrev pos d)

      | PDInductive (Some s, is) ->

	  let (env, x) = introduce_identifier env p in
	  let (env, ctx) = cbind_constant_formula_type_scheme env x s in
	    (ctx, env, x, mk_xcore_pdinductive pos 
	       (List.map (inductive_case pos env) is))

      | PDInductive (None, _) ->
	  assert false

and inductive_case pos env i = 
  let c = proposition env i in
  let d = PIdentifier.fresh_predicate_id (i.fpos, "_vdef") in
  let s = scheme i.fpos [] (CTrue i.fpos) [] [ (d, c) ] in
    CLet ([ s ], mk_xcore_xformula pos d)


(** Augment the environment with a logic function definition. *)

let logic_clause pos env v ty abs = 
  let (p, f) = open_logic_clause_abs abs in
  let (env, c, vs, h) = pattern pos ([], PIdentifier.Map.empty) env v p in
  let v = MultiEquation.variable MultiEquation.Flexible () in
  let pname = PIdentifier.fresh_value_id (pos, "_p") in
    ex (v :: vs)
      (CLet ([ Scheme (pos, [], [], CTrue pos, h) ], 
	     (let h = cbind_term PIdentifier.Map.empty (pname, c) v in
		CLet ([ Scheme (pos, [], [], CTrue pos, h) ], 
		      mk_xcore_logic_clause pos pname (formula env ty f)))))


let logic_cases_on pos env ty xs lcs = 
  let (env, (gs, xs, xts, tvs)) = introduce_identifiers_typing env xs in
    exists (fun i -> 
	      ex gs
		(CLet ([ monoscheme pos xts ],
		       (ty =?= mk_n_larrow tvs i) pos 
		       ^ mk_xcore_logic_cases_on pos xs 
			 (List.map (logic_clause pos env (mk_prod tvs) i) lcs))))

let logic_function pos env (p, fdef) = 
  match fdef with
    | LFInductive abs ->
	let (args, lcs) = open_lfi_abs abs in
	let (env, (gs, xs, xts, tvs)) = introduce_identifiers_typing env [p] in
	let x = Misc.destruct_single xs in
	let c = CLet ([ monoscheme pos xts ], 
		      logic_cases_on pos env (mk_prod tvs) args lcs) 
	in
	let d = PIdentifier.fresh_value_id (pos, "_fdef") in
	let s = scheme pos gs (CTrue pos) xts [ (d, c) ] in
	  ((fun c -> CLet ([ s ], c)), env, mk_xcore_logic_function pos x d)

    | LFAbbrev f ->
	
	let (env, (gs, xs, xts, tvs)) = 
	  introduce_identifiers_typing env [p] in
	let x = Misc.destruct_single xs in
	let c = mk_xcore_lfabbrev pos (formula env (mk_prod tvs) f) in
	let d = PIdentifier.fresh_predicate_id (f.fpos, "_vdef") in
	let s = scheme f.fpos gs (CTrue f.fpos) xts [ (d, c) ] in
	  ((fun c -> CLet ([ s ], c)), env, mk_xcore_logic_function pos x d)
      

(** Generate a typing constraint for a component under a 
    particular environment [env]. *)

let component env = function

  | CValue (ids, f, defs) ->
      
      (* The component is a toplevel value definition. *)

      let pos = defs.tpos in
      let (ctx, env, lt, t) = value_definition pos env (ids, f, defs) in
	(ctx, mk_xcore_cvalue defs.tpos lt t, env)

  | CRecValue (ids, f, defs) ->
      
      (* The component is a toplevel recursive value definition. *)

      let pos = defs.tpos in
      let (ctx, env, lt, t) = recvalue_definition pos env (ids, f, defs) in
	(ctx, mk_xcore_crecvalue defs.tpos lt t, env)

  | CTypeDef (t, k, tdef) ->

      (* The component is a type definition. *)
      
      let pos = PIdentifier.position (Var.Atom.basename t) in
      let (ctx, env, t, k, tdef) = type_definition pos env (t, k, tdef) in
	(ctx, mk_xcore_type_definition pos t k tdef, env)

  | CFact (status, f) ->

      (* The component is an axiom or a lemma. *)
      
      let pos = f.fpos in

	(Misc.id, mk_xcore_fact pos status (proposition env f), env)

  | CPredicate (pid, pdef) ->
      
      let pos = PIdentifier.position (Var.Atom.basename pid) in
      let (ctx, env, pid, pdef) = predicate_definition pos env (pid, pdef) in
	(ctx, mk_xcore_predicate_definition pos pid pdef, env)

  | CLogicFunction (pid, fdef) ->
      
      let pos = PIdentifier.position (Var.Atom.basename pid) in
      let (ctx, env, c) = logic_function pos env (pid, fdef) in
	(ctx, c, env)
	 

(** Generate a typing constraint for a list of components under 
    a particular environment [env]. *)

let rec components env = function

  | PEmpty pos ->
      mk_xcore_pempty pos

  | PConsComponent abs ->

      (* Ensure freshness of component's name. *)

      let (pos, cpnt, prog) = open_pcons_abs abs in

      (* Generate the constraint related to the component. *)

      let (ctx, c, env) = component env cpnt in

	ctx (mk_xcore_pcons pos c (components env prog))

(** The initial environment and initial constraint context. *)

let initial_env = 
  ((fun c -> fl internal_primitive_constants_variables c),
   CoreInferenceEnv.empty)

(** Generate a self-contained typing constraint for a program. *)

let generate_constraint_for_program p = 

  (* There is an initial typing context/environment that must
     be taken into account. *)

  let (ctx, env) = initial_env in

  let (env, ctx_pre) = 
    cbind_constant_formula_type_scheme env pre_id formula_type_scheme_of_pre 
  in
  let ctx = (fun c -> ctx_pre (ctx c)) in

    CoreInferenceInternals.reset ();

    (* The resulting constraint is built under this context and
       the constraint generation for the whole program can make
       use of it. *)

    ctx (components env p)

let program p =
  let _ = PIdentifier.reset () in
  let c = generate_constraint_for_program p in
    try 
      as_xcore_program (Solver.solve c)
    with Unifier.CannotUnify (pos, t1, t2) ->
      Error.error "during type checking" pos 
	(Printf.sprintf "cannot unify\n  `%s'\nand\n  `%s'\n"
	   (XCorePrinter.formula_type_as_string 
	      (CoreInferenceInternals.formula_type' 0 t1))
	   (XCorePrinter.formula_type_as_string
	      (CoreInferenceInternals.formula_type' 0 t2)))

open Options
open Arg

type options = {
  mutable show_explicit_ast : bool
}

let options = { show_explicit_ast = false } 

let process ast = 
  let xast = program ast in
  let internal_xast =     
    try 
      XCoreSyntax.import_program XCoreSyntax.Identifier.Map.empty xast 
    with XCoreSyntax.Var.UnboundIdentifier p ->
      Error.error "during type checking" (PIdentifier.position p)
	(Printf.sprintf 
	   "(internal error, please report)\n \
            `%s' is an introduced name that is not correctly bound."
	   (PIdentifier.as_string p))
  in
  let cooked_xast = XCoreCooking.cook internal_xast in
    if options.show_explicit_ast then (
      let raw_cooked_xast = 
	XCoreSyntax.export_program XCoreSyntax.Var.AtomIdMap.empty cooked_xast 
      in
	Printf.printf "%s\n" (XCorePrinter.program raw_cooked_xast));
    cooked_xast	

let options () = 
  mk_local_options 
    {
      env         = options;
      postprocess = (fun x -> x);
      options     = Arg.align [
	"--show-explicit-version", 
	Unit (fun x -> options.show_explicit_ast <- true), 
	" Show the result of type inference."
      ]
    } 

