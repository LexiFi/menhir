(* $Id$ *)

(** This module implements type inference. *)

open Positions
open Misc
open Sig
open MiniKindInferencer
open MiniConstraint
open MiniAlgebra
open MiniMultiEquation
open MiniTypingEnvironment
open MiniTypingExceptions
open MiniTypes
open MiniAst
open MiniEquationalTheory

(** {2 Inference} *)
let check_coercion_correctness eqt t1 t2 =
  is_eq_modulo eqt t1 t2

type fragment = 
    {
      gamma         : (crterm * position) StringMap.t;
      vars          : variable list;
      ex_vars       : variable list;
      tconstraint   : tconstraint;
      eq_theory     : eq_theory;
      eqt_not_false : bool;
      denv          : (tname * variable) list
    }

let empty_fragment = 
  {
    gamma         = StringMap.empty;
    vars          = [];
    ex_vars       = [];
    eq_theory     = empty_eq_theory;
    eqt_not_false = true;
    tconstraint   = CTrue undefined_position;
    denv          = []
  }
    
let join_fragment pos f1 f2 = 
  {
    gamma = 
      (try 
	StringMap.strict_union f1.gamma f2.gamma
       with StringMap.Strict x -> raise (NonLinearPattern (pos, x)));
    vars          = f1.vars @ f2.vars;
    ex_vars       = f1.ex_vars @ f2.ex_vars;
    tconstraint   = f1.tconstraint ^ f2.tconstraint;
    eq_theory     = join_eq_theory f1.eq_theory f2.eq_theory;
    eqt_not_false = f1.eqt_not_false && f2.eqt_not_false;
    denv          = f1.denv @ f2.denv
  }

(** [infer_pat_constraint ?flat tenv p t] generates the constraints 
    related to the matching of algebraic datatype. If [flat] is set, 
    the constraint is specialized for unguarded algebraic datatype. *)
let rec infer_pat_constraint tenv p t =
  let infer_pat = infer_pat_constraint tenv in
    match p with
	
      | PZero p 
      | PVar (p, _) 
      | PWildcard p ->
	  CTrue p

      | PPrimitive (pos, p) ->
	  (t =?= type_of_primitive (as_fun tenv) p) pos

      | PAlias (pos, name, p) ->    
	  (name <? t) pos ^ infer_pat p t

      | PTypeConstraint (pos, p, (vs, typ)) ->
	  assert (vs = []);
	  (t =?= intern pos tenv typ) pos ^ infer_pat p t

      | POr (pos, ps) | PAnd (pos, ps) ->
	  conj (List.map (fun p -> infer_pat p t) ps)

      | PData (pos, localvars, k, ps) when is_guarded_datacon tenv k ->
	  CTrue pos

      | PData (pos, localvars, k, ps) (* regular adt *) ->
          let (alphas, betas, kt, denv) =
            fresh_datacon_scheme pos tenv k localvars in
          let rt = result_type (as_fun tenv) kt
          and ats = arg_types (as_fun tenv) kt in
	    assert (List.length ps = List.length ats);
            ex ~pos alphas
              ((rt =?= t) pos
               ^ fl ~pos betas (conj (List.map2 infer_pat ps ats)))
	      
(** [infer_pat_fragment p t] generates a fragment that represents the 
    information gained by a success when matching p. *)
and infer_pat_fragment tenv p t =
  let join pos = List.fold_left (join_fragment pos) empty_fragment in
  let rec infpat t = function

    | PZero pos -> 
	{ empty_fragment with tconstraint = CFalse pos }
	  
    | PPrimitive (pos, _) 
    | PWildcard pos | POr (pos, []) | PAnd (pos, []) ->
	empty_fragment
	  
    | PVar (pos, name) ->
	let v = variable Flexible () in
	  { empty_fragment with 
	      gamma = StringMap.singleton name (TVariable v, pos);
	      tconstraint = (TVariable v =?= t) pos;
	      vars = [ v ]
	  }

    | PAlias (pos, name, p) ->
	let fragment = infpat t p in 
	  { fragment with 
	      gamma = StringMap.strict_add name (t, pos) fragment.gamma 
	  }

    | POr (pos, ps) ->
	assert false

    | PAnd (pos, ps) ->
	join pos (List.map (infpat t) ps)

    | PTypeConstraint (pos, p, (vs, typ)) ->
	assert (vs = []);
	infpat (intern pos  tenv typ) p

    | PData (pos, localvars, k, ps) when is_guarded_datacon tenv k ->

	(** Extract k's scheme. *)
	let (alphas, betas, kt, denv) = 
	  fresh_datacon_scheme pos tenv k localvars in
	let rt   = result_type (as_fun tenv) kt in
	let eps  = tycon_name rt 
	and ats  = arg_types (as_fun tenv) kt 
	and ets  = tycon_args rt 
	and tenv = add_type_and_kind_variables denv tenv in

	  (** Inverse the annotation type. *)
	let vars, c, eps', ets' = 
	  let ets' = tycon_args t in
	    if List.length ets' = List.length ets then
	      [], CTrue pos, tycon_name t, ets' 
	    else if ets' = [] then
	      (** The annotation is a type variable. *)
	      let fvs  = List.map (fun v -> variable Flexible ()) ets 
	      and fes  = variable Flexible () in
	      let ets' = List.map (fun v -> TVariable v) fvs 
	      and eps' = TVariable fes in
	      let t' = app eps' ets' in
		fes :: fvs, (t' =?= t) pos, eps', ets' 
	    else raise (TypingError pos)
	in
	  
	  (** Determine assignment for alphas. *)
	let phi = List.fold_left2
	  (fun acu t t' ->
	     match t' with
	       | (TVariable v) when List.memq v alphas ->
		   (* v is an alpha. *)
		   (v, variable ~structure:t Rigid ()) :: acu
	       | _ -> acu)
	  []
	  ets'
	  ets
	in
	let vars = snd (List.split phi) @ vars in
	let c = 
	  conj (List.map (fun (v, v') -> 
			    (TVariable v =?= TVariable v') pos) phi)
	in
	  (** Thread in depth. *)
	let fragment = 
	  if (List.length ps <> List.length ats) then 
	    raise (TypingError pos);
	  join pos (List.map2 infpat ats ps) 
	in

	let rrt = change_arterm_vars phi rt in
	let eqt, is_consistent_theory = 
	  try 
	    add_eq fragment.eq_theory rrt t, true 
	  with InconsistentEqTheory -> MiniEquationalTheory.empty_eq_theory, false
	in
	  { fragment with 
	      tconstraint   = c ^ fragment.tconstraint ^ (eps =?= eps') pos;
	      vars          = vars @ alphas @ fragment.vars;
	      ex_vars       = betas @ fragment.ex_vars;
	      denv          = fragment.denv @ denv;
	      eq_theory     = eqt;
	      eqt_not_false = is_consistent_theory
	  }

    | PData (pos, localvars, k, ps) -> 
	let (alphas, betas, kt, denv) = 
	  fresh_datacon_scheme pos tenv k localvars in
	let rt = result_type (as_fun tenv) kt
	and ats = arg_types (as_fun tenv) kt in
	let fragment = 
	  assert (List.length ps = List.length ats);
	  join pos (List.map2 infpat ats ps) in
	let tenv = add_type_and_kind_variables denv tenv in
	  { fragment with 
	      tconstraint = fragment.tconstraint ^ (t =?= rt) pos;
	      vars        = alphas @ fragment.vars;
	      ex_vars     = betas @ fragment.ex_vars;
	      denv        = fragment.denv @ denv
	  }
  in
    infpat t p

(** Constraint contexts. *)
type context =
    (crterm, variable) type_constraint -> (crterm, variable) type_constraint

let rec dvacc accu = function
  | PVar (_, name) ->
      StringSet.add name accu

  | PWildcard _ ->
      accu

  | PAlias (_, name, p) ->
      dvacc (StringSet.add name accu) p

  | PTypeConstraint (_, p, _) ->
      dvacc accu p

  | PData (_, _, dname, ps) ->
      List.fold_left dvacc accu ps

  | _ -> assert false

(** [dv p] returns the set of program variables defined (bound) by
    the pattern [p]. At the same time, it ensures that the pattern
    is well-formed. *)
let dv p =
  dvacc StringSet.empty p

(** [intern_data_constructor adt_name env_info dcon_info] returns
    env_info augmented by the data constructor typing information 
    is its definition is legal. 
*)
let intern_data_constructor pos vars adt_name env_info dcon_info = 
  let (tenv, acu, lrqs, let_env) = env_info
  and (pos, dname, qs, typ) = dcon_info in
  let all_vars = variables_of_typ typ in
  let vars = List.filter (fun v -> StringSet.mem v all_vars) (vars @ qs) in
  let rqs, rtenv = fresh_unnamed_rigid_vars pos tenv vars in
  let tenv' = add_type_variables rtenv tenv in 
  let ityp = intern pos tenv' typ in
    (* Check that [vars] are the regular type parameters. *)
  let _ = 
    let rtps = rigid_var_args (result_type (as_fun tenv) ityp) in
    let tvars = List.map (fun v -> let (_, v, _) = List.assoc v rtenv in
			    v) vars
    in
      if not (List.for_all (fun v -> List.memq v tvars) rtps) then
	  raise (InvalidDataConstructorDefinition (pos, dname))
  in
  let v = variable ~structure:ityp Flexible () in
    ((add_data_constructor tenv dname (rqs, ityp)),
     (dname, v) :: acu, 
     (rqs @ lrqs),
     StringMap.add dname (ityp, pos) let_env)
	
(** [infer_vdef pos tenv (pos, qs, p, e)] returns the constraint
    related to a value definition. *)
let rec infer_vdef pos eqt tenv (pos, qs, p, e) =
  let x = variable Flexible () in
  let tx = TVariable x in
  let rqs, rtenv = fresh_rigid_vars pos tenv qs in 
  let tenv' = add_type_variables rtenv tenv in
  let fragment = infer_pat_fragment tenv' p tx in 
  let tenv' = add_type_and_kind_variables fragment.denv tenv' in
    Scheme (pos, rqs, x :: fragment.vars, 
	    fl ~pos fragment.ex_vars (
	      fragment.tconstraint 
	      ^ infer_expr eqt tenv' e tx 
	      ^ infer_pat_constraint tenv' p tx),
	    fragment.gamma)

(** [infer_binding tenv b] examines a binding [b], updates the
    typing environment if it binds new types or generates 
    constraints if it binds values. 
*)
and infer_binding (eqt: eq_theory) tenv b =

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
		 | DAlgebraic (vars, ds) ->
		     let (tenv, ids, rqs, let_env) = 
		       List.fold_left
			 (intern_data_constructor pos vars name)
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
		 | _ -> assert false
		     
	  )
	  (tenv, fun c -> c)
	  tds

    | BindValue (pos, vdefs) ->

	let schemes = List.map (infer_vdef pos eqt tenv) vdefs in
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
		   | Implicit (name, e) ->

		       let v = variable Flexible () in
		       let (t : crterm) = TVariable v in
			 
			 schemes1,
		       rvs @ rqs2,
		       v :: fqs2,
		       StringMap.add name (t, pos) h2,
		       infer_expr eqt tenv' e t ^ c2,
		       c1

		   | Explicit (name, vs, typ, e) ->

		       intern_scheme pos tenv name qs typ :: schemes1,
		       rqs2,
		       fqs2,
		       h2,
		       c2,
		       fl rvs (infer_expr eqt tenv' e (intern pos tenv' typ)) 
		       ^ c1

		   | _ -> assert false

	    ) ([], [], [], StringMap.empty, CTrue pos, CTrue pos) vdefs in
	  
	  tenv, 
	fun c -> CLet (schemes1,
		       CLet ([ Scheme (pos, rqs2, fqs2, 
				       CLet ([ monoscheme h2 ], c2), h2) ],
			     c1 ^ c)
		      )
	  
and infer_match (eqt: eq_theory) tenv e t' clauses t =
  infer_expr eqt tenv e t' ^
    conj 
    (List.map 
       (fun (pos, p, e) -> 
	  let fragment = infer_pat_fragment tenv p t' 
	  and cp = infer_pat_constraint tenv p t'  in
	  let tenv = add_type_and_kind_variables fragment.denv tenv in
	  let eqt = join_eq_theory eqt fragment.eq_theory in
	  let subc = 
	    if fragment.eqt_not_false then 
	      infer_expr eqt tenv e t 
	    else 
	      (warning (Positions.string_of_pos pos ^^ ": dead branch.");
	       CTrue pos)
	  in
	    cp 
	    ^ fl ~pos fragment.ex_vars (
	      ex ~pos fragment.vars
		(CLet ([ Scheme (pos, [], [], 
				 fragment.tconstraint, 
				 fragment.gamma) ],
		       subc)))
       )
       clauses)


(** [infer_expr tenv d e t] generates a constraint that guarantees that [e]
    has type [t]. It implements the constraint generation rules for
    expressions. It may use [d] as an equation theory to prove coercion 
    correctness. *)
and infer_expr (eqt: eq_theory) tenv e (t : crterm) =
  match e with

    | EError (pos, pat) -> CFalse pos

    | EExists (pos, vs, e) ->
	let (fqs, denv) = fresh_flexible_vars pos tenv vs in 
	let tenv = add_type_variables denv tenv in
	  ex fqs (infer_expr eqt tenv e t)

    | EForall (pos, vs, e) ->
	let (rqs, denv) = fresh_rigid_vars pos tenv vs in 
	let tenv = add_type_variables denv tenv in
	let beta = variable Flexible () in
	let gt = TVariable beta in
	  CLet ([Scheme (pos, rqs, [beta],
			 infer_expr eqt tenv e gt, 
			 StringMap.singleton "z" (gt, pos)) ],
		("z" <? t) pos)

    | ECoerce (pos, e, vs, t1, t2) ->
	let (fqs, denv) = fresh_flexible_vars pos tenv vs in 
	let tenv = add_type_variables denv tenv in
        let it1 = intern pos tenv t1
        and it2 = intern pos tenv t2 in
          if check_coercion_correctness pos eqt it1 it2 then
	    ex fqs ((it2 =?= t) pos ^ infer_expr eqt tenv e it1)
          else raise (InvalidCoercion (pos, it1, it2))
	    
    | EVar (pos, name) ->
	(name <? t) pos

    | EAssertFalse pos ->
	CTrue pos

    | ELambda (pos, p, e) ->
        (* Allocate fresh type variables [x1] and [x2]. *)
        exists 
	  (* Allocate a fresh type variable for every variable 
	     defined by [p]. *)
	  (fun x1 -> exists 
	     (fun x2 ->
		let fragment = infer_pat_fragment tenv p x1 in
		let tenv = add_type_and_kind_variables fragment.denv tenv in
		  ex fragment.vars (
		    CLet (
		      (* Bind the variables of [p] via 
			 a monomorphic [let] constraint. *)
		      [ monoscheme fragment.gamma ],       
		      (* Require [x1] to be a valid type for [p]. *)
		      infer_pat_constraint tenv p x1 
		      ^ fragment.tconstraint
			(* Require [x2] to be a valid type for [e]. *)
		      ^ infer_expr eqt tenv e x2     
		    )
		  ) ^
		    (t =?= arrow tenv x1 x2) pos         
		    (* Require the expected type [t] to be an arrow 
		       of [x1] to [x2]. *)
             )
          )

    | EApp (pos, e1, e2) ->

	(* TEMPORARY [t] should be made monomorphic *)
	exists (fun x ->
		  infer_expr eqt tenv e1 (arrow tenv x t) ^
  		    infer_expr eqt tenv e2 x
	       )
	  
    | EBinding (_, b, e) ->
	snd (infer_binding eqt tenv b) (infer_expr eqt tenv e t)
	  
    | ETypeConstraint (pos, e, (vs, typ)) ->
	let (fqs, denv) = fresh_flexible_vars pos tenv vs in 
	let tenv = add_type_variables denv tenv in
	let ityp = intern pos tenv typ in
	  ex fqs ((t =?= ityp) pos ^ infer_expr eqt tenv e ityp)

    | EMatch (pos, ETypeConstraint (pos', e, (vs, t')), clauses) ->
	let (fqs, denv) = fresh_flexible_vars pos tenv vs in 
	let tenv = add_type_variables denv tenv in
	let t' = intern pos tenv t' in 
	  ex fqs 
	    (infer_expr eqt tenv e t' ^
	       infer_match eqt tenv e t' clauses t)

    | EMatch (pos, e, clauses) ->
	exists (fun x -> infer_match eqt tenv e x clauses t)

    | EDCon (pos, k, es) ->
	exists_list es 
	  (fun xs -> 
	     let (kt, c) = 
	       List.fold_left (fun (kt, c) (e, x) ->
				 arrow tenv x kt, c ^ infer_expr eqt tenv e x)
		 (t, CTrue pos)
		 (List.rev xs)
	     in
	       c ^ (k <? kt) pos)

    | EPrimApp (pos, c, args) -> 
	
	exists_list args 
	  (fun xs ->
	     let xts = snd (List.split xs) in
	     let ct = 
	       List.fold_left (fun acu x -> arrow tenv x acu) t xts 
	     in
	       (ct =?= type_of_primitive (as_fun tenv) c) pos ^
		 CConjunction (List.map (curry (infer_expr eqt tenv)) xs))

    | ERecordEmpty (pos) -> 
	(t =?= uniform (abs (as_fun tenv))) pos
	  
    | ERecordExtend (pos, bindings, exp) ->
	exists_list bindings 
	  (fun xs -> 
	     exists 
	       (fun x -> 
		  let labels = List.map extract_label_from_binding bindings 
		  and ts = assoc_proj2 xs in
		  let typed_labels = List.combine labels ts in
		    CConjunction 
		      ([
			 (t =?= record_type pos tenv typed_labels x) pos;
			 infer_expr eqt tenv exp x
		       ] @
			 List.map (infer_label eqt tenv) xs
		      ))
	  )

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
		   infer_expr eqt tenv e1 r'; 
		   infer_expr eqt tenv e2 x';
		   (t =?= r) pos
		 ]
	  )

    | ERecordAccess (pos, e1, label) ->
	
	exists (fun x -> 
	     exists (fun y -> 
		       let r = 
			 record_constructor (as_fun tenv) 
			   (rowcons label (pre (as_fun tenv) x) y)
		       in
			 infer_expr eqt tenv e1 r
			 ^ (t =?= x) pos))

and extract_label_from_binding (name, _) = name

and record_type pos tenv xs x =
    let xs = List.map (fun (b, t) -> (b, pre (as_fun tenv) t)) xs in
      (* FIXME: this test should be done by kind inference. *)
      (match are_distinct (fst (List.split xs)) with
	   Some x -> raise (MultipleLabels (pos, x))
	 | _ -> ());
      record_constructor (as_fun tenv) (n_rowcons xs x) 

and extend_record_binding pos tenv l t =
  exists3 
    (fun x x' y -> 
       (n_arrows tenv [ rowcons l x y; x' ] (rowcons l x' y) =?= t) pos
    )
    
and infer_label eqt tenv ((_, exp), t) =
  infer_expr eqt tenv exp t
    
(** [infer e] determines whether the expression [e] is well-typed
    in the empty environment. *)
let infer tenv e =
  exists (infer_expr empty_eq_theory tenv e)
    
(** [bind b] generates a constraint context that describes the
    top-level binding [b]. *)
let bind env b =
  infer_binding empty_eq_theory env b  

let infer_program env = 
  List.fold_left (fun (env, acu) b ->
		    let env, f = bind env b in
		      env, (fun c -> acu (f c)))
    (env, fun c -> c)

(* FIXME: beautify the following piece of code. *)
let init_env () = 
  let builtins = 
    Algebra.init_builtin_env (fun ?name () -> variable Rigid ?name:name ())
  in
  let kind_env = 
    List.fold_left (fun env (n, k) ->
		      add_type_constructor env n
			(k, variable ~name:n Flexible (), ref None))
      empty_environment
      MiniKindInferencer.initial_kind_env 
  in
  let init_ds vars adt_name acu ds = 
    if ds = [] then None, acu 
    else 
      let (env, acu, lrqs, let_env) as r = 
	List.fold_left 
	  (fun acu (d, rqs, ty) -> 
	     intern_data_constructor undefined_position vars adt_name acu 
	       (undefined_position, d, rqs, ty)
	  ) acu ds
      in
	(Some acu, r)
  in
    
  let (init_env, acu, lrqs, let_env) = 
    List.fold_left 
      (fun ((env, dvs, lrqs, let_env) as acu) 
	 (n, (kind, v, vars, ds)) -> 
	   let r = ref None in
	   let env = add_type_constructor env n 
	     (MiniKindInferencer.intern_kind (as_kind_env env) kind,
	      variable ~name:n Constant (),
	      r) in
	   let (dvs, acu) = init_ds vars n (env, dvs, lrqs, let_env) ds in
	     r := dvs;
	     acu
      )
      (kind_env, [], [], StringMap.empty)
      (List.rev builtins)
  in
  let vs = 
    Env.fold_left (fun vs (n, (_, v, _)) -> v :: vs)
      []
      init_env.type_info
  in
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
  c ((snd (infer_program env b)) (Sig.CDump Positions.undefined_position))

let register_tasks () =
  Processing.register
    generate_constraint_task ([], ignore)
    [ [ MiniCoercionInsertion.generate_coercions_task ] ]
    (fun t -> generate_constraint (List.hd t)) 
    (const true)
    
 
