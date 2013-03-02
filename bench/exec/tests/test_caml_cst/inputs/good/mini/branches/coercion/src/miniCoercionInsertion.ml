(* $Id$ *)

open Misc
open MiniAst
open MiniTypes
open Positions
open MiniPrettyPrinter
open Env
open StringSet
open List

let debug = ref false
let do_smart_app = ref false

(** {1 Equational theory.} *)

(** The type of an equational theory.
    Invariants: 
    - Type variables of types in [eqs] are also in [rigids] ;
    - Elements in [eqs] take the form TypVar, _ or _, TypVar ;
    - No cycles in [eqs] (<=> equations are normalizable).
*)
type equational_theory =
    {
      rigids : StringSet.t;
      eqs    : (typ * typ) list
    }

let show_type t =
  as_string print_type t

(** [show_eqt eqt] is a string representation of [eqt]. *)
let show_eqt eqt = 
    print_separated_list "," (fun (t, t') -> show_type t ^ "=" ^ show_type t') 
      eqt.eqs 
    ^ "{" 
    ^ print_separated_list " " (fun x -> x) (StringSet.elements eqt.rigids) 
    ^ "}"
    
(** Invariants. *)

let type_variables_are_rigids eqt = 
  fold_left (fun vs (t, t') -> vs ++ variables_of_typ t ++ variables_of_typ t') empty eqt.eqs
  <<: eqt.rigids

let equations_are_variable_assignments eqt =
  for_all (function TypVar _, _ | _, TypVar _ -> true | _ -> false) eqt.eqs

let is_cycle_equation = function
    TypVar (_, v), TypVar (_, v') -> false
  | (TypVar (_, v), t) | (t, TypVar (_, v)) -> 
      v <: variables_of_typ t
  | _ -> assert false

let eqt_invariants eqt =
  invariant (
    type_variables_are_rigids eqt 
    && equations_are_variable_assignments eqt
    && not (exists is_cycle_equation eqt.eqs)
  ) (show_eqt eqt)

(** Constructors. *)

let empty_eqt = 
  {
    rigids = set_of_list [ "int"; "unit"; "char"; "->"; "*" ];
    eqs    = []
  }
  -- (fun e -> assert (eqt_invariants e))

let join_eqt e1 e2 = 
  assert (eqt_invariants e1 && eqt_invariants e2);
  {
    rigids = e1.rigids ++ e2.rigids;
    eqs    = e1.eqs @ e2.eqs
  }
  -- (fun e -> assert (eqt_invariants e))

(** [is_rigid eqt x] is [true] if [x] is a rigid variable. *)
let is_rigid eqt x = 
  assert (eqt_invariants eqt);
  x <: eqt.rigids 

(** [is_rigid eqt t] is [true] if [t] is a rigid term. *)
let is_rigid_term eqt t = 
  assert (eqt_invariants eqt);
  typ_fold (fun is_r x -> is_r && is_rigid eqt x) true t 

(** Builtin constants. *)
let is_constant v =
  List.mem v [ "int"; "bool"; "unit"; "char" ]

(** Normalization. *)

(** [compare_var v1 v2] is an order between variables where the constants
    are uncomparable and the lowest bounds. *)
let compare_var v1 v2 = 
  assert (not (is_constant v1 && is_constant v2 && v1 <> v2));
  if is_constant v1 then 
    true
  else if is_constant v2 then
    false
  else 
    v1 < v2

(** [more_precise t t'] w.r.t [compare_var]. *)
let more_precise t t' = 
  match t, t' with
    | TypVar (_, v), TypVar (_, v') -> 
	if compare_var v v' then t else t'

    | TypVar _, t | t, TypVar _ -> 
	t

    | _ -> assert false

(** [representant eqt var] is the most precise type of the equivalence 
    class of [var]. *)
let representant eqt var =
  assert (eqt_invariants eqt);
  fold_left (fun cr (u, u') ->
       if u |=| var then 
	 more_precise u' cr
       else if u' |=| var then 
	 more_precise u cr
       else 
	 cr)
    var 
    eqt.eqs

(** [normalize eqt t] is the most precise type equal to [t] modulo
    [eqt]. *)
let rec normalize eqt = function 
    TypVar _ as t -> 
      let r = representant eqt t in
	if not (r |=| t) then 
	  normalize eqt r
	else 
	  r

  | TypApp (pos, t1, t2) -> 
      TypApp (pos, normalize eqt t1, map (normalize eqt) t2)

  | _ -> assert false

let is_eq eqt t t' = 
  assert (eqt_invariants eqt);
  normalize eqt t |=| normalize eqt t'

(** [(x =%= y) eqt] if typ [x] and typ [y] are equal modulo [eqt]. *)
let rec ( =%= ) x y eqt = 
  assert (eqt_invariants eqt);
  typ_fold2 (fun is_eq_m x y -> is_eq_m && (x |=| y || is_eq eqt x y)) true x y
    
(** [(s =!= s') eqt] if shape [s] and shape [s'] are equal modulo [eqt]. *)
let ( =!= ) (_, x) (_, y) eqt = 
  (x =%= y) eqt

(** [InconsistentEqTheory] is raised when the theory is equivalent to
    [false]. *)
exception InconsistentEqTheory of string

(** [add eqt t t'] inserts the equation [t] = [t'] into the equational 
    theory [eqt]. If this insertion implies a cycle into [eqt], 
    the exception [InconsistentEqTheory] is raised. *)
let rec add_eq eqt t t' = 
  assert (eqt_invariants eqt);
  typ_fold2 
    (fun eqt x y -> 	       
       if not ((x =%= y) eqt) && is_rigid_term eqt x && is_rigid_term eqt y then 
	 (let norm_x = normalize eqt x and norm_y = normalize eqt y in
	    if (match norm_x, norm_y with
		  | (TypVar _, _) | (_, TypVar _) -> 
		      is_cycle_equation (norm_x, norm_y)
		  | _ -> true) 
	    then 
	      raise (InconsistentEqTheory ((show_eqt eqt) ^"+"^(show_type t)
					   ^"="^(show_type t')))
	    else 
	      { eqt with eqs = (norm_x, norm_y) :: eqt.eqs })
       else 
	 eqt
    ) eqt t t'
  -- (fun e -> assert (eqt_invariants e))

(** [insert_rigids eqt rigids] introduces the [rigids] variables in [eqt]. *)
let insert_rigids eqt rigids =
  assert (eqt_invariants eqt);
  { eqt with rigids = eqt.rigids +! (set_of_list rigids) }
  -- (fun e -> assert (eqt_invariants e))

let show_shape (vars, et) = 
  print_separated_list " " (fun x -> x) vars ^"."
  ^ as_string print_type et

(** {1 Type approximation.} *)
module Fresher = FreshVars

(** The type approximation are of type [shape]. The first component is
    the flexible variables set. The invariant is :
    - depending on the current equational theory 
    - the free variable of the shape is either in the first component
    or a rigid variable. *)
type shape = string list * typ

let shape_invariants eqt ((vars, typ) as shape) =
  let flexs = set_of_list vars in
    invariant (
      flexs &+ eqt.rigids = empty && variables_of_typ typ <<: (flexs ++ eqt.rigids)
    ) (show_shape shape)

let flexibles_of (vs, _) =
  vs

let type_of (_, typ) = 
  typ

let fresh_var () = 
 "f"^(Fresher.string_of (Fresher.fresh_var ()))

let wildcard () =
  let v = fresh_var () in
    ([v], TypVar (Positions.undefined_position, v))

let disjoint_vars vars1 vars2 = 
  (set_of_list vars1) &+ (set_of_list vars2) = empty

let normalize_shape eqt ((vars, t) as s) = 
  assert (eqt_invariants eqt && shape_invariants eqt s); 
  (vars, normalize eqt t)
  -- fun s' -> assert (shape_invariants eqt s' && (s =!= s') eqt) 

let ( |~| ) (_, t1) (_, t2) = 
  t1 |=| t2

(** Unification of type approximations. *)

type substitution = (string * typ) list

let img theta v =
  try
    List.assoc v theta 
  with Not_found -> TypVar (undefined_position, v)

let rec subst theta = function
    TypVar (p, v) as t -> img theta v 
  | TypApp (p, t1, t2) -> TypApp (p, subst theta t1, map (subst theta) t2)
  | _                  -> assert false

let rec fresh_subst vars = 
  map (fun v -> (v, TypVar (undefined_position, fresh_var ()))) vars
  
let rename (vars, t) =
  let s = fresh_subst vars in
  let vars' = map (function TypVar (_, v) -> v | _ -> assert false) (snd (List.split s)) in
    (vars', subst s t)

let close_typ eqt typ =
  let vars = StringSet.diff (variables_of_typ typ) eqt.rigids in
    rename (StringSet.elements vars, typ)

let close eqt (vars, typ) = 
  close_typ eqt typ

let subst_shape eqt theta (vars, t') =
  close_typ eqt (subst theta t')

let var_of v t =
  v <: (variables_of_typ t)

let rec unify eqt t1 t2 =
(*  let _ = Printf.eprintf "%s |- %s =?= %s\n" 
    (show_eqt eqt)
    (as_string print_type t1)
    (as_string print_type t2)
  in
*)
  assert (eqt_invariants eqt);
  if (t1 =%= t2) eqt then []
  else 
    match t1, t2 with
	TypVar (_, v1), TypVar (_, v2) 
	  when not (is_rigid eqt v1 && is_rigid eqt v2) ->
	    (if is_rigid eqt v1 then [] else [(v1, t2)]) 
	    @ (if is_rigid eqt v2 then [] else [(v2, t1)])
	      
      | TypVar (_, v), t | t, TypVar (_, v) 
	  when not (is_rigid eqt v || var_of v t) ->
	  [ (v, t) ]
	    
      | TypApp (_, t1, t1s), TypApp (_, t2, t2s) 
	  when (List.length t1s = List.length t2s) ->
	  List.fold_left2 
	    (fun acu t1 t2 -> 
	       let theta' =  unify eqt (subst acu t1) (subst acu t2) in
		 List.map (fun (x, t) -> (x, subst theta' t)) acu @ theta')
	    (unify eqt t1 t2)
	    t1s
	    t2s

      | _ -> 
	  failwith ("Type error: unification failed for "^
		      (as_string print_type t1) ^ " and " ^
		      (as_string print_type t2))
		    
let unify_shape eqt ((vars1, t1) as s1) ((vars2, t2) as s2) = 
  assert (eqt_invariants eqt 
		&& shape_invariants eqt s1
		&& shape_invariants eqt s2);
  unify eqt t1 t2
  -- fun phi -> assert (subst_shape eqt phi s1 |~| subst_shape eqt phi s2)

let merge_shape eqt s1 s2 =
  let phi = unify_shape eqt s1 s2 in
  let r = subst_shape eqt phi s1 in
(*    Printf.eprintf "%s |- %s =?= %s      %s\n%!" 
      (show_eqt eqt)
      (show_shape s1) (show_shape s2) (show_shape r); *)
    r


let merge eqt t1 t2 =
  let phi = unify eqt t1 t2 in
    subst phi t1

(** Pruning **)

(* eqt' = eqt_1 /\ eqt_2 and eqt = eqt_1 *)
let is_head_ambiguous eqt eqt' t = 
  let merge_if_possible eqt acu (t1, t2) = 
    try
      let _ = merge eqt t t1 in
	t1 :: t2 :: acu
    with _ -> try 
      let _ = merge eqt t t2 in t1 :: t2 :: acu
    with _ -> acu
  in
  let instance_e = List.fold_left (merge_if_possible eqt) [] eqt.eqs
  and instance_e_e' = List.fold_left (merge_if_possible eqt') [] eqt'.eqs
  in
(*  let _ = 
    Printf.eprintf "%s:  %s\n" 
      (show_eqt eqt)
      (print_separated_list "," (as_string print_type) instance_e);
    Printf.eprintf "%s: %s\n" 
      (show_eqt eqt')
      (print_separated_list "," (as_string print_type) instance_e_e')
			       in *)
  let r = 
    if List.exists (fun t -> List.for_all (fun t' -> not (t |=| t')) instance_e) 
      instance_e_e' then (
      if !debug then Printf.eprintf "%s is ambiguous in %s wrt to %s\n"
	(as_string print_type t) (show_eqt eqt) (show_eqt eqt');
      true
    )
    else
      false
  in
(*    Printf.eprintf "Is %s ambiguous ? %B\n" 
      (as_string print_type t) r; *)
    r

let prune eqt eqt' (_, t) = 
  let rec prune_typ t = 
    if is_head_ambiguous eqt eqt' t then
      snd (wildcard ())
    else 
      typ_map prune_typ t 
  in
    close_typ eqt (prune_typ t)

(** Syntactic shortcuts. *)

let mkVar v = 
  TypVar (undefined_position, v)

let arrow eqt (vars1, t1) (vars2, t2) = 
  close_typ eqt (TypApp (undefined_position, mkVar "->", [ t1; t2 ]))

let app eqt (vars1, t1) s2 =
  let t2 = snd (List.split s2) in
    close_typ eqt (TypApp (undefined_position, t1, t2))

let rec closed_as_arrow eqt (vars, typ) =
  (* is the type an arrow ? *)
  match typ with
      TypApp (_, TypVar (_, "->"), [ s1; s2 ]) -> 
	close_typ eqt s1, close_typ eqt s2

    | TypVar (_, v) when not (is_rigid eqt v) ->
	wildcard (), wildcard ()

    | _ -> 
	let norm_typ = normalize eqt typ in
	  if norm_typ <> typ then
	    closed_as_arrow eqt (vars, norm_typ)
	  else 
	    failwith "Type error : arrow expected."

let domain eqt s = 
  fst (closed_as_arrow eqt s)

let codomain eqt s = 
  snd (closed_as_arrow eqt s)

let rec type_of_arrow eqt (vars, typ) =
  (* is the type an arrow ? *)
  match typ with
      TypApp (_, TypVar (_, "->"), [ s1; s2 ]) -> 
	s1, s2

    | TypVar (_, v) when not (is_rigid eqt v) ->
	typ, typ

    | _ -> 
	let norm_typ = normalize eqt typ in
	  if norm_typ <> typ then
	    type_of_arrow eqt (vars, norm_typ)
	  else 
	    failwith "Type error : arrow expected."



let var_name = function
  | TypVar (_, v) -> v
  | _ -> assert false
      
let annotation = function
    ETypeConstraint (_, _, t) -> t
  | _ -> assert false

let annotated = function
    ETypeConstraint (_, e, _) -> e
  | _ -> assert false
    
let annot pos e s = 
  ETypeConstraint (pos, e, s)

let explode_ae = function
    ETypeConstraint (_, e, t) -> (e, t)
  | _ -> assert false

let lookup env k = 
  try
    Env.lookup env k
  with Not_found -> failwith (Printf.sprintf ("Unbound '%s'.") k)

exception DeadBranch

let rec extract_fragment (env, eqt) et = function

  | PVar (pos, x) ->
      ([], PVar (pos, x), Env.add env x et, eqt)

  | PData (pos, betas, d, ps) ->

      (* Look for the data constructor's scheme. *)
      let (vars, t) = lookup env d in

	(* Extract its local type variables as fresh variables.  *)
      let lvars      = List.filter (fun v -> v.[0] = '\'') vars in
      let fresher    = 
	if betas = [] then 
	  List.map (fun v -> (v, mkVar (fresh_var ()))) lvars 
	else 
	  List.map2 (fun v b -> (v, mkVar b)) lvars betas
      in
      let fresh_vars = List.map var_name (assoc_proj2 fresher) in
      let t          = subst fresher t in

	(* Insert the type variables into the rigid variables of 
	   the equational theory. *)
      let eqt' = { eqt with rigids = 
	  StringSet.strict_union (set_of_list fresh_vars) eqt.rigids } 
      in
	
	(* Instantiate the scheme wrt the expected type. *)
      let vars, ft   = close_typ eqt' t in
      let lrft, ett  = type_of_last_result ft, snd et in

	(* Deduce the new equational theory. *)
      let eqt'       = 
	try 
	  List.fold_left 
	    (fun eqt' (v, t) -> add_eq eqt' (mkVar v) t) eqt' 
	    (unify empty_eqt lrft ett)
	with _ -> raise DeadBranch
      in

	(* Instantiate the flexible variables. *)
      let phi        = unify eqt' lrft ett in 
      let args_types = type_of_args ft in
      let pts        = List.map (fun t -> close_typ eqt' (subst phi t))args_types
      in
	if (List.length ps <> List.length pts) then
	  failwith (string_of_pos pos^": invalid pattern.");

	(* Determine subpattern's contributions. *)
	let (sbetas, pats, env, eqt) = 
	  List.fold_left2 
	    (fun (betas, pats, env, eqt) p s -> 
	       let betas', pat, env, eqt = extract_fragment (env, eqt) p s
	       in betas @ betas', pat :: pats, env, eqt) 
	    (fresh_vars, [], env, eqt') pts ps
	in
	  (sbetas, 
	   PData (pos, fresh_vars, d, List.rev pats), 
	   env, eqt)
		 
  | PTypeConstraint (pos, p, at) ->
      (* FIXME: add [et]'s contribution. *)
      extract_fragment (env, eqt) at p 

  | PWildcard _ as p ->
      ([], p, env, eqt)
	
  | _ -> 
      assert false

let pat_var = function
    PTypeConstraint (pos, ((PVar _) as p), _) -> p
  | PVar _ as p -> p
  | _ -> failwith "pattern variable expected"

let pat_annot = function
    PTypeConstraint (pos, p, at) -> at
  | _                            -> wildcard ()
    
let annot_if_present = function
    ETypeConstraint (_, e, s) -> s, e
  | e -> wildcard (), e

let normalized_pat eqt = function
    PTypeConstraint (pos, p, at) -> 
      PTypeConstraint (pos, p, normalize_shape eqt at)
  | x                            -> x

  
let normalization_coercion pos eqt exp s =
  let ns = normalize_shape eqt s in
    if not ((type_of s) |=| (type_of ns)) then
      ECoerce (pos, exp, flexibles_of s, type_of s, type_of ns)
    else
      exp

let inv_normalization_coercion pos eqt exp s =
  let ns = normalize_shape eqt s in
    ECoerce (pos, exp, flexibles_of s, type_of ns, type_of s)

let app_shape eqt s1 s2 = 
  snd (closed_as_arrow eqt (merge_shape eqt s1 (arrow eqt s2 (wildcard ()))))


(** {1 System W} *)

let rec as_wt_annot_binding eqt env = function
  | BindValue (pos, vs) -> 
      let avs, env = 
	fold_left (fun (avs, env) (pos, rigids, pat, exp) -> 
	     let eqt'            = insert_rigids eqt rigids in
	     let av, (vs,t)      = as_wt_annot_infer eqt' env exp in
	     let s               = rename (vs @ rigids, t) in
	     let _, _, env', eqt = extract_fragment (env, eqt) s pat
	     in
	       ((pos, rigids, pat, av) :: avs, Env.concat env env')
	  ) ([], env) vs
      in
	BindValue (pos, List.rev avs), (env, eqt)

  | BindRecValue (pos, vs) ->
      (* FIXME: uncorrect for mutually recursive values. *)
      let avs, env = 
	fold_left (fun (avs, env) (pos, rigids, pat, exp) -> 
	     let name, ((vs, t) as s), _, check =
	       match explicit_or_implicit pat exp with
		   Explicit (name, vs, typ, e) -> 
		     (name, (vs, typ), annot pos e (vs, typ), true)
		 | Implicit (name, e) -> 
		     (name, wildcard (), e, false)
		 | _ -> failwith "Invalid let rec"
	     in
	     let eqt'      = insert_rigids eqt rigids in
	     let env       = Env.add env name (rename (rigids @ vs, t)) in
	     let av, s     = 
	       if check then 
		 as_wt_annot_check eqt' env exp s, s
	       else 
		 as_wt_annot_infer eqt' env exp
	     in
	     let aav =
	       if is_rigid_term eqt (type_of s) then
		 annot pos av s
	       else 
		 av
	     in
	       ((pos, rigids, pat, aav) :: avs, env)
	  ) ([], env) vs
      in
	BindRecValue (pos, List.rev avs), (env, eqt)

  | TypeDec (pos, ts) as td ->
      td, List.fold_left 
	(fun (env, eqt) ->
	   (function _, _, tname, DAlgebraic (rqs, ds) ->
	      let eqt = { eqt with rigids = StringSet.add tname eqt.rigids }
	      in
		(fold_left (fun env (_, dname, lrqs, t) -> 
		      let ls = map (fun v -> (v,TypVar (pos, "'"^v))) lrqs 
		      and s = map (fun v -> (v, TypVar (pos, "_"^v))) rqs 
		      in
		      let vars = 
			map var_name 
			  (snd (List.split ls) @ (snd (List.split s))) in
			(dname, (vars, subst (ls @ s) t)) :: env)
		   env ds, eqt)
		  
	      | _ -> (env, eqt))) (env, eqt) ts

and as_wt_annot_infer eqt env exp =
  match exp with
    
    | EVar (pos, x) -> 
	let s = rename (lookup env x) in
	  (normalization_coercion pos eqt exp s, normalize_shape eqt s)

    | ELambda (pos, pat, e) ->
	let norm_pat_annot = normalize_shape eqt (pat_annot pat) in
	let _, _, env, _ = 
	  extract_fragment (env, eqt) norm_pat_annot (pat_var pat)
	in
	let e', s  = as_wt_annot_infer eqt env e in 
	  (ELambda (pos, normalized_pat eqt pat, e'), 
	   arrow eqt norm_pat_annot s)

    | EApp (pos, e1, e2) ->
	if !do_smart_app then
	  (* Implement smart-app *)
	  let e1', s = as_wt_annot_infer eqt env e1 in
          let s1, s2 = closed_as_arrow eqt s in
	  let t1, t2 = type_of s1, type_of s2 in
          let e2', s' = as_wt_annot_infer eqt env e2 in
          let phi = unify eqt t1 (type_of s') in
          let rs = close_typ eqt (subst phi t2) in
            (EApp (pos, e1', e2'), 
	     codomain eqt (merge_shape eqt (arrow eqt s' s2) s))
	else
	  (* Simple application rule. *)
	  let t1, s = as_wt_annot_infer eqt env e1 in
	  let t2 = as_wt_annot_check eqt env e2 (domain eqt s) in
	    (EApp (pos, t1, t2), codomain eqt s)

    | EDCon (pos, k, es) ->
	let vars,t as s = rename (lookup env k) in
	let _   = assert (List.length es = List.length (type_of_args t)) in
	let ts  = List.map (close_typ eqt) (type_of_args t) in
	let es' = List.map2 (as_wt_annot_check eqt env) es ts in
	  (EDCon (pos, k, es'), close_typ eqt (type_of_last_result t))

    | EPrimApp (pos, PIntegerConstant _, []) ->
	(exp, ([], mkVar "int"))

    | EPrimApp (pos, PCharConstant _, []) ->
	(exp, ([], mkVar "char"))

    | EPrimApp (pos, PUnit, []) ->
	(exp, ([], mkVar "unit"))

    | EMatch (pos, e, cs) -> 
	let e', sm = as_wt_annot_infer eqt env e in
	let cs'    = 
	  List.map (as_wt_annot_clause_check eqt env sm (wildcard ())) cs in
	  (EMatch (pos, annot pos e' sm, cs'), wildcard ())

    | ETypeConstraint (pos, e, t) ->
	let nt = normalize_shape eqt t in
	  (ETypeConstraint (pos, as_wt_annot_check eqt env e nt, nt), nt)

    | EBinding (pos, b, e) ->
	let (ab, (env, eqt)) = as_wt_annot_binding eqt env b in
	let at, et = as_wt_annot_infer eqt env e in
	  (EBinding (pos, ab, at), et)

    | ECoerce (pos, e, vars, t1, t2) ->
	let t = merge eqt (normalize eqt t1) (normalize eqt t2) in
	  as_wt_annot_infer eqt env (ETypeConstraint (pos, e, close_typ eqt t))

    | EForall (pos, vars, e) ->
	let eqt = insert_rigids eqt vars in
	let e', (vars', t) = as_wt_annot_infer eqt env e in
	  (EForall (pos, vars, e'), (vars @ vars', t))

    | x -> (x, wildcard ())

and as_wt_annot_check eqt env exp et : expression = 

  match exp with

    | EVar (pos, x) ->
	normalization_coercion pos eqt exp (rename (lookup env x))

    | ELambda (pos, pat, e) ->
	let pat_typ = pat_annot pat in
	let norm_pat_typ = normalize_shape eqt pat_typ in
	let s' = merge_shape eqt et (arrow eqt norm_pat_typ (wildcard ())) in
	let s1, s2  = closed_as_arrow eqt s' in
	let _, _, env, _  = extract_fragment (env, eqt) s1 (pat_var pat) in
	let e'      = as_wt_annot_check eqt env e s2 in
	  ELambda (pos, normalized_pat eqt pat, e')

    | EPrimApp _ ->
	exp

    | EApp (pos, e1, e2) ->
	if !do_smart_app then
	  (* Implement smart-app *)
	  let e1', s  = as_wt_annot_infer eqt env e1 in
	  let s = merge_shape eqt (arrow eqt (wildcard ()) et) s in
          let s1, s2  = closed_as_arrow eqt s in
	  let t1, t2 = type_of s1, type_of s2 in
          let e2', s' = as_wt_annot_infer eqt env e2 in
            EApp (pos, e1', e2')
	else
	  let e1', s = as_wt_annot_infer eqt env e1 in
	  let s1, s2 = closed_as_arrow eqt s in
	  let e2'    = as_wt_annot_check eqt env e2 s1 in
	    EApp (pos, e1', e2')
	    
    | EDCon (pos, k, es) ->
	let vars,t as s = rename (lookup env k) in
	let _   = assert (List.length es = List.length (type_of_args t)) in
	let phi = unify eqt (type_of_last_result t) (type_of et) in
	let ts  = List.map ((close_typ eqt) $ (subst phi)) (type_of_args t) in
	let es' = List.map2 (as_wt_annot_check eqt env) es ts in
	  EDCon (pos, k, es')

    | EMatch (pos, e, cs) -> 
	let e', sm = as_wt_annot_infer eqt env e in
	let cs'    = List.map (as_wt_annot_clause_check eqt env sm et) cs in
	  EMatch (pos, annot pos e' sm, cs')

    | ETypeConstraint (pos, e, t) ->
	let nt = normalize_shape eqt t in
	  ETypeConstraint (pos, as_wt_annot_check eqt env e nt, nt)

    | EBinding (pos, b, e) ->
	let (ab, (env, eqt)) = as_wt_annot_binding eqt env b in
	  EBinding (pos, ab, as_wt_annot_check eqt env e et)
	    
    | ECoerce (pos, e, vars, t1, t2) ->
	let t = merge eqt (normalize eqt t1) (normalize eqt t2) in
	  as_wt_annot_check eqt env (ETypeConstraint (pos, e, close_typ eqt t))
	    et

    | EForall (pos, vars, e) ->
	let eqt = insert_rigids eqt vars in
	  EForall (pos, vars, as_wt_annot_check eqt env e et)

    | x -> x

and as_wt_annot_clause_check eqt env it ot (pos, pat, exp) =
  try
    let bs, pat, env', eqt' = extract_fragment (env, eqt) it pat in
    let norm_ot = normalize_shape eqt' ot in
    let e' = as_wt_annot_check eqt' (Env.concat env env') exp norm_ot
    in
      (pos, pat, inv_normalization_coercion pos eqt' e' ot) 
  with DeadBranch -> (pos, pat, exp)

(** {1 System Z} *)
let rec z_annot_binding eqt env = function
  | BindValue (pos, vs) -> 
      let avs, env = 
	fold_left (fun (avs, env) (pos, rigids, pat, exp) -> 
	     let eqt'           = insert_rigids eqt rigids in
	     let et =
	       match explicit_or_implicit pat exp with
		   Explicit (name, vs, typ, e) ->
		     normalize_shape eqt (rename (rigids @ vs, typ))
		 | _ ->
		     (wildcard ())
	     in 
	     let av, ((vs, t) as s) = z_annot eqt' env et exp in
	     let _, av		    = annot_if_present av in 
	     let s_env              = rename (vs @ rigids, t) in
	     let _, _, env', eqt    = extract_fragment (env, eqt) s_env pat
	     in
	       ( (pos, rigids, pat, (annot pos av s)) :: avs, 
		 Env.concat env env')
	  ) ([], env) vs
      in
	BindValue (pos, List.rev avs), (env, eqt)

  | BindRecValue (pos, vs) ->
      (* FIXME: uncorrect for mutually recursive values. *)
      let avs, env = 
	fold_left (fun (avs, env) (pos, rigids, pat, exp) -> 
	     let name, ((vs, t) as s)  =
	       match explicit_or_implicit pat exp with
		   Explicit (name, vs, typ, e) -> 
		     (name, (vs, typ))
		 | Implicit (name, e) -> 
		     (name, wildcard ())
		 | _ -> failwith "Invalid let rec"
	     in
	     let eqt'      = insert_rigids eqt rigids in
	     let s         = rename (rigids @ vs, t) in
	     let env       = Env.add env name s in
	     let av, s     = z_annot eqt' env (normalize_shape eqt' s) exp in
	       ((pos, rigids, pat, av) :: avs, env)
	  ) ([], env) vs
      in
	BindRecValue (pos, List.rev avs), (env, eqt)

  | TypeDec (pos, ts) as td ->
      td, List.fold_left 
	(fun (env, eqt) ->
	   (function _, _, tname, DAlgebraic (rqs, ds) ->
	      let eqt = { eqt with rigids = StringSet.add tname eqt.rigids }
	      in
		(fold_left (fun env (_, dname, lrqs, t) -> 
		      let ls = map (fun v -> (v,TypVar (pos, "'"^v))) lrqs 
		      and s = map (fun v -> (v, TypVar (pos, "_"^v))) rqs 
		      in
		      let vars = 
			map var_name 
			  (snd (List.split ls) @ (snd (List.split s))) in
			(dname, (vars, subst (ls @ s) t)) :: env)
		   env ds, eqt)
		  
	      | _ -> (env, eqt))) (env, eqt) ts

and z_annot eqt env et exp = 
  if !debug then 
    Printf.eprintf "Up for \"%s\" \n  with shape \"%s\"\n%!"
      (as_string print_expression exp)
      (as_string print_type (snd et));
  let u, s = 
  match exp with
      
    | EVar (pos, x) ->
	let s = rename (lookup env x) in
	let s' = merge_shape eqt et (normalize_shape eqt s) in
	  (normalization_coercion pos eqt exp s, s')

    | ELambda (pos, pat, e) ->
	let annot = normalize_shape eqt (pat_annot pat) in
	let s' = merge_shape eqt et (arrow eqt annot (wildcard ())) in
	let s1, s2 = closed_as_arrow eqt s' in
	let _, _, env', _ = extract_fragment (env, eqt) s1 (pat_var pat) in
	let u, s'' = z_annot eqt (Env.concat env' env) s2 e in
	  (ELambda (pos, normalized_pat eqt pat, u), 
	   merge_shape eqt (arrow eqt (wildcard ()) s'') s')
	    
    | EPrimApp (pos, PIntegerConstant _, []) ->
	(exp, ([], mkVar "int"))

    | EPrimApp (pos, PCharConstant _, []) ->
	(exp, ([], mkVar "char"))

    | EPrimApp (pos, PUnit, []) ->
	(exp, ([], mkVar "unit"))

    | EApp (pos, e1, e2) ->
	let s1, e1 = annot_if_present e1
	and s2, e2 = annot_if_present e2 in
	let s' = merge_shape eqt s1 (arrow eqt s2 et) in
	let u1, s1 = z_annot eqt env s' e1  in
	let u2, s2 = z_annot eqt env (domain eqt s1) e2 in
	let s = app_shape eqt s1 s2 in
	  (EApp (pos, annot pos u1 s1, annot pos u2 s2), s)

    | EDCon (pos, k, es) ->
	let vars,t as s = rename (lookup env k) in
	let _   = assert (List.length es = List.length (type_of_args t)) in
	let phi = unify eqt (type_of_last_result t) (type_of et) in
	let ts  = List.map ((close_typ eqt) $ (subst phi)) (type_of_args t) in
	let es' = List.map2 (fun t e -> fst (z_annot eqt env t e)) ts es in
	  (EDCon (pos, k, es'), close_typ eqt (subst phi (type_of et)))

    | EMatch (pos, e, cs) -> 
	let sm, e = annot_if_present e in
	let (u, s) = z_annot eqt env sm e in
	let cs', ss' = List.split (
	  List.map (fun (pos, pat, e) -> 
		    try 
		      let bs, pat, env', eqt' = 
			extract_fragment (env, eqt) s pat in
		      let norm_et = normalize_shape eqt' et in
		      let u, s = z_annot eqt' env' norm_et e in
		      let ps = prune eqt eqt' s in
		      let s'' = merge_shape eqt ps et in
		      let u' = inv_normalization_coercion pos eqt' u et in
			(pos, pat, u'), ps
		    with DeadBranch -> (pos, pat, e), wildcard ())
	    cs)
	in
	let sc = List.fold_left (merge_shape eqt) (wildcard ()) ss' in
	  (EMatch (pos, annot pos u s, cs'), 
	   merge_shape eqt sc et)

    | ETypeConstraint (pos, e, t) ->
	let norm_t = normalize_shape eqt t in
	let u, s   = z_annot eqt env (merge_shape eqt et norm_t) e in
	  (ETypeConstraint (pos, u, norm_t), s)

    | EBinding (pos, b, e) ->
	let ab, (env, eqt) = z_annot_binding eqt env b in
	let u, s = z_annot eqt env et e in
	  EBinding (pos, ab, u), s
	    
    | ECoerce (pos, e, vars, t1, t2) ->
	let s = close_typ eqt (merge eqt (normalize eqt t1) (normalize eqt t2)) 
	in
	  z_annot eqt env s e
  in
    if !debug then 
      Printf.eprintf "=> \"%s\" \n  Inferred shape \"%s\"\n"
	(as_string print_expression u)
	(as_string print_type (snd s));
    u, s
    
let rec z_annot_binding_iter k show eqt env b =
  let rec iter k ((b, _) as a) =
    if k = 0 then a
    else 
      iter (k-1) (z_annot_binding eqt env b)
  in
    iter k (b, (env, eqt))

let initial_env = 
  let up = undefined_position in
  let arrow_type pos t1 t2 =
    TypApp (pos, TypVar (pos, "->"), [ t1; t2 ])
  and tuple_type2 pos t1 t2 = 
    TypApp (pos, TypVar (pos, "*"), [ t1; t2 ])
  and gen_tvar v = TypVar (up, v) in
    [ "_Tuple", 
      ([ "_a"; "_b" ], 
       arrow_type up (gen_tvar "_a") (arrow_type up (gen_tvar "_b")
				       (tuple_type2 up
					  (gen_tvar "_a") 
					  (gen_tvar "_b"))))
    ]

type inference_mode =
  | Wobbly 
  | Z

let generate_coercions show nb_pass preprocessing mode prog =
  let annot_binding = 
    match mode with
	Some Wobbly   -> as_wt_annot_binding 
      | Some Z        -> z_annot_binding_iter nb_pass show
      | None          -> fun eqt env b -> (b, (env, eqt))
  in
  if !show then 
    print_program 
      (List.rev_map (fun x -> x)
	 (fst (List.fold_left
		 (fun (abs, (env, eqt)) b -> 
		    let ab, (env, eqt) = annot_binding eqt env b in
		      (ab :: abs, (env, eqt)))
		 ([], (initial_env, empty_eqt))
		 prog
	      )));   
  if !preprocessing then 
    List.rev (fst (fold_left
		     (fun (abs, (env, eqt)) b -> 
			let ab, (env, eqt) = annot_binding eqt env b in
			  (ab :: abs, (env, eqt)))
		     ([], (initial_env, empty_eqt))
		     prog
		  ))
  else prog

let generate_coercions_task = "generate-coercions"

let register_tasks parse_program_task =
  let mode = ref None in
  let nb_pass = ref 2 in
  let preprocessing = ref false  
  and show_preprocessing = ref false 
  and set_mode = function
      "wobbly"  -> mode := Some Wobbly
    | "z"       -> mode := Some Z
    | _         -> mode := None
  in
  Processing.register
    generate_coercions_task 
    ([
       "--insert-coercions", Arg.Set preprocessing,
       " Enable local inference.";
       "--show-coercions", Arg.Set show_preprocessing,
       " Show inserted coercion.";
       "--nb-iter", Arg.Set_int nb_pass, 
       " Number of iteration (z mode only).";
       "--do-smart-app", Arg.Set do_smart_app,
       " Smart handling of application (wobbly mode only).";
       "--mode", Arg.Symbol ([ "wobbly"; "z" ], set_mode), 
       " Local inference mode can be \"wobbly\" or \"2-steps\".";
       "--debug-local-inference", Arg.Set debug, 
       " Enable debug mode for local inference."
     ], ignore)
    [ [ parse_program_task ] ]
    (fun t -> 
       if !show_preprocessing then preprocessing := true;
       generate_coercions show_preprocessing 
	 (!nb_pass)
	 preprocessing 
	 (!mode) (List.hd t)) 
    (const true)
