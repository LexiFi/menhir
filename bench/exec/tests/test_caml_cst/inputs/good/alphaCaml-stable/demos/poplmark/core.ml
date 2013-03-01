(*
 *  An evaluator and typechecker. Parts taken from the fullfsub implementation
 *  by the POPLmark team.
 *)

open Strings
open Fsub

(* -------------------------------------------------------------------------- *)
(* Type substitutions. *)

type type_subst =
    type_expr Type_var.AtomMap.t

let type_identity : type_subst =
  Type_var.AtomMap.empty

let type_singleton tyX tyT : type_subst =
  Type_var.AtomMap.singleton tyX tyT

class typeSubst (subst : type_subst) = object

  inherit map

  method tvar tyX =
    try
      Type_var.AtomMap.lookup tyX subst
    with Not_found ->
      TVar tyX

end

let typeSubst subst tyT =
  (new typeSubst subst)#type_expr tyT

let singleTypeSubst tyX tyT =
  typeSubst (type_singleton tyX tyT)
  
(* -------------------------------------------------------------------------- *)
(* Term substitutions. *)

type term_subst =
    term_expr Term_var.AtomMap.t

let term_identity : term_subst =
  Term_var.AtomMap.empty

let term_singleton x t : term_subst =
  Term_var.AtomMap.singleton x t

class termSubst (type_subst : type_subst) (term_subst : term_subst) = object

  inherit typeSubst (type_subst)

  method evar x =
    try
      Term_var.AtomMap.lookup x term_subst
    with Not_found ->
      EVar x

end

let termSubst type_subst term_subst t =
  (new termSubst type_subst term_subst)#term_expr t

let singleTermSubst x t =
  termSubst type_identity (term_singleton x t)
  
(* -------------------------------------------------------------------------- *)
(* Evaluation. *)

(* This evaluator is intended for use with closed terms -- this is
   evident in the fact the variables, or terms that contain a variable
   in an evaluation position, are not considered values.

   Yet, the code in [Main] actually calls the evaluator with open
   terms as arguments -- this happens when the user declares term
   variables without providing their value, see [TopTermBind].

   This is really a bug, but this code is only a demo, so I am not
   going to fix it for the moment. *)

exception NoRuleApplies

let rec isval = function
  | EAbs _ ->
      true
  | ERecord fields ->
      StringMap.fold (fun l ti result -> result && isval ti) fields true
  | ETyAbs _ ->
      true
  | _ ->
      false

let rec patSubst p v =
  match p, v with
  | PWildcard, _ ->
      term_identity
  | PVar (x, _), _ ->
      term_singleton x v
  | PRecord pfields, ERecord vfields ->
      StringMap.fold (fun li pi subst ->
	try
	  let vi = StringMap.find li vfields in
	  (* Patterns are assumed to be linear, so this is disjoint
	     union. *)
	  Term_var.AtomMap.union subst (patSubst pi vi)
	with Not_found ->
	  raise NoRuleApplies
      ) pfields term_identity
  | PRecord _, _ ->
      raise NoRuleApplies

let rec eval1 = function
  | EApp (EAbs abs1, v2) when isval v2 ->
      let x1, tyT1, t1 = open_term_term_abs abs1 in
      singleTermSubst x1 v2 t1
  | EApp (v1, t2) when isval v1 ->
      EApp (v1, eval1 t2)
  | EApp (t1, t2) ->
      EApp (eval1 t1, t2)
  | ERecord fields ->
      (* Fields are evaluated in the order they are stored
	 in memory, which may not reflect the order found in
	 the program text. *)
      let found = ref false in
      ERecord (StringMap.map (fun ti ->
	if !found || (isval ti) then
	  ti
	else begin
	  found := true;
	  eval1 ti
	end
      ) fields)
  | EProj ((ERecord fields as v1), l) when isval v1 ->
      begin
	try
	  StringMap.find l fields
	with Not_found ->
	  raise NoRuleApplies
      end
  | EProj (t, l) ->
      EProj (eval1 t, l)
  | ELet abs ->
      let p, t1, t2 = open_pat_term_abs abs in
      if isval t1 then
	termSubst type_identity (patSubst p t1) t2
      else
	ELet (create_pat_term_abs (p, eval1 t1, t2))
  | ETyApp (ETyAbs abs, tyT) ->
      let tyX, _, t = open_type_term_abs abs in
      termSubst (type_singleton tyX tyT) term_identity t
  | ETyApp (t, tyT) ->
      ETyApp (eval1 t, tyT)
  | _ ->
      raise NoRuleApplies

let rec eval t =
  try
    eval (eval1 t)
  with NoRuleApplies ->
    t

(* -------------------------------------------------------------------------- *)
(* Subtyping. *)

(* Contexts map type variables to their bounds. *)

type ctx =
    type_expr Type_var.AtomMap.t

let noctx =
  Type_var.AtomMap.empty

let rec subtype (ctx : ctx) tyS tyT =
  match tyS, tyT with
  | TVar x, TVar y when Type_var.Atom.equal x y ->
      true
  | TVar x, _ ->
      subtype ctx (Type_var.AtomMap.lookup x ctx) tyT
  | _, TTop ->
      true
  | TArrow (tyS1, tyS2), TArrow (tyT1, tyT2) ->
      subtype ctx tyT1 tyS1 && subtype ctx tyS2 tyT2
  | TForall absS, TForall absT ->
      let tyX, tyS1, tyS2 = open_type_type_abs absS
      and tyY, tyT1, tyT2 = open_type_type_abs absT in
      (* Compare the bounds. *)
      equal ctx tyS1 tyT1 &&
      (* Choose a common bound variable and compare the bodies. *)
      let tyT2 = subst_type_expr (Type_var.Subst.singleton tyY tyX) tyT2 in
      let ctx = Type_var.AtomMap.add tyX tyS1 ctx in
      subtype ctx tyS2 tyT2
  | TRecord fieldsS, TRecord fieldsT ->
      StringMap.fold (fun li tyTi result ->
	result && try
	  let tySi = StringMap.find li fieldsS in
	  subtype ctx tySi tyTi
	with Not_found ->
	  false
      ) fieldsT true
  | _, _ ->
      false

and equal ctx tyS tyT =
  subtype ctx tyS tyT && subtype ctx tyT tyS

(* -------------------------------------------------------------------------- *)
(* Typing. *)

(* Environments map term variables to their types. *)

type env =
    type_expr Term_var.AtomMap.t

let noenv =
  Term_var.AtomMap.empty

let rec promote ctx = function
  | TVar x ->
      promote ctx (Type_var.AtomMap.lookup x ctx)
  | t ->
      t

let rec typepat env ctx p tyT =
  match p with
  | PWildcard ->
      env
  | PVar (x, tyS) ->
      if not (subtype ctx tyT tyS) then
	failwith "type mismatch";
      Term_var.AtomMap.add x tyS env
  | PRecord pfields ->
      match promote ctx tyT with
      |	TRecord tfields ->
	  StringMap.fold (fun li pi env ->
	    try
	      let tyTi = StringMap.find li tfields in
	      typepat env ctx pi tyTi
	    with Not_found ->
	      failwith ("label " ^ li ^ " not found")
          ) pfields env
      | _ ->
	  failwith "record type expected"

let rec typeof env ctx = function
  | EVar x ->
      Term_var.AtomMap.lookup x env
  | EAbs abs ->
      let x, tyT, t = open_term_term_abs abs in
      TArrow (tyT, typeof (Term_var.AtomMap.add x tyT env) ctx t)
  | EApp (t1, t2) ->
      begin
	let tyT1 = typeof env ctx t1 in
	let tyT2 = typeof env ctx t2 in
	match promote ctx tyT1 with
	| TArrow (tyT11,tyT12) ->
	    if subtype ctx tyT2 tyT11
	    then tyT12
	    else failwith "type mismatch"
	| _ ->
	    failwith "arrow type expected"
      end
  | ERecord fields ->
      TRecord (StringMap.map (typeof env ctx) fields)
  | EProj (t, l) ->
      begin
	match promote ctx (typeof env ctx t) with
	| TRecord fields ->
	    begin
	      try
		StringMap.find l fields
	      with Not_found ->
		failwith ("label " ^ l ^ " not found")
	    end
	| _ ->
	    failwith "record type expected"
      end
  | ELet abs ->
      let p, t1, t2 = open_pat_term_abs abs in
      let env = typepat env ctx p (typeof env ctx t1) in
      typeof env ctx t2
  | ETyAbs abs ->
      let tyX, tyT, t = open_type_term_abs abs in
      let ctx = Type_var.AtomMap.add tyX tyT ctx in
      TForall (create_type_type_abs (tyX, tyT, typeof env ctx t))
  | ETyApp (t1, tyT2) ->
      begin
	match promote ctx (typeof env ctx t1) with
	| TForall abs1 ->
	    let tyX, tyT11, tyT12 = open_type_type_abs abs1 in
	    if subtype ctx tyT2 tyT11 then
	      singleTypeSubst tyX tyT2 tyT12
	    else
	      failwith "type mismatch"
	| _ ->
	    failwith "universal type expected"
      end

