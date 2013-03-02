(* $Id: xCoreSyntaxExt.ml 40 2007-10-01 14:20:27Z yann.regisgianas $ *)

open XCoreSyntax

let atom_id a = 
  Var.Atom.basename a

let atom_position a = 
  PIdentifier.position (Var.Atom.basename a)

let atom_as_string a = 
  PIdentifier.as_string (Var.Atom.basename a)

let mk_lformula pos f = 
  { fpos = pos; fvalue = f }

let remove_fannot = function
  | FAnnot (t, _) -> t.fvalue
  | t -> t

let mk_fannot pos f ty = 
  FAnnot (mk_lformula pos f, ty)

let explicit_formula = function
  | ExplicitFormula f -> f
  | _ -> assert false

let mk_formula_type_scheme vs ty = 
  FTScheme (create_formula_scheme_abs (vs, ty))

let mk_flam pos bs f = 
  FLam (create_lfun_abs (bs, mk_lformula pos f))

let mk_fprod pos fs = 
  FProd (List.map (mk_lformula pos) fs)

let rec destruct_fprod = function
  | FProd [ t ] -> destruct_fprod t.fvalue
  | FProd ts -> ts
  | FAnnot (t, _) -> destruct_fprod t.fvalue
  | t -> [ mk_lformula Position.dummy t ]

let mk_fprod_type = function
  | [ ty ] -> ty
  | tys -> FTProd tys

let mk_conj pos f1 f2 = 
  FApp (mk_lformula pos (FPrimitive PAnd),
	List.map (mk_lformula pos) [ f1; f2 ])

let rec mk_conjs pos = function
  | [] -> FTrue
  | [ x ] -> x
  | [ p1; p2 ] -> mk_conj pos p1 p2
  | x :: xs -> mk_conj pos x (mk_conjs pos xs)

let rec destruct_conjs g = 
  match g.fvalue with
  | FApp (f, args) ->
      (match f.fvalue, args with
	 | FPrimitive PAnd, [ t1; t2 ] ->
	     destruct_lconjs t1 @ destruct_lconjs t2
	 | _ -> [g])
  | _ -> [g]

and destruct_lconjs f = 
  destruct_conjs f

let mk_disj pos f1 f2 = 
  FApp (mk_lformula pos (FPrimitive POr),
	List.map (mk_lformula pos) [ f1; f2 ])

let rec mk_disjs pos = function
  | [] -> FTrue
  | [ x ] -> x
  | [ p1; p2 ] -> mk_disj pos p1 p2
  | x :: xs -> mk_disj pos x (mk_disjs pos xs)

let mk_fimply pos f1 f2 = 
  FApp (mk_lformula pos (FPrimitive PImply),
	List.map (mk_lformula pos) [ f1; f2 ])

let mk_fequiv pos f1 f2 = 
  FApp (mk_lformula pos (FPrimitive PEquiv),
	List.map (mk_lformula pos) [ f1; f2 ])

let mk_fforalltys pos vs x = 
  FForallTys (create_fforalltys_abs (vs, mk_lformula pos x))

let mk_lforall pos bs x = 
  FForall (create_lforall_abs (bs, [], mk_lformula pos x))

let mk_lforall_triggers pos bs ts x = 
  FForall (create_lforall_abs (bs, ts, mk_lformula pos x))

let mk_lexists pos bs x = 
  FExists (create_lexists_abs (bs, mk_lformula pos x))

let mk_fapp pos f args =
  FApp (mk_lformula pos f, List.map (mk_lformula pos) args)

let rec destruct_fapp f =
  match f.fvalue with
    | FApp (f, args) ->
	let (f, argss) = destruct_fapp f in
	  (remove_fannot f), args :: argss
    | FAnnot (f, _) ->
	destruct_fapp f
    | x ->
	x, []

let destruct_fapp f = 
  let (f, argss) = destruct_fapp f in
    (f, List.rev argss)

let mk_fkapp pos f args =
  FKApp (f, List.map (mk_lformula pos) args)

let mk_feq pos ty v1 v2 = 
  FEq (ty, mk_lformula pos v1, mk_lformula pos v2)

let mk_fneq pos ty v1 v2 = 
  FApp (mk_lformula pos (FPrimitive PNot), 
	[ mk_lformula pos (mk_feq pos ty v1 v2) ])

let mk_conj pos f1 f2 = 
  mk_fapp pos (FPrimitive PAnd) [ f1; f2 ]

let mk_type_scheme vs ty = 
  TScheme (create_scheme_abs (vs, ty))

let mk_prod_type = function
  | [ ty ] -> ty
  | ts -> TProd ts

let monoscheme ty = 
  TScheme (create_scheme_abs ([], ty))

let mk_lterm pos t = 
  { tpos = pos; tvalue = t }

let rec remove_fannot' f = 
  match f.fvalue with
    | FAnnot (f, ty) -> remove_fannot' f
    | _ -> f
  
let mk_prod = function
  | [ t ] -> t.tvalue
  | ts -> EProd ts

let mk_lprod pos ts = 
  mk_prod (List.map (mk_lterm pos) ts)

let as_eid = function
  | EId x -> x
  | _ -> assert false

let full_destruct_let_bindings bs = 
  let (xs, vvs, tys, bs) = 
    Misc.list_map_to_4_lists 
      (fun (x, TScheme abs) -> 
	 let (vs, ty) = open_scheme_abs abs in
	   (x, vs, ty, (x, ty)))
      bs
  in
    (xs, List.flatten vvs, tys, bs)

class map_with_pos = object (self)
  inherit map

  val mutable pos = Position.dummy

  method current_position = 
    pos

  method lformula f = 
    pos <- f.fpos;
    { fpos = f.fpos; fvalue = self#formula f.fvalue }

  method lterm t = 
    pos <- t.tpos;
    { tpos = t.tpos; tvalue = self#term t.tvalue }

end

class ['a] fold_with_pos = object (self)
  inherit ['a] fold

  val mutable pos = Position.dummy

  method current_position = 
    pos

  method lformula acu f = 
    pos <- f.fpos;
    self#formula acu f.fvalue 

  method lterm acu t = 
    pos <- t.tpos;
    self#term acu t.tvalue 

end

class substituer phi =
object

  inherit map

  method fvar y = 
    try 
      snd (List.find (fun (z, t) -> Var.Atom.equal z y) phi)
    with Not_found ->
      FVar y

end

let subst_formula (x, v) f = 
  (new substituer [(x, v)])#lformula f

let many_subst_formula phi f = 
  (new substituer phi)#lformula f

let as_fvar = function
  | FVar f -> f
  | _ -> assert false

exception NotInductiveCaseSyntax

let rec destruct_inductive_case = function
  | FForallTys abs ->
      let (vs, t) = open_fforalltys_abs abs in
      let (ts, bs, hypotheses, conclusion) = 
	destruct_inductive_case t.fvalue 
      in
	(vs @ ts, bs, hypotheses, conclusion)

  | FForall abs ->
      let (ins, _, t) = open_lforall_abs abs in
      let (ts, bs, hypotheses, conclusion) = 
	destruct_inductive_case t.fvalue 
      in
	(ts, ins @ bs, hypotheses, conclusion)

  | ((FApp (g, args)) as f) ->
      (match remove_fannot g.fvalue, args with
	 | FPrimitive PImply, [ h; c]  -> 
	     ([], [], [h], c.fvalue)
	       
	 | _ ->
	     ([], [], [], f))

  | FAnnot (f, _) ->
      destruct_inductive_case f.fvalue

  | _ -> 
      raise NotInductiveCaseSyntax

let rec destruct_poly_flam = function
  | FForallTys abs ->
      let (vs, t) = open_fforalltys_abs abs in
      let (vs', ins, body) = destruct_poly_flam t.fvalue in
	(vs @ vs', ins, body)

  | FLam abs ->
      let (ins, body) = open_lfun_abs abs in
      let (vs, ins', body) = destruct_poly_flam body.fvalue in
	(vs, ins @ ins', body)

  | FAnnot (t, ty) ->
      destruct_poly_flam t.fvalue

  | t -> 
      ([], [], t)

let rec destruct_poly_flam' = function
  | FForallTys abs ->
      let (vs, t) = open_fforalltys_abs abs in
      let (vs', ins, body) = destruct_poly_flam' t.fvalue in
	(vs @ vs', ins, body)

  | FLam abs ->
      let (ins, body) = open_lfun_abs abs in
	([], ins, body.fvalue)

  | t -> 
      ([], [], t)

let destruct_formula_type_scheme_as_poly_bindings ?ids pos (FTScheme abs) = 
  let (vs, ty) = open_formula_scheme_abs abs in
  let ids_from tys = 
    match ids with 
      | Some ids -> ids 
      | None -> 
	  List.map 
	    (fun _ -> 
	       Var.Atom.freshb (PIdentifier.fresh_value_id (pos, "x")))
	    tys
  in
    match ty with
      | FTCArrow (FTProd tys, ret_ty) ->
	    (vs, List.combine (ids_from tys) tys, ret_ty)

      | FTCArrow (ty, ret_ty) ->
	  (vs, List.combine (ids_from [ty]) [ ty ], ret_ty)

      | FTArrow (FTProd tys, ret_ty) ->
	  (vs, List.combine (ids_from tys) tys, ret_ty)

      | FTArrow (ty, ret_ty) ->
	  (vs, List.combine (ids_from [ty]) [ ty ], ret_ty)

      | _ -> (vs, [], ty)

let formula_monoscheme ty = 
  FTScheme (create_formula_scheme_abs ([], ty))

let rec pattern_as_binding acu = function
  | PVar (x, ty) -> (x, ty) :: acu
  | PApp (k, ps) -> List.fold_left pattern_as_binding acu ps

let pattern_as_bindings = pattern_as_binding []

let rec pattern_as_logic_value pos = function
  | PVar (x, _) -> FVar x
  | PApp (k, ps) -> FKApp (k, List.map (pattern_as_logic_lvalue pos) ps)

and pattern_as_logic_lvalue pos v = 
  mk_lformula pos (pattern_as_logic_value pos v)
	 
let injective_dataconstructor pos d s = 
  let (vs, bs, rty) = destruct_formula_type_scheme_as_poly_bindings pos s in
  let bs' = 
    List.map (fun (_, ty) -> 
		(Var.Atom.freshb (PIdentifier.fresh_value_id (pos, "x")), ty))
      bs
  in
  let tys = snd (List.split bs) in
  let xs = List.map (fun (x, _) -> FVar x) bs in
  let xs' = List.map (fun (x, _) -> FVar x) bs' in
    mk_fforalltys pos vs 
      (mk_lforall pos bs
	 (mk_lforall pos bs' 
	    (mk_fimply pos 
	       (mk_feq pos rty (mk_fkapp pos d xs) (mk_fkapp pos d xs'))
	       (mk_conjs pos 
		  (Misc.list_map3 (mk_feq pos) tys xs xs')))))

let eq_dataconstructor_app pos x (d, ty) =
  let s = mk_formula_type_scheme [] ty in
  let (_, bs, rty) = destruct_formula_type_scheme_as_poly_bindings pos s in
  let xs = List.map (fun (x, ty) -> mk_fannot pos (FVar x) ty) bs in
    mk_lexists pos bs 
      (mk_feq pos rty 
	 (mk_fannot pos (mk_fkapp pos d xs) rty)
	 (mk_fannot pos (FVar x) rty))

let complete_dataconstructors pos t ts ds =
  let x = Var.Atom.freshb (PIdentifier.fresh_value_id (pos, "x")) in
    mk_fforalltys pos ts 
      (mk_lforall_triggers pos 
	 [ x, FTApp (t, List.map (fun v -> FTVar v) ts) ]
	 [ x ]
	 (mk_disjs pos 
	    (List.map (eq_dataconstructor_app pos x) ds)))

(* FIXME: Bugged ! Must synchronise type variables. *)
let inductive_inversion_case pos gvs p bs c = 
  let args, tys = List.split bs in
  let args = 
    List.map2 (fun x ty -> (FAnnot (mk_lformula pos (FVar x), ty))) args tys
  in
  let ty = mk_fprod_type tys in
  let (vs, ins, hs, c) = destruct_inductive_case c.fvalue in
  let args' = match remove_fannot c with
    | FApp (_, args) -> args
    | FVar _ -> []
    | _ -> assert false
  in
(*    mk_lexists pos ins
      (mk_conjs pos 
	 ((mk_feq pos ty (mk_fprod pos args) (FProd args')) :: 
	    (List.map (fun f -> f.fvalue) hs)))  *)

    (mk_fforalltys pos gvs
       (mk_lforall pos bs
	  (mk_fforalltys pos vs
	     (mk_lforall pos ins
		(mk_fimply pos
		   (mk_fapp pos (FVar p) args)
		   (mk_fimply pos
		      (mk_feq pos ty (mk_fprod pos args) (FProd args'))
		      (mk_conjs pos (List.map (fun f -> f.fvalue) hs))))))))
      
(*    mk_lforall pos ins 
      (mk_fimply pos 
	 (mk_fapp pos (FVar p) (List.map args'))
	 (List.map (fun f -> f.fvalue) hs))*)

let inductive_inversion pos p s cases = 
  let (vs, bs, rty) = destruct_formula_type_scheme_as_poly_bindings pos s in
    mk_lformula pos 
      (mk_conjs pos 
	 (List.map (inductive_inversion_case pos vs p bs) cases))


let inhabited_types pos v ty = 
  let x = Var.Atom.freshb (PIdentifier.fresh_value_id (pos, "x")) in
    mk_lexists pos [ (x, ty) ] 
      (mk_feq pos ty 
	 (mk_fannot pos v ty) 
	 (mk_fannot pos (FVar x) ty))

let rec is_deferred_formula f = 
  match f.fvalue with

    | FForallTys abs ->
	let (vs, f) = open_fforalltys_abs abs in
	  is_deferred_formula f

    | FAnnot (f, _) ->
	is_deferred_formula f

    | FDeferred ->
	true

    | f ->
	false

class foralltys_extruder =
object (self)
  
  inherit map

  val mutable rs = Var.AtomSet.empty

  method rigid_vars = Var.AtomSet.fold (fun s x -> s :: x) rs []

  method fforalltys abs = 
    let (vs, t) = open_fforalltys_abs abs in
      rs <- List.fold_left (fun s x -> Var.AtomSet.add x s)  rs vs;
      self#formula t.fvalue
      
end

let extrude_foralltys f = 
  let extruder = new foralltys_extruder in
  let f = extruder#lformula f in
    mk_lformula f.fpos 
      (FForallTys (create_fforalltys_abs (extruder#rigid_vars, f)))
    
