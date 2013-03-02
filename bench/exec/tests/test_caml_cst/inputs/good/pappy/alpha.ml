(* $Header: /home/yquem/cristal/fpottier/cvs/pappy/alpha.ml,v 1.3 2004/03/20 17:22:38 fpottier Exp $ *)

open MLast

module StringSet = Set.Make(String)

let rec dvpat accu = function
  | PaAcc (_, _, _)
      (* A qualified identifier. We do not regard it as free. *)
  | PaAny _
      (* Wildcard. *)
  | PaChr (_, _)
  | PaInt (_, _)
  | PaInt32 (_, _)
  | PaInt64 (_, _)
  | PaNativeInt (_, _)
  | PaFlo (_, _)
  | PaStr (_, _)
      (* Constant. *)
  | PaTyp (_, _)
      (* # pattern (polymorphic variants). *)
  | PaUid (_, _)
      (* Constructor. *)
  | PaVrn (_, _)
      (* Polymorphic variant constructor. *)
    ->
      accu
  | PaAli (_, pat1, pat2)
      (* Alias pattern. *)
  | PaOrp (_, pat1, pat2)
      (* Or pattern. *)
    ->
      dvpat (dvpat accu pat1) pat2
  | PaAnt (_, pat)
      (* Antiquotation. Presumably without any effect? *)
  | PaLab (_, _, Some pat)
  | PaOlb (_, _, Some (pat, None))
      (* Labeled argument. *)
  | PaTyc (_, pat, _)
      (* Type constraint. *)
    ->
      dvpat accu pat
  | PaOlb (_, label, Some (pat, Some expr)) ->
      assert false; (* TEMPORARY *)
      (* Allowing default expressions in patterns would require defining fv for patterns as well! *)
      (* For the moment, we assume that patterns have no free variables. *)
      dvpat accu pat
  | PaApp (_, pat1, pat2) ->
      (* Constructor application. *)
      assert (StringSet.is_empty (dv pat1));
      dvpat accu pat2
  | PaArr (_, pats)
      (* Array. *)
  | PaTup (_, pats)
      (* Tuple. *)
    ->
      List.fold_left dvpat accu pats
  | PaLab (_, id, None)
  | PaOlb (_, id, None)
      (* Labeled argument with punning. *)
  | PaLid (_, id)
      (* Identifier. *)
    ->
      StringSet.add id accu
  | PaRng (_, pat1, pat2) ->
      (* Range pattern. *)
      assert (StringSet.is_empty (dv pat1) && StringSet.is_empty (dv pat2));
      accu
  | PaRec (_, lps) ->
      (* Record. *)
      List.fold_left (fun accu (_, pat) -> dvpat accu pat) accu lps

and dv pat =
  dvpat StringSet.empty pat

let rec is_module_path = function
  | ExAcc (_, expr1, _) ->
      is_module_path expr1
  | ExUid (_, _) ->
      ()
  | _ ->
      raise Not_found

let rec fvexpr accu = function
  | ExAcc (_, expr1, expr2) ->
      begin
	(* Module or record access. *)
	try
	  is_module_path expr1;
	  (* Qualified identifier. We do not regard it as free. *)
	  accu
	with Not_found ->
	  (* Record access. *)
	  fvexpr accu expr1
      end
  | ExApp (_, expr1, expr2)
      (* Application. *)
  | ExAre (_, expr1, expr2)
      (* Array access. *)
  | ExAss (_, expr1, expr2)
      (* Assignment. *)
  | ExSte (_, expr1, expr2)
      (* String access. *)
    ->
      fvexpr (fvexpr accu expr1) expr2
  | ExAnt (_, expr)
      (* Antiquote. *)
  | ExAsr (_, expr)
      (* Assertion. *)
  | ExCoe (_, expr, _, _)
      (* Coercion. *)
  | ExLab (_, _, Some expr)
  | ExOlb (_, _, Some expr)
      (* Labeled argument. *)
  | ExLaz (_, expr)
      (* Lazy. *)
  | ExSnd (_, expr, _)
      (* Message send. *)
  | ExTyc (_, expr, _)
      (* Type constraint. *)
    ->
      fvexpr accu expr
  | ExArr (_, exprs)
      (* Array creation. *)
  | ExSeq (_, exprs)
      (* Sequence. *)
  | ExTup (_, exprs)
      (* Tuple. *)
    ->
      List.fold_left fvexpr accu exprs
  | ExAsf _
      (* assert false. *)
  | ExChr (_, _)
  | ExFlo (_, _)
  | ExInt (_, _)
  | ExInt32 (_, _)
  | ExInt64 (_, _)
  | ExNativeInt (_, _)
  | ExStr (_, _)
      (* Constants. *)
  | ExNew (_, _)
      (* New. *)
  | ExUid (_, _)
      (* Constructor. *)
  | ExVrn (_, _)
      (* Polymorphic variant constructor. *)
    ->
      accu
  | ExFor (_, id, expr1, expr2, _, exprs) ->
      (* For loop. *)
      StringSet.union (fvexpr (fvexpr accu expr1) expr2)
	              (StringSet.remove id (List.fold_left fvexpr StringSet.empty exprs))
  | ExFun (_, pat_when_exprs) ->
      (* Function definition. *)
      fvpwes accu pat_when_exprs
  | ExIfe (_, expr1, expr2, expr3) ->
      (* If. *)
      fvexpr (fvexpr (fvexpr accu expr1) expr2) expr3
  | ExLab (_, id, None)
  | ExOlb (_, id, None)
      (* Labeled argument with punning. *)
  | ExLid (_, id)
      (* Identifier. *)
    ->
      StringSet.add id accu
  | ExLet (_, false, pat_exprs, expr) ->
      (* Let. *)
      StringSet.union (List.fold_left (fun accu (_, expr) -> fvexpr accu expr) accu pat_exprs)
                      (StringSet.diff (fv expr)
		                      (List.fold_left (fun accu (pat, _) -> dvpat accu pat) StringSet.empty pat_exprs))
  | ExLet (_, true, pat_exprs, expr) ->
      (* Let rec. *)
      StringSet.union accu
		      (StringSet.diff (List.fold_left (fun accu (_, expr) -> fvexpr accu expr) (fv expr) pat_exprs)
				      (List.fold_left (fun accu (pat, _) -> dvpat accu pat) StringSet.empty pat_exprs))
  | ExLmd (_, _, mexpr, expr) ->
      assert false (* TEMPORARY not implemented yet *)
  | ExMat (_, expr, pat_when_exprs)
      (* Match. *)
  | ExTry (_, expr, pat_when_exprs)
      (* Try. *)
    ->
      fvpwes (fvexpr accu expr) pat_when_exprs
  | ExOvr (_, field_exprs) ->
      (* Object override. *)
      List.fold_left (fun accu (_, expr) -> fvexpr accu expr) accu field_exprs
  | ExRec (_, field_exprs, oexpr) ->
      (* Record. *)
      Option.fold fvexpr (List.fold_left (fun accu (_, expr) -> fvexpr accu expr) accu field_exprs) oexpr
  | ExWhi (_, expr, exprs) ->
      (* While loop. *)
      List.fold_left fvexpr (fvexpr accu expr) exprs

and fvpwes accu pat_when_exprs =
  List.fold_left (fun accu (pat, wh, expr) ->
    StringSet.union accu (StringSet.diff (fvexpr (Option.fold fvexpr StringSet.empty wh) expr) (dv pat))
  ) accu pat_when_exprs

and fv expr =
  fvexpr StringSet.empty expr

(* TEMPORARY pour implanter le renommage, ne pas oublier de defaire le punning! *)
module StringMap = Map.Make(String)

let isolate_numeric_suffix s =
  let rec loop i =
    if (i > 0) then
      match s.[i-1] with
      | '0' .. '9' ->
	  loop (i-1)
      | _ ->
	  i
    else
      i
  in
  let n = String.length s in
  let i = loop n in
  String.sub s 0 i,
  if i = n then 0 else int_of_string (String.sub s i (n-i))

let gensym forbidden id =
  let base, suffix = isolate_numeric_suffix id in
  let rec loop i =
    let candidate = Printf.sprintf "%s%d" base i in
    if forbidden candidate then
      loop (i+1)
    else
      candidate
  in
  loop (suffix + 1)

module Renaming = struct

  type t = string StringMap.t

  let find id (r : t) =
    try
      StringMap.find id r
    with Not_found ->
      id

  let is_in_image id r =
    StringMap.fold (fun _ id' result -> (id = id') || result) r false

end

let genpat forbidden1 forbidden2 renaming pat =

  let renaming =
    ref renaming in

  let rename id =
    if forbidden1 id then
      (* Because of or patterns, we may encounter the same identifier twice,
	 and must rename it consistently. *)
      try
	StringMap.find id !renaming
      with Not_found ->
	let id' = gensym forbidden2 id in
	renaming := StringMap.add id id' !renaming;
	id'
    else
      id
  in

  let rec gen pat =
    match pat with
    | PaAcc (_, _, _)
	(* A qualified identifier. We do not regard it as free. *)
    | PaAny _
	(* Wildcard. *)
    | PaChr (_, _)
    | PaInt (_, _)
    | PaInt32 (_, _)
    | PaInt64 (_, _)
    | PaNativeInt (_, _)
    | PaFlo (_, _)
    | PaStr (_, _)
	(* Constant. *)
    | PaTyp (_, _)
	(* # pattern (polymorphic variants). *)
    | PaUid (_, _)
	(* Constructor. *)
    | PaVrn (_, _)
	(* Polymorphic variant constructor. *)
      ->
	pat
    | PaAli (loc, pat1, pat2) ->
	(* Alias pattern. *)
	PaAli (loc, gen pat1, gen pat2)
    | PaOrp (loc, pat1, pat2) ->
	(* Or pattern. *)
	PaOrp (loc, gen pat1, gen pat2)
    | PaAnt (loc, pat) ->
	(* Antiquotation. Presumably without any effect? *)
	PaAnt (loc, gen pat)
    | PaLab (loc, label, Some pat) ->
	(* Labeled argument. *)
	PaLab (loc, label, Some (gen pat))
    | PaOlb (loc, label, Some (pat, None)) ->
	(* Labeled argument. *)
	PaOlb (loc, label, Some (gen pat, None))
    | PaTyc (loc, pat, ctyp) ->
	(* Type constraint. *)
	PaTyc (loc, gen pat, ctyp)
    | PaOlb (loc, label, Some (pat, Some expr)) ->
	assert false (* TEMPORARY *)
    | PaApp (loc, pat1, pat2) ->
	(* Constructor application. *)
	assert (StringSet.is_empty (dv pat1));
	PaApp (loc, pat1, gen pat2)
    | PaArr (loc, pats) ->
	(* Array. *)
	PaArr (loc, List.map gen pats)
    | PaTup (loc, pats) ->
	(* Tuple. *)
	PaTup (loc, List.map gen pats)
    | PaLab (loc, id, None) ->
	(* Labeled argument with punning. *)
	let id' = rename id in
	if id = id' then
	  pat
	else
	  PaLab (loc, id, Some (PaLid (loc, id')))
    | PaOlb (loc, id, None) ->
	(* Labeled argument with punning. *)
	let id' = rename id in
	if id = id' then
	  pat
	else
	  PaOlb (loc, id, Some (PaLid (loc, id'), None))
    | PaLid (loc, id) ->
	(* Identifier. *)
	PaLid (loc, rename id)
    | PaRng (_, pat1, pat2) ->
	(* Range pattern. *)
	assert (StringSet.is_empty (dv pat1) && StringSet.is_empty (dv pat2));
	pat
    | PaRec (loc, lps) ->
	(* Record. *)
	PaRec (loc, List.map (fun (label, pat) -> (label, gen pat)) lps)

  in
  let pat = gen pat in
  !renaming, pat

let rec rename renaming expr =
  match expr with
  | ExAcc (loc, expr1, expr2) ->
      begin
	(* Module or record access. *)
	try
	  is_module_path expr1;
	  (* Qualified identifier. We do not regard it as free. *)
	  expr
	with Not_found ->
	  (* Record access. *)
	  ExAcc (loc, rename renaming expr1, expr2)
      end
  | ExApp (loc, expr1, expr2) ->
      (* Application. *)
      ExApp (loc, rename renaming expr1, rename renaming expr2)
  | ExAre (loc, expr1, expr2) ->
      (* Array access. *)
      ExAre (loc, rename renaming expr1, rename renaming expr2)
  | ExAss (loc, expr1, expr2) ->
      (* Assignment. *)
      ExAss (loc, rename renaming expr1, rename renaming expr2)
  | ExSte (loc, expr1, expr2) ->
      (* String access. *)
      ExSte (loc, rename renaming expr1, rename renaming expr2)
  | ExAnt (loc, expr) ->
      (* Antiquote. *)
      ExAnt (loc, rename renaming expr)
  | ExAsr (loc, expr) ->
      (* Assertion. *)
      ExAsr (loc, rename renaming expr)
  | ExCoe (loc, expr, ctyp1, ctyp2) ->
      (* Coercion. *)
      ExCoe (loc, rename renaming expr, ctyp1, ctyp2)
  | ExLab (loc, label, Some expr) ->
      (* Labeled argument. *)
      ExLab (loc, label, Some (rename renaming expr))
  | ExOlb (loc, label, Some expr) ->
      (* Labeled argument. *)
      ExOlb (loc, label, Some (rename renaming expr))
  | ExLaz (loc, expr) ->
      (* Lazy. *)
      ExLaz (loc, rename renaming expr)
  | ExSnd (loc, expr, label) ->
      (* Message send. *)
      ExSnd (loc, rename renaming expr, label)
  | ExTyc (loc, expr, ctyp) ->
      (* Type constraint. *)
      ExTyc (loc, rename renaming expr, ctyp)
  | ExArr (loc, exprs) ->
      (* Array creation. *)
      ExArr (loc, List.map (rename renaming) exprs)
  | ExSeq (loc, exprs) ->
      (* Sequence. *)
      ExSeq (loc, List.map (rename renaming) exprs)
  | ExTup (loc, exprs) ->
      (* Tuple. *)
      ExTup (loc, List.map (rename renaming) exprs)
  | ExAsf _
      (* assert false. *)
  | ExChr (_, _)
  | ExFlo (_, _)
  | ExInt (_, _)
  | ExInt32 (_, _)
  | ExInt64 (_, _)
  | ExNativeInt (_, _)
  | ExStr (_, _)
      (* Constants. *)
  | ExNew (_, _)
      (* New. *)
  | ExUid (_, _)
      (* Constructor. *)
  | ExVrn (_, _)
      (* Polymorphic variant constructor. *)
    ->
      expr
  | ExFor (loc, id, expr1, expr2, df, exprs) -> (
      (* For loop. *)
      let expr1 = rename renaming expr1
      and expr2 = rename renaming expr2 in
      match enter renaming (PaLid (loc, id)) (lazy (List.fold_left fvexpr StringSet.empty exprs)) with
      | renaming, PaLid (_, id) ->
	  ExFor (loc, id, expr1, expr2, df, List.map (rename renaming) exprs)
      | _ ->
	  assert false
    )
  | ExFun (loc, pat_when_exprs) ->
      (* Function definition. *)
      ExFun (loc, List.map (rename_pwe renaming) pat_when_exprs)
  | ExIfe (loc, expr1, expr2, expr3) ->
      (* If. *)
      ExIfe (loc, rename renaming expr1, rename renaming expr2, rename renaming expr3)
  | ExLab (loc, id, None) ->
      (* Labeled argument with punning. *)
      let id' = Renaming.find id renaming in
      if id = id' then
	expr
      else
	ExLab (loc, id, Some (ExLid (loc, id')))
  | ExOlb (loc, id, None) ->
      (* Labeled argument with punning. *)
      let id' = Renaming.find id renaming in
      if id = id' then
	expr
      else
	ExOlb (loc, id, Some (ExLid (loc, id')))
  | ExLid (loc, id) ->
      (* Identifier. *)
      ExLid (loc, Renaming.find id renaming)
  | ExLet (loc, false, pat_exprs, expr) -> (
      (* Let. *)
      let exprs = List.map (fun (_, expr) -> rename renaming expr) pat_exprs in
      match enter renaming (PaTup (loc, List.map fst pat_exprs)) (lazy (fv expr)) with
      | renaming, PaTup (_, pats) ->
	  ExLet (loc, false, List.combine pats exprs, rename renaming expr)
      | _ ->
	  assert false
    )
  | ExLet (loc, true, pat_exprs, expr) -> (
      (* Let rec. *)
      let scope = lazy (List.fold_left (fun accu (_, expr) -> fvexpr accu expr) (fv expr) pat_exprs) in
      match enter renaming (PaTup (loc, List.map fst pat_exprs)) scope with
      | renaming, PaTup (_, pats) ->
	  let exprs = List.map (fun (_, expr) -> rename renaming expr) pat_exprs in
	  ExLet (loc, true, List.combine pats exprs, rename renaming expr)
      | _ ->
	  assert false
    )
  | ExLmd (_, _, mexpr, expr) ->
      assert false (* TEMPORARY not implemented yet *)
  | ExMat (loc, expr, pat_when_exprs) ->
      (* Match. *)
      ExMat (loc, rename renaming expr, List.map (rename_pwe renaming) pat_when_exprs)
  | ExTry (loc, expr, pat_when_exprs) ->
      (* Try. *)
      ExTry (loc, rename renaming expr, List.map (rename_pwe renaming) pat_when_exprs)
  | ExOvr (loc, field_exprs) ->
      (* Object override. *)
      ExOvr (loc, List.map (fun (field, expr) -> (field, rename renaming expr)) field_exprs)
  | ExRec (loc, field_exprs, oexpr) ->
      (* Record. *)
      ExRec (loc, List.map (fun (field, expr) -> (field, rename renaming expr)) field_exprs,
	     Option.map (rename renaming) oexpr)
  | ExWhi (loc, expr, exprs) ->
      (* While loop. *)
      ExWhi (loc, rename renaming expr, List.map (rename renaming) exprs)

and enter renaming pat scope =
  let renaming = StringSet.fold StringMap.remove (dv pat) renaming in
  let forbidden1 id = Renaming.is_in_image id renaming in
  let forbidden2 id = forbidden1 id || StringSet.mem id (Lazy.force scope) in
  genpat forbidden1 forbidden2 renaming pat

and rename_pwe renaming (pat, wh, expr) =
  let scope = lazy (Option.fold fvexpr (fv expr) wh) in
  let renaming, pat = enter renaming pat scope in
  (pat, Option.map (rename renaming) wh, rename renaming expr)

