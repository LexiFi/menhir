(* $Id: coreSyntaxExt.ml 25 2007-09-27 15:01:06Z yann.regisgianas $ *)

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

open CoreSyntax

let mk_lterm pos t = 
  { tpos = pos; tvalue = t }
    
let mk_lformula pos f = 
  { fpos = pos; fvalue = f }

let mk_flam pos bs f = 
  FLam (create_lfun_abs (bs, mk_lformula pos f))

let mk_flam' pos xs f =
  let (vs, bs) = 
    Misc.list_fold_map 
      (fun vs x -> 
	 let v = Var.Atom.freshb (PIdentifier.fresh_type_id (pos, "a")) in
	   (v :: vs, (x, FTVar v)))
      [] xs
  in
    FExistsTys (create_fexiststys_abs (vs, 
				       mk_lformula pos (mk_flam pos bs f)))

let mk_fapp pos f args =
  FApp (mk_lformula pos f, List.map (mk_lformula pos) args)

let mk_fprod pos fs = 
  FProd (List.map (mk_lformula pos) fs)

let mk_feq pos f1 f2 = 
  FEq (mk_lformula pos f1, mk_lformula pos f2)

let rec mk_conj pos = function
  | [] -> FTrue
  | [ x ] -> x
  | f :: fs ->
      FApp (mk_lformula pos (FPrimitive PAnd),
	    List.map (mk_lformula pos) [ f; mk_conj pos fs ])


let mk_fexiststys pos vs t = 
  FExistsTys (create_fexiststys_abs (vs, mk_lformula pos t))

let mk_fforalltys pos vs t = 
  FForallTys (create_fforalltys_abs (vs, mk_lformula pos t))

let rec destruct_eforalltys = function
  | EForallTys abs ->
      let (vs, t) = open_eforalltys_abs abs in
      let (vs', t) = destruct_eforalltys t.tvalue in
	(vs @ vs', t)

  | EExistsTys abs ->
      let (vs, t) = open_eexiststys_abs abs in
      let (vs', u) = destruct_eforalltys t.tvalue in
	(vs', EExistsTys (create_eexiststys_abs (vs, mk_lterm t.tpos u)))

  | EProd ts ->
      let (vs, ts) = 
	Misc.list_fold_map (fun vs t ->
			      let (vs', u) = destruct_eforalltys t.tvalue in
				(vs @ vs', mk_lterm t.tpos u))
	  [] ts
      in
	(vs, EProd ts)

  | EAnnot (t, ty) ->
      let (vs, u) = destruct_eforalltys t.tvalue in
	(vs, EAnnot (mk_lterm t.tpos u, ty))

  | t -> 
      ([], t)

let full_destruct_let_bindings bs = 
  let (xs, vvs, tys, bs) = 
    Misc.list_map_to_4_lists 
      (fun (x, TScheme abs) -> 
	 let (vs, ty) = open_scheme_abs abs in
	   (x, vs, ty, (x, ty)))
      bs
  in
    (xs, List.flatten vvs, tys, bs)

let rec full_destruct_exists_pair = function
  | FExistsTys abs ->
      let (vs, t) = open_fexiststys_abs abs in
      let (vs', (t1, t2)) = full_destruct_exists_pair t.fvalue in
	(vs @ vs', (t1, t2))

  | FProd [ t1; t2 ] ->
      ([], (t1.fvalue, t2.fvalue))

  | FAnnot (f, _) ->
      full_destruct_exists_pair f.fvalue

  | _ -> assert false

let explicit_formula = function
  | ExplicitFormula f -> f
  | _ -> assert false

class map_with_pos =
object (self)

  inherit map

  val mutable pos = Position.dummy

  method pos = pos

  method lterm t = 
    pos <- t.tpos;
    { tpos = t.tpos; tvalue = self#term t.tvalue }

end
