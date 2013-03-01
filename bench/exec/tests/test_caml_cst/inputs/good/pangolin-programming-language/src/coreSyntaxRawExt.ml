(* $Id: coreSyntaxRawExt.ml 20 2007-09-27 12:48:10Z yann.regisgianas $ *)

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

open CoreSyntax.Raw
open Position

type term_type_annotation = type_parameters * term_type

type flexible_bindings = (var * term_type_annotation) list

type formula_type_annotation = type_parameters * formula_type

let mk_lformula (pos, f) = 
  { 
    fpos   = pos;
    fvalue = f
  }

let mk_lterm (pos, t) = 
  { 
    tpos   = pos;
    tvalue = t
  }
    
let same_pos_lterm t t' = 
  {
    tpos   = t.tpos;
    tvalue = t'
  }

let annot_with_types t tys = 
  let ty = 
    match tys with
      | []     -> TPrimitive TUnit
      | [ ty ] -> ty
      | tys    -> TProd tys
  in
    same_pos_lterm t (EAnnot (t, ty))

let extract_fvs bs = 
  let (xs, ftys) = List.split bs in
  let (fvs, tys) = List.split ftys in
  let fvs = List.flatten fvs in
    assert (PIdentifier.distinct fvs);
    (fvs, xs, tys, List.combine xs tys)
      
let wildcard () =
  let x = PIdentifier.fresh_type_id (Position.dummy, "w") in
    ([ x ], TVar x)
      
let fwildcard () =
  let x = PIdentifier.fresh_type_id (Position.dummy, "w") in
    ([ x ], FTVar x)
  
let mk_flam' pos xs f =
    let (vs, bs) =
      Misc.list_fold_map
   (fun vs x ->
      let (v, t) = fwildcard () in
        (v @ vs, (x, t))) [] xs
    in
      mk_lformula (pos, FExistsTys (vs, mk_lformula (pos, FLam (bs, f))))
    
let abstract_optional_formula_by xs = function
  | ImplicitFormula pos -> ImplicitFormula pos
  | ExplicitFormula f -> ExplicitFormula (mk_flam' f.fpos xs f)
      
let mk_value ts (bs, f) t = 
  let (fvs, xs, tys, _) = extract_fvs bs in
    (xs, abstract_optional_formula_by xs f, 	      
     same_pos_lterm t 
       (EForallTys (ts, 
		    same_pos_lterm t 
		      (EExistsTys ((fvs, annot_with_types t tys))))))
            
let mk_flexible_bounded_formula q bs f =
   let (fvs, _, _, b) = extract_fvs bs in
     FExistsTys (fvs, mk_lformula (f.fpos, q b f))

let mk_flexible_bounded_term q bs t =
   let (fvs, _, _, b) = extract_fvs bs in
     EExistsTys (fvs, mk_lterm (t.tpos, q b t))

let mk_implicit_output pos = 
  ([ (PIdentifier.fresh_value_id (pos, "result"), wildcard ()) ], 
   ImplicitFormula pos)
    
type flexible_fbindings = (var * formula_type_annotation) list

type logic_flexible_bindings = flexible_bindings * optional_lformula

type function_value =
    flexible_fbindings * logic_flexible_bindings * logic_flexible_bindings
    * lterm

let mk_lam (largs, (ins, pre), (outs, post), t) = 
  let (largs_fvs, lxs, _, largs) = extract_fvs largs in
  let (ins_fvs, xs, _, ins) = extract_fvs ins in
  let (outs_fvs, ys, _, outs) = extract_fvs outs in
  let pre = 
    abstract_optional_formula_by lxs (abstract_optional_formula_by xs pre)
  in
  let post = 
    abstract_optional_formula_by lxs 
      (abstract_optional_formula_by xs 
	 (abstract_optional_formula_by ys post))
  in
  let elam = ELam (largs, (ins, pre), (outs, post), t) in
    EExistsTys (largs_fvs @ ins_fvs @ outs_fvs, same_pos_lterm t elam)
      
let mk_fapp pos f args =
  FApp (mk_lformula (pos, f), args)
    
let mk_value_component ts lt t =
  let (xs, f, t) = mk_value ts lt t in
    CValue (xs, f, t)

let mk_functional_component ts g (largs, ins, outs, t) = 
  let pos = t.tpos in
  let lam = mk_lam (largs, ins, outs, t) in
  let f = ImplicitFormula pos in
    mk_value_component ts ([ (g, wildcard ()) ], f) (same_pos_lterm t lam)
      
let mk_recursive_functions_component fdefs =
  let (ts, fs, lams) = 
    List.fold_left (fun (tvs, fs, lams) (ts, f, (largs, ins, outs, t)) ->
	      (ts @ tvs, f :: fs, 
	       mk_lterm (PIdentifier.position f, 
			 mk_lam (largs, ins, outs, t)) 
	       :: lams))
      ([], [], [])
      fdefs
  in
  let pos = (List.hd lams).tpos in
    CRecValue (fs, ImplicitFormula pos, 
	       mk_lterm (pos, EForallTys (ts, mk_lterm (pos, EProd lams))))
      
      
