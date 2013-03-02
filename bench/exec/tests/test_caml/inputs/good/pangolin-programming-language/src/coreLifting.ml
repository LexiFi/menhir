(* $Id: coreLifting.ml 25 2007-09-27 15:01:06Z yann.regisgianas $ *)

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
open CoreSyntaxExt

exception NotValue

let rec lift_term_type = function
  | TVar x -> FTVar x
  | TApp (tycon, ts) -> FTApp (tycon, List.map lift_term_type ts)
  | TArrow (ins, outs) -> FTCArrow (lift_term_type ins, lift_term_type outs)
  | TProd ts -> FTProd (List.map lift_term_type ts)
  | TPrimitive p -> FTPrimitive p

let lift_type_scheme (TScheme abs) =
  let (vs, ty) = open_scheme_abs abs in
    FTScheme (create_formula_scheme_abs (vs, lift_term_type ty))

let to_logic_bindings bs = 
  List.map (fun (x, ty) -> (x, lift_term_type ty)) bs 

let rec lift_value = function
  | EId x -> (FVar x, false)
  | EKApp (k, args) -> (FKApp (k, List.map lift_lvalue' args), false)
  | EPrimitive p -> (FPrimitive (PEPrimitive p), false)
  | ELam abs ->
      let (largs, (ins, pre), outs_abs, body) = open_fun_abs abs in
      let (outs, post) = open_function_output_abs outs_abs in
	(FProd [ explicit_formula pre; explicit_formula post ], true)
  | EProd [ t ] ->
      lift_value t.tvalue
  | EProd vs ->
      (FProd (List.map lift_lvalue' vs), false)
  | EExistsTys abs ->
      let (vs, t) = open_eexiststys_abs abs in
      let (t, fun_flag) = lift_lvalue t in
	(FExistsTys (create_fexiststys_abs (vs, t)), fun_flag)
  | EAnnot (t, ty) ->
      let (t, fun_flag) = lift_lvalue t in
	(FAnnot (t, lift_term_type ty), fun_flag)

  | EForallTys _ ->
      assert false


  | _ -> raise NotValue

and lift_lvalue v = 
  let (lv, fun_flag) = lift_value v.tvalue in
    (mk_lformula v.tpos lv, fun_flag)
      
and lift_lvalue' v =
  fst (lift_lvalue v)


