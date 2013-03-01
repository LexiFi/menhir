(* $Id: corePostConditionsInference.ml 25 2007-09-27 15:01:06Z yann.regisgianas $ *)

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

open Misc
open CoreSyntax
open CoreSyntaxExt
open CoreLifting

(* Transform an optional formula into an explicit formula if necessary. *)
let change_if_nothing f new_f = 
  match f with
    | ImplicitFormula pos -> ExplicitFormula (new_f pos)
    | f -> f

(* Abstract a formula with respect to a list of computational variables. *)
let abstract_formula pos bs f = 
  mk_flam pos (to_logic_bindings bs) f

(* Build the precondition \(x_1 : t_1, ..., x_n : t_n). True *)
let true_pre_if_nothing largs ins f =
  change_if_nothing f 
    (fun pos -> mk_lformula pos
       (mk_flam pos largs (abstract_formula pos ins FTrue)))

(* Idem for postcondition. *)
let true_post_if_nothing largs ins outs f =
  change_if_nothing f 
    (fun pos -> mk_lformula pos
       (mk_flam pos largs
	  (abstract_formula pos ins (abstract_formula pos outs FTrue))))

(* Given a term [t], compute an inferred postcondition if possible. 
   The list of variables [xs] is the names given to the each components
   of the computation's result. *)
let rec infer_postcondition_of_term pos t xs =
  match t.tvalue with

    (* Handle type introduction operators. *)

    | EForallTys abs ->
	let (vs, t) = open_eforalltys_abs abs in
	let f = infer_postcondition_of_term pos t xs in
	  mk_lformula pos (FForallTys (create_fforalltys_abs (vs, f)))

    | EExistsTys abs ->
	let (vs, t) = open_eexiststys_abs abs in
	let f = infer_postcondition_of_term pos t xs in
	  mk_lformula pos (FExistsTys (create_fexiststys_abs (vs, f)))

    | EAnnot (t, ty) ->
	infer_postcondition_of_term pos t xs

    (* An application [g [largs] (args)]. *)

    | EApp (g, largs, args) ->
	(match g.tvalue with

	   (* We can safely assume that the function is a variable thanks
	      to the module {!ANormalForm}.  *)

	   | EId g -> 
	       let resultp = mk_fprod pos (List.map (fun x -> FVar x) xs) in

		 (* We return : 
		    [fun xs. post (g) (largs, lift args) xs]

		    FIXME: see TODO file. *)

		 mk_lformula pos 
		   (mk_flam' pos xs
		      (mk_fapp pos 
			 (mk_fapp pos 
			    (mk_fapp pos
			       (mk_fapp pos 
				  (FPrimitive Post)
				  [ FVar g ])
			       [ FProd (largs) ])
			    [ FProd (List.map (lift_lvalue $$ fst) args) ])
			 [ resultp ]))

	   | _ -> assert false)

    | x -> 
	try 
	  let (vs, t) = destruct_eforalltys t.tvalue in

	    (* The term is a value [v]. The postcondition is 
	       [fun xs. lift v = xs]. *)

	    mk_lformula pos
	      (mk_fforalltys pos vs (value_equality pos xs (lift_value t)))
	with NotValue ->
	  mk_lformula pos (mk_flam' pos xs FTrue)

(* This function tries to lift a term as a logical value. If this term
   is not a value, [NotValue] is raised. 

   If the value is a function [f], its lifting is a pair of predicates
   which is not convenient in practice. To avoid the projection of
   this pair to pollute the code, we build directly a projected equivalent
   proposition. More precisely, if lift t = (t1, t2) we generate 
   [pre (f) = t1 /\ post (f) = t2] instead of [f = (t1, t2)]. *)
and value_equality pos xs (v, fun_flag) =
  if fun_flag then (
    match xs with
      | [ f ] -> 
	  let (vs, (t1, t2)) = full_destruct_exists_pair v in
	    (mk_flam' pos xs
	       (mk_fexiststys pos vs
		  (mk_conj pos
		     [ mk_feq pos (mk_fapp pos (FPrimitive Pre) [FVar f]) t1;
		       mk_feq pos (mk_fapp pos (FPrimitive Post) [FVar f]) t2
		     ])))
      | _ -> assert false
  )
  else 
    let resultp = mk_fprod pos (List.map (fun x -> FVar x) xs) in
      (mk_flam' pos xs (mk_feq pos v resultp))

(* Helper function. *)
let infer_postcondition_of_term f t result =
  change_if_nothing f (fun pos -> infer_postcondition_of_term pos t result)

(* This class implements a mapping operator that fill missing 
   postconditions. *)
class postconditions_inferencer =
object (self)
  inherit map_with_pos

  method elam abs =
    let (largs, (ins, pre), outs_abs, body) = open_fun_abs abs in
    let (outs, post) = open_function_output_abs outs_abs in
    let touts = 
      create_function_output_abs (outs, 
				  true_post_if_nothing largs ins outs post) 
    in
      ELam (create_fun_abs (largs, (ins, 
				    true_pre_if_nothing largs ins pre), touts,
			    self#lterm body))

  method elet abs = 
    let (xs, f, t1, t2) = open_let_abs abs in
    let t1 = self#lterm t1 in
    let f = infer_postcondition_of_term f t1 xs in
      ELet (create_let_abs (xs, f, t1, self#lterm t2))
  
  method eletrec abs = 
    let (xs, f, t1, t2) = open_letrec_abs abs in
    let t1 = self#lterm t1 in
    let f = infer_postcondition_of_term f t1 xs in
      ELetRec (create_letrec_abs (xs, f, t1, self#lterm t2))

  method cvalue (xs, f, t) = 
    let t = self#lterm t in
    let f = infer_postcondition_of_term f t xs in
      CValue (xs, f, t)

  method crecvalue (xs, f, t) = 
    let t = self#lterm t in
    let f = infer_postcondition_of_term f t xs in
      CRecValue (xs, f, t)
  
end

let postconditions_inferencer = 
  new postconditions_inferencer 

let process p = 
  postconditions_inferencer#program p
    
  
