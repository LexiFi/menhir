(* $Id: xCoreCooking.ml 40 2007-10-01 14:20:27Z yann.regisgianas $ *)

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

open XCoreSyntax
open XCoreSyntaxExt

class cleaner = object (self)

  inherit map

  method fforall abs =
    let (bs, ts, t) = open_lforall_abs abs in
    let fvs = free_lformula t in
      FForall 
	(create_lforall_abs 
	   (List.filter (fun (x, ty) -> Var.AtomSet.mem x fvs) bs, 
	    List.filter (fun x -> Var.AtomSet.mem x fvs) ts, 
	    self#lformula t))

  method fforalltys abs =
    let (ts, t) = open_fforalltys_abs abs in
    let fvs = free_lformula t in
      FForallTys 
	(create_fforalltys_abs 
	   (List.filter (fun x -> Var.AtomSet.mem x fvs) ts, 
	    self#lformula t))

end

let remove_unused_forall t =
  (new cleaner)#lformula t

class annotation_cleaner = object (self)

  inherit map

  method fprod ts = 
    match ts with
      | [ t ] -> self#formula t.fvalue
      | ts -> FProd (List.map self#lformula ts)

  method fannot (t, ty) =
    match (self#lformula t).fvalue with
      | FAnnot (t, ty') ->
	  assert (equal_formula_type ty ty');
	  FAnnot (t, ty)
      | x -> FAnnot (t, ty)

end

let remove_multiple_annotation t =
  (new annotation_cleaner)#lformula t 

class foralltys_extruder =
object (self)
  
  inherit map

  val mutable rs = Var.AtomSet.empty

  method rigid_vars = Var.AtomSet.fold (fun s x -> s :: x) rs []

  method fforalltys abs = 
    let (vs, t) = open_fforalltys_abs abs in
      rs <- List.fold_left (fun s x -> 
		      (* for an extrusion to be correct, the type variables
			 must be fresh with respect to the context. *)
		      assert (not (Var.AtomSet.mem x s));
		      Var.AtomSet.add x s) 
	rs vs;
      self#formula t.fvalue
      
end

let extrude_foralltys f = 
  let extruder = new foralltys_extruder in
  let f = extruder#lformula f in
    mk_lformula f.fpos 
      (FForallTys (create_fforalltys_abs (extruder#rigid_vars, f)))
    
class cooker =
object (self)

  inherit map

  method lformula f = 
    remove_unused_forall (remove_multiple_annotation f)

  method cook_lformula f = 
    self#lformula (extrude_foralltys f)

  method cfact (s, f) = 
    CFact (s, self#cook_lformula f)

  method optional_lformula = function
    | ExplicitFormula f -> ExplicitFormula (self#cook_lformula f)
    | x -> x

  method logic_clause_body f = 
    self#cook_lformula f

  method predicate_lformula f = 
    self#cook_lformula f

end

let cook p = 
  (new cooker)#program p






