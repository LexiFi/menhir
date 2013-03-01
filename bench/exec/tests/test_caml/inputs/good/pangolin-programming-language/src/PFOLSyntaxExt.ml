(* $Id: PFOLSyntaxExt.ml 61 2007-10-12 14:45:27Z yann.regisgianas $ *)

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



open PFOLSyntax

class cleaner = object (self)

  inherit map

  method forall abs =
    let (bs, ts, t) = open_forall_abs abs in
    let fvs = free_predicate t in
      Forall 
	(create_forall_abs 
	   (List.filter (fun (x, ty) -> Var.AtomSet.mem x fvs) bs, 
	    List.filter (fun x -> Var.AtomSet.mem x fvs) ts, 
	    self#predicate t))

end

let remove_unused_forall t =
  (new cleaner)#predicate t

class annotation_cleaner = object (self)

  inherit map

  method annot (t, ty) =
    match self#term t with
      | Annot (t, ty') | Prod [ Annot (t, ty') ] ->
	  assert (equal_term_type ty ty');
	  Annot (t, ty)
      | t -> Annot (t, ty)

end

let remove_multiple_annotation t =
  (new annotation_cleaner)#predicate t 

