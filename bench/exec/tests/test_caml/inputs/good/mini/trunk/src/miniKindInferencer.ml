(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: miniKindInferencer.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module implements a simple kind inferencer. *)

open CoreAlgebra
open MultiEquation
open Positions
open MiniAst
open MiniTypingExceptions
open Misc

module Ast = MiniAst
   
module IdSet = Set.Make (struct
			   type t = string
			   let compare = compare
			 end)

module RowDomain = BasicSetEquations.Make (struct
					     
    include IdSet

    let print s =
      try
	let label = choose s in
	fold (fun label accu ->
	  label ^ "+" ^ accu
        ) (remove label s) label
      with Not_found ->
	""

  end)

type variable = 
    descriptor UnionFind.point
      
and descriptor = 
    {
      mutable structure : term option;
      mutable name : tname;
      mutable constant : bool;
    }

and term =
  | App of variable * variable
  | Row of RowDomain.term

type t = variable

type env = (tname -> t) * (tname -> t -> unit)

let count = ref 0

let new_name () =
  incr count;
  TName ("V" ^ string_of_int !count)

let variable ?(name : tname option) () =
  let constant = (name <> None) 
  and name = match name with None -> new_name () | Some n -> n in
    UnionFind.fresh { structure = None; name = name; constant = constant }

let structure v = 
  (UnionFind.find v).structure
    
let iter_term f = function
    App (t1, t2) -> 
      f t1;
      f t2
  | _ -> ()

let iter f v = iter_term f (unSome (structure v))
		 
let lookup id tenv = (fst tenv) id
	
let bind id t tenv = (snd tenv) id t; tenv
	       
let term_handler t = 
  UnionFind.fresh {
    name = TName "";
    structure = Some t;
    constant = false
  }
    
let times = "__x"
let arrow = "__=>"
let star = "__*"
	     
let count = ref 0
	      	
let symbol tenv i = 
  (fst tenv) i
    
let binop op x y = 
  let w = term_handler (App (op, x)) in
    term_handler (App (w, y))    

let star =
  variable ~name:(TName "@") ()

let arrow =
  variable ~name:(TName "=>") ()

let times = 
  variable ~name:(TName "x") ()
          
let mkarrow = 
  binop arrow
    
let mktimes = 
  binop times
    
let rec mkconstructor tenv arity =
  if arity = 0 then star else
    mkarrow star (mkconstructor tenv (arity - 1))  

let row d = 
  term_handler (Row d)

let is_star env k = 
  UnionFind.equivalent k star
    
let fresh_kind () = 
  variable ()
    
let rec kind_arity env v = 
  let arrow_sym = symbol env arrow in
    match (UnionFind.find v).structure with
      | None -> 
	  if is_star env v then 0
	  else
	    assert false
      | Some t ->
	  (match t with 
	     | App (s, k) when s = arrow_sym -> 1 + kind_arity env k
	     | App (k, _) -> kind_arity env k
	     | _ -> 0)

let rec print_term = function
  | App (v1, v2) -> String.concat "" [ "(" ; print v1 ; "," ; print v2 ; ")" ]
  | Row v -> "Row("^ RowDomain.print v ^ ")"
      
and print v = 
  match (UnionFind.find v).structure with
    | None -> name v
    | Some t -> print_term t

and name v = 
  match (UnionFind.find v).name with
      TName name -> name

let is_constant v = (UnionFind.find v).constant		    

let assign_point k1 k2 = 
  let name, has_name = 
    if is_constant k1 then name k1, true
    else if is_constant k2 then name k2, true
    else "", false
  in 
    UnionFind.union k1 k2;
    if has_name then (
      (UnionFind.find k2).name <- TName name; 
      (UnionFind.find k2).constant <- true
    ) 

let assign pos k1 k2 = 
  iter (fun k -> if UnionFind.equivalent k1 k then raise (KindError pos)) k2; 
  assign_point k1 k2

let occur_check v1 v2 = 
  let rec variables acu v = 
    match (structure v) with 
      | None -> if not (List.memq v acu) then v::acu else acu
      | Some (App (v1, v2)) -> variables (variables acu v1) v2
      | _ -> acu
  in
  let vars1 = variables [] v1 
  and vars2 = variables [] v2 
  in
    List.memq v1 vars2 ||
    List.memq v2 vars1 

let rec unify pos k1 k2 = 
(*  Printf.eprintf "%s =?= %s\n" (print k1) (print k2); *)
  if not (UnionFind.equivalent k1 k2) then (
    if occur_check k1 k2 then
      raise (KindError pos);

    match structure k1, structure k2 with
	
      | None, None -> 
	  if (is_constant k1 && is_constant k2 && name k1 <> name k2) then
	    raise (KindError pos);
	  assign_point k1 k2

      | (None, _ | _, None) when is_constant k1 || is_constant k2 -> 
	  raise (KindError pos)
	    
      | None, _ -> 
	  assign pos k1 k2

      | _, None -> 
	  assign pos k2 k1

      | Some (App (t1, t2)), Some (App (t1', t2')) ->
	  unify pos t1 t1';
	  unify pos t2 t2'

      | Some (Row d1), Some (Row d2) ->
	  RowDomain.unify d1 d2

      | _ -> assert false)

(* FIXME: Should be done on internal term representation to factorize *)
let rec infer env t = 
  match t with
      TypVar (p, id) -> 
	lookup id env

    | TypApp (p, tc, ts) ->
	let k = variable () in
	let kd = 
	  List.fold_right (fun t acu -> mkarrow (infer env t) acu) 
	    ts k 
	    
	in
	  unify p (infer env tc) kd; 
	  k

    | TypRowCons (p, attributes, t) ->
	List.iter (fun (_,ta) -> check p env ta star) attributes;
	let defined_labels = 
	  List.fold_left 
	    (fun acu (LName l,_) -> 
	       if IdSet.mem l acu then raise (MultipleLabels (p, LName l))
	       else IdSet.add l acu) 
	    IdSet.empty 
	    attributes in
	let domain = RowDomain.svariable () in
	  check p env t (row (RowDomain.sum defined_labels domain));
	  row domain

    | TypRowUniform (p, typ) ->
	row (RowDomain.svariable ())
	  
and check pos env t k = 
  unify pos (infer env t) k

let rec intern_kind env = function
  | KStar -> star 
  | KTimes (k1, k2) -> mktimes (intern_kind env k1) (intern_kind env k2)
  | KArrow (k1, k2) -> mkarrow (intern_kind env k1) (intern_kind env k2)
  | KEmptyRow -> term_handler (Row (RowDomain.empty))

