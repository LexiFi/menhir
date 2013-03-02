open Positions
open Misc
open Sig
open MiniKindInferencer
open MiniConstraint
open MiniAlgebra
open MiniMultiEquation
open MiniTypingEnvironment
open MiniTypingExceptions
open MiniTypes

type eq_theory =
    (crterm * crterm) list 
      
let empty_eq_theory = []
  
let join_eq_theory e e' = e @ e'

let rec is_rigid_var v = 
  match variable_structure v with
      Some t -> fold (fun t f -> f && is_rigid_var t) t true
    | None -> is_rigid v  

and is_rigid_term = function
    TVariable v -> is_rigid_var v 
  | TTerm t ->
      MiniAlgebra.fold (fun t f -> f && is_rigid_term t) t true 
  
let unequal_constants v v' =
  (UnionFind.find v).kind = Constant &&
  (UnionFind.find v').kind = Constant &&
  (not (v == v'))

exception InconsistentEqTheory

let add_eq e t1 t2 = 
(*  Printf.eprintf "Add %s === %s ? in \n" 
    (MiniTermPrinter.print_term false t1)
    (MiniTermPrinter.print_term false t2);
*)
  let rec add t1 t2 acu = 
    let add_if_rigid t1 t2 e = 
      if is_rigid_term t1 && is_rigid_term t2 then 
	(t1, t2) :: e
      else e
    in
    match (t1, t2) with

      | TVariable v, TVariable v' when unequal_constants v v' ->
	  raise InconsistentEqTheory

      | TVariable v, t | t, TVariable v -> 
	    add_if_rigid t1 t2 acu

      | TTerm st1, TTerm st2 ->
	  fold2 add st1 st2 (add_if_rigid t1 t2 acu)
  in
    add t1 t2 e

let singleton_eq t1 t2 = add_eq empty_eq_theory t1 t2
  
let eq_class e t = 
  List.fold_left (fun c (t1, t2) -> 
		    if t1 = t then 
		      t2 :: c 
		    else if t2 = t then 
		      t1 :: c
		    else c) [ t ] e
    
let find_non_rigid_in eqt =
  let (ts, ts') = List.split eqt in
  try 
    List.find (notf is_rigid_term) ts 
  with Not_found -> 
    try 
      List.find (notf is_rigid_term) ts'
    with Not_found -> assert false

let rec is_eq_modulo pos eqt t1 t2 = 
(*  Printf.eprintf "Check %s =?= %s in \n" 
    (MiniTermPrinter.print_term false t1)
    (MiniTermPrinter.print_term false t2);
  List.iter (fun (v, v') -> Printf.eprintf "%s = %s\n "
	       (MiniTermPrinter.print_term false v)
	       (MiniTermPrinter.print_term false v')) eqt;
  Printf.eprintf "\n";  
*)
  let ec1 = eq_class eqt t1 and ec2 = eq_class eqt t2 in
(*    Printf.eprintf "%s[%s]\n%s[%s]\n"
      (MiniTermPrinter.print_term false t1)
      (print_separated_list " " (MiniTermPrinter.print_term false) ec1)
      (MiniTermPrinter.print_term false t2)
      (print_separated_list " " (MiniTermPrinter.print_term false) ec2); 
*)
    List.exists (fun t -> List.memq t ec2) ec1 
    || List.exists 
      (fun t -> List.exists 
	 (fun t' -> 
	    match t, t' with
	      | TVariable v, TVariable v' -> 
		  v == v'

	      | TTerm t, TTerm t' -> 
		  fold2 (fun t1 t2 acu -> acu && is_eq_modulo pos eqt t1 t2)
		    t t'
		    true

	      | _ -> false
		  
	 ) ec1
      ) ec2


      
       
