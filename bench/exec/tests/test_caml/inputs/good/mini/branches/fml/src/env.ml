(* $Id$ *)

(** This module implements an environment. *)

type 'a t = (string * 'a) list
    
(** [filtered_lookup pred env] search for the first element of [env]
    that verifies the predicate [pred]. 
*)
let filtered_lookup pred = 
  let rec chop = function
    | [] -> None
    | a :: q when pred a -> Some a
    | a :: q -> chop q
  in chop 
       
let exists = List.exists

let lookup env x = List.assoc x env
  
let filter env f = 
  List.fold_left (fun acu (_, x) -> if f x then x :: acu else acu) [] env
    
let empty = []
  
let add env x t = (x, t) :: env
  
let concat = ( @ )

let iter = List.iter

let fold_left = List.fold_left
