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

(* $Id: env.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module implements an environment. *)

type ('a, 'b) t = ('a * 'b) list
    
(** [filtered_lookup pred env] search for the first element of [env]
    that verifies the predicate [pred]. 
*)
let filtered_lookup pred = 
  let rec chop = function
    | [] -> 
	None

    | (a,_) :: q when pred a -> 
	Some a

    | (a,_) :: q -> 
	chop q
  in chop
       
let exists = 
  List.exists

let lookup env x = 
  List.assoc x env
  
let filter env f = 
  List.fold_left (fun acu (_, x) -> if f x then x :: acu else acu) [] env
    
let empty = 
  []
  
let add env x t = 
  (x, t) :: env
  
let concat = 
  ( @ )

let iter = 
  List.iter

let fold_left = 
  List.fold_left

let map = 
  List.map
