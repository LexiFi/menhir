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

(* $Id: misc.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module contains miscellaneous utilities. *)

(** [iter] is similar to [List.iter], but does not require [f] to
    return a result of type [unit]. Use with caution. *)
let rec iter f = function
  | [] ->
      ()
  | a :: l ->
      let _ = f a in
      iter f l

(** If [l] is a list of pairs of a key and a datum, and if [p] is a
    predicate on keys, then [assocp p l] returns the datum associated
    with the first key that satisfies [p] in [l]. It raises
    [Not_found] if no such key exists. *)
let rec assocp p = function
  | [] ->
      raise Not_found
  | (key, data) :: l ->
      if p key then data else assocp p l

(** Sets of strings. *)
module StringSet = Set.Make(String)

(** Maps over strings. *)
module StringMap = struct

  include Map.Make(String)

  let singleton key data =
    add key data empty

  exception Strict of string

  let strict_add key data m =
    try
      let _ = find key m in
      raise (Strict key)
    with Not_found ->
      add key data m

  let union m1 m2 =
    fold add m1 m2

  let strict_union m1 m2 =
    fold strict_add m1 m2

  let domain m = 
    fold (fun k _ acu -> StringSet.add k acu) m StringSet.empty 

end

(** A debugging flag. *)
let debug =
  ref true

(** Prints a list of elements, with one occurrence of the separator
    between every two consecutive elements. *)
let print_separated_list separator print_elem xs =
  
  let rec loop x = function
    | [] ->
	print_elem x
    | y :: xs ->
	print_elem x ^
	separator ^
	loop y xs
  in

  match xs with
  | [] ->
      ""
  | x :: xs ->
      loop x xs


let make_indexes default  =   
  (** A hash table maps all known identifiers to integer values. It
      provides one direction of the global mapping. *)
  let table =
    Hashtbl.create 1023
  and
  (** An infinite array maps all known integer values to identifiers. It
      provides the other direction of the global mapping. *)
  array =
    InfiniteArray.make default (* Dummy data. *) 
    (** A global counter contains the next available integer label. *)
  and counter =
    ref 0
  in
  (** [import s] associates a unique label with the identifier [s],
      possibly extending the global mapping if [s] was never encountered
      so far. Thus, if [s] and [t] are equal strings, possibly allocated
      in different memory locations, [import s] and [import t] return
      the same label. The identifier [s] is recorded and may be later
      recovered via [export]. *)
  let import s =
    try
      Hashtbl.find table s
    with Not_found ->
      let i = !counter in
      Hashtbl.add table s i;
      InfiniteArray.set array i s;
      counter := i + 1;
      i

  (** [export i] provides access to the inverse of the global mapping,
      that is, associates a unique identifier with every label. The
      identifier associated with a label is the one originally supplied
      to [import]. *)
  and export i =
    assert (i < !counter);
    InfiniteArray.get array i
      
  and find s = 
    Hashtbl.find table s
  in
    (import, export, find)

(* FIXME: sort what is still necessary or not. *)

let rec last = function
    [] -> raise Not_found
  | [ a ] -> a
  | a :: q -> last q

let curry f = fun (x, y) -> f x y

let switch_args f = fun x y -> f y x

let safe_find x = 
  try
    x
  with Not_found -> assert false

let notf f = fun x -> not (f x)

let eqf x = fun y -> y = x

let twice f x y = (f x, f y)

exception Inconsistency

let pmapq conv v = 
  try
    List.assq v conv
  with Not_found -> v

let default d = function
    Some x -> x
  | None -> d

let is_now v v' = 
  match v with
  | None -> v'
  | Some x -> if (v <> v') then raise Inconsistency else v

let rec itern n f = 
  if n = 0 then [] else (f ()) :: (itern (n-1) f)
  
let updatef f x v = List.map (fun (y, v') -> if x = y then (x, f v) else (y, v')) 

let update x v = updatef (fun x -> x) x v


let set_of_list = 
  List.fold_left (fun a c -> StringSet.add c a) StringSet.empty 

let intersect s1 s2 = 
  StringSet.inter s1 s2 <> StringSet.empty

let map_union m1 m2 = 
  StringMap.fold StringMap.add m1 m2

exception InvalidOptionUse

let unSome = function
  | None -> raise InvalidOptionUse
  | Some x -> x

let unSomef f = fun x -> unSome (f x)

let split3 l = List.fold_left (fun (l1,l2,l3) (a, b, c) -> a::l1, b::l2, c::l3)
	       ([], [], []) l

let split4 l = List.fold_left 
		 (fun (l1,l2,l3,l4) (a, b, c, d) -> a::l1, b::l2, c::l3, d::l4)
	       ([], [], [], []) l

let split5 l = 
  List.fold_left 
    (fun (l1,l2,l3,l4,l5) (a, b, c, d, e) -> a::l1, b::l2, c::l3, d::l4, e::l5)
	       ([], [], [], [], []) l

let rec transpose l =
  match l with
    | [] -> []
    | [] :: _ -> []
    | [ a ] :: _ -> [ List.map List.hd l ]
    | _ -> let hs, ts = 
	List.split (List.map (function l -> List.hd l, List.tl l) l)
      in hs :: transpose ts

let list_unionq l1 = 
  List.fold_left (fun acu x -> if not (List.memq x acu) then x::acu else acu)
    l1 

let list_removeq x = function
  | [] -> []
  | a :: q when x == a -> q
  | l -> l

let const f = fun v -> f

let array_associ x a = 
  let len = Array.length a in
  let rec chop i = 
    if i < len then 
      if fst a.(i) = x then i
      else chop (i + 1)
    else
      raise Not_found 
  in
    chop 0

let array_assoc x a = 
  snd a.(array_associ x a)

let assoc_proj1 l =
  fst (List.split l)

let assoc_proj2 l =
  snd (List.split l)

let proj1_3 (x,_,_) = x
let proj2_3 (_,y,_) = y
let proj3_3 (_,_,z) = z

let proj1_4 (x,_,_,_) = x
let proj2_4 (_,y,_,_) = y
let proj3_4 (_,_,z,_) = z
let proj4_4 (_,_,_,t) = t

let proj1_5 (x,_,_,_,_) = x
let proj2_5 (_,y,_,_,_) = y
let proj3_5 (_,_,z,_,_) = z
let proj4_5 (_,_,_,t,_) = t
let proj5_5 (_,_,_,_,v) = v

let split3 l = 
  List.fold_left (fun (xs, ys, zs) (x, y, z) -> (x :: xs, y :: ys, z :: zs))
    ([],[],[]) l

let isNonef f = fun x -> f x = None

(* FIXME: not tail rec. *)
let rec gcombine l r = 
  match l, r with
    | [], r -> [], [], r
    | a :: q, [] -> [], l, []
    | a :: p, b :: q -> let g, rl, rr = gcombine p q in
	(a, b) :: g, rl, rr

(* FIXME: optimize. *)
let list_map_array f l = 
  let t = Array.of_list l in
   Array.map f t

let list_iteri f l =
  ignore (List.fold_left (fun acu x -> f acu x; acu+1) 0 l)

let list_mapi f l =
  List.rev (snd (List.fold_left (fun (i,q) x -> (i+1, f i x ::q)) (0,[]) l))

exception NonDisjointCase 
exception Failure of exn list

type ('a, 'b) either = Left of 'a | Right of 'b

let one_of e1 e2 =
  let r_e1 = try (Some (e1 ()), None) with ex -> (None, Some ex)
  and r_e2 = try (Some (e2 ()), None) with ex -> (None, Some ex)
  in
    match r_e1, r_e2 with
      | (None, Some ex1), (None, Some ex2) -> raise (Failure [ ex1; ex2 ])
      | (Some v1, None), (Some v2, None)   -> raise NonDisjointCase
      | (Some v1, None), (None, Some ex)   -> Left  (v1, ex)
      | (None, Some ex), (Some v2, None)   -> Right (v2, ex)
      | _                                  -> assert false

let reraise e exn1 exn2 =
  try e () with ex when ex = exn1 -> raise exn2 | ex -> raise ex

let just_try e =
  try Some (e ()) with Not_found -> None

let ( ^^ ) = ( ^ )

let opt_apply f x = 
  match f with
    | Some f -> Some (f x)
    | None -> None

let list_foralli f = 
  let rec test i = function
      [] -> true
    | a :: q -> if f i a then test (i+1) q else false
  in
    test 0 

let list_existsi f = 
  let rec test i = function
      [] -> false
    | a :: q -> if f i a then true else test (i+1) q 
  in
    test 0 

let list_mapi2 f = 
  let rec loop i l1 l2 = 
    match (l1, l2) with
	[], [] -> []
      | r :: rs, q :: qs -> f i r q :: (loop (i+1) rs qs)
      | _ -> failwith "Invalid arguments for mapi2"
  in
    loop 0 

let are_distinct l = 
  let rec fold acu = function
      [] -> None
    | x :: q -> if List.mem x acu then Some x else fold (x::acu) q
  in fold [] l

let all_equal l = 
  let rec fold acu = function
      [] -> true, acu
    | x :: q -> (match acu with 
		     None -> fold (Some x) q 
		   | Some y -> if x = y then fold acu q else (false, None))
  in
    fold None l

