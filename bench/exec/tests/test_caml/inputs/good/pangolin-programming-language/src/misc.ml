(* $Id: misc.ml 25 2007-09-27 15:01:06Z yann.regisgianas $ *)

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

let id x = x

let ( $$ ) f g = 
  fun x -> g (f x)

let ( !< ) f =
  fun x y -> f y x

let destruct_list = function
  | x :: xs -> (x, xs)
  | _ -> assert false

let extract n l = 
  let rec loop acu n l = 
    if n = 0 then List.rev acu, l else
      match l with
	| [] -> raise Not_found
	| x :: xs -> loop (x :: acu) (n - 1) xs
  in
    loop [] n l

let destruct_single = function
  | [ x ] -> x
  | _ -> assert false

let augment l x = 
  l := x :: !l

let list_fold_map f init l =
  let rec flm acu l' = function
    | [] -> 
	(acu, List.rev l')
    | x :: xs -> 
	let (acu, y) = f acu x in
	  flm acu  (y :: l') xs
  in
    flm init [] l

let list_fold_map2 f init l1 l2 =
  let rec flm acu l' l1' l2' = 
    match l1', l2' with
    | [], [] -> 
	(acu, List.rev l')
    | x1 :: xs1, x2 :: xs2 -> 
	let (acu, y) = f acu x1 x2 in
	  flm acu (y :: l') xs1 xs2
    | _ -> assert false
  in
    flm init [] l1 l2

let rec list_map_to_4_lists f = function
  | [] -> ([], [], [], [])
  | x :: xs -> 
      let (x1, x2, x3, x4) = f x in
      let (l1, l2, l3, l4) = list_map_to_4_lists f xs in
	(x1 :: l1, x2 :: l2, x3 :: l3, x4 :: l4)

let xor b1 b2 = 
  match b1, b2 with
    | true, true -> false
    | false, false -> false
    | _ -> true

let rec enumerate (l, i) = function
  | [] -> 
      (List.rev l, i)
  | x :: xs -> 
      enumerate ((i, x) :: l, (i + 1)) xs

let enumerate l = 
  enumerate ([], 0) l

(* FIXME: should use strassen algorithm to be optimal. *)

(* multiply [m1] by [m2] in [m3]. *)
let mult_bool_matrix n m1 m2 m3 = 
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do 
      m3.(i).(j) <- false;
      for k = 0 to n - 1 do
	m3.(i).(j) <- m3.(i).(j) || m1.(i).(k) && m2.(k).(j)
      done
    done
  done

let matrix_copy m = 
  Array.map Array.copy m
	  
let print_matrix m = 
  Array.iter (fun l -> 
		Array.iter (Printf.eprintf "%b ") l;
		Printf.eprintf "\n") m

let starred m = 
  let n = Array.length m in
  let m1 = matrix_copy m and m2 = matrix_copy m in
  let f = ref false in
    mult_bool_matrix n m m1 m2;
    while (m2 <> m1) do
      if !f then mult_bool_matrix n m m2 m1 else mult_bool_matrix n m m1 m2;
      f := not !f;
    done;
    m2

(** First class module for associative structure. *)
type ('t, 'from, 'target) assoc_module = {
  empty     : 't;
  add   : 'from -> 'target -> 't -> 't;
  find  : 'from -> 't -> 'target
}

let transitive_closure f d { empty = map_empty; add = map_add; find = map_find } = 

  (* Enumerate nodes. *)

  let (e, size) = enumerate d in

  (* Create adjacency matrix. *)

  let m = Array.make_matrix size size false in 

    List.iter (fun (i, c) -> 
		 List.iter (fun (j, d) -> m.(i).(j) <- i = j || f c d) e) e;

    (* Compute its star. *)

    let t = starred m in

    (* [m] is a mapping from object to indices. *)

    let m = List.fold_left (fun m (i, c) -> map_add c i m) map_empty e in

    (* The successors of [i]. *)

    let succs = Array.init size
      (fun i -> 
	 let s = ref [] in
	   for j = 0 to size - 1 do
	     if t.(i).(j) && i <> j then
	       s := List.assoc j e :: !s
	   done;
	   !s)
    in

      (* One dependency question. *)

      ((fun c1 c2 ->
	  let i1 = map_find c1 m in
	  let i2 = map_find c2 m in 
	    t.(i1).(i2)),

       (fun c -> succs.(map_find c m)))

let cut_last l =
  let rec cut = function
    | [] -> 
	assert false
    | [ x ] -> 
	([], x)
    | x :: xs -> 
	let (xs, y) = cut xs in 
	  (x :: xs, y)
  in
    cut l

module StringSet = Set.Make (String)
module StringMap = Map.Make (String)

let list_unionq l1 =
  List.fold_left (fun acu x -> if not (List.memq x acu) then x::acu else acu)
    l1

let rec last = function
  | [] -> assert false
  | [ x ] -> x
  | x :: xs -> last xs

let unSome = function Some x -> x | _ -> assert false

let notf f = fun x -> not (f x)

let const x = fun _ -> x

let map_union m1 m2 =
  StringMap.fold StringMap.add m1 m2

let default d = function
    Some x -> x
  | None -> d

let twice f x y = (f x, f y)

(** [iter] is similar to [List.iter], but does not require [f] to
    return a result of type [unit]. Use with caution. *)
let rec iter f = function
  | [] ->
      ()
  | a :: l ->
      let _ = f a in
      iter f l

let list_assocp pred x l =
  snd (List.find (fun (y, _) -> pred x y) l)

let repeat ?(from=0) n c =
  let rec repeat i = 
    if i = n then [] else c i :: repeat (i + 1) 
  in
    repeat from 

let fconst c =
  fun () -> c

let rec list_remove_assoc_if pred = function
  | [] -> []
  | (x, t) :: xs -> if pred x then xs 
    else (x, t) :: list_remove_assoc_if pred xs
  

let list_same_length l1 l2 = 
  List.length l1 = List.length l2

let rec list_map3 f l1 l2 l3 = 
  match (l1, l2, l3) with
    | [], [], [] -> []
    | x1 :: xs1, x2 :: xs2, x3 :: xs3 -> 
	(f x1 x2 x3) :: (list_map3 f xs1 xs2 xs3)
    | _ -> assert false

let list_mapi f l =
  let rec map c = function
    | [] -> []
    | x :: xs -> f c x :: map (c + 1) xs
  in
    map 0 l

let tmp_filename_from_string ?(remove=true) ext s g = 
  let f = Filename.temp_file "pangolin" ext in
  let fc = open_out f in
    output_string fc s;
    close_out fc;
    let r = g f in
      if remove then ignore (Sys.command (Printf.sprintf "rm %s" f));
      r

module Cache (In : Hashtbl.HashedType) (Out : sig type t end) =
struct
  module H = Hashtbl.Make (In) 


  type cache_fun = (In.t -> Out.t) -> In.t -> Out.t

  let cache_in_file filename = 
    let h = try 
      let f = open_in filename in
      let h = (Marshal.from_channel f : Out.t H.t) in
	close_in f;
	h
    with Sys_error _ -> H.create 65537
    in
    let cache f x = 
      (try
	 H.find h x
       with Not_found ->
	 let ret = f x in
	   H.add h x ret;
	   ret)
    in
    let cache_save () = 
      let f = open_out filename in
	Marshal.to_channel f h [];
	close_out f
    in
      (cache, cache_save)
end
	  	
module ExMake (X : sig type 'a t end) : sig 
  type t
  val pack : 'a X.t -> t
  type 'a user = { f : 'b. 'b X.t -> 'a }
  val use : 'a user -> t -> 'a
end = struct
  type 'a user = { f : 'b. 'b X.t -> 'a }
  type t = { pack : 'a. 'a user -> 'a }
  let pack impl = { pack = fun user -> user.f impl }
  let use f p = p.pack f
end

    
