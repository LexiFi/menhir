(* This module provides [map] and [fold] functions for the predefined
   containers. *)

open Asynchrony

(* ------------------------------------------------------------------------- *)

(* Lists. *)

module List = struct

  type 'a t =
      'a list

  let map =
    List.map

  let rec self_map f ws =
    match ws with
    | [] ->
	ws
    | x :: xs ->
	let y = f x
	and ys = map f xs in
	if x == y && xs == ys then
	  ws
	else
	  y :: ys

  let fold =
    List.fold_left

  let rec fold2 f accu l1 l2 =
    match l1, l2 with
    | [], [] ->
	accu
    | a1 :: l1, a2 :: l2 ->
	let accu = f accu a1 a2 in
	fold2 f accu l1 l2
    | _, _ ->
	raise asynchrony

end

(* ------------------------------------------------------------------------- *)

(* Options. *)

module Option = struct

  type 'a t =
      'a option

  let map f = function
    | None ->
	None
    | Some x ->
	Some (f x)

  let self_map f o =
    match o with
    | None ->
	o
    | Some x ->
	let y = f x in
	if x == y then
	  o
	else
	  Some y

  let fold f accu = function
    | None ->
	accu
    | Some x ->
	f accu x

  let fold2 f accu o1 o2 =
    match o1, o2 with
    | None, None ->
	accu
    | Some x1, Some x2 ->
	f accu x1 x2
    | None, Some _
    | Some _, None ->
	raise asynchrony

end

(* ------------------------------------------------------------------------- *)

(* Decorations. *)

type 'a decoration =
    Location.location * 'a

module Decoration = struct

  type 'a t =
      'a decoration

  let map f (l, x) =
    (l, Location.under l f x)

  let self_map f ((l, x) as d) =
    let y = Location.under l f x in
    if x == y then
      d
    else
      (l, y)

  let fold f accu (l, x) =
    Location.under l (fun () -> f accu x) ()

  let fold2 f accu (_, x1) (_, x2) =
    f accu x1 x2 (* don't update [current] when traversing two structures -- don't know which location to choose *)

end

