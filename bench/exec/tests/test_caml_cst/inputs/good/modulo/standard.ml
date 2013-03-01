(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/standard.ml,v 1.4 2002/12/17 08:30:30 fpottier Exp $ *)

(* This module defines a few general-purpose functions. *)

let rec power f q x =
  match q with
  | 0 ->
      x
  | _ ->
      power f (q-1) (f x)

let flatmap f l =
  List.flatten (List.map f l)

(* [mapconcat f l1 l2] is equivalent to [(List.map f l1) @ l2]. *)

let mapconcat f l1 l2 =
  List.fold_right (fun elem1 accu ->
    (f elem1) :: accu
  ) l1 l2

(* [mapfold f accu l] is a combination of [fold_left] and [map]. The
   list is traversed from left to right, and the image of every
   element through [f] is computed, while maintaining an accumulator.
   The function returns the final accumulator and the list of
   images. *)

let rec mapfold f accu = function
  | [] ->
      accu, []
  | some :: more ->
      let accu, some = f accu some in
      let accu, more = mapfold f accu more in
      accu, some :: more

(* [has_duplicates o l] tells whether the list [l] contains duplicates.
   [o] must be an appropriate ordering function for the list's elements. *)

exception Duplicate

let has_duplicates o l =

  let rec look last = function
    | [] ->
	()
    | elem :: rest ->
	if o last elem = 0 then
	  raise Duplicate
	else
	  look elem rest in

  match List.sort o l with
  | [] ->
      false
  | elem :: rest ->
      try
	look elem rest;
	false
      with Duplicate ->
	true

