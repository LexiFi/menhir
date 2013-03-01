(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/standard.ml,v 1.5 2000/03/01 11:05:59 fpottier Exp $ *)

(* This module implements miscellaneous common operations, which may be considered as missing from the language's
   standard library, hence its name. *)

(* [option (Some value)] returns [value]. *)

let option = function
  | Some value ->
      value
  | None ->
      assert false

(* [do_option (Some value) action] invokes [action value]. [do_option None action] has no effect. *)

let do_option value action =
  match value with
  | Some value ->
      action value
  | None ->
      ()

(* [fold_option action (Some value) accu] returns [action value accu]. [fold_option action None accu] returns
   [accu]. *)

let fold_option action value accu =
  match value with
  | Some value ->
      action value accu
  | None ->
      accu

(* [map_option f v] applies [f] to [v]'s contents, if [v] differs from [None]. *)

let map_option f = function
  | None ->
      None
  | Some v ->
      Some (f v)

(* [array_of_measured_list n list] turns [list] into an array. [list] must have length [n]. *)

let array_of_measured_list n = function
  | [] ->
      [||]
  | head :: tail ->
      let a = Array.create n head in
      let rec fill i = function
	| [] ->
	    a
	| head :: tail ->
	    a.(i) <- head;
	    fill (i + 1) tail in
      fill 1 tail

(* [filter_map] expects a ``constructive predicate'' [p] and a list [l]. A ``constructive predicate'' is a function
   which, given an element [x], returns either another [x'], or nothing. [filter_map p l] returns the list obtained
   by applying [p] to each element of [l], and gathering any elements returned by [p]. [filter_map] is thus a
   combination of [filter] and [map]. *)

let rec filter_map p = function
  | [] ->
      []
  | elem :: rest ->
      match p elem with
      | None ->
	  filter_map p rest
      | Some elem' ->
	  elem' :: (filter_map p rest)

(* [test] accepts a bit field and a list of pairs, each of which contains a mask and an arbitrary value. It returns
   a list containing all values whose corresponding mask yields a non-zero result in a logical \verb+and+ against the
   bit field. *)

let rec test bitfield = function
  | [] ->
      []
  | (mask, flag) :: rest ->
      if bitfield land mask <> 0 then
	flag :: (test bitfield rest)
      else
	test bitfield rest

