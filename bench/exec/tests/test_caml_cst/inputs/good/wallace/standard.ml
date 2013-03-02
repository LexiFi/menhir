(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/standard.ml,v 1.3 2000/02/11 16:15:52 fpottier Exp $ *)

(* This module implements miscellaneous common operations, which may be considered as missing from the language's
   standard library, hence its name. *)

(* [do_option (Some value) action] invokes [action value]. [do_option None action] has no effect. *)

let do_option value action =
  match value with
  | Some value ->
      action value
  | None ->
      ()

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

