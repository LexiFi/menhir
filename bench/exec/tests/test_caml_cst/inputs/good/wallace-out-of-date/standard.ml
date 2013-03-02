(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/standard.ml,v 1.2.6.6 1999/04/05 16:45:40 francois Exp $ *)

open Errors

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Applying an associative binary operator to a list with at least one element.

*)

let list_apply_assoc_op1 operator = function
    [] ->
      raise (CantHappen "Empty list passed to Standard.list_apply_assoc_op1.")
  | elem :: rest ->
      List.fold_right operator rest elem
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A utility to iterate over two lists at once. The two lists must have the same length.

*)

let rec list_fold_right2 f l1 l2 accu = match (l1, l2) with
  [], [] ->
    accu
| h1 :: t1, h2 :: t2 ->
    f h1 h2 (list_fold_right2 f t1 t2 accu)
| _ ->
    raise (CantHappen "The two lists have different lengths in Standard.list_fold_right2")
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Like assoc, returns the element of the list, but also returns the list without this element so that it will only
be looked up once.

*)

let rec destructive_list_assoc key = function
    [] ->
      raise Not_found
  | ((key', elem) as pair) :: rest ->
      if key = key' then elem, rest
      else begin
        let result, rest = destructive_list_assoc key rest in
      	result, pair :: rest
      end
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Computing a log_2 from positive integers to integers. Tail recursive.

*)

let log2 n =
  let rec log2 accu n =
    if n = 1 then accu
    else log2 (succ accu) (n lsr 1) in
  log2 0 n
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Filtering a list.

*)

let list_filter predicate list =
  let rec filter = function
      [] ->
	[]
    | (elem :: rest) as list ->
	let rest' = filter rest in
	if predicate elem then
	  if rest == rest' then list else elem :: rest'
	else
	  rest' in
  filter list
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

When the domain type and the image type are identical, List.map can be optimized to preserve sharing.

*)

let list_endo_map f list =
  let rec map = function
      [] ->
	[]
    | (elem :: rest) as list ->
	let rest' = map rest
	and elem' = f elem in
	if (elem == elem') & (rest == rest') then list
	else elem' :: rest' in
  map list
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Extracting a value out of an option type, doing something with it, and doing nothing if it's absent.

*)

let do_option value action =
  match value with
    Some value ->
      action value
  | None ->
      ()
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Extracts a value out of an option. The value must be present.

*)

let option = function
    None ->
      raise (CantHappen "Standard.option")
  | Some value ->
      value
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Simultaneously iterating over three lists.

*)

let rec list_iter3 f l1 l2 l3 =
  match l1, l2, l3 with
    [], [], [] ->
      ()
  | a1 :: l1, a2 :: l2, a3 :: l3 ->
      f a1 a2 a3;
      list_iter3 f l1 l2 l3
  | _, _, _ ->
      raise (CantHappen "Standard.list_iter3")
;;

let rec list_fold_left3 f accu l1 l2 l3 =
  match l1, l2, l3 with
    [], [], [] ->
      accu
  | a1 :: l1, a2 :: l2, a3 :: l3 ->
      list_fold_left3 f (f accu a1 a2 a3) l1 l2 l3
  | _, _, _ ->
      raise (CantHappen "Standard.list_fold_left3")
;;

let rec list_fold_right3 f l1 l2 l3 accu =
  match l1, l2, l3 with
    [], [], [] ->
      accu
  | a1 :: l1, a2 :: l2, a3 :: l3 ->
      f a1 a2 a3 (list_fold_right3 f l1 l2 l3 accu)
  | _, _, _ ->
      raise (CantHappen "Standard.list_fold_right3")
;;

