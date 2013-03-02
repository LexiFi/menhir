(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/misc.ml,v 1.5 2004/04/09 17:13:39 fpottier Exp $ *)

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

end

(** A debugging flag. *)
let debug =
  ref false

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

