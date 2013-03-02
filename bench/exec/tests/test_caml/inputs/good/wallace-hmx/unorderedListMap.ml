(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/unorderedListMap.ml,v 1.3 2000/02/11 16:15:55 fpottier Exp $ *)

(* This is a trivial implementation of general-purpose maps, based on unordered lists. Its [add] operation works in
   constant time, but its [lookup] function is very inefficient, since its complexity is not even linear in the number
   of bindings in the map; it is, in fact, linear in the number of previous [add] operations.

   Note that, even though [GMap.S] allows some map operations to take advantage of a total ordering over keys, this
   implementation does not take advantage of it. *)

(* Maps are simply lists. *)

type ('a, 'b) t =
    ('a * 'b) list

(* Orderings are unused in this implementation, except to perform equality tests. *)

type 'a ordering = 'a -> 'a -> int

(* The empty map. *)

let empty =
  []

(* [lookup o k m] looks up the value associated to the key [k] in the map [m], and raises [Not_found] if no value is
   bound to [k]. It performs a linear search. *)

let rec lookup o key = function
  | [] ->
      raise Not_found
  | (key', data) :: rest ->
      if o key key' = 0 then data
      else lookup o key rest

(* [add o k d m] returns a map whose bindings are all bindings in [m], plus a binding of the key [k] to the datum
   [d]. If a binding already exists for [k], it is overridden. This is done in constant time by \verb+cons+'ing a new
   binding in front of the old map. *)

let add _ key data map =
  (key, data) :: map

(* [singleton k d] returns a map whose only binding is from [k] to [d]. *)

let singleton key data =
  [key, data]

