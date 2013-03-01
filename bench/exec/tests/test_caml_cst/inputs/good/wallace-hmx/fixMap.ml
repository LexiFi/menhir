(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/fixMap.ml,v 1.8 2000/02/11 16:15:48 fpottier Exp $ *)

(* This module accepts a general-purpose implementation of maps, and specializes its signature by fixing the type
   of keys, as well as the ordering on keys. *)

module Make (X : GMap.S) (Key : Map.OrderedType) = struct

  type key = Key.t
  type 'a t = (key, 'a) X.t

  let empty = X.empty
  let lookup k m = X.lookup Key.compare k m
  let add k d m = X.add Key.compare k d m
  let singleton = X.singleton

  type 'a decision = 'a -> 'a -> 'a

  let is_empty = X.is_empty
  let cardinal = X.cardinal
  let lookup_and_remove k m = X.lookup_and_remove Key.compare k m
  let union m1 m2 = X.union Key.compare m1 m2
  let fine_union decide m1 m2 = X.fine_union Key.compare decide m1 m2
  let iter = X.iter
  let fold = X.fold
  let fold_rev = X.fold_rev
  let iter2 = X.iter2
  let map = X.map
  let endo_map = X.endo_map

  module Domain = FixSet.Make(X.Domain)(Key)

  let domain = X.domain
  let lift = X.lift
  let corestrict d m = X.corestrict Key.compare d m

end

