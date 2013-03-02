(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/fixSet.ml,v 1.6 2000/02/11 16:15:48 fpottier Exp $ *)

(* This module accepts a general-purpose implementation of sets, and specializes its signature by fixing the type
   of elements, as well as the ordering on elements. *)

module Make (X : GSet.S) (Element : Map.OrderedType) = struct

  type element = Element.t
  type t = element X.t

  let empty = X.empty
  let is_empty = X.is_empty
  let singleton = X.singleton

  (* Exception re-labeling isn't currently built into O'Caml. *)

  exception NotSingleton

  let is_singleton s =
    try
      X.is_singleton s
    with X.NotSingleton ->
      raise NotSingleton

  let make2 x1 x2 = X.make2 Element.compare x1 x2

  let cardinal = X.cardinal

  let mem x s = X.mem Element.compare x s
  let add x s = X.add Element.compare x s
  type decision = element -> element -> element
  let fine_add decision x s = X.fine_add Element.compare decision x s
  let remove x s = X.remove Element.compare x s

  let union s1 s2 = X.union Element.compare s1 s2
  let fine_union decision s1 s2 = X.fine_union Element.compare decision s1 s2
  let diff s1 s2 = X.diff Element.compare s1 s2
  let disjoint s1 s2 = X.disjoint Element.compare s1 s2

  let iter = X.iter
  let fold = X.fold
  let fold_rev = X.fold_rev
  let iter2 = X.iter2
  let iterator = X.iterator
  let exists = X.exists

  let compare s1 s2 = X.compare Element.compare s1 s2
  let equal s1 s2 = X.equal Element.compare s1 s2
  let subset s1 s2 = X.subset Element.compare s1 s2
  let filter p s = X.filter Element.compare p s

  let map f s = X.map Element.compare f s
  let monotone_map = X.monotone_map
  let endo_map = X.endo_map

end

