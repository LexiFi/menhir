(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/Attic/rowsig.ml,v 1.1.2.12 1999/04/05 16:45:39 francois Exp $ *)
(*

A boilerplate module. The interface documents which operations are required to deal with rows. The implementation
provides these operations by taking them from our various "set" and "map" modules.

*)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Any implementation of the SetSig signature can be used. The corresponding implementation of maps is then used.

*)

module MySet = (Set7 : Setsig.SetSig)
module MyMap = Genmap.Make(MySet)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Row signatures are sets of strings, while rows are maps from strings to types.

*)

type row_set = string MySet.t
type 'a row_map = (string, 'a) MyMap.t

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Most operations simply call through to the underlying modules.

*)

module RowSet = struct

  let empty = MySet.empty Pervasives.compare
  let is_empty = MySet.is_empty
  let elements = MySet.elements
  let subset = MySet.subset
  let diff = MySet.diff
  let union = MySet.union
  let fold = MySet.fold
  let compare = MySet.compare
  let lift = MyMap.lift

  let freeze = MySet.freeze
  let unfreeze x = MySet.unfreeze x Pervasives.compare

end

module RowMap = struct

  let empty () = MyMap.empty Pervasives.compare
  let is_empty = MyMap.is_empty

  exception StrictAdd
  let add key value m = try MyMap.add key value m with MyMap.StrictAdd -> raise StrictAdd

  let find = MyMap.find
  let cardinality = MyMap.cardinality
  let domain m = MyMap.domain Pervasives.compare m
  let union = MyMap.union
  let iter = MyMap.iter
  let iter2 = MyMap.iter2
  let iter3 = MyMap.iter3
  let fold = MyMap.fold
  let fold2 = MyMap.fold2
  let map = MyMap.map
  let disjoint = MyMap.disjoint
  let split = MyMap.split

  let freeze = MyMap.freeze
  let unfreeze m = MyMap.unfreeze m Pervasives.compare

end

