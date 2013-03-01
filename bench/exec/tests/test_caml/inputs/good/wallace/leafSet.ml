(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/leafSet.ml,v 1.2 2000/02/11 16:15:49 fpottier Exp $ *)

(* This signature documents the operations required of sets of variables. *)

module type S = sig

  type 'a t
  type 'a ordering = 'a -> 'a -> int

  val empty: 'a t
  val is_empty: 'a t -> bool
  val singleton: 'a -> 'a t
  exception NotSingleton
  val is_singleton: 'a t -> 'a
  val mem: 'a ordering -> 'a -> 'a t -> bool
  val add: 'a ordering -> 'a -> 'a t -> 'a t
  val remove: 'a ordering -> 'a -> 'a t -> 'a t
  val union: 'a ordering -> 'a t -> 'a t -> 'a t
  val iter: ('a -> unit) -> 'a t -> unit
  val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val exists: ('a -> bool) -> 'a t -> bool
  val filter: 'a ordering -> ('a -> bool) -> 'a t -> 'a t
  val compare: 'a ordering -> ('a t) ordering
  val map: 'b ordering -> ('a -> 'b) -> 'a t -> 'b t

end

