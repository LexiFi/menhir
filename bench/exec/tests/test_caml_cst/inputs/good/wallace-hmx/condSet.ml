(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/condSet.ml,v 1.7 2000/02/11 16:15:47 fpottier Exp $ *)

(* This signature documents the operations required of sets of conditional constraints. *)

module type S = sig

  type 'a t
  type 'a ordering = 'a -> 'a -> int

  val empty: 'a t
  val is_empty: 'a t -> bool
  val add: 'a ordering -> 'a -> 'a t -> 'a t
  val union: 'a ordering -> 'a t -> 'a t -> 'a t
  val iter: ('a -> unit) -> 'a t -> unit
  val map: 'b ordering -> ('a -> 'b) -> 'a t -> 'b t

end

