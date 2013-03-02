(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/toy/context.ml,v 1.3 2000/02/11 16:16:32 fpottier Exp $ *)

(* This signature documents the operations required of contexts, i.e. maps from identifiers to types. This is a
   sub-signature of [GMap.Fixed]. *)

module type S = sig

  type key
  type 'a t
  type 'a decision = 'a -> 'a -> 'a

  val empty: 'a t
  val singleton: key -> 'a -> 'a t
  val lookup_and_remove: key -> 'a t -> 'a * 'a t
  val fine_union: 'a decision -> 'a t -> 'a t -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val map: ('a -> 'b) -> 'a t -> 'b t

end

