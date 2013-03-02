(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/rowMap.ml,v 1.8 2000/02/11 16:15:51 fpottier Exp $ *)

(* The implementation of rows is parameterized over an implementation of ``fixed'' maps, i.e. maps whose keys have
   some abstract type. This signature (a sub-signature of [GMap.Fixed]) defines our requirements. *)

module type S = sig

  type key
  type 'a t

  val empty: 'a t
  val lookup: key -> 'a t -> 'a
  val add: key -> 'a -> 'a t -> 'a t

  type 'a decision = 'a -> 'a -> 'a

  val is_empty: 'a t -> bool
  val cardinal: 'a t -> int
  val union: 'a t -> 'a t -> 'a t
  val fine_union: 'a decision -> 'a t -> 'a t -> 'a t

  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val iter2: (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit

  val map: ('a -> 'b) -> 'a t -> 'b t
  val endo_map: ('a -> 'a) -> 'a t -> 'a t

  (* [Domain]'s signature is, of course, a sub-signature of [GSet.Fixed], except its elements are specified to be
     keys. *)

  module Domain : sig

    type element = key
    type t

    val empty: t
    val is_empty: t -> bool
    val singleton: element -> t

    type decision = element -> element -> element

    val fine_union: decision -> t -> t -> t
    val diff: t -> t -> t
    val disjoint: t -> t -> bool

    val compare: t -> t -> int
    val equal: t -> t -> bool
    val subset: t -> t -> bool

  end

  val domain: 'a t -> Domain.t
  val lift: (key -> 'a) -> Domain.t -> 'a t
  val corestrict: 'a t -> Domain.t -> 'a t

end

