(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/leafSetMap.ml,v 1.2 2000/02/11 16:15:49 fpottier Exp $ *)

(* This signature documents the operations required of maps over sets of leaves. *)

module type S = sig

  module Set : sig

    type 'a t
    type 'a ordering = 'a -> 'a -> int

    val make2: 'a ordering -> 'a -> 'a -> 'a t
    val add: 'a ordering -> 'a -> 'a t -> 'a t
    val union: 'a ordering -> 'a t -> 'a t -> 'a t
    val iter: ('a -> unit) -> 'a t -> unit

  end

  module Map : sig

    type ('a, 'b) t
    type 'a ordering = 'a -> 'a -> int

    val empty: ('a, 'b) t
    val find: 'a ordering -> 'a Set.t -> ('a, 'b) t -> 'b
    val add: 'a ordering -> 'a Set.t -> 'b -> ('a, 'b) t -> ('a, 'b) t

  end

end

