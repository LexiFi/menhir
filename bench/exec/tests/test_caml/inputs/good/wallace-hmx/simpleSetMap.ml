(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/simpleSetMap.ml,v 1.2 2000/02/11 16:15:51 fpottier Exp $ *)

(* This module, parameterized over an implementation of sets, provides maps whose keys are sets. As its name implies,
   its implementation is straightforward. *)

module Make (X : sig

  type 'a t
  type 'a ordering = 'a -> 'a -> int

  val empty: 'a t
  val make2: 'a ordering -> 'a -> 'a -> 'a t
  val add: 'a ordering -> 'a -> 'a t -> 'a t
  val union: 'a ordering -> 'a t -> 'a t -> 'a t
  val iter: ('a -> unit) -> 'a t -> unit
  val compare: 'a ordering -> 'a t -> 'a t -> int
  val memp: ('a -> int) -> 'a t -> 'a

end) = struct

  (* Our [Set] component is simply [X]. *)

  module Set = X 

  module Map = struct

    (* Maps are simply sets of pairs, where each pair has a key (i.e. a set of basic elements) as its first
       component and a piece of data as its second component. *)

    type ('a, 'b) t = (('a X.t) * 'b) X.t
    type 'a ordering = 'a X.ordering

    (* Since keys are unique within a given map, pairs are ordered after their first component. The following
       function, which must be passed an ordering [o] over basic elements, compares two such pairs. *)

    let compare o (key1, _) (key2, _) =
      X.compare o key1 key2

    let search o key1 (key2, _) =
      X.compare o key1 key2

    (* The empty map is an empty set of pairs. *)

    let empty =
      X.empty

    (* Finding a key requires looking for a pair, given its first element only. This is made possible by the
       fact that pairs are ordered according to their first elements. *)

    let find o key m =
      let _, data = X.memp (search o key) m in
      data

    (* Adding a new binding. *)

    let add o key data m =
      X.add (compare o) (key, data) m

  end

end

