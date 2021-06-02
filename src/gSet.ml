(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This signature describes several implementations of sets, including
   [Patricia], [AtomicBitSet], and [SparseBitSet]. *)

module type S = sig

  (* Elements are assumed to have a natural total order. *)

  type element

  (* Sets. *)

  type t

  (* The empty set. *)

  val empty: t

  (* [is_empty s] tells whether [s] is the empty set. *)

  val is_empty: t -> bool

  (* [singleton x] returns a singleton set containing [x] as its only
     element. *)

  val singleton: element -> t

  (* [is_singleton s] tests whether [s] is a singleton set. *)

  val is_singleton: t -> bool

  (* [cardinal s] returns the cardinal of [s]. *)

  val cardinal: t -> int

  (* [choose s] returns an arbitrarily chosen element of [s], if [s]
     is nonempty, and raises [Not_found] otherwise. *)

  val choose: t -> element

  (* [mem x s] returns [true] if and only if [x] appears in the set
     [s]. *)

  val mem: element -> t -> bool

  (* [add x s] returns a set whose elements are all elements of [s],
     plus [x]. *)

  val add: element -> t -> t

  (* [remove x s] returns a set whose elements are all elements of
     [s], except [x]. *)

  val remove: element -> t -> t

  (* [union s1 s2] returns the union of the sets [s1] and [s2]. *)

  val union: t -> t -> t

  (* [inter s t] returns the set intersection of [s] and [t], that is,
     $s\cap t$. *)

  val inter: t -> t -> t

  (* [disjoint s1 s2] returns [true] if and only if the sets [s1] and
     [s2] are disjoint, i.e. iff their intersection is empty. *)

  val disjoint: t -> t -> bool

  (* [iter f s] invokes [f x], in turn, for each element [x] of the
     set [s]. Elements are presented to [f] in increasing order. *)

  val iter: (element -> unit) -> t -> unit

  (* [fold f s seed] invokes [f x accu], in turn, for each element [x]
     of the set [s]. Elements are presented to [f] in increasing
     order. The initial value of [accu] is [seed]; then, at each new
     call, its value is the value returned by the previous invocation
     of [f]. The value returned by [fold] is the final value of
     [accu]. In other words, if $s = \{ x_1, x_2, \ldots, x_n \}$,
     where $x_1 < x_2 < \ldots < x_n$, then [fold f s seed] computes
     $([f]\,x_n\,\ldots\,([f]\,x_2\,([f]\,x_1\,[seed]))\ldots)$. *)

  val fold: (element -> 'b -> 'b) -> t -> 'b -> 'b

  (* [elements s] is a list of all elements in the set [s]. *)

  val elements: t -> element list

  (* [compare] is an ordering over sets. *)

  val compare: t -> t -> int

  (* [equal] implements equality over sets. *)

  val equal: t -> t -> bool

  (* [subset] implements the subset predicate over sets. *)

  val subset: t -> t -> bool

  (* TODO: find a better name.
     [quick_subset a b] is a faster test for set inclusion if it is known that
     either [a] is a subset of [b] or they are disjoint (a ⊆ b ⋁ a ∩ b = ∅).

     This is the case when [a] belongs to partition refined from a set
     containing [b]. This test happens many times during lookahead classes
     computation and the specialized [quick_subset] can be significantly faster
     (an order of magnitude in practice for large alphabets).
  *)
  val quick_subset: t -> t -> bool

  (** {1 Decomposing sets}

      These functions implements the [Refine.DECOMPOSABLE] interface.
      We cannot reference it here as [Refine] is implemented using bitsets,
      that would create a reference cycle.
  *)

  (* [compare_minimum l r] order two sets by comparing their least element *)
  val compare_minimum : t -> t -> int

  (* [extract_prefix l r] split l in two sets (l_min, l_rest) such that:
     - l_min contains elements strictly smaller than the all elements of [r]
     - l_rest contains other elements
  *)
  val extract_prefix : t -> t -> t * t

  (* [extract_common l r] decomposes l and r in (min, l', r') such that :
     - [min] is the set of minimal elements that are part of both [l] and [r]
     - [l = min U l'] and [r = min U r']
  *)
  val extract_common : t -> t -> t * (t * t)

  (* [interval_union l] computes the union of an ordered list of intervals.
     This is an optimized special case of union *)
  val interval_union : t list -> t
end
