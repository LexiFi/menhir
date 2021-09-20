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

(** [Refine] computes the refined partition of a list of overlapping set: the
    smallest list of non-overlapping sets such that each set is a subset of one
    of the input set.

    This is useful when computing the set of transitions of a DFA when the
    alphabet is large: rather than representing a transition for each letter
    separately, we group them by subset having the same destination.
*)

module type DECOMPOSABLE = sig

  type t
  (** The abstract type representing sets that we want to refine. *)

  val is_empty : t -> bool
  (** A set can be tested for emptiness. *)

  val compare_minimum : t -> t -> int
  (** Order two nonempty sets by their minimal element.

      If we had an function to extract the minimal element, then
      [compare_minimum x y = Element.compare (minimum x) (minimum y)].

      This function is never called on an empty set.
  *)

  val interval_union : t list -> t
  (** Computes the union of a list of nonempty sets.
      [interval_union] requires that all sets [s_i] in the list are ordered by
      both their minimum elements and maximum elements and does not overlap:
      [maximum s_i < minimum s_(i+1)]
  *)

  val extract_unique_prefix : t -> t -> t * t
  (** When [compare_minimum s1 s2 < 0],
      [extract_unique_prefix s1 s2] splits [s1] in [s1_head, s1_tail] such that
      [s1_head] is made of elements of [s1] that are strictly smaller than any
      element in [s2] and [s1_tail] is made of other elements.

      That is, assuming s1 < s2 by [compare_minimum]:
      - for all h in s1_head, t1 in s1_tail and h < t1
      - for all h in s1_head, t2 in s2 and h < t2
      - [s1 = s1_head U s1_tail]
  *)

  val extract_shared_prefix : t -> t -> t * (t * t)
  (** When [compare_minimum s1 s2 = 0],
      [extract_shared_prefix s1 s2 = (common, s1', s2')] such that

      - [common] are elements that are both in [s1] and [s2]
      - [common] elements are smaller than any element in [s1'] and [s2']
      - [s1 = common U s1'] and [s2 = common U s2']
  *)
end

(** The type of refined sets *)
module type S = sig

  type t
  (** Type of a set, like [DECOMPOSABLE.t] *)

  val partition : t list -> t list
  (* Returns the refined partition of a list of sets.

     [zs] is a partition of [xs], iff:
     1) Each element of [xs] can be decomposed in elements of [zs], that is
        for all x in xs, there exists a subset zs' of zs such that x = U zs'
     2) Subsets in [zs] does not overlap, that is
        for all z1, z2 in zs, z1 inter z2 = emptyset
     3) They cover the same universe, that is
        [U zs = U xs]

     [ys = partition xs] is the coarsest partition of the list of sets [xs],
     represented by the list of nonempty sets [ys]. Furthermore, the list [ys]
     is sorted by [compare_minimum].

     It is the coarsest in the sense that for all partition zs of xs and z in
     zs, there exists y in ys such that z is a subset of y.
  *)

  val annotated_partition : (t * 'a) list -> (t * 'a list) list
  (* Returns the refined partition of a list of sets. *)

  val partition_and_total : t list -> t list * t
  (* Returns the refined partition of a list of sets as well as the union of
     all of them. *)
end

module Make (Set : DECOMPOSABLE) : S with type t := Set.t
