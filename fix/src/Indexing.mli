(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

(** Type-level indexation: construct types whose values are enforced to belong
    to finite sets of integers [0..n-1] and are represented as [int] at
    runtime. Two values from different subsets cannot be mixed.

    A phantom parameter ['n] to represent a set at the type-level.
*)

(** A value of type [c : n cardinal] witnesses the fact that the set [n] has
      cardinal [c].
      A [cardinal] is always greater than or equal to 0.
*)
type 'n cardinal
val cardinal : 'n cardinal -> int

(** A value of type [i : n index] is an integer that is guaranteed to belong
    to the set [n].
    If [c : n cardinal], then [0 <= i < c].

    Note: elements of a finite set are called [index] because their main
    purpose is to index information in fixed-size vectors.
    See [Vector] sub-module below.
*)
type 'n index = private int

(** Type-level sets are introduced by modules (to create fresh type names).
    A new set is represented by a pair of a fresh abstract type [n] and a
    [cardinal] value that represents the cardinal of the set.  *)
module type CARDINAL = sig type n val n : n cardinal end

(** Create a new type for a set with a determined cardinal. *)
module Const(X : sig val cardinal : int end) : CARDINAL
val const : int -> (module CARDINAL)

(** The empty set *)
module Empty: CARDINAL

(** "Gensym", for sets whose cardinality is not yet known.
    Creates a new set to which elements can be added as long as its cardinal
    has not been observed. *)
module Gensym() : sig
  include CARDINAL

  (** Add a new element is the set if [cardinal] has not been forced yet.
      It is forbidden to call [fresh] after forcing the cardinal. *)
  val fresh : unit -> n index
end

(** Sum of two sets.
    These definitions implements the disjoint union operator L + R. *)

(** The type [either] is used to tell whether a value belongs to the left or
    the right set *)
type ('l, 'r) either =
  | L of 'l
  | R of 'r

(** The SUM module type.
    It defines a set [n] and exposes the isomorphism between [n] and [l + r].
*)
module type SUM = sig
  type l and r
  include CARDINAL
  val inj_l : l index -> n index
  val inj_r : r index -> n index
  val prj : n index -> (l index, r index) either
end

(** Introduce a new set that is the sum of [L] and [R].
    It is strict in [L.cardinal] but not [R.cardinal]: if [R] is an instance
    of [Gensym()] that has not been forced, new elements can still be added.
    Forcing the resulting cardinal forces [R.cardinal] too.
*)
module Sum(L : CARDINAL)(R : CARDINAL) :
  SUM with type l := L.n
       and type r := R.n

val sum : 'l cardinal -> 'r cardinal ->
  (module SUM with type l = 'l and type r = 'r)

(** Manipulate elements from a finite set *)
module Index : sig
  type 'n t = 'n index
  val of_int : 'n cardinal -> int -> 'n index
  val to_int : 'n index -> int

  val enumerate : 'n cardinal -> (unit -> 'n index)

  val iter : 'n cardinal -> ('n index -> unit) -> unit
end

(** Manipulate fixed-size vectors, whose domain is a type-level [set] *)
type ('n, 'a) vector = private 'a array

module Vector : sig
  type ('n, 'a) t = ('n, 'a) vector

  val get : ('n, 'a) t -> 'n index -> 'a
  val set : ('n, 'a) t -> 'n index -> 'a -> unit

  val length : ('n, 'a) t -> 'n cardinal
  val empty : (_, _) t

  val make : 'n cardinal -> 'a -> ('n, 'a) t
  val make' : 'n cardinal -> (unit -> 'a) -> ('n, 'a) t
  val init : 'n cardinal -> ('n index -> 'a) -> ('n, 'a) t
  val map : ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
end

(** Syntactic sugar to manipulate finite vectors *)

module Infix : sig

  (** [v.%(i)] is [Vector.get v i] *)
  val (.%())   : ('n, 'a) vector -> 'n index -> 'a

  (** [v.%(i) <- x] is [Vector.set v i x] *)
  val (.%()<-) : ('n, 'a) vector -> 'n index -> 'a -> unit

  (** A shortcut for consing an element in a vector of list.
      [v.%::(i) <- x] cons [x] to the list at index [i] in [v] *)
  val (.%::()<-) : ('n, 'a list) vector -> 'n index -> 'a -> unit
end
