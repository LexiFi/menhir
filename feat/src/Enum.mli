(******************************************************************************)
(*                                                                            *)
(*                                     Feat                                   *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT license, as described in the file LICENSE.               *)
(******************************************************************************)

(* An enumeration of type ['a enum] can be loosely thought of as a set of
   values of type ['a], equipped with a notion of size. More precisely, it
   is a function of a size [s] to a subset of inhabitants of size [s],
   presented as a sequence. *)

(* We expose the fact that enumerations are functions, instead of making [enum]
   an abstract type, because this allows the user to make recursive definitions.
   It is up to the user to ensure that recursion is well-founded; as a rule of
   thumb, every recursive call must appear under [pay]. It is also up to the
   user to take precautions so that these functions have constant time
   complexity (beyond the cost of an initialization phase). This is typically
   achieved by using a memoizing fixed point combinator [fix] instead of
   ordinary recursive definitions. *)

type 'a enum =
  int -> 'a IFSeq.seq

(* [empty] is the empty enumeration. *)
val empty: 'a enum
val zero : 'a enum

(* The enumeration [just x] contains just the element [x].
   Its size is considered to be zero. *)
val just: 'a -> 'a enum

(* The enumeration [enum x] contains the elements in the sequence [xs].
   Their size is considered to be zero. *)
val enum: 'a IFSeq.seq -> 'a enum

(* The enumeration [pay e] contains the same elements as [e],
   but their size is increased by one. *)
val pay: 'a enum -> 'a enum

(* [sum e1 e2] is the union of the enumerations [e1] and [e2]. It is up
   to the user to ensure that the sets [e1] and [e2] are disjoint. *)
val sum    : 'a enum -> 'a enum -> 'a enum
val ( ++ ) : 'a enum -> 'a enum -> 'a enum

(* [exists xs e] is the union of all enumerations of the form [e x], where [x]
   is drawn from the list [xs]. (This is an indexed sum.) It is up to the user
   to ensure that the sets [e1] and [e2] are disjoint. *)
val exists: 'a list -> ('a -> 'b enum) -> 'b enum

(* [product e1 e2] is the Cartesian product of the enumerations
   [e1] and [e2]. *)
val product: 'a enum -> 'b enum -> ('a * 'b) enum
val ( ** ) : 'a enum -> 'b enum -> ('a * 'b) enum

(* [product e1 e2] is a subset of the Cartesian product [product e1 e2]
   where the sizes of the left-hand and right-hand pair components must
   differ by at most one. *)
val balanced_product: 'a enum -> 'b enum -> ('a * 'b) enum
val ( *-* )         : 'a enum -> 'b enum -> ('a * 'b) enum

(* [map phi e] is the image of the enumeration [e] through the function [phi].
   It is up to the user to ensure that [phi] is injective. *)
val map: ('a -> 'b) -> 'a enum -> 'b enum

(* The enumeration [finite xs] contains the elements in the list [xs].
   They are considered to have size zero. *)
val finite: 'a list -> 'a enum

(* [bool] is the enumeration [false; true]. *)
val bool: bool enum

(* If [elem] is an enumeration of the type ['a], then [list elem] is an
   enumeration of the type ['a list]. It is worth noting that every call
   to [list elem] produces a fresh memoizing function, so (if possible)
   one should avoid applying [list] twice to the same argument; that
   would be a waste of time and space. *)
val list: 'a enum -> 'a list enum

(* Suppose we wish to enumerate lists of elements, where the validity of an
   element depends on which elements have appeared earlier in the list. For
   instance, we might wish to enumerate lists of instructions, where the set
   of permitted instructions at some point depends on the environment at this
   point, and each instruction produces an updated environment. If [fix] is a
   suitable fixed point combinator and if the function [elem] describes how
   elements depend on environments and how elements affect environments, then
   [dlist fix elem] is such an enumeration. Each list node is considered to
   have size 1. Because the function [elem] produces a list (as opposed to an
   enumeration), an element does not have a size. *)

(* The fixed point combinator [fix] is typically of the form [curried fix],
   where [fix] is a fixed point combinator for keys of type ['env * int].
   Memoization takes place at keys that are pairs of an environment and a
   size. *)

(* The function [elem] receives an environment and must return a list of pairs
   of an element and an updated environment. *)

val dlist:
  ('env -> 'a list enum) Fix.fix ->
  ('env -> ('a * 'env) list) ->
  ('env -> 'a list enum)

(* [sample m e i j k] is a sequence of at most [m] elements of every size
   comprised between [i] (included) and [j] (excluded) extracted out of the
   enumeration [e], prepended in front of the existing sequence [k]. At every
   size, if there at most [m] elements of this size, then all elements of this
   size are produced; otherwise, a random sample of [m] elements of this size
   is produced. *)
val sample: int -> 'a enum -> int -> int -> 'a Seq.t -> 'a Seq.t
