(******************************************************************************)
(*                                                                            *)
(*                                     Feat                                   *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT license, as described in the file LICENSE.               *)
(******************************************************************************)

(* A signature for implicit finite sequences. *)

(* These sequences are implicit, which means that they are not explicitly
   represented in memory as actual sequences of elements; instead, they are
   described by a data structure which contains enough information to produce
   an arbitrary element upon request. This design decision imposes some
   constraints on the operations that can be efficiently supported; for
   instance, [filter] is not supported. *)

(* This signature forms an applicative functor. *)

module type IFSEQ = sig

  (* ['a seq] is the type of sequences whose elements have type ['a]. *)
  type 'a seq

  (* Constructors. *)

  (* [empty] is a sequence of length zero. *)
  val empty: 'a seq
  val zero : 'a seq

  (* [singleton x] is a sequence of length one whose single element is [x]. *)
  val singleton: 'a -> 'a seq
  val one:       'a -> 'a seq

  (* [rev xs] is the reverse of the sequence [xs]. *)
  val rev: 'a seq -> 'a seq

  (* [sum s1 s2] is the concatenation of the sequences [s1] and [s2]. *)
  val sum    : 'a seq -> 'a seq -> 'a seq
  val ( ++ ) : 'a seq -> 'a seq -> 'a seq

  (* [product s1 s2] is the Cartesian product of the sequences [s1] and [s2].
     Its length is the product of the lengths of [s1] and [s2]. The first pair
     component is considered most significant. *)
  val product: 'a seq -> 'b seq -> ('a * 'b) seq
  val ( ** ) : 'a seq -> 'b seq -> ('a * 'b) seq

  (* [map phi s] is the image of the sequence [s] through the function [phi].
     If the user wishes to work with sequences of pairwise distinct elements,
     then [phi] should be injective. If furthermore the user wishes to work
     with sequences that enumerate all elements of a type, then [phi] should
     be surjective. *)
  val map: ('a -> 'b) -> 'a seq -> 'b seq

  (* [up i j] is the sequence of the integers from [i] included up to [j]
     excluded. *)
  val up: int -> int -> int seq

  (* Observations. *)

  (* The type [index] is the type of integers used to represent indices and
     lengths. *)
  type index

  (* [length s] is the length of the sequence [s]. *)
  val length: 'a seq -> index

  (* [get s i] is the [i]-th element of the sequence [s]. The index [i] must
     be comprised between zero included and [length s] excluded. *)
  val get: 'a seq -> index -> 'a

  (* [foreach s k] iterates over all elements of the sequence [s]. Each element
     in turn is passed to the loop body [k]. *)
  val foreach: 'a seq -> ('a -> unit) -> unit

  (* [to_seq s k] produces an explicit representation of the sequence [s] as a
     sequence in the sense of OCaml's standard library module [Seq]. This
     sequence is prepended in front of the existing sequence [k]. *)
  val to_seq: 'a seq -> 'a Seq.t -> 'a Seq.t

end
