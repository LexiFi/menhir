(******************************************************************************)
(*                                                                            *)
(*                                     Feat                                   *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT license, as described in the file LICENSE.               *)
(******************************************************************************)

open IFSeqSig

(* An implementation of the signature SEQ,
   instantiated with unbounded integers. *)

include IFSEQ with type index = Z.t

(* Iterated sum. *)
val bigsum: 'a seq list -> 'a seq

(* Indexed iterated sum. *)
val exists: 'a list -> ('a -> 'b seq) -> 'b seq

(* [sample m s k] is an explicit sequence of at most [m] elements extracted
   out of the implicit sequence [s], prepended in front of the existing
   sequence [k]. If [length s] at most [m], then all elements of [s] are
   produced. Otherwise, a random sample of [m] elements extracted out of
   [s] is produced. *)
val sample: int -> 'a seq -> 'a Seq.t -> 'a Seq.t
