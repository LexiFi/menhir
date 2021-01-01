(******************************************************************************)
(*                                                                            *)
(*                                     Feat                                   *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT license, as described in the file LICENSE.               *)
(******************************************************************************)

(* The default implementation of the signature SEQ is based on IFSeqSyn,
   instantiated with the unbounded integers provided by [zarith]. *)

include IFSeqSyn.Make(Z)

(* Iterated sum. *)

let bigsum ss =
  List.fold_left sum zero ss

(* Indexed iterated sum. *)

let exists (xs : 'a list) (s : 'a -> 'b seq) : 'b seq =
  bigsum (List.map s xs)

(* Extract a randomly chosen sample of [m] elements out of a sequence [s]. *)

(* We do not protect against repetitions, as they are unlikely when [s] is
   long. *)

(* For some reason, [Z.random] does not exist, and [Random.int] stops
   working at [2^30]. [Bigint.random] works around these problems. *)

let rec sample (m : int) (s : 'a seq) (k : 'a Seq.t) : 'a Seq.t =
  if m > 0 then
    fun () ->
      let i = Bigint.random (length s) in
      let x = get s i in
      Seq.Cons (x, sample (m - 1) s k)
  else
    k

(* If the sequence [s] is short enough, then produce all of its elements;
   otherwise produce a randomly chosen sample, as above. *)

let sample (m : int) (s : 'a seq) (k : 'a Seq.t) : 'a Seq.t =
  if length s <= Z.of_int m then to_seq s k else sample m s k
