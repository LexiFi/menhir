(******************************************************************************)
(*                                                                            *)
(*                                     Feat                                   *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT license, as described in the file LICENSE.               *)
(******************************************************************************)

(* Uniform random generation of large integers. Copied and adapted from Jane
   Street's bignum library. *)

(* [random state range] chooses a [depth] and generates random values using
   [Random.State.bits state], called [1 lsl depth] times and concatenated. The
   preliminary result [n] therefore satisfies [0 <= n < 1 lsl (30 lsl depth)].

   In order for the random choice to be uniform between [0] and [range-1],
   there must exist [k > 0] such that [n < k * range <= 1 lsl (30 lsl depth)].
   If so, [n % range] is returned. Otherwise the random choice process is
   repeated from scratch.

   The [depth] value is chosen so that repeating is uncommon (1 in 1000 or
   less). *)

let bits_at_depth (depth : int) : int =
  30 lsl depth

let range_at_depth (depth : int) : Z.t =
  Z.(one lsl (bits_at_depth depth))

let rec choose_bit_depth_for_range_from range depth =
  if Z.geq (range_at_depth depth) range then depth
  else choose_bit_depth_for_range_from range (depth + 1)

let choose_bit_depth_for_range (range : Z.t) : int =
  choose_bit_depth_for_range_from range 0

let rec random_bigint_at_depth (state : Random.State.t) depth : Z.t =
  if depth = 0 then
    Z.of_int (Random.State.bits state)
  else
    let depth = depth - 1 in
    let prefix = random_bigint_at_depth state depth in
    let suffix = random_bigint_at_depth state depth in
    Z.(prefix lsl (bits_at_depth depth) lor suffix)

let random_value_is_uniform_in_range range depth n =
  let k = Z.(range_at_depth depth / range) in
  Z.lt n Z.(k * range)

let rec large_random_at_depth state range depth =
  let result = random_bigint_at_depth state depth in
  if random_value_is_uniform_in_range range depth result
  then Z.(result mod range)
  else large_random_at_depth state range depth

let large_random state range =
  let tolerance_factor = Z.of_int 1000 in
  let depth = choose_bit_depth_for_range Z.(range * tolerance_factor) in
  large_random_at_depth state range depth

let random state range =
  if Z.leq range Z.zero then
    failwith (Printf.sprintf "Bigint.random: argument %s <= 0" (Z.to_string range))
  else if Z.lt range Z.(one lsl 30) then
    Z.of_int (Random.State.int state (Z.to_int range))
  else
    large_random state range

let random range =
  random (Random.get_state()) range
