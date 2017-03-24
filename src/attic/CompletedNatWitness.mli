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

(* This is an enriched version of [CompletedNat], where we compute not just
   numbers, but also sequences of matching length. *)

(* A property is either [Finite (n, xs)], where [n] is a natural number and
   [xs] is a sequence of length [n]; or [Infinity]. *)

type 'a t =
| Finite of int * 'a Seq.seq
| Infinity

val bottom: 'a t
val equal: 'a t -> 'b t -> bool
val is_maximal: 'a t -> bool

val compare: 'a t -> 'b t -> int

val epsilon: 'a t
val singleton: 'a -> 'a t

val min: 'a t -> 'a t -> 'a t
val add: 'a t -> 'a t -> 'a t

val min_lazy: 'a t -> (unit -> 'a t) -> 'a t
val add_lazy: 'a t -> (unit -> 'a t) -> 'a t

val min_cutoff: 'a t -> (int -> 'a t) -> 'a t
val add_cutoff: (* cutoff: *) int -> 'a t -> (int -> 'a t) -> 'a t

val print: ('a -> string) -> 'a t -> string
val to_int: 'a t -> int
val extract: 'a t -> 'a list
