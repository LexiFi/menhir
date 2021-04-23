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

type t = Finite of int | Infinity

let equal p1 p2 =
  match (p1, p2) with
  | Finite i1, Finite i2 ->
      i1 = i2
  | Infinity, Infinity ->
      true
  | _, _ ->
      false

let bottom = Infinity

let zero = Finite 0

let one = Finite 1

let is_maximal p = match p with Finite 0 -> true | _ -> false

let min p1 p2 =
  match (p1, p2) with
  | Finite i1, Finite i2 ->
      if i1 <= i2 then p1 else p2
  | p, Infinity | Infinity, p ->
      p

let max p1 p2 =
  match (p1, p2) with
  | Finite i1, Finite i2 ->
      if i1 < i2 then p2 else p1
  | _, Infinity | Infinity, _ ->
      Infinity

let max_lazy p1 p2 = match p1 with Infinity -> p1 | _ -> max p1 (p2 ())

let min_lazy p1 p2 = match p1 with Finite 0 -> p1 | _ -> min p1 (p2 ())

let add p1 p2 =
  match (p1, p2) with
  | Finite i1, Finite i2 ->
      Finite (i1 + i2)
  | _, _ ->
      Infinity

let sub p1 i2 =
  match p1 with Finite i1 -> Finite (i1 - i2) | Infinity -> Infinity

let add_lazy p1 p2 = match p1 with Infinity -> Infinity | _ -> add p1 (p2 ())

let print p =
  match p with Finite i -> string_of_int i | Infinity -> "infinity"

let to_int p = match p with Finite i -> i | Infinity -> max_int

let compare p1 p2 =
  match (p1, p2) with
  | Infinity, Infinity ->
      0
  | Infinity, _ ->
      1
  | _, Infinity ->
      -1
  | Finite n1, Finite n2 ->
      compare n1 n2

let ( < ) p1 p2 = compare p1 p2 < 0

let ( <= ) p1 p2 = compare p1 p2 <= 0

let ( > ) p1 p2 = compare p1 p2 > 0

let ( >= ) p1 p2 = compare p1 p2 >= 0
