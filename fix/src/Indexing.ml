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

open Sigs

type 'n cardinal = int lazy_t

let cardinal (lazy x : 'n cardinal) : int = x

type 'n index = int

module type CARDINAL = sig type n val n : n cardinal end

module Const(X : sig val cardinal : int end) =
struct
  type n
  let () = assert (X.cardinal >= 0)
  let n = lazy X.cardinal
end

module Empty = struct
  type n
  let n = lazy 0
end

let const c : (module CARDINAL) =
  assert (c >= 0);
  (module struct type n let n = lazy c end)

module Gensym() = struct
  type n
  let counter = ref 0
  let n = lazy !counter

  let fresh () =
    assert (not (Lazy.is_val n));
    let result = !counter in
    incr counter;
    result
end

(** Sum of two sets.
    These definitions implements the disjoint union operator L + R. *)

type ('l, 'r) either =
  | L of 'l
  | R of 'r

module type SUM = sig
  type l and r
  include CARDINAL
  val inj_l : l index -> n index
  val inj_r : r index -> n index
  val prj : n index -> (l index, r index) either
end

module Sum(L : CARDINAL)(R : CARDINAL) =
struct
  type n = unit

  type l = L.n
  type r = R.n

  let l_n = cardinal L.n
  let r_n = R.n

  let n =
    if Lazy.is_val r_n then
      let n = l_n + cardinal r_n in
      lazy n
    else
      lazy (l_n + cardinal r_n)

  let inj_l x = x
  let inj_r y = l_n + y
  let prj x = if x < l_n then L x else R (x - l_n)
end

let sum (type l r)
    (l : l cardinal)
    (r : r cardinal) =
  let module L = struct type n = l let n = l end in
  let module R = struct type n = r let n = r end in
  (module Sum(L)(R) : SUM with type l = l and type r = r)

(** Manipulate elements from a finite set *)
module Index = struct
  type 'n t = 'n index

  let of_int (c : _ cardinal) i =
    let lazy c = c in
    assert (i >= 0 && i < c); i

  let to_int i = i

  exception End_of_set

  let enumerate (c : 'n cardinal) =
    let c = cardinal c in
    let k = ref 0 in
    (fun () ->
       let result = !k in
       if result >= c then raise End_of_set;
       incr k;
       result)

  let iter (c : 'n cardinal) f =
    let lazy c = c in
    for i = 0 to c - 1 do
      f i
    done
end

(** Manipulate fixed-size vectors, whose domain is a type-level [set] *)
type ('n, 'a) vector = 'a array

module Vector = struct
  type ('n, 'a) t = ('n, 'a) vector

  (* Modular abstraction guarantee that get and set calls are always safe. *)
  let get = Array.unsafe_get
  let set = Array.unsafe_set
  let set_cons t i x = set t i (x :: get t i)

  let length vec = let c = Array.length vec in lazy c

  let empty = [||]

  let make (n : _ cardinal) v =
    let n = cardinal n in
    Array.make n v

  let make' (n : _ cardinal) f =
    let n = cardinal n in
    if n = 0 then
      empty
    else
      Array.make n (f ())

  let init (n : _ cardinal) f =
    let n = cardinal n in
    Array.init n f

  let map = Array.map
end