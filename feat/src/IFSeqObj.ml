(******************************************************************************)
(*                                                                            *)
(*                                     Feat                                   *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT license, as described in the file LICENSE.               *)
(******************************************************************************)

(* This is an implementation of implicit finite sequences as objects, that is,
   records of closures. This is the implementation style proposed in the Feat
   paper by Duregard et al. *)

(* In this implementation, the constructors have time complexity O(1),
   under the assumption that the arithmetic operations provided by [Z]
   cost O(1) as well. *)

module Make (Z : sig
  type t
  val zero: t
  val one: t
  val of_int: int -> t
  val pred: t -> t
  val add: t -> t -> t
  val sub: t -> t -> t
  val mul: t -> t -> t
  val div_rem: t -> t -> t * t
  val equal: t -> t -> bool
  val lt: t -> t -> bool
  exception Overflow
  val to_int: t -> int
end) = struct

type index =
  Z.t

(* A sequence stores its length (which is computed at construction time)
   as well as [get] and [foreach] methods. *)

(* The [foreach] method takes a Boolean sense as a parameter. This allows
   us to easily implement the [foreach] method of a reversed sequence, as
   produced by the [rev] combinator. *)

(* For comments on [to_seq], see IFSeqSyn. *)

type ('a, 'b) producer =
  ('a -> 'b Seq.t -> 'b Seq.t) -> 'b Seq.t -> 'b Seq.t

type 'a seq = {
  length : index;
  get    : index -> 'a;
  foreach: bool -> ('a -> unit) -> unit;
  to_seq : 'b . bool -> ('a, 'b) producer;
}

let is_empty s =
  Z.equal s.length Z.zero

let out_of_bounds () =
  failwith "Index is out of bounds."

let empty =
  let length = Z.zero
  and get _ = out_of_bounds()
  and foreach _sense _k = ()
  and to_seq _sense _cons k = k in
  { length; get; foreach; to_seq }

let zero =
  empty

let singleton x =
  let length = Z.one
  and get i = if Z.equal i Z.zero then x else out_of_bounds()
  and foreach _sense k = k x
  and to_seq _sense cons k = cons x k in
  { length; get; foreach; to_seq }

let one =
  singleton

let rev s =
  let length =
    s.length
  and get i =
    s.get (Z.sub (Z.pred s.length) i)
  and foreach sense k =
    s.foreach (not sense) k
  and to_seq sense cons k =
    s.to_seq (not sense) cons k
  in
  { length; get; foreach; to_seq }

let sum s1 s2 =
  let length =
    Z.add s1.length s2.length
  and get i =
    if Z.lt i s1.length then s1.get i
    else s2.get (Z.sub i s1.length)
  and foreach sense k =
    let s1, s2 = if sense then s1, s2 else s2, s1 in
    s1.foreach sense k;
    s2.foreach sense k
  and to_seq sense cons k =
    let s1, s2 = if sense then s1, s2 else s2, s1 in
    s1.to_seq sense cons (fun () -> s2.to_seq sense cons k ())
  in
  { length; get; foreach; to_seq }

let sum s1 s2 =
  (* To save space and reduce access time, we recognize an empty sequence as a
     neutral element for concatenation. *)
  if is_empty s1 then
    s2
  else if is_empty s2 then
    s1
  else
    sum s1 s2

let ( ++ ) =
  sum

let product s1 s2 =
  let length =
    Z.mul s1.length s2.length
  and get i =
    let q, r = Z.div_rem i s2.length in
    s1.get q, s2.get r
  and foreach sense k =
    s1.foreach sense (fun x1 ->
      s2.foreach sense (fun x2 ->
        k (x1, x2)
      )
    )
  and to_seq sense cons k =
    s1.to_seq sense (fun x1 k ->
      s2.to_seq sense (fun x2 k ->
        cons (x1, x2) k
      ) k
    ) k
  in
  { length; get; foreach; to_seq }

let product s1 s2 =
  (* To save space, we recognize an empty sequence as an absorbing element for
     product. This also eliminates the risk of a division by zero in the above
     code, which could arise if the user attempts an out-of-bounds access. *)
  if is_empty s1 || is_empty s2 then
    empty
  else
    product s1 s2

let ( ** ) =
  product

let map phi s =
  let length = s.length
  and get i = phi (s.get i)
  and foreach sense k = s.foreach sense (fun x -> k (phi x))
  and to_seq sense cons k = s.to_seq sense (fun x k -> cons (phi x) k) k in
  { length; get; foreach; to_seq }

let map phi s =
  (* To save space, we recognize an empty sequence as a fixed point for map. *)
  if is_empty s then
    empty
  else
    map phi s

let rec interval sense a b : (int, 'b) producer =
  fun cons k ->
    if a < b then
      (* Compute the first element [x] and the parameters [a] and [b]
         of the recursive call. *)
      let x, a, b = if sense then a, a+1, b else b-1, a, b-1 in
      (* Produce [x] and delay the recursive call. *)
      cons x (fun () -> interval sense a b cons k ())
    else
      k

let up a b =
  if a < b then
    let length = Z.of_int (b - a)
    and get i =
      match Z.to_int i with
      | exception Z.Overflow ->
          out_of_bounds()
      | i ->
          let x = a + i in
          if x < a || b <= x then
            out_of_bounds()
          else
            x
    and foreach sense k =
      if sense then
        for x = a to b - 1 do
          k x
        done
      else
        for x = b - 1 downto a do
          k x
        done
    and to_seq sense cons k =
      interval sense a b cons k
    in
    { length; get; foreach; to_seq }
  else
    empty

let length s =
  s.length

let get s i =
  s.get i

let foreach s k =
  s.foreach true k

let cons x xs =
  fun () -> Seq.Cons (x, xs)

let to_seq s k =
  s.to_seq true cons k

end
