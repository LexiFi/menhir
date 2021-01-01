(******************************************************************************)
(*                                                                            *)
(*                                     Feat                                   *)
(*                                                                            *)
(*                        François Pottier, Inria Paris                       *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT license, as described in the file LICENSE.               *)
(******************************************************************************)

(* This is an implementation of implicit finite sequences as syntax, that is,
   algebraic data structures. This style should be more efficient than the one
   used in IFSeqObj, because fewer memory blocks are allocated (one block per
   construct instead of typically three) and because it opens the door to
   rebalancing schemes -- e.g., trees of binary [Sum] nodes can be balanced. *)

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

(* The data constructors [Rev], [Sum], [Product], [Map] are annotated with the
   length of the sequence. *)

(* The child of [Rev] cannot be [Empty], [Singleton], or [Rev]. *)

(* The children of [Sum], [Product], [Map] cannot be [Empty]. *)

(* In [Up (a, b)], we require [a < b], so the sequence is nonempty. *)

type _ seq =
| Empty    : 'a seq
| Singleton: 'a -> 'a seq
| Rev      : index * 'a seq -> 'a seq
| Sum      : index * 'a seq * 'a seq -> 'a seq
| Product  : index * 'a seq * 'b seq -> ('a * 'b) seq
| Map      : index * ('a -> 'b) * 'a seq -> 'b seq
| Up       : int * int -> int seq

let is_empty (type a) (s : a seq) : bool =
  match s with
  | Empty ->
      true
  | Singleton _ ->
      false
  | Rev _ ->
      false
  | Sum _ ->
      false
  | Product _ ->
      false
  | Map _ ->
      false
  | Up _ ->
      false

let length (type a) (s : a seq) : index =
  match s with
  | Empty ->
      Z.zero
  | Singleton _ ->
      Z.one
  | Rev (length, _) ->
      length
  | Sum (length, _, _) ->
      length
  | Product (length, _, _) ->
      length
  | Map (length, _, _) ->
      length
  | Up (a, b) ->
      Z.of_int (b - a)

let out_of_bounds () =
  failwith "Index is out of bounds."

let empty =
  Empty

let zero =
  empty

let singleton x =
  Singleton x

let one =
  singleton

let rev (type a) (s : a seq) : a seq =
  match s with
  | Empty ->
      s
  | Singleton _ ->
      s
  | Rev (_, s) ->
      s
  | Sum _ ->
      Rev (length s, s)
  | Product _ ->
      Rev (length s, s)
  | Map _ ->
      Rev (length s, s)
  | Up _ ->
      Rev (length s, s)

let sum s1 s2 =
  if is_empty s1 then
    s2
  else if is_empty s2 then
    s1
  else
    let length = Z.add (length s1) (length s2) in
    Sum (length, s1, s2)

let ( ++ ) =
  sum

let product s1 s2 =
  if is_empty s1 || is_empty s2 then
    empty
  else
    let length = Z.mul (length s1) (length s2) in
    Product (length, s1, s2)

let ( ** ) =
  product

let map phi s =
  if is_empty s then
    empty
  else
    Map (length s, phi, s)

let up a b =
  if a < b then
    (* We might wish to also check that [b - a] does not overflow. *)
    Up (a, b)
  else
    Empty

let rec get : type a . a seq -> index -> a =
  fun s i ->
    match s with
    | Empty ->
        out_of_bounds()
    | Singleton x ->
        if Z.equal i Z.zero then x else out_of_bounds()
    | Rev (n, s) ->
        get s (Z.sub (Z.pred n) i)
    | Sum (_, s1, s2) ->
        let n1 = length s1 in
        if Z.lt i n1 then get s1 i
        else get s2 (Z.sub i n1)
    | Product (_, s1, s2) ->
        let q, r = Z.div_rem i (length s2) in
        get s1 q, get s2 r
    | Map (_, phi, s) ->
        phi (get s i)
    | Up (a, b) ->
        match Z.to_int i with
        | exception Z.Overflow ->
            out_of_bounds()
        | i ->
            let x = a + i in
            if x < a || b <= x then
              out_of_bounds()
            else
              x

let rec foreach : type a . a seq -> bool -> (a -> unit) -> unit =
  fun s sense k ->
    match s with
    | Empty ->
        ()
    | Singleton x ->
        k x
    | Rev (_, s) ->
        foreach s (not sense) k
    | Sum (_, s1, s2) ->
        let s1, s2 = if sense then s1, s2 else s2, s1 in
        foreach s1 sense k;
        foreach s2 sense k
    | Product (_, s1, s2) ->
        foreach s1 sense (fun x1 ->
          foreach s2 sense (fun x2 ->
            k (x1, x2)
          )
        )
    | Map (_, phi, s) ->
        foreach s sense (fun x -> k (phi x))
    | Up (a, b) ->
        if sense then
          for x = a to b - 1 do
            k x
          done
        else
          for x = b - 1 downto a do
            k x
          done

let foreach s f =
  foreach s true f

(* In order to avoid concatenation [Seq.concat] and flattening [Seq.flat_map],
   a producer of a sequence of type ['a Seq.t] must be parameterized over a
   construction function [cons] and a continuation [k]. Thus, a producer has
   a type of the following form: *)

type ('a, 'b) producer =
  ('a -> 'b Seq.t -> 'b Seq.t) -> 'b Seq.t -> 'b Seq.t

(* [interval sense a b] produces the sequence of integers between [a] included
   and [b] excluded. If [sense] is [true], this sequence is produced in
   ascending order; otherwise, it is produced in descending order. *)

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

(* [to_seq s sense] produces the sequence [s]. If [sense] is [true], then this
   sequence is produced in order; otherwise, it is produced in reverse order. *)

(* Parameterizing this definition with [cons] and [k] allows us to avoid using
   [Seq.concat] and [Seq.flat_map]. Without these parameters, the treatment of
   sums and products would require calls to these higher-order functions. *)

let rec to_seq : type a b . a seq -> bool -> (a, b) producer =
  fun s sense cons k ->
    match s with
    | Empty ->
        k
    | Singleton x ->
        cons x k
    | Rev (_, s) ->
        to_seq s (not sense) cons k
    | Sum (_, s1, s2) ->
        let s1, s2 = if sense then s1, s2 else s2, s1 in
        to_seq s1 sense cons (fun () -> to_seq s2 sense cons k ())
    | Product (_, s1, s2) ->
        to_seq s1 sense (fun x1 k ->
          to_seq s2 sense (fun x2 k ->
            cons (x1, x2) k
          ) k
        ) k
    | Map (_, phi, s) ->
        to_seq s sense (fun x k -> cons (phi x) k) k
    | Up (a, b) ->
        interval sense a b cons k

let cons x xs =
  fun () -> Seq.Cons (x, xs)

let to_seq s k =
  to_seq s true cons k

end
