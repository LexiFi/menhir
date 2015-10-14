(* This module implements resizable arrays, that is, arrays that can
   grow upon explicit request. *)

type 'a t

(* [make capacity x] creates a resizable array of logical length 0, whose
   physical length is initially [capacity], and whose default element is [x].
   The default element is used to fill empty slots in the physical array. *)

val make: int -> 'a -> 'a t

(* [length a] returns the current logical length of the array [a]. *)

val length: 'a t -> int

(* [resize a n] changes the logical length of the array [a] to [n]. If the
   length decreases, any excess elements are lost. The capacity of the
   underlying physical array remains the same. If the length increases, the
   new positions are filled with the array's default element, as initially
   supplied to [make]. The capacity of the underlying physical array grows
   by at least a factor of two. *)

val resize: 'a t -> int -> unit

(* [get a i] returns the element contained at offset [i] in the array [a].
   Slots are numbered 0 and up. [i] must be strictly less than the array's
   current logical length. *)

val get: 'a t -> int -> 'a

(* [set a i x] sets the element contained at offset [i] in the array [a]
   to [x]. Slots are numbered 0 and up. [i] must be strictly less than
   the array's current logical length. *)

val set: 'a t -> int -> 'a -> unit

