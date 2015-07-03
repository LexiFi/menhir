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

val epsilon: 'a t
val singleton: 'a -> 'a t

val min: 'a t -> 'a t -> 'a t
val add: 'a t -> 'a t -> 'a t

val min_lazy: 'a t -> (unit -> 'a t) -> 'a t
val add_lazy: 'a t -> (unit -> 'a t) -> 'a t

val min_cutoff: 'a t -> (int -> 'a t) -> 'a t
val add_cutoff: (* cutoff: *) int -> 'a t -> (int -> 'a t) -> 'a t

val until_finite: 'a t -> (unit -> 'a t) -> 'a t

val print: ('a -> string) -> 'a t -> string
