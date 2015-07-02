(* This is an enriched version of [CompletedNat], where we compute not just
   numbers, but also lists of matching length. During the fixed point
   computation, instead of manipulating actual lists, we manipulate only
   recipes for constructing lists. These recipes can be evaluated by the user
   after the fixed point has been reached. *)

(* A property is either [Finite (n, xs)], where [n] is a natural number and
   [xs] is a (recipe for constructing a) list of length [n]; or [Infinity]. *)

type 'a t =
| Finite of int * 'a list Lazy.t
| Infinity

val bottom: 'a t
val equal: 'a t -> 'b t -> bool
val is_maximal: 'a t -> bool

val min: 'a t -> 'a t -> 'a t
val add: 'a t -> 'a t -> 'a t

val min_lazy: 'a t -> 'a t Lazy.t -> 'a t
val add_lazy: 'a t -> 'a t Lazy.t -> 'a t

val print: ('a -> string) -> 'a t -> string
