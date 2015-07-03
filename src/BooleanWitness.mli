(* This is an enriched version of [Boolean], where we compute not just a
   Boolean property, [Unreachable] or [Reachable], but also (in the latter
   case) a path (a list). During the fixed point computation, instead of
   manipulating actual lists, we manipulate only recipes for constructing
   lists. These recipes can be evaluated by the user after the fixed point has
   been reached. *)

(* A property is either [Reachable xs], where [xs] is a (recipe for
   constructing a) path; or [Unreachable]. *)

type 'a t =
| Reachable of 'a list Lazy.t
| Unreachable

val bottom: 'a t
val equal: 'a t -> 'b t -> bool
val is_maximal: 'a t -> bool

val epsilon: 'a t
val singleton: 'a -> 'a t

val min: 'a t -> 'a t -> 'a t
val add: 'a t -> 'a t -> 'a t

val min_lazy: 'a t -> (unit -> 'a t) -> 'a t
val add_lazy: 'a t -> (unit -> 'a t) -> 'a t

val print: ('a -> string) -> 'a t -> string
