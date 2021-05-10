module type S = sig
  include Map.S

  module Domain : Set.S with type elt = key

  val cardinal : 'a t -> int

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t

  val restrict : Domain.t -> 'a t -> 'a t

  val domain : 'a t -> Domain.t

  val multiple_add : key -> 'a -> 'a list t -> 'a list t

  val of_list : (key * 'a) list -> 'a t
end

module Make : functor (Ord : Map.OrderedType) -> S with type key = Ord.t
