module type Terminal = sig
  type t

  module Set : Set.S with type elt = t

  val equal : t -> t -> bool

  val print : t -> string
end

module type Symbol = sig
  type t
end

module type Node = sig
  type t

  val number : t -> int

  module Set : Set.S with type elt = t

  module Map : Map.S with type key = t
end
