module Make (X : sig

  type t

  val compare: t -> t -> int

end) : sig

  type t =
      X.t * X.t

  val compare: t -> t -> int

end = struct

  type t =
      X.t * X.t

  let reorder (x, y as pair) =
    if X.compare x y > 0 then
      (y, x)
    else
      pair

  let compare pair1 pair2 =
    Order.lexicographic_pair X.compare X.compare (reorder pair1) (reorder pair2)

end
