(* The Boolean lattice. *)

type property =
    bool

let bottom =
  false

let equal (b1 : bool) (b2 : bool) =
  b1 = b2

let is_maximal b =
  b

let union (b1 : bool) (b2 : bool) =
  b1 || b2

