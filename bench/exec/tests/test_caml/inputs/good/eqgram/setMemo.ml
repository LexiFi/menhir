module Make (Set : sig

  type elt
  type t

  val empty: t
  val add: elt -> t -> t
  val mem: elt -> t -> bool

end) = struct

  open Set

  let create () =

    (* Allocate a reference that holds a set. *)

    let table =
      ref empty
    in

    (* Define a function that tests for membership in the current
       set and, at the same time, inserts. *)

    let test element =
      mem element !table ||
      begin
	table := add element !table;
	false
      end
    in

    test

end
