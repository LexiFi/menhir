module Make (X : sig
  type t
  val equal : t -> t -> bool
end) = struct

  type element =
    X.t

  (* Because we expect every set to be very small (typically 1 or 2 elements),
     we represent a set in memory simply as a list (without duplicate
     elements). This is more economical than a redundant representation
     (a pair of a list and a set), and should be just as fast, or faster. *)

  type t =
    X.t list

  let empty =
    []

  let singleton x =
    [x]

  (* Of course, [mem] is inefficient. This is linear search. *)

  let rec mem x ys =
    match ys with
    | [] ->
        false
    | y :: ys ->
        X.equal x y || mem x ys

  (* In [diff], we could temporarily convert [xs] into a set so as to speed up
     membership queries, but this seems not worth the trouble (and could be
     slower, if sets are indeed very small). *)

  let diff ys xs =
    List.filter (fun y -> not (mem y xs)) ys

  (* List concatenation is optimized to save work and memory when the
     second list is empty. *)

  let (@) xs ys =
    match ys with [] -> xs | _ -> xs @ ys

  let union xs ys =
    xs @ diff ys xs

  let elements xs =
    xs

end
