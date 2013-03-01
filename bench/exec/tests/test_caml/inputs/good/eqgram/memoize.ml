module Make (M : sig

  type key
  type 'a t

  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a

end) = struct

  open M

  let memoize (f : key -> 'a) =
    let m : 'a t ref =
      ref empty
    in
    fun x ->
      try
	find x !m
      with Not_found ->
	let y = f x in
	m := add x y !m;
	y

end

