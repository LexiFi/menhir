(* This module helps sequentially number a collection of elements. *)

module Make (X : sig

  (* Maps over elements. *)

  type key
  type 'a t
  
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a

end) (B : sig

  open X

  (* Iteration over all elements. *)

  val iter: (key -> unit) -> unit

  (* The base number. *)

  val base: int

end) = struct
 
  open X
  open B

  let dummy =
    ref None

  let n =
    ref 0

  let () =
    iter (fun x ->
      n := !n + 1;
      match !dummy with
      | None ->
	  dummy := Some x;
      | Some _ ->
	  ()
    )

  let n =
    !n

  let fail _ =
    assert false

  let index, xedni =
    match !dummy with
    | None ->
	assert (n = 0);
	fail, fail
    | Some dummy ->
	let direct, inverse, i =
	  ref empty,
	  Array.make n dummy,
	  ref base
	in
	iter (fun x ->
	  let number = !i in
	  direct := add x number !direct;
	  inverse.(number - base) <- x;
	  i := number + 1
	);
	let direct x =
	  try
	    find x !direct
	  with Not_found ->
	    assert false
	and inverse number =
	  inverse.(number - base)
	in
	direct, inverse

end

