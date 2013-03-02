module Make (X : sig

  (* Items. *)

  type item

  (* Blocks. *)

  type block

  (* Each item must carry [prev], [next], and [blck] fields. *)

  val get_prev: item -> item
  val set_prev: item -> item -> unit
  val get_next: item -> item
  val set_next: item -> item -> unit
  val get_blck: item -> block
  val set_blck: item -> block -> unit

  (* Each block must carry [size] and [item] fields. *)

  val get_size: block -> int
  val set_size: block -> int -> unit
  val get_item: block -> item option
  val set_item: block -> item option -> unit

end) = struct

  open X

  (* The items of each block are organized in a possibly empty,
     circular, doubly linked list, via the [prev] and [next]
     fields. *)

  (* Each item is attached to a block, via the [blck] field. *)

  (* Each block keeps track of its size, via the [size] field.  It
     keeps track of an arbitrary item that belongs to it, via the
     [item] field. *)

  let init_block b =
    set_size b 0;
    set_item b None

  let attach x b =
    match get_item b with
    | None ->
	set_prev x x;
	set_next x x;
	set_blck x b;
	set_size b 1;
	set_item b (Some x)
    | Some predecessor ->
	let successor = get_next predecessor in
	set_prev x predecessor;
	set_next x successor;
	set_next predecessor x;
	set_prev successor x;
	set_size b (get_size b + 1)

  let detach x =
    let b = get_blck x in
    let size = get_size b in
    if size = 1 then begin
      set_size b 0;
      set_item b None
    end
    else
      let predecessor = get_prev x
      and successor = get_next x in
      set_next predecessor successor;
      set_prev successor predecessor;
      set_size b (size - 1);
      set_item b (Some predecessor)

  let init_item x b =
    attach x b

  let move x b =
    detach x;
    attach x b

  let block =
    get_blck

  let cardinal =
    get_size

  let choose =
    get_item

  let fold f b accu =
    match get_item b with
    | None ->
	accu
    | Some head ->
	let rec fold x n accu =
	  if n = 0 then
	    accu
	  else
	    fold (get_next x) (n - 1) (f x accu)
	in
	fold head (get_size b) accu

  let elements b =
    fold (fun x xs -> x :: xs) b []

  let robust_fold f b accu =
    List.fold_right f (elements b) accu

end


