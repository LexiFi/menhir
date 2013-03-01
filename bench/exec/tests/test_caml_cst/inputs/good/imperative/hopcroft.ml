(* ---------------------------------------------------------------------------- *)

(* Internal data structures. *)

type 'point item = {

    (* Each item represents a user point, to which it carries a pointer. *)

    item_point: 'point;

    (* For each function, each item is the end point of several edges,
       whose start points we keep track of. *)

    mutable item_fun_edges: 'point item list array(*[f]*);

    (* Items are organized in blocks. *)

    mutable item_prev: 'point item;
    mutable item_next: 'point item;
    mutable item_block: 'point block;

  }

and 'point block = {

    (* Items are organized in blocks. *)

    mutable block_size: int;
    mutable block_item: 'point item option;

    (* To allow splitting a block in time proportional to the number
       of elements to be removed, we need to associate a ``quitter''
       block with each block. This association lasts while [split] is
       being called, and is broken when [commit] is called. *)

    mutable block_quitter: 'point block option;

    (* This field allows determining, in constant time, whether a pair
       $(k, B)$ appears in the splitter bag. *)

    mutable block_queued: bool array(*[f]*);

  } 

(* ---------------------------------------------------------------------------- *)

(* The functor header. *)

module Make (P : sig

  (* User points. *)

  type point

  (* Each user point must carry an [item] field. *)

  val get_item: point -> point item
  val set_item: point -> point item -> unit

  (* The set of all points and the initial partition. This is a list of
     disjoint blocks, where each block is itself represented as a non-empty
     list of distinct points. *)

  val partition: point list list

  (* The number of functions with respect to which we are performing
     refinement. *)

  val f: int

  (* The function graph. This is a list of edges. Each edge consists of a
     source point, a function label, comprised between [0] inclusive and [f]
     exclusive, and a target point. *)

  val edges: (point * int * point) list

end) = struct

open P

(* ---------------------------------------------------------------------------- *)

(* Set up a bag of pairs of a function index and a block. *)

module Bag =
  Bag.Make (struct
    type item = int * point block
    let get_mark (k, b) = b.block_queued.(k)
    let set_mark (k, b) m = b.block_queued.(k) <- m
  end)

(* ---------------------------------------------------------------------------- *)

(* Ensure that blocks represent a partition of all items. *)

module Partition =
  DoublyLinkedPartition.Make (struct
    type i = point item
    type b = point block
    type item = i
    type block = b
    let get_prev x = x.item_prev
    let set_prev x y = x.item_prev <- y
    let get_next x = x.item_next
    let set_next x y = x.item_next <- y
    let get_blck x = x.item_block
    let set_blck x b = x.item_block <- b
    let get_size b = b.block_size
    let set_size b s = b.block_size <- s
    let get_item b = b.block_item
    let set_item b x = b.block_item <- x
  end)

(* ---------------------------------------------------------------------------- *)

(* Setting up blocks. *)

let new_block () =
  let block = {
    block_size = 0; (* dummy *)
    block_item = None; (* dummy *)
    block_quitter = None;
    block_queued = Array.make f false;
  } in
  Partition.init_block block;
  block

let add_block points =
  let block = new_block() in
  List.iter (fun p ->
    let rec item = {
      item_point = p;
      item_prev = item; (* dummy *)
      item_next = item; (* dummy *)
      item_block = block; (* dummy *)
      item_fun_edges = Array.make f [];
    } in
    Partition.init_item item block;
    P.set_item p item
  ) points

let () =
  List.iter add_block partition

(* ---------------------------------------------------------------------------- *)

(* Setting up edges. *)

(* [add_edge (p1, k, p2)] adds an edge for function [k] from point [p1] to
   point [p2]. Complexity: O(1). *)

let add_edge (p1, k, p2) =

  (* Obtain the items corresponding to these points. *)

  let item1 = get_item p1
  and item2 = get_item p2 in

  (* Add [item1] to [item2]'s list of predecessors. *)

  item2.item_fun_edges.(k) <- item1 :: item2.item_fun_edges.(k);

  (* [item2]'s block is now a possible splitter for function
     [k]. Insert it into the splitter bag. *)

  Bag.add (k, Partition.block item2)

let () =
  List.iter add_edge edges

(* ---------------------------------------------------------------------------- *)

(* Splitting blocks. *)

(* [split item] takes the element [item] off its block. This splits
   the block (if not already split). Complexity: O(1). *)

let split item =
  let block = Partition.block item in

  (* If a quitter block doesn't already exist, create one. *)

  let quitter =
    match block.block_quitter with
    | None ->
	let quitter = new_block() in
	block.block_quitter <- Some quitter;
	quitter
    | Some quitter ->
	quitter
  in

  (* Move the item off the block and into the quitter. *)

  Partition.move item quitter

(* ---------------------------------------------------------------------------- *)

(* Committing splits. *)

(* [commit block] commits a split. That is, if both the block and its
   quitter are non-empty, then the quitter becomes a new regular
   block. Otherwise, the quitter disappears. Complexity: O(f) +
   O(|quitter|). *)

(* Note that it is harmless to call [commit] twice on the same block. *)

let commit block =
  match block.block_quitter with
  | None ->
      ()
  | Some quitter ->

      (* Break the association between the block and its quitter. *)

      block.block_quitter <- None;

      (* Check whether any elements remain in the original block. *)

      if Partition.cardinal block = 0 then

	(* The main block is empty, so no splitting has to be
	   done. Move all elements back to the main block. *)

	Partition.robust_fold (fun x () ->
	  Partition.move x block
        ) quitter ()

      else

	(* The quitter must be turned into a real block. *)

	(* Update the queues of possible splitters. For each function
	   [k], if the pair [(k, block)] was in the queue, then [(k,
	   quitter)] is added to the queue too. If it wasn't, then the
	   smaller of the two blocks is added. *)

	let smaller =
	  if Partition.cardinal block < Partition.cardinal quitter then block else quitter
	in
	for k = 0 to f-1 do
	  Bag.add (k, if Bag.mem (k, block) then quitter else smaller)
	done

let commit item =
  commit (Partition.block item)

(* ---------------------------------------------------------------------------- *)

(* Refining one block with respect to one function. Complexity: O(size
   of block) + O(f . size of block's preimage). *)

let refine (k, block) =

  (* Define iteration over all sources of edges labeled [k] that
     (currently) point into [block]. *)

  let targets =
    Partition.elements block
  in

  let iter f =
    List.iter (fun y ->
      List.iter f y.item_fun_edges.(k)
    ) targets
  in

  (* Iterate over all such sources, and split them off their blocks. Note that
     no source can be split twice, since a point cannot be a source for two
     distinct edges. *)

  iter split;

  (* Iterate again, and commit all splits. *)

  iter commit

(* ---------------------------------------------------------------------------- *)

(* The algorithm's main loop. Complexity: O(f^2n\log n). *)

let () =
  Bag.repeat refine

(* ---------------------------------------------------------------------------- *)

(* Publish a function that maps each item to a representative member of its
   block. *)

let representative (p : point) : point =
  match Partition.choose (Partition.block (get_item p)) with
  | None ->
      assert false (* no block can be empty *)
  | Some y ->
      y.item_point

end

