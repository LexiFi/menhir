(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/Attic/paigeTarjan.ml,v 1.1.2.12 1999/02/11 16:05:07 francois Exp $ *)
(*

An implementation of a hybrid algorithm, which brings together Hopcroft's algorithm for minimizing deterministic
finite-state automata, and Paige and Tarjan's algorithm, which is a generalization of the former to the
non-deterministic case.

In our complexity analysis, n stands for the number of points, m stands for the number of edges, and r stands for the
number of functions and relations.

More specifically, rf stands for the number of functions, and rr stands for the number of relations. The distinction
is not relevant in the complexity analysis, since rf + rr = r, but it is made in the algorithm, because the functional
case can be handled more efficiently.

*)

open Errors

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Repeat the signature of our functor argument.

*)

module Make (P : sig

  type point

  val store_item: point -> 'a -> unit
  val get_item: point -> 'a

  val set_representative: point -> point -> unit

end) = struct

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Internal type declarations for our data structures.

*)

  type point = P.point

  (* Items are the elements of the set to be partitioned. *)

  type item = {

      (* Each item represents a user point, to which it carries a pointer. *)

      item_point: point;

      (* Each item belongs to a block. The items of a block are linked together in a doubly linked list. *)

      mutable item_prev: item option;
      mutable item_next: item option;
      mutable item_block: block;

      (* For each function or relation, each item is the end point of several edges. In the case of functions, it
	 suffices to record the edge's start point. In the case of relations, the edge also points to a reference
	 cell, which tells how many edges with the same start point have end points in the same block. *)

      mutable item_incoming_functional_edges: item list array(*rf*);
      mutable item_incoming_relational_edges: edge list array(*rr*);

      (* During the initialization process, each item carries the number of its outgoing edges for each relation.
         This field is unused once initialization is over. *)

      mutable item_initial_count: count array(*rr*);

      (* This field is used temporarily during steps 3 to 7 of the main loop. *)

      mutable item_transient_count: count;

      (* This field allows an efficient detection of duplicates while building sets of items. *)

      mutable item_transient_mark: bool

    }

  (* This structure represents an edge of some relation. The edges of functions are represented in a simpler
     way (see above). *)

  and edge = {

      (* The edge's origin. *)

      edge_source: item;

      (* A pointer to a cell which contains a count of all edges with the same source and the same destination
	 block. All of these edges point the same cell, of course, which allows easy updates when splitting a
	 block. *)

      mutable edge_count: count

    } 

  (* Blocks are the constituents of the partition being refined. They are disjoint sets of items. *)

  and block = {

      (* Each block carries the number of its items. *)

      mutable block_size: int;

      (* A pointer to the head of the doubly linked list of items. *)

      mutable block_item: item option;

      (* For each relation, we maintain a super-partition, i.e. a partition of the set of blocks, with respect to
	 which the current partition is known to be stable. Each block belongs to a superblock. The blocks of a
	 superblock are linked together in a doubly linked list. *)

      mutable block_prev: block option array(*rr*);
      mutable block_next: block option array(*rr*);
      mutable block_superblock: superblock array(*rr*);

      (* To allow splitting a block in time proportional to the number of elements to be removed, we need to
	 associate a 'quitter' block with each block. This association lasts while 'split' is being called,
	 and is broken when 'commit' is called. *)

      mutable block_transient_quitter: block option;

      (* This field allows determining, in constant time, whether a pair (k, B) appears in the splitter queue. *)

      mutable block_in_splitter_queue: bool array(*rf*);

    } 

  (* Superblocks are the constituents of a super-partition. They are disjoint sets of blocks. *)

  and superblock = {

      (* Each superblock carries the index of the relation to which its super-partition is associated. This allows
	 using a single waiting queue for all superblocks. *)

      superblock_relation: int;

      (* Each superblock carries the number of its blocks. This allows determining which superblocks are compound,
	 i.e. contain more than one element. When no superblocks are compound, then the super-partition coincides
	 with the partition, which means that the latter is stable with respect to itself. If necessary, this field
	 could be removed, since it is possible to check in constant time whether a superblock is compound. *)

      mutable superblock_size: int;

      (* A pointer to the head of the doubly linked list of blocks. *)      

      mutable superblock_block: block option

    } 

  (* These cells count the number of edges with the same source and the same destination block. They are mutable
     and shared among edges. *)

  and count = int ref

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Global variables.

*)

  (* These are used during the initialization process. The former holds a pointer to the current block, if such
     a block exists. (It exists as soon as add_point has been called at least once.) The latter holds pointers
     to the superblocks. At first, there exists exactly one superblock per relation; this superblock contains all
     blocks. *)

  let current_block = ref None
  let current_superblocks = ref [||]

  (* A list of all existing items. It is used to iterate over all items during the final pass, where representatives
     are chosen. *)

  let partition = ref []

  (* The number of functions, and the number of relations. *)

  let rf = ref 0
  let rr = ref 0

  (* A waiting queue of all compound superblocks. *)

  let compound_queue = Queue.create()

  (* A waiting queue of pairs (k, B), where B is a possible splitter block for function k. *)

  let splitter_queue = Queue.create()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Setting the function & relation count. This must be done ahead of time, so we can allocate arrays of appropriate size
during the initialization process. For each relation, we create a superblock, which shall contain all blocks.
Complexity: O(r)

*)

  let set_counts inRf inRr =

    (* Initialize the globals. *)

    current_block := None;
    current_superblocks := [||];

    partition := [];

    rf := inRf;
    rr := inRr;

    Queue.clear compound_queue;
    Queue.clear splitter_queue;

    (* Create the superblocks. *)

    current_superblocks := Array.init inRr (fun k ->
      {
	superblock_relation = k;
	superblock_size = 0;
	superblock_block = None
      }
    )

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Defining the initial partition. Complexity: O(r) per call, hence O(rn) for defining all points and initial blocks.

*)

  let add_point create_new_block p =

    let last_block = !current_block in

    let block = match last_block, create_new_block with
      None, _
    | Some _, true ->

	(* Create a new block. *)

	let block = {
	  block_size = 0;
	  block_item = None;
	  block_prev = Array.make !rr None;
	  block_next = Array.make !rr last_block;
	  block_superblock = Array.copy !current_superblocks;
	  block_transient_quitter = None;
	  block_in_splitter_queue = Array.create !rf false
	} in
	let someblock = Some block in

	(* If a previous block already existed, link it back to the new one, so the superblocks' doubly linked lists
	   are up-to-date. *)

	Standard.do_option last_block (fun last_block ->
	  last_block.block_prev <- Array.make !rr someblock
	);

	(* Make the new block the head of these doubly linked lists. Update the superblocks' element counts, and add
	   them to the queue if they become compound. *)

	Array.iter (fun superblock ->
	  superblock.superblock_block <- someblock;
	  superblock.superblock_size <- superblock.superblock_size + 1;
	  if superblock.superblock_size = 2 then
	    Queue.add superblock compound_queue
	) !current_superblocks;

        (* Make this the current block. *)
	
	current_block := someblock;
	block

    | Some block, false ->
	block in

    (* Create an item. *)

    let head = block.block_item in

    let item = {
      item_point = p;
      item_prev = None;
      item_next = head;
      item_block = block;
      item_incoming_functional_edges = Array.make !rf [];
      item_incoming_relational_edges = Array.make !rr [];
      item_initial_count = Array.init !rr (fun _ -> ref 0);
      item_transient_count = ref 0;
      item_transient_mark = false;
    } in
    let someitem = Some item in

    (* Add it at the head of the current block. *)

    Standard.do_option head (fun head -> head.item_prev <- someitem);
    block.block_item <- someitem;
    block.block_size <- block.block_size + 1;

    (* Add a link from this point to its newly created item. *)

    P.store_item p item;

    (* Add this item to the list of all items. *)

    partition := item :: !partition

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Defining functions and relations. Complexity: O(1) per call, hence O(m) to define all edges. Note that functions
(resp. relations) are indexed from 0 (inclusive) up to rf (resp. rr) (exclusive).

*)

  let add_functional_link k p1 p2 =

    (* Obtain the items corresponding to these points. *)

    let item1 = (P.get_item p1 : item)
    and item2 = (P.get_item p2 : item) in

    (* Add item1 to item2's list of predecessors. *)

    item2.item_incoming_functional_edges.(k) <- item1 :: item2.item_incoming_functional_edges.(k);

    (* item2's block is now a possible splitter for function a. Add an appropriate entry to the splitter queue, if not
       already done. *)

    let block = item2.item_block in
    if not block.block_in_splitter_queue.(k) then begin
      Queue.add (k, block) splitter_queue;
      block.block_in_splitter_queue.(k) <- true
    end

  let add_relational_link k p1 p2 =

    (* Obtain the items corresponding to these points. *)

    let item1 = (P.get_item p1 : item)
    and item2 = (P.get_item p2 : item) in

    (* Create an edge. During the initialization process, each item's transient_count field points to a reference cell
       which counts its outgoing edges. *)

    let count = item1.item_initial_count.(k) in
    incr count;

    let edge = {
      edge_source = item1;
      edge_count = count
    } in

    (* Add the edge to item2's list of incoming edges. *)

    item2.item_incoming_relational_edges.(k) <- edge :: item2.item_incoming_relational_edges.(k)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Looping over a block. Complexity: O(1) per element.

*)

  let iter_block action block =

    let rec iter_forward = function
	None ->
	  ()
      | Some item ->
	  action item;
	  iter_forward item.item_next in

    iter_forward block.block_item

  let block_elements block =

    let rec build accu = function
	None ->
	  accu
      | Some item ->
	  build (item :: accu) item.item_next in

    build [] block.block_item

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Taking an element off a block. This splits the block (if not already split). Complexity: O(1)

*)

  let split item =

    let block = item.item_block in

    (* If a quitter block doesn't already exist, create one. For now, this block is not added to its superblocks yet
       (i.e. the superblocks' doubly linked lists and counters are not updated). This shall be done when comitting the
       split. *)

    let quitter = match block.block_transient_quitter with
      None ->
	let quitter = {
	  block_size = 0;
	  block_item = None;
	  block_prev = Array.make !rr None;
	  block_next = Array.make !rr None;
	  block_superblock = Array.copy block.block_superblock;
	  block_transient_quitter = None;
	  block_in_splitter_queue = Array.make !rf false
	} in
	block.block_transient_quitter <- Some quitter;
	quitter
    | Some quitter ->
	quitter in

    (* Move the item off the block. *)

    let someprev = item.item_prev
    and somenext = item.item_next in
    begin
      match someprev, somenext with
	None, None ->
	  block.block_item <- None;
      | None, Some next ->

	  (* Here, the item was the head of its block's main list. Make the next element the new list head. In the
	     following two cases, we shall know that the item cannot possibly be the list head, so we shall not need
	     to update block.block_item. *)

	  next.item_prev <- None;
	  block.block_item <- somenext

      | Some prev, None ->
	  prev.item_next <- None
      | Some prev, Some next ->
	  next.item_prev <- someprev;
	  prev.item_next <- somenext
    end;
    block.block_size <- block.block_size - 1;
    
    (* Move the item into the quitter. *)

    let someitem = Some item in
    let head = quitter.block_item in
    quitter.block_item <- someitem;
    item.item_prev <- None;
    item.item_next <- head;
    Standard.do_option head (fun head -> head.item_prev <- someitem);
    quitter.block_size <- quitter.block_size + 1

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Comitting a split. If both the block and its quitter are non-empty, a new block is created.
Complexity: O(r) + O(|quitter|)

*)

  let commit block =

    match block.block_transient_quitter with
      None ->
	()
    | (Some quitter) as somequitter ->

	(* Break the association between the two blocks. *)

	block.block_transient_quitter <- None;

	(* Check whether any elements remain in the main block. *)

	if block.block_size = 0 then begin

	  (* The main block is empty, so no splitting has to be done. Move all elements back to the main block. *)

	  block.block_item <- quitter.block_item;
	  block.block_size <- quitter.block_size

	end
	else begin

	  let someblock = Some block in

	  (* The quitter must be turned into a real block. *)

          (* Update the queues of possible splitters. For each function k, if the pair (k, block) was in the queue,
	     then (k, quitter) is added to the queue too. If it wasn't, then the smaller block is added. *)

	  let smaller = if block.block_size < quitter.block_size then block else quitter in

	  Array.iteri (fun k queued ->
	    let splitter = if queued then quitter else smaller in
	    Queue.add (k, splitter) splitter_queue;
	    splitter.block_in_splitter_queue.(k) <- true
	  ) block.block_in_splitter_queue;

	  (* Update the superblocks' doubly linked lists and sizes. *)

	  Array.iteri (fun k superblock ->
	  
	    quitter.block_prev.(k) <- someblock;
	    quitter.block_next.(k) <- block.block_next.(k);
	    Standard.do_option block.block_next.(k) (fun next -> next.block_prev.(k) <- somequitter);
	    block.block_next.(k) <- somequitter;

	    superblock.superblock_size <- superblock.superblock_size + 1;
	    if superblock.superblock_size = 2 then
	      Queue.add superblock compound_queue

          ) block.block_superblock;

	  (* Change the quitter's items' ownership. *)

	  iter_block (fun item ->
	    item.item_block <- quitter
	  ) quitter

	end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The body of the main loop in Paige and Tarjan's algorithm.

Here is a quick complexity study. The body of the main loop is made up of 7 steps, whose respective complexities are
as follows:

	1 O(1)
	2 O(1)
	3 O(B) + O(preB)
	4 O(r.preB)
	5 O(B) + O(preB)
	6 O(r.preB)
	7 O(B) + O(preB)

The added complexity of these steps is O(B) + O(r.preB). So, refining with respect to one relation and one block B can
be considered to have a cost of O(1) + O(r.pre(x)) per element x of B. That said, any element x appears in at most
rlog n blocks B used as refining sets, since each successive such set is at most half the size of the previous one,
for a given relation. By summing over all blocks B used for refinement and over all elements in such block, we obtain
a total complexity bound of O((n + rm)rlog n) for the main loop. If N=n+m is adopted as a measure of the input's size,
then the complexity is O(r^2.Nlog N).

*)

  let paigeTarjan superblockS =

    (* Step 1. Select a refining block S. It is a compound block of one of the super-partitions, X_k. We examine the
       first two blocks in it, and select the smaller, which we call B. (Thus, B's size must less than half the size
       of S.) Complexity: O(1). *)

    let k = superblockS.superblock_relation in

    let blockB =
      match superblockS.superblock_block with
	Some block1 -> (
	  match block1.block_next.(k) with
	    Some block2 ->
	      if block1.block_size <= block2.block_size then block1 else block2
	  | None ->
	      raise (CantHappen "One-element superblock found in the compound queue.")
	)
      | None ->
	  raise (CantHappen "Empty superblock found in the compound queue.") in

    (* Step 2. Remove B from S and create a new (simple) block S' of X_k containing B as its only block. If S
       is still compound, we put S back into the compound queue. Complexity: O(1). *)

    let someprev = blockB.block_prev.(k)
    and somenext = blockB.block_next.(k) in
    begin
      match someprev, somenext with
	None, None ->
	  raise (CantHappen "S has two elements, we just checked it.")
      | None, Some next ->
	  next.block_prev.(k) <- None;
	  superblockS.superblock_block <- somenext
      | Some prev, None ->
	  prev.block_next.(k) <- None
      | Some prev, Some next ->
	  next.block_prev.(k) <- someprev;
	  prev.block_next.(k) <- somenext
    end;
    superblockS.superblock_size <- superblockS.superblock_size - 1;
    if superblockS.superblock_size > 1 then
      Queue.add superblockS compound_queue;

    let superblockS' = {
      superblock_relation = k;
      superblock_size = 1;
      superblock_block = Some blockB
    } in
    blockB.block_superblock.(k) <- superblockS';
    blockB.block_prev.(k) <- None;
    blockB.block_next.(k) <- None;

    (* Step 3. Copy the elements of B into a temporary set B'. (This facilitates splitting B with respect to itself
       during the refinement.) Compute the inverse image of B, by scanning the edges xEy such that that y is in B.
       Duplicates are suppressed by marking elements as they are encountered and linking them together for later
       unmarking. During the same scan, we compute count(x, B), store it in a new count record, and make x point
       to it. Complexity: O(B) + O(preB). *)

    (* Step 4. (Refine Q with respect to B.) For each block D of Q containing some element of preB, split D into
       D1 = D \cap preB and D2 = D - D1. We do this by scanning the elements of preB. To process an element x in
       preB, we determine the block D of Q containing it, and create a quitter block for D if one does not already
       exist. We move x from D to its quitter. Complexity: O(preB). 

       After doing all the splitting, we commit everything, i.e. we turn quitter blocks into real blocks if
       necessary. Complexity: O(r.preB). *)

    (* The two steps are intermixed, so as to save a constant time factor. Additionally, we avoid building preB as
       an explicit list by allocating it on the execution stack, as done by the non-tail-recursive function below. *)

    let copyB' = block_elements blockB in

    let rec loop items edges = match (items, edges) with
      items, edge :: edges ->
	  
	(* First traversal of this edge, while going up. *)

	let x = edge.edge_source in
	if x.item_transient_mark then begin
	  incr x.item_transient_count;
	  loop items edges
	end
	else begin
	  x.item_transient_mark <- true;
	  x.item_transient_count <- ref 1;
	  split x; (* step 4 *)

	  loop items edges;

          (* Second traversal of this edge, while coming back. The above code has already been executed for
	     all elements, so we can reset the marks and commit our changes. Note that commit might be called
	     several times on the same block, but we shouldn't lose much time. *)

	  x.item_transient_mark <- false;
	  commit x.item_block (* step 4 *)
	end

    | y :: items, [] ->
	loop items y.item_incoming_relational_edges.(k)
    | [], [] ->
	() in

    loop copyB' [];

    (* Step 5. (Compute preB - pre(S-B).) Scan the edges xEy such that y is in B'. (B may have been split, so it
       is no longer useable.) To process an edge xEy, determine count(x, B) (to which x points, thanks to step 3)
       and count(x, S) (to which xEy points, by convention). If they are equal, add x to preB - pre(S-B) if it is
       has not been added already. Complexity: O(B) + O(preB). *)

    (* Step 6. (Refine Q with respect to S-B.) We proceed exactly as in step 4, but scan preBpreSB instead
       of preB. Complexity: O(r.preB). *)

    (* Step 7. (Update counts.) Scan the edges xEy such that y is in B'. To process an edge xEy, decrement
       count(x, S) (to which xEy points). If this count becomes zero, make xEy point to count(x, B) (to which
       x points). Complexity: O(B) + O(preB). *)

    (* Once again, these three steps are intermixed, to save time, and the stack is used to avoid allocating
       preB - pre(S-B) in the heap. *)

    let rec loop items edges = match (items, edges) with
      items, edge :: edges ->
	  
	let x = edge.edge_source in
	let xcount = x.item_transient_count
	and ecount = edge.edge_count in

	if (not x.item_transient_mark) & (!xcount = !ecount) then begin
	  x.item_transient_mark <- true;
	  split x; (* step 6 *)

	  loop items edges;

	  x.item_transient_mark <- false;
	  commit x.item_block (* step 6 *)  
	end
	else
	  loop items edges;

	(* step 7 *)

	decr ecount;
	if !ecount = 0 then
	  edge.edge_count <- xcount

    | y :: items, [] ->
	loop items y.item_incoming_relational_edges.(k)
    | [], [] ->
	() in

    loop copyB' []

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The body of the main loop in Hopcroft's algorithm.

*)

  let hopcroft (k, blockB) =

    (* Step 3. Loop over all edges labeled k which point into this block, and split their start points off their
       blocks. Note that no marks are necessary, since a point cannot be a start point for two distinct edges. *)

    (* Step 4. Commit the splitting. *)

    let rec loop items edges = match (items, edges) with
      items, x :: edges ->
	split x;
	loop items edges;
	commit x.item_block
    | y :: items, [] ->
	loop items y.item_incoming_functional_edges.(k)
    | [], [] ->
	() in

    loop (block_elements blockB) []
    
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The main loop of our hybrid algorithm.

*)

  let rec loop () =

    try
      hopcroft (Queue.take splitter_queue);
      loop()
    with Queue.Empty ->
      try
	paigeTarjan (Queue.take compound_queue);
	loop()
      with Queue.Empty ->
	()


  let run () =

    (* Run the main loop until both queues are empty. *)

    loop();

    (* At this point, we may describe the resulting partition to the user. *)

    List.iter (fun item ->
      let block = item.item_block in
      match block.block_item with
	None ->
	  raise (CantHappen "Empty block during the final step in PaigeTarjan.")
      | Some head ->
	  P.set_representative item.item_point head.item_point
    ) !partition;

    (* Re-initializing our globals allows the GC to collect all structures left by this run. *)

    set_counts 0 0

end

