(* $Header: /home/yquem/cristal/fpottier/cvs/queues/fibonacciHeap.ml,v 1.1 2003/04/22 07:14:36 fpottier Exp $ *)

(* This module implements Fibonacci heaps, as described by Fredman and Tarjan
   in ``Fibonacci Heaps and Their Uses in Improved Network Optimization
   Algorithms'', Journal of the ACM 34(3), July 1987. *)

(* The module is parameterized by the type of the elements. Elements must be
   equipped with an ordering that reflects their priority. *)

module Make (X : sig

  type t
  val compare: t -> t -> int

end) = struct

  (* The type of elements. *)

  type element = X.t

  (* Every heap node contains an element, which reflects its priority as well
     as other client-specific information; a mark, which tells whether it has
     lost one of its children since it was assigned its current parent (the
     mark of a root node has no meaning); a pointer to its parent node, which
     is [null] if the node is a root node; a pointer to its left and right
     siblings, forming a circular doubly-linked list; a rank, which counts its
     immediate children; and a pointer to one of its children, which is [null]
     if there are none. *)

  type node = {
      mutable key: element;
      mutable mark: bool;
      mutable parent: node;
      mutable left: node;
      mutable right: node;
      mutable rank: rank;
      mutable child: node;
      mutable valid: bool (* for sanity checking only; could be removed. *)
    } 

  (* Ranks are represented not by integers, but by memory cells, organized in
     a doubly linked-list. Each rank cell contains a (transient) pointer to a
     node, which is used during the linking step only. *)

  and rank = {
      order: int;
      mutable succ: rank;
      mutable pred: rank;
      mutable node: node (* transient *)
    } 

  (* The [null] node is a sentinel. Only its address matters; its fields are
     never accessed. *)

  let null : node =
    Obj.magic None

  (* Module [Rank] abstracts away a few basic operations on ranks. *)

  module Rank : sig

    (* [zero] is the rank associated with the integer [zero]. The function
       [succ] is defined for every rank, while [pred] is defined for every
       non-zero rank. Thus, ranks appear to be in bijection with natural
       integers. *)

    val zero: rank
    val succ: rank -> rank
    val pred: rank -> rank

    (* [fold f accu rank] iterates over every rank from [rank] down to [zero],
       passing its [node] field to [f] if it isn't [null], then clearing
       it. The accumulator [accu] is threaded through calls to [f]. *)

    val fold: ('a -> node -> 'a) -> 'a -> rank -> 'a

  end = struct

    let rec zero = {
      order = 0;
      succ = zero;
      pred = zero;
      node = null
    } 

    let succ rank =
      let succ = rank.succ in
      if succ == zero then
	let succ = {
	  order = rank.order + 1;
	  succ = zero;
	  pred = rank;
	  node = null
	} in
	rank.succ <- succ;
	succ
      else
	succ

    let pred rank =
      assert (rank != zero);
      rank.pred

    let rec fold f accu here =
      let node = here.node in
      let accu =
	if node != null then begin
	  here.node <- null;
	  f accu node
	end
	else
	  accu in
      if here == zero then
	accu
      else
	fold f accu here.pred

  end

  (* Module [CircularList] abstracts away a few basic operations on circular
     doubly-linked lists. *)

  module CircularList : sig

    (* A circular doubly-linked list is represented by a possibly null
       node. *)

    type list =
	node

   (* [insert node list] inserts the node [node] into the list [list]. The
       fields [node.left] and [node.right] are overwritten. *)

    val insert: node -> list -> unit

    (* [remove node] removes the node [node] from the list to which it
       belongs. The modified list is returned. The fields [node.left] and
       [node.right] are left dangling. *)

    val remove: node -> list

    (* [merge node1 node2] combines the lists to which [node1] and [node2]
       belong -- which must be distinct -- into a single list. [node1] and
       [node2] must be non-null. *)

    val merge: node -> node -> unit

    (* [iter f list] successively applies [f] to every node in the
       circular doubly-linked list [list]. [f] is allowed to overwrite
       the node's [left] and [right] fields. *)

    val iter: (node -> unit) -> list -> unit

  end = struct

    type list =
	node

    let insert node list =
      assert (node != null);
      if list == null then begin
	node.left <- node;
	node.right <- node
      end
      else
	let right = list.right in
	list.right <- node;
	node.left <- list;
	node.right <- right;
	right.left <- node

    let remove node =
      assert (node != null);
      let left = node.left in
      if left == node then
	null
      else
	let right = node.right in
	left.right <- right;
	right.left <- left;
	right

    let merge node1 node2 =
      assert ((node1 != null) && (node2 != null));
      let left1 = node1.left
      and right2 = node2.right in
      left1.right <- right2;
      right2.left <- left1;
      node2.right <- node1;
      node1.left <- node2

    let iter f list =
      if list != null then
	let rec loop here =
	  let there = here.right in
	  f here;
	  if list != there then
	    loop there
	in
	loop list

  end

  (* A heap is accessed through a pointer to its minimum element, which may be
     [null] if the heap is empty. *)

  type t =
      node

  (* [create()] returns a fresh, empty heap. *)

  let create () =
    null

  (* [compare] compares two nodes based on their keys. *)

  let compare node1 node2 =
    X.compare node1.key node2.key

  (* [meld h1 h2] melds the heaps [h1] and [h2], producing a new heap, which
     is returned. The heaps [h1] and [h2] are destroyed in the process. The
     operation's amortized time complexity is $O(1)$. *)

  let meld h1 h2 =
    if h1 == null then h2
    else if h2 == null then h1
    else begin
      CircularList.merge h1 h2;
      if compare h1 h2 < 0 then h1 else h2
    end

  (* [insert x h] inserts the element [x] into the heap [h]. The newly
     allocated heap node and the modified heap are returned. The heap [h] is
     destroyed. The operation's amortized time complexity is $O(1)$. *)

  let insert x h =
    let rec node = {
      key = x;
      mark = false;
      parent = null;
      left = node;
      right = node;
      rank = Rank.zero;
      child = null;
      valid = true
    } in
    node, meld node h

  (* [minimum h] returns a minimum element in heap [h]. The heap [h] is
     unaffected. The exception [Not_found] is raised if [h] is empty. The
     operation's worst-case time complexity is $O(1)$. *)

  let minimum h =
    if h == null then raise Not_found else h.key

  (* [assign node1 node2] makes [node2] a child of [node1], and returns
     [node1]. The element associated with [node1] must be less than or equal
     to that associated with [node2]. [node2]'s [left] and [right] fields are
     overwritten. [node2]'s mark is cleared, because it is assigned a new
     parent. *)

  let assign node1 node2 =
    node1.rank <- Rank.succ node1.rank;
    CircularList.insert node2 node1.child;
    node1.child <- node2;
    node2.parent <- node1;
    node2.mark <- false;
    node1

  (* [link node1 node2] links the trees [node1] and [node2] into a single
     tree, while preserving the heap condition, and returns the new tree. *)

  let link node1 node2 =
    if compare node1 node2 < 0 then
      assign node1 node2
    else
      assign node2 node1

  (* [extract h] extracts a minimum element of the heap [h]. The element and
     the modified heap are returned. The heap [h] is destroyed. The node
     associated with the extracted element becomes invalid. The exception
     [Not_found] is raised if [h] is empty. The operation's amortized time
     complexity is $O(\log n)$, where [n] is the number of elements currently
     in the heap. *)

  let extract h =
    if h == null then
      raise Not_found
    else

      (* We first determine the minimum node and remove it from the heap. Each
	 of its children thus loses its previous parent. They conceptually
	 become root nodes, although we do not immediately insert them into
	 the heap. *)

      let minimum = h in
      minimum.valid <- false;
      let h = CircularList.remove minimum in
      let children = minimum.child in
      CircularList.iter (fun child ->
	child.parent <- null;
      ) children;

      (* Then, we order all of the remaining root nodes, including the minimum
	 node's children, by rank. The [node] field of [rank] objects is used
	 for this purpose. When two trees are found to have the same rank,
	 they are linked together. At the same time, we determine the maximum
	 rank encountered during this process. *)

      let max = ref Rank.zero in

      let rec place node1 =
	let rank1 = node1.rank in
	if rank1.order > !max.order then
	  max := rank1;
	let node2 = rank1.node in
	if node2 == null then
	  rank1.node <- node1
	else begin
	  rank1.node <- null;
	  place (link node1 node2)
	end
	    
      in
      CircularList.iter place h;
      CircularList.iter place children;

      (* We now iterate over the rank scale and create a heap out of its
	 contents. We iterate from the maximum rank determined above down to
	 zero, rather than over all ranks ever created, so as to obtain the
	 appropriate complexity. To create a heap, we insert all nodes back
	 into a circular doubly-linked list. At the same time, we determine
	 the new heap's minimum element. When done, we return the element
	 associated with the previous minimum node and the new heap. *)

      minimum.key, Rank.fold (fun h node ->
	CircularList.insert node h;
	if (h == null) || (compare node h < 0) then node else h
      ) null !max

  (* [cut h node parent] cuts the edge between [node] and its parent [parent],
     which must both be nodes of the heap [h]. The node [node] conceptually
     becomes a root node, but its [left] and [right] fields are left
     dangling. The heap's minimum element is unaffected. If [parent] is a root
     node, the process stops there. Otherwise, if [parent] was unmarked, it is
     marked; if it was marked, we recursively cut the edge between [parent]
     and its own parent, and [parent] is made a root node. This is known as a
     cascading cut. *)

  let rec cut h node parent =
    parent.rank <- Rank.pred parent.rank;
    node.parent <- null;
    parent.child <- CircularList.remove node;
    assert (compare node h >= 0);
    let grandparent = parent.parent in
    if grandparent != null then
      if parent.mark then begin
	cut h parent grandparent;
	CircularList.insert parent h
      end
      else
	parent.mark <- true

  (* [decrease h node x] changes the element associated with the node [node],
     which must be a valid node within the heap [h], to [x]. The element [x]
     must be less than, or equal to, the element previously associated with
     that node. The modified heap is returned. The original heap is
     destroyed. The operation's amortized time complexity is $O(1)$. *)

  (* We detach the node from its parent, if it has one, then change its key
     and (if necessary) update the heap's minimum element. The constant
     amortized time bound comes from the fact that the number of cascading
     cuts that take place during a sequence of heap operations is bounded by
     the number of [decrease] and [delete] operations. *)

  let decrease h node key =
    assert node.valid;
    assert (X.compare key node.key <= 0);
    let parent = node.parent in
    if parent != null then begin
      cut h node parent;
      CircularList.insert node h
    end;
    node.key <- key;
    if X.compare key h.key < 0 then node else h

  (* [delete h node] removes the element associated with the node [node],
     which must be a valid node within the heap [h], from the heap [h]. The
     node [node] is made invalid. The modified heap is returned. The
     original heap is destroyed. The operation's amortized time complexity
     is $O(\log n)$, where [n] is the number of elements currently in the
     heap. *)

  (* If the node to be removed is the minimum node, we simply use [extract].
     Otherwise, we detach the node from its parent, if it has one, and turn
     its children into root nodes. *)

  let delete h node =
    if h == node then
      let _, h = extract h in
      h
    else begin
      assert node.valid;
      node.valid <- false;
      let parent = node.parent in
      if parent == null then
	let _ = CircularList.remove node in
	()
      else
	cut h node parent;
      let children = node.child in
      if children != null then begin
	CircularList.iter (fun child ->
	  child.parent <- null
        ) children;
	CircularList.merge children h
      end;
      h
    end

(* TEMPORARY *)
(* Crude debugging support. Assumes element type is int.

  let rec print h =
    if h == null then
      ""
    else
      let s = ref "" in
      let rec check node =
	let k = string_of_int (Obj.magic node.key) in
	if node.child != null then begin
	  s := !s ^ k ^ "(";
	  CircularList.iter check node.child;
	  s := !s ^ ")"
	end
	else
	  s := !s ^ k
      in
      CircularList.iter check h;
      !s

*)

end

