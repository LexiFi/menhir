type 'a node = {

  (* The client information associated with this node. *)

  data: 'a;

  (* This node's incoming and outgoing edges: possibly empty,
     doubly-linked lists of edges. *)

  mutable outgoing: 'a edges;
  mutable incoming: 'a edges;

  (* A transient mark, always set to [false], except when checking
     against duplicate insertions in a successor list. *)
  
  mutable marked: bool;

}

and 'a edges =
    'a edge option

and 'a edge = {

  (* This edge's destination node. *)

  destination: 'a node;

  (* This edge's reverse edge, whose source and destination nodes
     are swapped. *)

  reverse: 'a edge;

  (* All edges with a common source node are organized in a
     doubly-linked, circular list. *)

  mutable prev: 'a edge;
  mutable next: 'a edge;

}

(* [create data] creates a new node, with no incident edges, with
   client information [data]. Time complexity: constant. *)

let create (data : 'a) : 'a node = {
  data = data;
  outgoing = None;
  incoming = None;
  marked = false;
}

(* [data node] returns the client information associated with
   the node [node]. Time complexity: constant. *)

let data (node : 'a node) : 'a =
  node.data

(* [fold f edges accu] folds the function [f] over the doubly-linked
   list [edges], with initial accumulator [accu]. This iterator is
   fragile, that is, the function [f] is not allowed to mutate the
   list while the list is being traversed. Time complexity: linear
   in the length of the list. *)

let fold (f : 'a edge -> 'b -> 'b) (edges : 'a edges) (accu : 'b) : 'b =
  match edges with
  | None ->
      accu
  | Some (head : 'a edge) ->
      let rec fold edge accu =
	if edge == head then
	  accu
	else
	  fold edge.next (f edge accu)
      in
      fold head.next (f head accu)

(* [predecessors node] returns a list of [node]'s predecessors.
   Time complexity: linear in the length of the output list. *)

let predecessors (node : 'a node) : 'a node list =
  fold (fun edge accu ->
    edge.destination :: accu
  ) node.incoming []

(* [insert edge edges] inserts the newly-created edge [edge] into the
   doubly-linked list [edges]. It is assumed that [edge.prev] and
   [edge.next] initially point to [edge] itself. The doubly-linked
   list is modified in place and its new head is returned. Time
   complexity: constant. *)

let insert (edge : 'a edge) (edges : 'a edges) : 'a edges =
  match edges with
  | None ->
      Some edge
  | Some (prev : 'a edge) ->
      let next : 'a edge = prev.next in
      edge.prev <- prev; (* was [edge] *)
      edge.next <- next; (* was [edge] *)
      prev.next <- edge; (* was [next] *)
      next.prev <- edge; (* was [prev] *)
      edges

(* [delete edge edges] removes the edge [edge] from the doubly-linked
   list [edges]. It is assumed that [edge] is initially a member of
   the list. The doubly-linked list is modified in place and its new
   head is returned. Time complexity: constant. *)

let delete (edge : 'a edge) (edges : 'a edges) : 'a edges =
  let next = edge.next in
  if next == edge then
    None
  else
    let prev = edge.prev in
    next.prev <- prev;
    prev.next <- next;
    match edges with
    | None ->
	assert false
    | Some (head : 'a edge) ->
	if head == edge then
	  Some next
	else
	  edges

(* [link src dst] creates a new edge from [src] to [dst], together
   with its reverse edge. No check is performed against the creation
   of duplicate edges. Time complexity: constant. *)

let link (src : 'a node) (dst : 'a node) : unit =
  let rec direct : 'a edge = {
    destination = dst;
    reverse = reverse;
    prev = direct;
    next = direct;
  }
  and reverse : 'a edge = {
    destination = src;
    reverse = direct;
    prev = reverse;
    next = reverse;
  }
  in
  src.outgoing <- insert direct src.outgoing;
  dst.incoming <- insert reverse dst.incoming

(* [set_successors src dsts] creates an edge from the node [src] to
   each of the nodes in the list [dsts]. Duplicate elements in the
   list [dsts] are ignored, so that no duplicate edges are created. It
   is assumed that [src] initially has no successors. Time complexity:
   linear in the length of the input list. *)

let set_successors (src : 'a node) (dsts : 'a node list) : unit =
  assert (src.outgoing = None);
  let rec loop = function
    | [] ->
	()
    | dst :: dsts ->
	if dst.marked then
	  loop dsts
	else begin
	  dst.marked <- true;
	  link src dst;
	  loop dsts;
	  dst.marked <- false
	end
  in
  loop dsts

(* [clear_successors node] removes all of [node]'s outgoing edges.
   Time complexity: linear in the number of edges that are removed. *)
  
let clear_successors (node : 'a node) : unit =
  fold (fun edge () ->
    let successor = edge.destination in
    successor.incoming <- delete edge.reverse successor.incoming
  ) node.outgoing ();
  node.outgoing <- None

