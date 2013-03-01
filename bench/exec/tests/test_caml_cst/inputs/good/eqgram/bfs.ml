(* This module provides a simple reachability test through a directed
   graph. It is implemented via breadth-first search and runs in
   linear time. *)

module type GRAPH = sig

  type node

  (* We assume each node has a unique index. Indices must range from
     $0$ to $n-1$, where $n$ is the number of nodes in the graph. *)

  val n: int
  val index: node -> int

  (* Iterating over a node's immediate successors. *)

  val successors: (node -> unit) -> node -> unit

  (* Iterating over all entry nodes -- these are the start nodes
     for the reachability computation. *)

  val iter: (node -> unit) -> unit

end

module Run (G : GRAPH) (D : sig val discover: int -> G.node -> unit end) = struct

  open G

  let index node = (* TEMPORARY *)
    let i = index node in
    assert (i >= 0 && i < n);
    i

  let distance =
    Array.create n (-1)

  (* We use two FIFO queues instead of just one, so as to know when to
     increment the distance counter. *)

  let current =
    Queue.create()

  let held =
    Queue.create()

  (* When a node is reached, we check whether it is being discovered
     or re-discovered. In the former case, we record its distance and
     enqueue it into the queue of [held] nodes. *)

  let reach k node =
    let i = index node in
    let d = distance.(i) in
    assert (d <= k);
    if d < 0 then begin
      D.discover k node;
      distance.(i) <- k;
      Queue.add node held
    end

  (* The graph's entry nodes are initially discovered at distance 0. *)

  let () =
    iter (reach 0)

  (* As long the current queue is non-empty, process the nodes in it,
     enqueuing their successors (if necessary) into the held
     queue. When the current queue becomes empty, increment the
     distance counter and transfer the contents of the held queue into
     the current queue. When both queues are empty, stop. *)

  let rec loop k =
    if Queue.is_empty current then
      if Queue.is_empty held then
	() (* done *)
      else begin
	Queue.transfer held current;
	loop (k+1) (* continue at next level *)
      end
    else
      let node = Queue.take current in
      successors (reach k) node; (* reach successors *)
      loop k (* continue at this level *)

  let () =
    loop 0

  (* Publish this information. *)

  let distance node =
    distance.(index node)

end

