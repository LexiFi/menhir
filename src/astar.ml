(* This module implements A* search, following Hart, Nilsson,
   and Raphael (1968).

   To each visited graph node, the algorithm associates an
   internal record, carrying various information. For this
   reason, the algorithm's space complexity is, in the worst
   case, linear in the size of the graph.

   The mapping of nodes to internal records is implemented
   via a hash table, while the converse mapping is direct
   (via a record field).

   Nodes that remain to be examined are kept in a priority
   queue, where the priority of a node is the cost of the
   shortest known path from the start node to it plus the
   estimated cost of a path from this node to a goal node.
   (Lower priority nodes are considered first).

   It is the use of the second summand that makes A* more
   efficient than Dijkstra's standard algorithm for finding
   shortest paths in an arbitrary graph. In fact, when
   [G.estimate] is the constant zero function, A* coincides
   with Dijkstra's algorithm. One should note that A* is
   faster than Dijkstra's algorithm only when a path to some
   goal node exists. Otherwise, both algorithms explore the
   entire graph, and have similar time requirements.

   The priority queue is implemented as an array of doubly
   linked lists. *)

module Make (G : sig

  (* Graph nodes. *)
  type node
  include Hashtbl.HashedType with type t := node

  (* Edge labels. *)
  type label

  (* The source node(s). *)

  val sources: (node -> unit) -> unit

  (* [successors n f] presents each of [n]'s successors, in
     an arbitrary order, to [f], together with the cost of
     the edge that was followed. *)
  val successors: node -> (label -> int -> node -> unit) -> unit

  (* An estimate of the cost of the shortest path from the
     supplied node to some goal node. For algorithms such as
     A* and IDA* to find shortest paths, this estimate must
     be a correct under-approximation of the actual cost. *)
  val estimate: node -> int

end) = struct

  type cost = int

  (* Nodes with low priorities are dealt with first. *)
  type priority = cost

  type inode = {
      (* Graph node associated with this internal record. *)
      this: G.node;
      (* Cost of the best known path from a source node to this node. (ghat) *)
      mutable cost: cost;
      (* Estimated cost of the best path from this node to a goal node. (hhat) *)
      estimate: cost;
      (* Best known path from a source node to this node. *)
      mutable path: G.label list;
      (* Previous node on doubly linked priority list *)
      mutable prev: inode;
      (* Next node on doubly linked priority list *)
      mutable next: inode;
      (* The node's priority, if the node is in the queue; -1 otherwise *)
      mutable priority: priority;
  }

  (* This auxiliary module maintains a mapping of graph nodes
     to internal records. *)

  module M : sig

    (* Adds a binding to the mapping. *)
    val add: G.node -> inode -> unit

    (* Retrieves the internal record for this node.
       Raises [Not_found] no such record exists. *)
    val get: G.node -> inode

  end = struct

    module H =
      Hashtbl.Make(struct include G type t = node end)

    let t = H.create 100003

    let add node inode =
      H.add t node inode
       
    let get node =
      H.find t node

  end

  (* This auxiliary module maintains a priority queue of
     internal records. *)

  module P : sig

    (* Adds this node to the queue. *)
    val add: inode -> priority -> unit

    (* Adds this node to the queue, or changes its
       priority, if it already was in the queue. It
       is assumed, in the second case, that the priority
       can only decrease. *)
    val add_or_decrease: inode -> priority -> unit

    (* Retrieve a node with lowest priority of the queue. *)
    val get: unit -> inode option

  end = struct

    (* Maximum allowed priority. *)
    let max = 264

    (* Array of pointers to the doubly linked lists,
       indexed by priorities. *)
    let a = Array.make max None

    (* Index of lowest nonempty list. *)
    let best = ref max

    (* Adjust node's priority and insert into doubly linked list. *)
    let add inode priority =
      assert (priority < max);
      inode.priority <- priority;
      match a.(priority) with
      | None ->
	  a.(priority) <- Some inode;
	  if priority < !best then
	    best := priority
      | Some inode' ->
	  inode.next <- inode';
	  inode.prev <- inode'.prev;
	  inode'.prev.next <- inode;
	  inode'.prev <- inode

    (* Takes a node off its doubly linked list. Does not adjust
       [best]. *)
    let remove inode =
      if inode.next == inode then
	a.(inode.priority) <- None
      else begin
        a.(inode.priority) <- Some inode.next;
	inode.next.prev <- inode.prev;
	inode.prev.next <- inode.next;
	inode.next <- inode;
	inode.prev <- inode
      end;
      inode.priority <- -1

    let get () =
      if !best = max then
	None
      else
	match a.(!best) with
	| None ->
	    assert false
	| Some inode as result ->
            remove inode;
            (* look for next nonempty bucket *)
            while (!best < max) && (a.(!best) = None) do
	      incr best
	    done;
	    result

    let add_or_decrease inode priority =
      if inode.priority >= 0 then
	remove inode;
      add inode priority

  end

  (* Initialization. *)

  let () =
    G.sources (fun node ->
      let rec inode = {
        this = node;
        cost = 0;
        estimate = G.estimate node;
        path = [];
        prev = inode;
        next = inode;
        priority = -1
      } in
      M.add node inode;
      P.add inode inode.estimate
    )

  let expanded =
    ref 0

  (* Access to the search results (after the search is over). *)

  let distance node =
    try (M.get node).cost with Not_found -> max_int

  let path node =
    (M.get node).path (* let [Not_found] escape if no path was found *)

  (* Search. *)

  let rec search f =

    (* Pick the open node that currently has lowest fhat,
       that is, lowest estimated distance to a goal node. *)

    match P.get() with
    | None ->
       (* Finished. *)
       distance, path

    | Some inode ->
        let node = inode.this in

        (* Let the user know about this newly discovered node. *)
        f (node, inode.path);

        (* Monitoring. *)
        incr expanded;

        (* Otherwise, examine its successors. *)
        G.successors node (fun label edge_cost son ->

          (* Determine the cost of the best known path from the
             start node, through this node, to this son. *)
          let new_cost = inode.cost + edge_cost in

          try
            let ison = M.get son in
            if new_cost < ison.cost then begin

              (* This son has been visited before, but this new
                 path to it is shorter. If it was already open
                 and waiting in the priority queue, increase its
                 priority; otherwise, mark it as open and insert
                 it into the queue. *)

              let new_fhat = new_cost + ison.estimate in
              P.add_or_decrease ison new_fhat;
              ison.cost <- new_cost;
              ison.path <- label :: inode.path

            end
          with Not_found ->

            (* This son was never visited before. Allocate a new
               status record for it and mark it as open. *)

            let e = G.estimate son in
            let rec ison = {
              this = son;
              cost = new_cost;
              estimate = e;
              path = label :: inode.path;
              prev = ison;
              next = ison;
              priority = -1
            } in
            M.add son ison;
            P.add ison (new_cost + e)

        );

        search f

end
