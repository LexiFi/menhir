(* This module provides a simple reachability test through a directed
   graph. It is implemented via depth-first search and runs in
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

module Run (G : GRAPH) = struct

  open G

  let reachable =
    Array.create n false

  let rec visit node =
    let i = index node in
    if not reachable.(i) then begin
      reachable.(i) <- true;
      successors visit node
    end

  let () =
    iter visit

  let reachable node =
    reachable.(index node)

end

