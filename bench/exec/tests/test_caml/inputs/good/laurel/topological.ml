(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/topological.ml,v 1.2 1999/12/21 13:48:23 fpottier Exp $ *)

(* Given an acyclic graph $G$, this functor provides functions which allow iterating over the graph in topological
   order. Functor application, as well as each graph traversal, has complexity $O(V+E)$, where $V$ is the number
   of vertices in the graph, and $E$ is the number of its edges. It is essential that the graph be acyclic; otherwise,
   not every node will be visited. *)

module Sort (G : Graph.BasicIter) = struct

  (* Compute each node's in degree. *)

  let degree =
    Array.create G.n 0

  let _ =
    G.iter (fun node ->
      G.successors (fun successor ->
	let i = G.index successor in
	degree.(i) <- degree.(i) + 1
      ) node;
    )

  (* This is the main iterator. *)

  let fold action accu =

    (* Use a fresh copy of [degree], so as to avoid affecting it. *)

    let degree = Array.copy degree in

    (* Create a queue, and fill it with all nodes of in-degree 0. *)

    let queue = Queue.create() in
    G.iter (fun node ->
      if degree.(G.index node) = 0 then
	Queue.add node queue
    );

    (* Walk the graph, in topological order. *)

    let rec walk accu =
      if Queue.length queue = 0 then
	accu
      else
	let node = Queue.take queue in
	let accu = action node accu in
	G.successors (fun successor ->
	  let i = G.index successor in
	  degree.(i) <- degree.(i) - 1;
	  if degree.(i) = 0 then
	    Queue.add successor queue
        ) node;
	walk accu in

    walk accu

  (* This is a derived version of the iterator, which doesn't maintain an accumulator. *)

  let iter action =
    fold (fun node () -> action node) ()

end

