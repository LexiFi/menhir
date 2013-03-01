module type GRAPH = sig

  type node

  (* We assume each node has a unique index. Indices must range from
     $0$ to $n-1$, where $n$ is the number of nodes in the graph. *)

  val n: int
  val index: node -> int

  (* Iterating over a node's immediate successors. *)

  val successors: (node -> unit) -> node -> unit

  (* Iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end

module Sort (G : GRAPH) = struct

  let iter f =

    (* Allocate an array of Boolean marks. *)

    let marked = Array.make G.n false in

    (* Perform depth-first search. *)

    let rec dfs x =
      let ix = G.index x in
      if not marked.(ix) then begin
	marked.(ix) <- true;
	G.successors dfs x;
	f x
      end
    in

    G.iter dfs

  let fold f accu =
    let accu =
      ref accu
    in
    iter (fun x ->
      accu := f x !accu
    );
    !accu

end

