(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/complete.ml,v 1.8 2000/01/18 17:53:05 fpottier Exp $ *)

(* Given a description of a graph [G] with a start node, this functor completes it by providing a function which
   allows iterating over all nodes. The function performs a depth-first traversal of the graph. *)

module Iter (G : Graph.BasicStart) = struct

  (* Repeat of each [G]'s fields. *)

  type node = G.node

  let n = G.n
  let index = G.index
  let successors = G.successors
  let start = G.start

  (* Define the iterator. *)

  let iter action =
    let visited = Array.create n false in
    let rec visit node =
      let i = index node in
      if not visited.(i) then begin
	visited.(i) <- true;
	action node;
	successors visit node
      end in
    visit start

end

(* Given a description of a graph [G] which allows access to a node's successors, this functor completes it by
   providing a function which allows constant-time access to a node's predecessors. The computation is done at
   functor application; its complexity is $O(V+E)$ (assuming [G.successors] gives constant-time access to a node's
   successors). *)

module Pred (G : Graph.BasicIter) = struct

  (* Repeat of each [G]'s fields. *)

  type node = G.node

  let n = G.n
  let index = G.index
  let successors = G.successors
  let iter = G.iter

  (* Build each node's predecessor list. *)

  let predecessors =
    Array.create G.n []

  let _ =
    G.iter (fun node ->
      G.successors (fun successor ->
	let i = G.index successor in
	predecessors.(i) <- node :: predecessors.(i)
      ) node
    )

  (* Give access to the list. *)

  let predecessors action node =
    List.iter action predecessors.(G.index node)

end

(* This functor is similar to the previous one, but works with labelled graphs. *)

module LabelledPred (G : Graph.LabelledIter) = struct

  (* Repeat of each [G]'s fields. *)

  type node = G.node
  type label = G.label

  let n = G.n
  let index = G.index
  let successors = G.successors
  let iter = G.iter

  (* Build each node's predecessor list. *)

  let predecessors =
    Array.create G.n []

  let _ =
    G.iter (fun node ->
      G.successors (fun label successor ->
	let i = G.index successor in
	predecessors.(i) <- (label, node) :: predecessors.(i)
      ) node
    )

  (* Give access to the list. *)

  let predecessors action node =
    List.iter (fun (label, node) -> action label node) predecessors.(G.index node)

end

(* Given a flow graph [G], this functor makes sure that its start (resp. end) node leads to (resp. is reachable from)
   every node. Otherwise, an appropriate exception is raised. Intuitively speaking, [WrongStartNode] signals the
   existence of dead code, while [WrongSinkNode] signals the existence of an endless loop. Both exceptions carry
   boolean arrays, where the entry associated with node [v] holds [true] if and only if [v] is live. *)

module Half (E : sig val error: bool array -> unit end) (G : Graph.Flow) = struct

  let live =
    Array.create G.n false

  let () =
    let module G = Iter(G) in
    G.iter (fun x ->
      live.(G.index x) <- true
    );
    Array.iter (fun marked ->
      if not marked then
	E.error live
    ) live

end

exception WrongStartNode of bool array
exception WrongSinkNode of bool array

module Live (G : Graph.Flow) = struct

  module Half1 = Half (struct let error live = raise (WrongStartNode live) end) (G)
  module Half2 = Half (struct let error live = raise (WrongSinkNode  live) end) (G)

end

(* Given a labelled flow graph, this forgetful functor removes its labels. *)

module Strip (G : Graph.LabelledFlowIter) = struct
  type node = G.node
  let n = G.n
  let index = G.index
  let successors action node =
    G.successors (fun _ node ->
      action node
    ) node
  let predecessors action node =
    G.predecessors (fun _ node ->
      action node
    ) node
  let start = G.start
  let sink = G.sink
  let iter = G.iter
end

