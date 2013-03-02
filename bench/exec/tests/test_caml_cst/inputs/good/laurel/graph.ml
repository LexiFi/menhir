(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/graph.ml,v 1.6 2000/01/18 17:53:05 fpottier Exp $ *)

(* This module defines several generic graph signatures. There is some redundancy, but I do not know how to avoid
   it given O'Caml's current module language. *)

module type Core = sig

  type node

  (* We assume each node has a unique index. Indices must range from $0$ to $n-1$, where $n$ is the number of nodes in
     the graph. *)

  val n: int
  val index: node -> int

end

module type Basic = sig

  include Core

  (* Given a node, this function allows iterating over its immediate successors. *)

  val successors: (node -> unit) -> node -> unit

end

module type Labelled = sig

  include Core

  (* Given a node, this function allows iterating over its immediate successors. Edges are labelled. *)

  type label

  val successors: (label -> node -> unit) -> node -> unit

end  

module type BasicIter = sig

  include Basic

  (* This function allows iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end

module type LabelledIter = sig

  include Labelled

  (* This function allows iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end

module type BasicPred = sig

  include Basic

  (* Given a node, this function allows iterating over its immediate predecessors. *)

  val predecessors: (node -> unit) -> node -> unit

end

module type LabelledPred = sig

  include Labelled

  (* Given a node, this function allows iterating over its immediate predecessors. Edges are labelled. *)

  val predecessors: (label -> node -> unit) -> node -> unit

end

module type BasicStart = sig

  include Basic

  (* A start node is a node from which every node is reachable. *)

  val start: node

end

module type LabelledStart = sig

  include Labelled

  (* A start node is a node from which every node is reachable. *)

  val start: node

end

module type BasicIterPred = sig

  include BasicIter

  (* Given a node, this function allows iterating over its immediate predecessors. *)

  val predecessors: (node -> unit) -> node -> unit

end

module type LabelledIterPred = sig

  include LabelledIter

  (* Given a node, this function allows iterating over its immediate predecessors. *)

  val predecessors: (label -> node -> unit) -> node -> unit

end

module type BasicIterStart = sig

  include BasicIter

  (* A start node is a node from which every node is reachable. *)

  val start: node

end

module type LabelledIterStart = sig

  include LabelledIter

  (* A start node is a node from which every node is reachable. *)

  val start: node

end

module type BasicPredStart = sig

  include BasicPred

  (* A start node is a node from which every node is reachable. *)

  val start: node

end

module type LabelledPredStart = sig

  include LabelledPred

  (* A start node is a node from which every node is reachable. *)

  val start: node

end

module type LabelledStartEnd = sig

  include LabelledStart

  (* A sink node is a node which is reachable from every node. *)

  val sink: node

end

module type Flow (* BasicPredStartEnd *) = sig

  include BasicPredStart

  (* A sink node is a node which is reachable from every node. *)

  val sink: node

end

module type LabelledFlow (* LabelledPredStartEnd *) = sig

  include LabelledPredStart

  (* A sink node is a node which is reachable from every node. *)

  val sink: node

end

module type FlowIter = sig

  include Flow

  (* This function allows iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end

module type LabelledFlowIter = sig

  include LabelledFlow

  (* This function allows iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end

module type LabelledStartEndIter = sig

  include LabelledStartEnd

  (* This function allows iterating over all nodes. *)

  val iter: (node -> unit) -> unit

end

