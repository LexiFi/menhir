(* This signature defines an implicit representation for graphs where
   edges have integer costs, there is a distinguished start node, and
   there is a set of distinguished goal nodes. It is also assumed that
   some geometric knowledge of the graph allows safely estimating the
   cost of shortest paths to goal nodes. If no such knowledge is
   available, [estimate] should be the constant zero function. *)

module Make (G : sig

  (* Graph nodes. *)
  type node
  include Hashtbl.HashedType with type t := node

  (* Edge labels. *)
  type label

  (* The graph's start node. *)
  val start: node

  (* Whether a node is a goal node. *)
  val is_goal: node -> bool

  (* [successors n f] presents each of [n]'s successors, in
     an arbitrary order, to [f], together with the cost of
     the edge that was followed. *)
  val successors: node -> (label -> int -> node -> unit) -> unit

  (* An estimate of the cost of the shortest path from the
     supplied node to some goal node. This estimate must
     be a correct under-approximation of the actual cost. *)
  val estimate: node -> int

end) : sig

  (* This function produces a shortest path from the start
     node to some goal node. It raises [Not_found] if no
     such path exists. *)
  val path: unit -> G.node list

end
