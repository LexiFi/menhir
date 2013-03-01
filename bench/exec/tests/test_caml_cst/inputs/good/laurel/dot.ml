(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/dot.ml,v 1.2 1999/12/22 15:21:25 fpottier Exp $ *)

(* This module creates a textual representation of a graph [G], appropriate for feeding to the tool \texttt{dot}. *)

(* We begin by defining several utility functions. *)

open Printf

let graph action =
  let buffer = Buffer.create 1024 in
  bprintf buffer "digraph G {\n";
  action buffer;
  bprintf buffer "}\n";
  Buffer.contents buffer

let define_nodes name iter buffer =
  iter (fun node ->
    bprintf buffer "\"%s\" ;\n" (name node);
  )

let define_edges style name iter successors buffer =
  iter (fun node ->
    successors (fun successor ->
      bprintf buffer "\"%s\" -> \"%s\" %s ;\n" (name node) (name successor) style
    ) node
  )

(* Here come the user-visible parts. *)

module Make (G : sig

  type node

  val name: node -> string
  val iter: (node -> unit) -> unit
  val successors: (node -> unit) -> node -> unit

end) = struct

  let output =
    graph (fun buffer ->
      define_nodes G.name G.iter buffer;
      define_edges "" G.name G.iter G.successors buffer
    )

end

module Double (G : sig

  type node

  val name: node -> string
  val iter: (node -> unit) -> unit
  val successors: (node -> unit) -> node -> unit

end)(H : sig

  type node = G.node

  val successors: (node -> unit) -> node -> unit

end) = struct

  let output =
    graph (fun buffer ->
      define_nodes G.name G.iter buffer;
      define_edges "" G.name G.iter G.successors buffer;
      define_edges "[style=dashed]" G.name G.iter H.successors buffer
    )

end

