(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/reverse.ml,v 1.2 1999/12/21 13:48:23 fpottier Exp $ *)

(* Given a flow graph $G$, this functor yields a new flow graph, which is identical to $G$, except the direction of
   flow edges is reversed. *)

module Flow (G : Graph.Flow) = struct

  type node = G.node

  let n =
    G.n

  let index =
    G.index

  let start =
    G.sink

  let sink =
    G.start

  let successors =
    G.predecessors

  let predecessors =
    G.successors

end

