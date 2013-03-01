(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/height.ml,v 1.6 2000/01/11 17:25:14 fpottier Exp $ *)

(* Given an abstract view of a method's bytecode array, this functor computes the method's stack height at every
   point. *)

(* This exception is raised if the analysis fails. *)

exception Failure

(* This is the return signature of the functor described below. *)

module type S = sig

  type node

  (* This function maps every program point to the stack's height at this point. *)

  val height: node -> int

  (* The maximum stack height. *)

  val max: int

end

(* This functor accepts an annotated control flow graph [G]. Every edge in [G] carries a label which tells whether it
   is a normal flow edge (which include subroutine call and return edges), or an exceptional edge. Furthermore, [G]
   must provide functions which allow querying the instruction found at a given node about its stack usage. The
   analyzer has no knowledge of the instruction set. Its only knowledge of exception handlers is given by the presence
   of [Throw] edges in [G]. *)

module Run (G : sig

  (* Edges in [G] are expected to be labelled with [Normal], [Throw] or [Imaginary]. [Pseudo] edges are superfluous,
     and will be ignored if provided. An edge labelled [Imaginary k] is considered as virtually emptying the stack,
     except it preserves its [k] top words. In particular, this mechanism allows stack height to be defined at all
     nodes. *)

  include Graph.LabelledStart with type label = Flow.label

  (* This function allows querying the instruction contained in a given node about its stack usage. The function
     should return a pair of integers. The first (resp. second) integer represents the number of stack words which are
     consumed (resp. produced) by the instruction. Consumption is assumed to take place only along [Normal] and
     [Throw] edges, so the first integer need not be defined at a node which has no such outgoing edges. Production is
     assumed to take place only along [Normal] edges, so the second integer need not be defined at a node which has no
     such outgoing edges. *)

  val stack_usage: node -> int * int

end) = struct

  type node = G.node

  (* Allocate an array where height information shall be stored. Record the stack's maximum height in a separate
     variable. *)

  let height =
    Array.create G.n None

  let max =
    ref 0

  (* This function simulates an instruction's stack consumption/production along a [Normal] edge. *)

  let consume h node =
    let down, up = G.stack_usage node in
    let h = h - down in
    if h < 0 then
      raise Failure;
    h + up

  (* Perform a depth-first graph traversal. *)

  exception IgnoreEdge

  let rec visit h node =
    if h > !max then
      max := h;
    height.(G.index node) <- Some h;

    (* Update every successor's stack height information, and visit it if not already done. *)

    G.successors (fun label node' ->
      try

	let h = match label with
	| Flow.Normal ->
	    consume h node
	| Flow.Throw ->
	    let _ = consume h node in
	    1
	| Flow.Pseudo ->
	    raise IgnoreEdge
	| Flow.Imaginary k ->
	    if h < k then
	      raise Failure;
	    k in

	match height.(G.index node') with
	| None ->
	    visit h node'
	| Some h' ->
	    if h <> h' then
	      raise Failure

      with IgnoreEdge ->
	()
    ) node

  let () =
    visit 0 G.start

  (* Make sure the stack height is now defined at every node. *)

  let height =
    Array.map Standard.option height

  (* Publish a function which maps every program point to its stack height. *)

  let height node =
    height.(G.index node)

  (* Publish the maximum stack height. *)

  let max =
    !max

end

