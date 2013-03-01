(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/sSA.ml,v 1.7 2000/01/19 15:58:53 fpottier Exp $ *)

(* This module implements two functors, with the same signature, which allow performing the initial allocation of type
   variables at the beginning of the typed bytecode analysis. One is a smart algorithm, based on a static single
   assignment (SSA) computation, which takes advantage of the control flow graph's shape to allocate a very small
   number of type variables. (For instance, a local variable which is never written to by the given method will be
   associated with only one type variable, instead of one per program point.) The other is a naïve algorithm, used
   for comparison purposes. *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Definitions} *)

(* The following are our functors' argument signatures. *)

module type In = sig

  (* Edges in [G] are expected to be labelled. In particular, every \texttt{jsr} instruction must be linked to the
     instruction immediately following it through a [Pseudo] edge. The effect of pseudo-edges is to pretend that the
     values of the local variables after a \texttt{jsr} may be identical to their values before the call. This allows
     ``subroutine polymorphism''. If pseudo-edges were omitted, the analysis would conclude that the values of the
     local variables after a \texttt{jsr} are equal to their values at the end of the subroutine, which would be
     correct, but would prevent polymorphism. *)

  include Graph.LabelledFlowIter with type label = Flow.label

  (* This function allows querying the instruction contained in a given node about its stack usage. The function
     should return a pair of integers. The first (resp. second) integer represents the number of stack words which are
     consumed (resp. produced) by the instruction. Consumption is assumed to take place only along [Normal] and
     [Throw] edges, so the first integer need not be defined at a node which has no such outgoing edges. Production is
     assumed to take place only along [Normal] edges, so the second integer need not be defined at a node which has no
     such outgoing edges. *)

  val stack_usage: node -> int * int

  (* This function allows determining which local variables (if any) are directly affected (i.e. written to) at a
     given node. \texttt{jsr} instructions should be considered as affecting no variable. So should imaginary nodes. *)

  val affected: (int -> unit) -> node -> unit

  (* The number of local variables used by the method. Locals are numbered from 0 to [locals]$ - 1$. *)

  val locals: int

end

module type Creator = sig

  type node
  type variable

  (* This function allows creating a fresh type variable to represent the content of a local variable or a stack
     location at a given (set of) program point(s). It is passed one of the nodes it is associated with.
     TEMPORARY might want to set up a mechanism to allow recording *all* of the nodes *)

  val fresh: node -> variable

end

(* The following is our functors' result signature. *)

module type Out = sig

  type node
  type variable

  (* This function maps a program point and a local variable index to a type variable. *)

  val local: node -> int -> variable

  (* This function maps a program point and a stack position index (strictly less than the stack's height at the given
     node) to a type variable. *)

  val stack: node -> int -> variable

end

(* This functor implements the main algorithm. *)

module Smart
    (G : In)
    (H : Height.S with type node = G.node)
    (C : Creator with type node = G.node)
= struct

  type node = G.node
  type variable = C.variable

  (* An auxiliary, internal exception. *)

  exception GotIt of variable option

  (* Compute an [APT] structure for [G]'s reverse control flow graph. This allows computing static single assignment
     forms for [G]. *)

  module APT = APT.Make (Reverse.Flow (Complete.Strip (G)))

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Allocating type variables} *)

  (* [allocate] expects an array of Boolean values, [distinguished], telling which nodes of [G] need to be allotted
     fresh type variables. The auxiliary parameter [live] is a predicate on nodes, which tells whether a given node
     needs to be given a type variable at all. (Empty stack locations need not be given type variables.) [allocate]
     performs type variable allocation, and returns an array which maps nodes to variables (actually, variable
     [option]s). *)

  let allocate live distinguished =

    (* Create an array to hold the mapping from nodes to variables. *)

    let variable =
      Array.create G.n None in

    (* Now, perform a depth-first traversal of the graph, allocating or copying type variables as we go. *)

    let rec visit v node =
      let index = G.index node in

      (* If the node isn't live, it needn't be given a type variable. Otherwise, decide which type variable to use at
	 this node. If it is distinguished, we create a fresh type variable. Otherwise, we re-use its predecessors'
	 type variable. It must have at least one predecessor, because only the start node may have no predecessors,
	 and the start node is distinguished. Furthermore, all of its predecessors must use the same type variable,
	 because otherwise the node would also be distinguished -- this is the very purpose of the SSA computation. *)

      let v =
	if live node then
	  if distinguished.(index) then Some (C.fresh node) else v
	else
	  None in
      variable.(index) <- v;

      (* Continue the traversal. If some successor of ours has already been visited, then either it is
	 distinguished, or [v] must also have been associated with it. In either case, we can skip it. *)

      G.successors (fun _ node' ->
	if variable.(G.index node') = None then
	  visit v node'
      ) node in

    visit None G.start;
    variable

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Dealing with local variables} *)

  (* Allocate an array which shall hold the type variable associated with every local variable and every program
     point. *)

  let local =
    Array.create G.locals [||]

  (* Deal with each local variable, in turn. *)

  let () =
    for k = 0 to G.locals - 1 do

      (* Create an array which allows marking program points where a fresh type variable should be allocated to
	 represent local variable number [k]. *)

      let distinguished =
	Array.create G.n false in

      (* Perform a static single assignment computation. *)

      APT.frontier
	(fun node -> distinguished.(G.index node) <- true)
	(fun action ->

	  (* Enumerate all edges which may affect local variable [k], and mark their end nodes as distinguished. The
	     graph's start node also needs to be marked, because this is where the variable's initial value is set. *)

	  action G.start;

	  G.iter (fun node ->
	    G.affected (fun k' ->
	      if k' = k then
		G.successors (fun _ node' ->
		  action node'
                ) node
            ) node
          )

        );

      (* Use the computation's results to drive type variable allocation. In the case of local variables, all
	 locations are ``live'', i.e. every local variable exists at every program point. This allows us to transform
	 the [variable option] array into a [variable] array after creating it. *)

      local.(k) <- Array.map Standard.option (allocate (fun _ -> true) distinguished)

    done

  (* Publish a function which maps a program point and a local variable index to a type variable. *)

  let local node k =
    local.(k).(G.index node)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Dealing with the stack} *)

  (* Allocate an array which shall hold the type variable associated with every stack position and every program
     point. *)

  let stack =
    Array.create H.max [||]

  (* Deal with each stack position, in turn. *)

  let () =
    for k = 0 to H.max - 1 do

      (* Create an array which allows marking program points where a fresh type variable should be allocated to
	 represent stack position number [k]. *)

      let distinguished =
	Array.create G.n false in

      (* Perform a static single assignment computation. *)

      APT.frontier
	(fun node -> distinguished.(G.index node) <- true)
	(fun action ->

	  (* Enumerate all edges which may affect stack position [k], and mark their end nodes as distinguished. *)

	  action G.start;

	  G.iter (fun node ->
	    let h = H.height node in

	    G.successors (fun label node' ->
	      match label with
	      |	Flow.Normal ->

		  (* Determine the instruction's normal stack usage. Because we are dealing with a normal flow edge,
		     this makes sense. *)

		  let down, up = G.stack_usage node in
		  
		  (* On a normal flow edge, the instruction consumes [down] operands and produces [up] results,
		     so affected positions range from [h - down] (inclusive) up to [h] or [h - down + up]
		     (whichever is greater) (exclusive). *)

		  if (k >= h - down) & ((k < h) or (k < h - down + up)) then
		    action node'

	      |	Flow.Throw ->

		  (* On an exceptional edge, the stack is emptied before pushing the exception object, so all
		     locations are affected, except those which are not location 0 and were empty already. *)

		  if (k = 0) or (k < h) then
		    action node'

	      |	Flow.Imaginary p ->

		  (* On an imaginary edge, the stack is emptied except for its [p] top words. If the stack height was
		     exactly [p], then no locations are affected. Otherwise, all locations are affected, except those
		     which were empty already. *)

		  if (h <> p) & (k < h) then
		    action node'

	      |	Flow.Pseudo ->

		  (* Pseudo-edges will never be taken during actual execution; they are part of a voluntary
		     approximation. So, which locations are affected by a pseudo-edge is irrelevant. We can
		     pretend none are. *)

		  ()

            ) node
          )
        );

      (* Use the computation's results to drive type variable allocation. To avoid creating type variables which
	 describe empty stack locations, we build a predicate which tells which stack locations are empty. *)

      let live node =
	k < H.height node in

      stack.(k) <- allocate live distinguished

    done

  (* Publish a function which maps a program point and a stack position index (strictly less than the stack's height
     at the given node) to a type variable. *)

  let stack node k =
    Standard.option stack.(k).(G.index node)

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{A naïve algorithm} *)

(* This functor implements a naïve algorithm, which allocates one variable per stack position. It is also correct,
   but should make the analysis much less efficient. *)

module Naive
    (G : In)
    (H : Height.S with type node = G.node)
    (C : Creator with type node = G.node)
= struct

  type node = G.node
  type variable = C.variable

  (* This function performs the allocation of [m] variables per program point. The predicate [live] tells which
     variables are live at a given node. The function returns a function which maps a program point and an index
     to a type variable. *)

  let allocate live m =
    let table = Array.init m (fun k ->
      let table = Array.create G.n None in
      G.iter (fun node ->
	if live k node then
	  table.(G.index node) <- Some (C.fresh node)
      );
      table
    ) in
    fun node k ->
      Standard.option table.(k).(G.index node)

  let local =
    allocate (fun _ _ -> true) G.locals

  let stack =
    allocate (fun k node -> k < H.height node) H.max

end

