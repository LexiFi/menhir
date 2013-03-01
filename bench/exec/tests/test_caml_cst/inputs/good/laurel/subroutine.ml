(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/subroutine.ml,v 1.8 2000/01/19 15:58:54 fpottier Exp $ *)

(* This module allows analyzing the subroutine call graph of a given Java/JavaCard method. *)

(* This exception is raised at functor application time if the analyzer cannot find a valid interpretation of the
   subroutine structure. Because the analyzer uses a trial-and-error process, including backtracking, it is very
   difficult to report a meaningful error; the most meaningful error is not necessarily the one found in the last
   branch of the search tree. *)

exception Failure of string (* TEMPORARY *)

(* This functor accepts an annotated control flow graph [G]. Every edge in [G] carries a label which tells whether it
   is a normal flow edge (which includes subroutine call edges), an exceptional edge, or a pseudo-edge. Furthermore,
   [G] must provide functions which allow inspecting the instruction found at a given node. The analyzer only has
   limited knowledge of the instruction set: it only knows about \texttt{jsr}, \texttt{ret}, and \texttt{astore}. It
   also needs to know which local variable (if any) is written to by each instruction. Its only knowledge of exception
   handlers is given by the presence of [Throw] edges in [G].

   When the functor is applied, it attempts to compute a valid interpretation of the way subroutines are organized in
   [G]. If it fails, it raises [Failure]. Otherwise, it guarantees, in particular, that every subroutine will return
   to its caller (provided it returns at all). It returns a structure which allows determining which subroutine a given
   program point belongs to, which local variables are possibly affected by a given subroutine call, and which program
   points may call a given subroutine. *)

module type Out = sig

  type node

  (* The abstract type of subroutine identifiers. *)

  type identifier

  (* This function maps every node to its owner subroutine. *)

  val owner: node -> identifier

  (* This function maps every \texttt{jsr} node to the owner subroutine of its target node. *)

  val target: node -> identifier

  (* This function maps every subroutine (other than the root subroutine) to the set of local variables it
     affects. *)

  module IntSet : Set.S with type elt = int

  val affected: identifier -> IntSet.t

  (* This function maps every subroutine (other than the root subroutine) to the set of its call points. *)

  val callers: identifier -> node list

end

module Analyze (G : sig

  (* We assume edges in the control flow graph [G] to be labelled as follows. [Normal] edges correspond to regular
     flow edges, including subroutine calls. (\texttt{ret} instructions give rise to \emph{no} edges.) [Throw] edges
     correspond to jumps to an exception handler, due to some exception being thrown. [Pseudo] edges link every
     \texttt{jsr} instruction to the instruction immediately following it. Thus, a \texttt{jsr} instruction gives rise
     to two edges: a [Normal] edge to its target, and a [Pseudo] edge to the instruction behind it. The destination
     node of any [Imaginary] edge is assumed to belong to the root subroutine. *)

  include Graph.LabelledIterStart with type label = Flow.label

  (* This function allows determining which instruction is contained in a given node. Imaginary nodes contain no
     instruction. *)

  type opcode =
    | Jsr of node
    | Ret of int
    | Astore of int
    | Other

  val opcode: node -> opcode option

  (* This function allows determining which local variables (if any) are directly affected (i.e. written to) at a
     given node. \texttt{jsr} instructions should be considered as affecting no variable. So should imaginary nodes. *)

  val affected: (int -> unit) -> node -> unit

end) = struct

  type node = G.node

  (* Sets of integers are used when determining which local variables are affected by a given subroutine. *)

  module IntSet = Set.Make (struct
    type t = int
    let compare = (-)
  end)

  let strict_add x s =
    if IntSet.mem x s then
      raise (Failure "strict_add")
    else
      IntSet.add x s

  (* Determine how many instructions are \texttt{jsr} targets. In addition to the root subroutine, this gives us the
     number of subroutines. (We assume each subroutine has a single entry point.) *)

  let nsub =
    ref 1

  let is_header =
    Array.create G.n None

  (* A single pass over the graph allows determining which nodes are \texttt{jsr} targets, and assigning an integer
     number to each of them. These numbers range up from 1.

     Furthermore, we make sure each \texttt{jsr} target is an \texttt{astore} instruction, and we record the number of
     its local variable into a hash table (we cannot use an array, because this would require knowing the number of
     subroutines in advance).

     Third, we compute the set of call points for every subroutine, also recorded using a hash table. This table
     holds references to lists of call points. *)

  let distinguished =
    Hashtbl.create 139

  let callers =
    Hashtbl.create 139

  let () =
    G.iter (fun node ->
      match G.opcode node with
      |	Some (G.Jsr node') -> (
	  let i' = G.index node' in
	  match is_header.(i') with
	  | Some identifier ->

	      (* This node is already known to be a subroutine header. Record a new caller for it. *)

	      let callers = Hashtbl.find callers identifier in
	      callers := node :: !callers;

	  | None ->

	      (* This node is now discovered to be a subroutine header. Increment the subroutine counter, and
		 associate a new subroutine identifier with this node. *)

	      let identifier = !nsub in
	      nsub := identifier + 1;
	      is_header.(i') <- Some identifier;

	      (* Record this subroutine's caller. *)

	      Hashtbl.add callers identifier (ref [ node ]);

	      (* Make sure this node contains an \texttt{astore} instruction, and record its operand. *)

	      match G.opcode node' with
	      |	Some (G.Astore local) ->
		  Hashtbl.add distinguished identifier local
	      | _ ->
		  raise (Failure "jsr target should be astore")

      )
      |	_ ->
	  ()
    )

  (* The start node is also a header, because it is the header of the main subroutine. We make sure it is \emph{not}
     a \texttt{jsr} target, for otherwise it would be counted twice, leading to possibly inconsistent results. *)

  let () =
    let is = G.index G.start in
    match is_header.(is) with
    | Some _ ->
	raise (Failure "start should not be a jsr target")
    | None ->
	is_header.(is) <- Some 0

  (* Define an abstract type of subroutine identifiers. Internally, identifiers are simply integers; 0 stands for
     the root subroutine, while others are numbered starting at 1. *)

  type identifier = int

  (* Define the algorithm's state. *)

  type state = {

      (* This array maps every node to its owner subroutine, if it has been discovered so far. *)

      owner: (identifier option) array;

      (* This array maps every subroutine to its father in the subroutine call graph, if one has been discovered so
	 far. *)

      father: (identifier option) array;

      (* This waiting queue contains the set of edges which still need to be examined. The algorithm is implemented
	 as a forward flow analysis. One of the algorithm's invariants states that whenever an edge is queued, the
	 owner subroutine of its source node is set. *)

      mutable queue: (G.node * G.label * G.node) list

    }

  (* This auxiliary function creates a copy of a [state] record. *)

  let copy state = {
    owner = Array.copy state.owner;
    father = Array.copy state.father;
    queue = state.queue
  } 

  (* This auxiliary function enqueues a node's outgoing edges, provided this hasn't be done already in the past. *)

  let enqueue state node =
    match state.owner.(G.index node) with
    | None ->
	G.successors (fun label node' ->
	  state.queue <- (node, label, node') :: state.queue
        ) node
    | Some _ ->
	()

  (* This auxiliary function sets a node's owner, and fails if it was already set differently. *)

  let set_owner state node identifier =
    let i = G.index node in
    match state.owner.(i) with
    | None ->
	state.owner.(i) <- Some identifier
    | Some identifier' ->
	if identifier <> identifier' then
	  raise (Failure "owner set differently")

  (* This auxiliary function sets a subroutine's father, and fails if it was already set differently. It also fails
     if the subroutine is the root subroutine. *)

  let set_father state sub father =
    if sub = 0 then
      raise (Failure "attempt to set root's father");
    match state.father.(sub) with
    | None ->
	state.father.(sub) <- Some father
    | Some father' ->
	if father <> father' then
	  raise (Failure "father set differently")

  (* Define the algorithm's initial state. *)

  let initial = 
    {
      owner = Array.create G.n None;
      father = Array.create !nsub None;
      queue = []
    }

  let () =
    enqueue initial G.start;
    set_owner initial G.start 0

  (* Here comes the algorithm's main loop. *)

  let rec loop state =
    match state.queue with

    | (node, label, node') :: queue -> (
	state.queue <- queue;

	(* We have dequeued an edge. Determine the owner subroutine of its source node (which must have been set
	   already). *)

	let identifier = Standard.option state.owner.(G.index node) in

	(* Enqueue the destination node's outgoing edges, unless this node has already been visited in the past. *)

	enqueue state node';

	(* Determine which kind of edge we are dealing with, and enforce appropriate constraints. *)

	match label with
	| Flow.Normal -> (

	    match G.opcode node with
	    | Some (G.Jsr _) ->

		(* The destination node of a subroutine call edge belongs to its own subroutine (it is a subroutine
		   header), whose father is the owner of the edge's source node. *)

		let identifier' = Standard.option is_header.(G.index node') in
		set_owner state node' identifier';
		set_father state identifier' identifier;
		loop state

	    | Some _ ->

	        (* The destination node of a normal flow edge must belong to the same subroutine as its source
	           node. *)

		set_owner state node' identifier;
		loop state

	    | None ->
		assert false (* Some imaginary node has a [Normal] outgoing edge. *)

        )
	| Flow.Pseudo ->

	    (* The destination node of a pseudo-edge must belong to the same subroutine as its source node. *)

	    set_owner state node' identifier;
	    loop state

	| Flow.Imaginary _ ->

	    (* The destination node of an imaginary edge must belong to the root subroutine. *)

	    set_owner state node' 0;
	    loop state

	| Flow.Throw ->

	    (* The destination node of an exceptional edge may belong to the source node's subroutine, or to any of
	       its ancestors in the call tree. Thus, we have several choices, which we try successively, beginning
	       with the call tree's root. This may, in general, lead to exponential behavior, but this should never
	       be a problem in practice, since the call tree is usually very shallow.

	       The root subroutine has no father; each subroutine has at most one father; the current subroutine must
	       be reachable, within the call graph, from the root subroutine. From these three facts, it follows that
	       the call graph is acyclic, i.e. it is indeed a tree. This guarantees that the search for the current
	       node's ancestors terminates. *)

	    let rec ancestors action identifier =
	      match state.father.(identifier) with
	      |	None ->
		  action identifier
	      |	Some father ->
		  try
		    ancestors action father
		  with Failure _ ->
		    action identifier in

	    ancestors (fun ancestor ->

	      (* Create a copy of the current state, where the destination edge's owner subroutine is set to
		 [ancestor]. Use this copy to continue the search. *)

	      let state = copy state in
	      set_owner state node' ancestor;
	      loop state

	    ) identifier

    )
    | [] ->

	(* We have reached a leaf of our search tree. That is, we have managed to perform a full traversal of the
	   control flow graph, without detecting any inconsistency. A few checks remain to be performed to guarantee
	   that the algorithm's current state offers a consistent view of the way subroutines are organized. *)

	(* Make sure every \texttt{ret} instruction uses its subroutine's distinguished local. This includes checking
	   that the root subroutine contains no \texttt{ret} instructions. *)

	G.iter (fun node ->
	  let index = G.index node in
	  match G.opcode node with
	  | Some (G.Ret local) -> (
	      try
		let expected =
		  Hashtbl.find distinguished (Standard.option state.owner.(index)) in
		if local <> expected then
		  raise (Failure "ret uses some other local")
	      with Not_found ->
		raise (Failure "root subroutine contains ret")
          )
	  | _ ->
	      ()
        );

	(* Obtain an iterator on the call tree. *)

	let module CallTree = struct
	  type node = identifier
	  let n = !nsub
	  let index x = x
	  let successors action x =
	    Standard.do_option state.father.(x) action
	  let iter action =
	    for x = 0 to !nsub - 1 do
	      action x
	    done
	end in

	let module CallTree = Complete.Pred (CallTree) in

	(* There now remains to compute the set of locals which are modified by every subroutine, and make sure that
	   its distinguished local, as well as its ancestors', isn't modified. (This implies, in particular, that two
	   subroutines on the same call branch have different distinguished locals.) This will guarantee that every
	   subroutine does indeed return to its caller. *)

	let affected =
	  Array.create !nsub IntSet.empty in

	(* In a first pass, determine which locals are affected by each subroutine, not counting its initial
	   \texttt{astore} instruction, and not counting the effects of its \texttt{jsr} calls. *)

	G.iter (fun node ->
	  let index = G.index node in
	  match is_header.(index) with
	  | Some identifier when identifier > 0 ->

	      (* If this instruction is a subroutine header, ignore its effect for now. *)

	      ()

	  | _ ->

	      (* Otherwise, if this instruction affects variables, add them to the set of variables affected by the
		 instruction's owner subroutine. *)

	      G.affected (fun local ->
		let owner = Standard.option state.owner.(index) in
		affected.(owner) <- IntSet.add local affected.(owner)
	      ) node

	);

	(* In a second pass, which is a traversal of the subroutine call tree, compute the set of all variables
	   directly or indirectly affected by a subroutine, and make sure its distinguished local isn't affected,
	   except of course by the subroutine's first instruction. *)

	let rec visit identifier =

	  (* Check our children, and accumulate their effects. *)

	  CallTree.predecessors (fun child ->
	    visit child;
	    affected.(identifier) <- IntSet.union affected.(child) affected.(identifier)
	  ) identifier;

	  (* If this is the root subroutine, we are done. *)

	  if identifier <> 0 then

	    (* Otherwise, make sure our distinguished local isn't modified by us or by our children. Then, add it to
	       the set of locals which we affect. *)

	    affected.(identifier) <- strict_add
		(Hashtbl.find distinguished identifier) affected.(identifier) in

	visit 0;

	(* We now have a plausible view of the subroutine call tree. Let us end the search here. *)

	state, affected

  (* Run the algorithm. It returns its final state, as well as a table which lists the local variables affected by
     every subroutine. *)

  let final, affected =
    loop initial

  (* Publish a function which maps every node to its owner subroutine. *)

  let owner node =
    Standard.option final.owner.(G.index node)

  (* Publish a function which maps every \texttt{jsr} node to the owner subroutine of its target node. *)

  let target node =
    match G.opcode node with
    | Some (G.Jsr target) ->
	owner target
    | _ ->
	assert false

  (* Publish a function which maps every subroutine (other than the root subroutine) to the set of local variables it
     affects. *)

  let affected identifier =
    assert (identifier > 0);
    affected.(identifier)

  (* Publish a function which maps every subroutine (other than the root subroutine) to the set of its call points. *)

  let callers identifier =
    assert (identifier > 0);
    !(Hashtbl.find callers identifier)

end

