(* This module provides an implementation of Tarjan's algorithm for
   finding the strongly connected components of a graph.

   The algorithm runs when the functor is applied. Its complexity is
   $O(V+E)$, where $V$ is the number of vertices in the graph $G$, and
   $E$ is the number of edges. *)

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

module Run (G : GRAPH) = struct

  (* Define the internal data structure associated with each node. *)

  type data = {

      (* Each node records which graph node it stands for. *)

      node: G.node;

      (* Each node carries a flag which tells whether it appears
	 within the SCC stack (which is defined below). *)

      mutable stacked: bool;

      (* Each node carries a number. Numbers represent the order in
	 which nodes were discovered. *)

      mutable number: int;

      (* Each node [x] records the lowest number associated to a node
	 already detected within [x]'s SCC. *)

      mutable low: int;

      (* Each node carries a pointer to its strongly connected
	 component. This field is initialized with a dummy value and
	 used after the algorithm is finished to store its results. *)

      mutable scc: scc;

    } 

  (* Define the internal data structure that represents a strongly
     connected component. This data structure is used by the algorithm
     to store its results. *)

  and scc = {

      (* Each strongly connected component carries an index. Indices
	 are allocated sequentially. *)

      index: int;

      (* Each strongly connected component carries a list of its
	 members. *)

      mutable members: G.node list;

      (* Each strongly connected component carries a list of its
	 successors. *)

      mutable successors: scc list;

   }

  (* Define a dummy value of type [scc]. *)

  let dummy : scc = {
    index = -1;
    members = [];
    successors = [];
  }

  (* Define a mapping from external nodes to internal ones. Here, we
     simply use each node's index as an entry into a global array. *)

  let table =

    (* Create the array. We initially fill it with [None], of type
       [data option], because we have no meaningful initial value of
       type [data] at hand. *)

    let table = Array.create G.n None in

    (* Initialize the array. *)

    G.iter (fun x ->
      table.(G.index x) <- Some {
	node = x;
	stacked = false;
	number = 0;
	low = 0;
        scc = dummy
      }
    );

    (* Define a function which gives easy access to the array. It maps
       each node to its associated piece of internal data. *)

    function x ->
      match table.(G.index x) with
      |	Some dx ->
	  dx
      |	None ->
	  assert false (* Indices do not cover the range $0\ldots n$, as expected. *)

  (* Create an empty stack, used to record all nodes which belong to
     the current SCC. *)

  let scc_stack = Stack.create()

  (* Initialize a function which allocates numbers for (internal)
     nodes. A new number is assigned to each node the first time it is
     visited. Numbers returned by this function start at 1 and
     increase. Initially, all nodes have number 0, so they are
     considered unvisited. *)

  let mark =
    let counter = ref 0 in
    fun dx ->
      incr counter;
      dx.number <- !counter;
      dx.low <- !counter

  (* Initialize a counter of the strongly connected components. *)

  let scc_index =
    ref 0

  (* Look at all nodes of the graph, one after the other. Any
     unvisited nodes become roots of the search forest. *)

  let () = G.iter (fun root ->
    let droot = table root in

    if droot.number = 0 then begin

      (* This node hasn't been visited yet. Start a depth-first walk
	 from it. *)

      mark droot;
      droot.stacked <- true;
      Stack.push droot scc_stack;

      let rec walk x =
	let dx = table x in

	G.successors (fun y ->
	  let dy = table y in

	  if dy.number = 0 then begin

	    (* $y$ hasn't been visited yet, so $(x,y)$ is a regular
	       edge, part of the search forest. *)

	    mark dy;
	    dy.stacked <- true;
	    Stack.push dy scc_stack;

	    (* Continue walking, depth-first. *)

	    walk y;
	    if dy.low < dx.low then
	      dx.low <- dy.low

	  end
	  else if (dy.low < dx.low) & dy.stacked then begin

	    (* The first condition above indicates that $y$ has been
	       visited before $x$, so $(x, y)$ is a backwards or
	       transverse edge. The second condition indicates that
	       $y$ is inside the same SCC as $x$; indeed, if it
	       belongs to another SCC, then the latter has already
	       been identified and moved out of [scc_stack]. *)

	    if dy.number < dx.low then
	      dx.low <- dy.number

	  end

	) x;

	(* We are done visiting $x$'s neighbors. *)

	if dx.low = dx.number then begin

	  (* $x$ is the entry point of a SCC. The whole SCC is now
	     available; move it out of the stack. We pop elements out
	     of the SCC stack until $x$ itself is found. *)

	  let i = !scc_index in
	  scc_index := i + 1;

	  let scc = {
	    index = i;
	    members = [];
	    successors = []
	  } in

	  let rec loop () =
	    let element = Stack.pop scc_stack in
	    element.stacked <- false;
            scc.members <- element.node :: scc.members;
	    element.scc <- scc;
	    if element != dx then
	      loop() in

	  loop()

	end in

      walk root

    end
  )

  (* Let us now construct and publish a graph of the strongly
     connected components. *)

  type node =
      scc

  let n =
    !scc_index

  let index scc =
    scc.index

  (* Allocate and initialize a table of all strongly connected components. *)

  let sccs : scc array =
    Array.make n dummy

  let () =
    G.iter (fun x ->
      let dx = table x in
      let scc = dx.scc in
      let i = scc.index in
      assert (0 <= i && i < n);
      sccs.(i) <- scc
    )

  (* Iteration over all strongly connected components. *)

  let iter f =
    Array.iter f sccs

  (* Conversion from graph nodes to strongly connected components
     and back. *)

  let scc x =
    (table x).scc

  let members scc =
    scc.members

  (* Construct the edges that relate each component to its successors. *)

  let () =
    iter (fun scc1 ->
      List.iter (fun x ->
	G.successors (fun y ->
	  let scc2 = scc y in
	  if scc1 != scc2 then
	    scc1.successors <- scc2 :: scc1.successors
	) x
      ) scc1.members
    )

  (* Eliminate duplicate edges. *)

  let equal scc1 scc2 =
    scc1.index = scc2.index

  let equivalent x1 x2 =
    equal (scc x1) (scc x2)

  let compare scc1 scc2 =
    scc1.index - scc2.index

  let cons x = function
    | [] ->
	[ x ]
    | (y :: _) as ys ->
	if compare x y = 0 then
	  ys
	else
	  x :: ys

  let rec weed = function
    | [] ->
	[]
    | x :: xs ->
	cons x (weed xs)

  let weed xs =
    weed (List.fast_sort compare xs)

  let () =
    iter (fun scc ->
      scc.successors <- weed scc.successors
    )

  let successors f scc =
    List.iter f scc.successors

end

