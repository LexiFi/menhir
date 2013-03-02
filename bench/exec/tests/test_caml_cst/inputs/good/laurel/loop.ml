(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/loop.ml,v 1.1 1999/12/23 15:55:56 fpottier Exp $ *)

(* Given a flow graph [G], this functor computes its loop post-dominator forest, as described in Bilardi and Pingali's
   ``A Framework for Generalized Control Dependence'' (PLDI'96). Complexity is $O(E)$, where $E$ is the number of
   edges in [G].

   For the algorithm to yield correct results with respect to its specification, the graph's end node must have
   no successors. *)

module Make (G : Graph.Flow) = struct

  (* Obtain [G]'s augmented post-dominator tree. *)

  module APT = APT.Make(G)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Precompute sets of distinguished predecessors} *)

  (* Steps 2 and 3. To each vertex [v], associate a list of edges of the form $x\rightarrow v$, where $v$ is not $x$'s
     immediate post-dominator.
     
     No edges are created when [v] is the root of the post-dominator tree, because then $x$'s immediate post-dominator
     is either [v] (if $x$ and and [v] are distinct) or undefined (if $x$ equals [v]). *)

  let edges =
    Array.create G.n []

  let _ =
    APT.PostDom.iter (fun x ->
      APT.PostDom.successors (fun ipdx ->
	let iipdx = G.index ipdx in
	G.successors (fun v ->
	  let iv = G.index v in
	  if iipdx <> iv then
	    edges.(iv) <- x :: edges.(iv)
        ) x
      ) x
    );
    assert (edges.(G.index G.sink) = [])

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Implement a special-purpose stack} *)

  (* Step 4. Implement a stack of nodes, where each node can appear at most once, and which allows constant-time
     access from one node to the node above it in the stack.

     We do this by implementing the stack as a doubly-linked list, and using an auxiliary array to map node indices
     to list cells. *)

  type cell = {
      content: G.node;
      mutable next: cell option;
      prev: cell option
    } 

  let top =
    ref None

  let cells =
    Array.create G.n None

  let push node =

    (* Create a new cell. *)
    
    let cell = {
      content = node;
      next = None;
      prev = !top
    } in

    (* Record a mapping from this node to this cell in the array. *)

    assert (cells.(G.index node) = None);
    cells.(G.index node) <- Some cell;

    (* Update the previous cell's [next] pointer, if there was one. Then, push the new cell on top of the stack. *)

    Standard.do_option !top (fun top ->
      top.next <- Some cell
    );
    top := Some cell

  let pop () =
    match !top with
    | Some cell ->
	
	(* Retrieve the node. *)

	let node = cell.content in

	(* Erase its mapping in the array. *)

	cells.(G.index node) <- None;

	(* Pop the cell off the stack. Update the previous cell's [next] pointer, if there was one. *)

	top := cell.prev;
	Standard.do_option !top (fun top ->
	  top.next <- None
        )

    | None ->
	assert false

  let pushed_next node =

    (* Find the cell associated with this node on the stack, and return the node contained in the next cell. *)

    match cells.(G.index node) with
    | Some { next = Some { content = node }} ->
	node
    | _ ->
	assert false

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Build the Sibling Connectivity Graph} *)

  (* Prepare an array where crowns shall be marked. *)

  let count =
    Array.create G.n 0

  (* Prepare an array where SCG edges shall be stored. *)

  let scg =
    Array.create G.n []

  (* Step 5. Visit nodes in a depth-first walk over the post-dominator tree. *)

  let rec visit v =
    push v;

    (* For each $x\rightarrow v$ in [edge.(v)], let $y$ be the node pushed immediately on top of $x$'s immediate
       post-dominator. Add edge $x\rightarrow y$ to the SCG.

       Notice that, because $x\rightarrow v$ exists in [G], any post-dominator of $x$ is also a post-dominator of
       $v$. This implies that [ipdx] post-dominates $v$. Because we are performing a depth-first traversal of the
       post-dominator tree, and we are currently visiting $v$, [ipdx] must currently be on the stack. Furthermore,
       because [v] and [ipdx] differ, there must be some element above [ipdx] on the stack. This justifies our
       invocation of [pushed_next].

       If the edge to be added forms a self loop, then we simply mark the node as a crown, instead of adding the
       edge. *)

    List.iter (fun x ->
      APT.PostDom.successors (fun ipdx ->
	let y = pushed_next ipdx in
	let ix = G.index x in
	if ix = G.index y then
	  count.(ix) <- 2
	else
	  scg.(ix) <- y :: scg.(ix)
      ) x
    ) edges.(G.index v);

    APT.PostDom.predecessors visit v;
    pop()

  let _ =
    visit G.sink

  (* Encapsulate our findings into a structure which represents the SCG. Notice that, because we did not check for
     duplicates when inserting edges into the SCG, the graph might contain redundant edges (i.e. edges with the
     same source and destination nodes). However, this should not be a problem for our purposes. *)

  module SCG = struct
    type node = G.node
    let n = G.n
    let index = G.index
    let iter = APT.PostDom.iter
    let successors action node =
      List.iter action scg.(G.index node)
  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Determine the set of crowns} *)

  (* Run Tarjan's algorithm to find the strongly connected components of the SCG. *)

  module Component = Tarjan.Run(SCG)

  (* Determine which nodes belong to components of size 2 at least, i.e. to a cycle. To do this, we count the number
     of nodes which each representative element stands for. Because any node with a self loop already has a count of 2
     (initialized during the previous step), crowns are exactly the nodes whose representative node has a count of 2
     at least. *)

  let _ =
    SCG.iter (fun node ->
      let i = G.index (Component.representative node) in
      count.(i) <- count.(i) + 1
    )

  let crowns action =
    SCG.iter (fun node ->
      if count.(G.index (Component.representative node)) >= 2 then
	action node
    )

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Compute its iterated post-dominance frontier} *)

  (* As noted by Bilardi and Pingali, the loop post-dominance forest is obtained by pruning the post-dominance tree.
     The question is, which nodes are cutoff nodes, i.e. nodes whose parent link should be erased? It turns out that
     the set of cutoff nodes is exactly the iterated post-dominance frontier of the crown set. We can compute it in
     linear time using the APT. *)

  (* Whether or not the graph's end node appears in the crown set is irrelevant, since it contributes no extra points
     to the frontier (it may only be control dependent on itself), and whether or not it is deemed to be a cutoff
     point does not affect the forest (it has no parent anyway). *)

  let cutoff =
    Array.create G.n false

  let () =
    APT.frontier (fun x -> cutoff.(G.index x) <- true) crowns

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Build the loop post-dominance forest} *)

  (* A straightforward description of the forest is obtained by pruning the post-dominator tree at cutoff nodes. *)

  module PostDom = struct

    type node = G.node
    let n = G.n
    let index = G.index
    let iter = SCG.iter

    let successors action node =
      if not cutoff.(G.index node) then
	APT.PostDom.successors action node

  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Answer [cd] queries} *)

  (* This code is essentially identical to the one found in module [APT], except it correctly handles the case where
     the edge's source node has no parent in the forest. In the case of the post-dominator tree, a node with no
     parents must be the root node, which has an empty [cd] set. Here, a node with no parents may have a non-empty
     [cd] set. (It would, in fact, be found to have a parent if the forest was made into a tree by adding a new root
     node on top of all orphaned nodes.) *)

  let cd action c v =

    (* Determine [c]'s parent node's index. If it has none, use a non-existent index, so as to allow the forthcoming
       loop to go all the way to, and include, the (local) root node. *)

    let iu = ref (-1) in
    PostDom.successors (fun u ->
      iu := G.index u
    ) c;

    (* List all nodes [w] found on the interval $[v, u)$, where [u] is [c]'s parent node, in the post-dominator
       forest. *)

    let rec loop w =
      if G.index w <> !iu then begin
	action w;
	PostDom.successors loop w
      end in

    loop v

end

