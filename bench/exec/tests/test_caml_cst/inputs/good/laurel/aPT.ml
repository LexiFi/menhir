(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/aPT.ml,v 1.4 2000/01/04 10:51:30 fpottier Exp $ *)

(* Given a control flow graph [G], this functor builds its Augmented Post-dominator Tree, as described in Pingali and
   Bilardi's ``APT: A Data Structure for Optimal Control Dependence Computation'' (PDLI'95). The data structure
   requires space $O(E)$, and it is built in time $O(E)$, where $E$ is the number of edges in [G].

   The algorithm deems that the graph's end node is never control dependent on itself. Because of this peculiarity,
   this node must have no successors -- otherwise, the algorithm would yield incorrect results with respect to the
   definition of control dependence. If this is a problem, then one should add a distinguished end node, which is
   a successor of the graph's original end node, and has no other predecessors or successors. *)

module Make (G : Graph.Flow) = struct

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Obtaining [G]'s post-dominator tree} *)

  (* Run Lengauer and Tarjan's algorithm, to compute [G]'s post-dominator tree. *)

  module PostDom0 = Lengauer.Run(Reverse.Flow(G))

  (* Complete our view of the post-dominator tree. Currently, given a node, we can find its immediate postdominator,
     i.e. its father in the tree. We wish to obtain a function which, given a node, yields its children in the tree. *)

  module PostDom = Complete.Pred (struct

    type node = G.node
    let n = G.n
    let index = G.index

    let iter =
      let module G = Complete.Iter(G) in
      G.iter

    let successors action node =
      Standard.do_option (PostDom0.dominator node) action

  end)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Answering [cd] queries} *)

  let cd action c v =

    (* Determine [c]'s parent node. If it has none, then the answer is the empty set: no node is control dependent on
       the flow graph's end node. *)

    PostDom.successors (fun u ->
      let iu = G.index u in

      (* List all nodes [w] found on the interval $[v, u)$ in the post-dominator tree. *)

      let rec loop w =
	if G.index w <> iu then begin
	  action w;
	  PostDom.successors loop w
	end in

      loop v

    ) c

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Building a Roman Chariots problem} *)

  (* Compute each node's level in the post-dominator tree, as well as the tree's height. This information is used when
     computing iterated post-dominance frontiers. By convention, the root has level 0, and children have higher
     levels. The tree's height is the level of the deepest leaf, plus one. *)

  let level =
    Array.create G.n 0 (* for instance *)

  let height =
    ref 0

  (* Create a list of routes. *)

  let routes =
    ref []

  (* Visit all nodes of the post-dominator tree in top-down order. *)

  let rec visit depth u =
    let iu = G.index u in

    (* Record [u]'s level. If necessary, update the tree's height. *)

    level.(iu) <- depth;
    if depth = !height then
      incr height;

    (* At each node [u], enumerate [u]'s children in the post-dominator tree. For each such child [c], enumerate edges
       $c\rightarrow v$ in [G]. *)

    PostDom.predecessors (fun c ->
      G.successors (fun v ->

	(* The [cd] set associated with edge $c\rightarrow v$ is the interval $[v, u)$ in the post-dominator tree.
	   (Recall that [u] must be an ancestor of [v] in the tree.) If [v] equals [u], then the interval is empty.
	   Otherwise, we create a new chariot route which describes it. Notice that the new route is added in front of
	   the existing route list. Because we visit the tree in top-down order, the routes end up sorted by
	   increasing top nodes. This order is exploited in step 5 of the APT construction. *)

	if G.index v <> iu then
	  routes := (c, v, u) :: !routes;

      ) c;
    ) u;
    PostDom.predecessors (visit (depth + 1)) u

  let () =
    visit 0 G.sink

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{[conds] preprocessing} *)

  (* Step 1. For each node [v], compute the number of routes whose top node (resp. bottom node) is [v]. *)

  let bcount =
    Array.create G.n 0

  let tcount =
    Array.create G.n 0

  let () =
    List.iter (fun (_, bottom, top) ->
      let ib = G.index bottom in
      bcount.(ib) <- bcount.(ib) + 1;
      let it = G.index top in
      tcount.(it) <- tcount.(it) + 1
    ) !routes

  (* Step 2. Compute, for each node [v], the size $A_v$ of the answer set which shall be obtained when querying [v].
     To do so, we visit the post-dominator tree, bottom-up. The number of routes which contain [v] is equal
     to the number of routes which contain one if its children, minus those which end at [v], plus those which
     begin at [v].

     Step 3. Determine boundary nodes. This is done by a greedy, bottom-up algorithm, which tries to make zones as
     large as possible (i.e. create as few boundary nodes as possible) without breaking a constant-factor relationship
     between zone size and answer set size. The constant factor is determined by [alpha]. Query time increases with
     [alpha], while space usage decreases with [alpha]. Here, [alpha] is set to 1.

     Both steps are performed during a single bottom-up traversal. *)

  let asize =
    Array.create G.n 0 (* for instance *)

  let boundary =
    Array.create G.n true (* for instance *)

  let zsize =
    Array.create G.n 0 (* for instance *)

  let rec visit v =
    let asum = ref 0
    and zsum = ref 0 in
    PostDom.predecessors (fun c ->
      visit c;
      let ic = G.index c in
      asum := !asum + asize.(ic);
      zsum := !zsum + zsize.(ic)
    ) v;
    let iv = G.index v in
    asize.(iv) <- bcount.(iv) - tcount.(iv) + !asum;

    (* If [v] is a leaf node (which we determine by comparing the total size of its children's subzones with 0),
       or if making [v] an interior node would make the current zone too large with respect to [v]'s answer set,
       then we must make [v] a boundary node. *)

    if (!zsum = 0) or (!zsum > (* $\alpha\times$ *) asize.(iv)) then begin
      boundary.(iv) <- true;
      zsize.(iv) <- 1
    end
    else begin
      boundary.(iv) <- false;
      zsize.(iv) <- !zsum + 1
    end

  let () =
    visit G.sink

  (* Step 4. Determine, for each node [v], the next boundary node in the path from [v] to the root. This is easily
     computed in top-down order. *)

  let next_boundary =
    Array.create G.n None

  let rec visit v =

    (* If [v] is the root, then it does not have a next boundary node. *)

    PostDom.successors (fun ipdv ->

      (* Otherwise, let [ipdv] be its immediate post-dominator. If [ipdv] is a boundary node, then [v]'s next
	 boundary node is [ipdv]; otherwise, it is [ipdv]'s next boundary node. *)

      let iv = G.index v
      and iipdv = G.index ipdv in

      next_boundary.(iv) <-
	if boundary.(iipdv) then Some ipdv
	else next_boundary.(iipdv)

    ) v;
    PostDom.predecessors visit v

  let () =
    visit G.sink

  (* Step 5. Construct [cached.(v)] for each node [v], i.e. the set of routes which shall be cached at node [v].
     Because routes are currently sorted in increasing order of top endpoint, and because our construction again
     reverses the order, the list cached at each node ends up sorted in decreasing order of top endpoint. *)

  let cached =
    Array.create G.n []

  let () =
    List.iter (fun ((_, bottom, top) as route) ->

      let tnum = PostDom0.number top in

      (* Determine in which nodes this route should be cached. These are its bottom node, as well as all boundary
	 nodes found on the route. A simple loop, starting at the bottom node and going up until passing the top
	 node, finds all these nodes. *)
      
      let rec loop w =
	
	(* Determine whether [top] is a proper ancestor of [w]. If so, [w] lies on the route. Otherwise, the loop
	   ends. *)

	if tnum < PostDom0.number w then begin
	  let iw = G.index w in
	  cached.(iw) <- route :: cached.(iw);
	  Standard.do_option next_boundary.(iw) loop
	end in

      loop bottom
	
    ) !routes

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Answering [conds] queries} *)

  (* [query] is an internal version of [conds], with one extra parameter, [marked], which allows skipping certain
     nodes during the visiting process. It is unused when performing isolated queries; it is used only when computing
     iterated post-dominance frontiers. *)

  let query marked action q =
    let qnum = PostDom0.number q in

    (* [visit v] determines whether any of the routes cached at node [v], and within its descendants within the
       current zone, contain the node [q]. If so, these routes are added to [answer]. *)

    let rec visit v =

      (* Examine the routes cached at node [v]. We examine routes in decreasing top node order, which allows us to
	 break out of the loop as soon as we find a route which does not contain [q]. *)

      let rec examine = function
	| [] ->
	    ()
	| ((source, bottom, top) as route) :: routes ->

	    (* Determine whether [q] belongs to this route, i.e. whether [top] is a proper ancestor of [q]. We know
	       [v] belongs to the route, and we know [q] is an ancestor of [v], so [q] and [top] must belong to the
	       same branch of the post-dominator tree. This allows us to compare them simply by comparing their
	       depth-first search numbers, as computed by Lengauer and Tarjan's algorithm. *)

	    if PostDom0.number top < qnum then begin
	      action source bottom;
	      examine routes
	    end in

      let iv = G.index v in
      examine cached.(iv);

      (* If [v] is a boundary node, then the search stops here. Otherwise, [v]'s descendants belong to the same zone
	 as [v], so the search goes on.

	 Any children of [v] which are marked are skipped. *)

      if not boundary.(iv) then
	PostDom.predecessors (fun c ->
	  if not (marked c) then
	    visit c
        ) v in

    visit q

  (* When answering a single query, there are no marked nodes, i.e. all nodes are to be visited normally. *)

  let conds action q =
    query (fun _ -> false) action q

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Computing iterated post-dominance frontiers} *)

  let frontier action nodes =

    (* Create a priority queue. Priorities correspond to levels in the post-dominator tree, so an array of size
       [height] suffices to implement the queue. *)

    let queue =
      Array.create !height [] in

    (* Create an array which allows marking nodes. A node is marked when it is added to the queue. Thus, queries can
       avoid visiting any nodes below a marked node, since it is known that they have been queried before. *)

    let marked =
      Array.create G.n false in

    (* Define queue insertion. *)

    let insert x =
      let ix = G.index x in
      if not marked.(ix) then begin
	marked.(ix) <- true;
	let lx = level.(ix) in
	queue.(lx) <- x :: queue.(lx)
      end in
  
    (* Insert every node in the initial set into the priority queue. *)

    nodes insert;

    (* Extract one element from the priority queue, deal with it, and continue. Dealing with a node [q] can only
       cause nodes with smaller or equal levels to be inserted into the queue. This property allows us to perform
       only a single downward walk over the priority array. *)
      
    let rec walk lq =
      match queue.(lq) with
      |	q :: rest ->
	  queue.(lq) <- rest;

	  (* After dequeuing an element [q], we present it to the caller by invoking [action]. Then, we perform a
	     [conds] query on [q], so as to find all edges on which [q] is control dependent. The query is sped up
	     by stopping at marked nodes. Whenever an edge is found, we insert its source node into the queue. (At
	     this point, marks also help us avoid duplicates.) *)

	  action q;
	  query (fun x -> marked.(G.index x)) (fun source _ -> insert source) q;

	  walk lq
      |	[] ->
	  if lq > 0 then
	    walk (lq - 1) in

    walk (!height - 1)

end

