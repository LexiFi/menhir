open Printf
open Print
open Sigs
open Bool3

module Make (Proof : PROOF) = struct

  module Expand =
    Expand.Make (Proof)

  module Display =
    Display.Make (Proof)

  open Proof

  (* ------------------------------------------------------------------------ *)

  (* A goal is determined when it is either refuted or proved. *)

  let determined goal =
    match status goal with
    | BUndetermined ->
	false
    | BTrue
    | BFalse ->
	true

  let undetermined goal =
    not (determined goal)

  (* A goal is unexpanded if it has no structure yet. *)

  let unexpanded goal =
    match structure goal with
    | Absent ->
	true
    | Disjunction _ ->
	false

  let expanded goal =
    not (unexpanded goal)

  (* ------------------------------------------------------------------------ *)

  (* A view of the accessible, undetermined portion of the proof net.
     Only the [n] field is missing, to be filled in when available. *)

  module Undetermined = struct

    type node =
	Goal.goal

    let index =
      index

    let successors f goal =
      successors (fun _ goal ->
	if undetermined goal then
	  f goal
      ) goal

    let iter f =
      iter_initial (fun goal ->
	if undetermined goal then
	  f goal
      )

  end
    
  (* ------------------------------------------------------------------------ *)

  (* Proof net analysis code. Used only to help understand the structure
     of these graphs. *)

  module Distribution = struct
    module IntMap = Map.Make (struct
      type t = int
      let compare = compare
    end)
    type t = int IntMap.t
    let empty = IntMap.empty
    let count v distrib =
      let previous =
	try
	  IntMap.find v distrib
	with Not_found ->
	  0
      in
      IntMap.add v (previous + 1) distrib
    let print b distrib =
      IntMap.iter (fun v count ->
	bprintf b "  %d elements at level %d.\n" count v
      ) distrib
  end

  let percentage k n =
    100.0 *. (float_of_int k /. float_of_int n)

  let analysis () =
    
    (* Count nodes, edges, degrees. *)

    let total_nodes = n()
    and undetermined_nodes = ref 0
    and undetermined_expanded_nodes = ref 0
    and edges = ref 0
    and degree = ref Distribution.empty in

    iter (fun goal ->
      if undetermined goal then begin
	incr undetermined_nodes;
	if expanded goal then begin
	  incr undetermined_expanded_nodes;
	  let d = ref 0 in
	  successors (fun _ _ ->
	    incr edges;
	    incr d;
	  ) goal;
	  degree := Distribution.count !d !degree
	end
      end
    );

    (* Display. *)

    w (fun b ->
      bprintf b "%d nodes, of which %d undetermined (%.0f%%), of which %d expanded (%.0f%%).\n"
	total_nodes
	!undetermined_nodes (percentage !undetermined_nodes total_nodes)
	!undetermined_expanded_nodes (percentage !undetermined_expanded_nodes total_nodes);
      bprintf b "There are %d edges out of undetermined expanded nodes.\n"
	!edges;
      bprintf b "The mean out degree of such a node is %.2f.\n"
	(float_of_int !edges /. float_of_int !undetermined_expanded_nodes);
      bprintf b "The degree distribution is as follows:\n";
      Distribution.print b !degree
    );

    (* Examine the depth distribution within the accessible,
       unexpanded nodes. *)

    let accessible = ref 0
    and accessible_unexpanded = ref 0
    and depths = ref Distribution.empty
    and total_depths = ref 0 in

    let module B = Bfs.Run (struct
      include Undetermined
      let n = n()
    end) (struct

      let discover depth goal =
	incr accessible;
	assert (depth >= 0);
	if unexpanded goal then begin
	  incr accessible_unexpanded;
	  depths := Distribution.count depth !depths;
	  total_depths := !total_depths + depth
	end

    end) in

    (* Display. *)

    w (fun b ->
      bprintf b "%d nodes (%.0f%%) are accessible, of which %d (%.0f%%) unexpanded.\n"
	!accessible (percentage !accessible total_nodes)
	!accessible_unexpanded (percentage !accessible_unexpanded total_nodes);
      bprintf b "The mean depth of accessible, unexpanded nodes is %.2f.\n"
	(float_of_int !total_depths /. float_of_int !accessible_unexpanded);
      bprintf b "The depth distribution is as follows:\n";
      Distribution.print b !depths
    )

    (* TEMPORARY count edges of each kind *)
    (* TEMPORARY count which edges participate in cycles? count cycles? *)

  (* ------------------------------------------------------------------------ *)

  (* A proof net is finished when all initial nodes are determined. *)

  let finished () =
    fold_initial (fun goal finished ->
      finished && determined goal
    ) true

  (* ------------------------------------------------------------------------ *)

  (* This function is called once in a while. It re-evaluates all goals
     in the proof net, removes dead edges, re-computes which goals are
     accessible and how deep each goal lies within the net. *)

  let evaluate, display_evaluation_timer =
    Monitor.time "Evaluation" (fun () ->
      let module E = NetEval.Run (Proof) in
      ()
    )

  let examine () =

    if true then (* toggle *)
      fprintf stderr "Re-evaluating (%d nodes)...\n%!"
	(n());

    (* Re-evaluate and clean up the proof net. *)

    evaluate();

    if true then (* toggle *)
      display_evaluation_timer();

    if false then (* toggle *)
      analysis();

    (* Determine which goals are accessible and how deep each goal
       lies within the net. *)

    (* One could choose to remove all inaccessible nodes altogether,
       so as to free up some memory. Instead, we keep them around, in
       case they become accessible again in the future; this could
       save some work. It also fits better with our convention that
       all goals in a proof net are sequentially numbered. TEMPORARY *)

    (* Use this information to assign a new priority to each unexpanded
       goal.

       A goal's priority is based partly on its depth, so the proof
       net is expanded breadth-first. Inaccessible goals, which, by
       convention, lie at depth (-1), are not inserted into the
       queue.

       A goal's priority is also based partly on its weight, as
       defined in [Expand], so that lighter goals are expanded
       first. *)

    (* TEMPORARY one could wish to give a goal high priority when many
       paths lead to it; how? *)

    let queue =
      ref PriorityQueue.empty in

    let module B = Bfs.Run (struct
      include Undetermined
      let n = n()
    end) (struct

      let priority depth goal =
	let weight = Expand.weight goal in
	4 * depth + weight (* TEMPORARY experiment; is depth important? *)

      let discover depth goal =
	assert (depth >= 0);
	if unexpanded goal then begin
	  if false then (* Toggle to display discovery messages. *)
	    w (fun b ->
	      bprintf b "Discovering goal %a at depth %d.\n" Goal.print goal depth
            );
	  queue := PriorityQueue.insert (priority depth goal) goal !queue
	end

    end) in

    set_priority_queue !queue

  (* ------------------------------------------------------------------------ *)

  (* Here is how a proof net grows. We repeatedly extract a goal out of the
     priority queue and expand it (which causes subgoals to be added to the
     net and to its priority queue). Every once in a while, we examine the
     entire net so as to check whether the termination criterion is met and
     so as to re-assign new priorities to all goals. The constant [chunk]
     determines how often this occurs. *)

  (* TEMPORARY chunk size could grow (exponentially?) with time, amortizing
     the cost of evaluation pauses. *)

  let dump_above_threshold = (* The "dump" file is written when this number of goals is reached. *)
    ref None

  (* TEMPORARY large chunk sizes tend to lead to more complex proofs.
     How do we avoid that? *)

  let rec expand k =
   if k = 0 then
     true (* interrupted *)
   else

     let () =
       match !dump_above_threshold with
       | Some threshold when threshold < n() ->
	   let ch = open_out "dump" in
	   wrap Display.print ch ();
	   close_out ch;
	   dump_above_threshold := None
       | _ ->
	   ()
     in

     match extract() with
     | None ->
	 examine();
	 assert (finished()); (* otherwise, we are in trouble *)
	 false (* done *)
     | Some goal ->
	 if false then (* Toggle to display goal extraction messages. *)
	   w (fun b ->
	     bprintf b "Extracting goal %a.\n" Goal.print goal
           );
	 Expand.complete goal;
	 expand (k-1)

  let expand, display_expansion_timer =
    Monitor.time "Expansion" expand

  (* ------------------------------------------------------------------------ *)

  (* The main loop alternates between expansion and evaluation. *)

  let rec run () =
    examine();
    let continue = expand Settings.chunk in
    if true then begin (* toggle *)
      display_expansion_timer();
      Expand.stats();
    end;
    if continue then
      run()

  (* ------------------------------------------------------------------------ *)

  (* Now, here is how a proof net grows, starting from scratch. *)

  let prove goals =

    Expand.create goals;

    run();

    if true then begin (* Toggle to dump the entire proof net to a text file. *)
      let ch = open_out Settings.proofname in
      wrap Display.print ch ();
      close_out ch
    end;
    if true then begin (* Toggle to dump the entire proof net to a dot file. *)
      let ch = open_out Settings.dotname in
      wrap Display.bdot ch ();
      close_out ch
    end

end

(* TEMPORARY produce proofs in a simple format (expansion at a specific position;
   congruence; transitivity) and write a small, trusted checker *)

(* TEMPORARY produce counter-examples when a goal is refuted *)

(* TEMPORARY when the prover diverges, it is essentially impossible to tell
   why. Think about automatic discovery of goals to be inserted into the
   cut dictionaries? *)

(* TEMPORARY it still feels as if priorities are badly assigned. How do we
   restrict attention to goals that a human would consider? Budget? Why
   does the prover diverge on [arith]? *)

(* TEMPORARY put newly installed goals in a separate bag, not in the main
   priority queue (unless attack goal); measure size of bag *)

(* TEMPORARY it is tempting to assign greater priority to goals that
   will turn out to have small out degree. (But how do we know in
   advance?) This could lead us down avenues that are not interesting,
   but depth will counter-balance that. *)

