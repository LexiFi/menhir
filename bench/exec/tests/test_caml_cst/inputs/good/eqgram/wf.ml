open Printf
open Print
open Sigs
open Bool3

module Make (Proof : PROOF) = struct

  open Proof

  (* ------------------------------------------------------------------------ *)

  (* As far as well-foundedness is concerned, only undetermined goals
     have outgoing edges. Indeed, a goal that has been proved or
     refuted can be viewed as self-evident, and how it was proved or
     refuted becomes irrelevant. *)

  let successors_if_undetermined f goal =
    match status goal with
    | BTrue
    | BFalse ->
	()
    | BUndetermined ->
	successors f goal

  let filter_successors_if_undetermined f goal =
    match status goal with
    | BTrue
    | BFalse ->
	()
    | BUndetermined ->
	filter_successors f goal

  (* ------------------------------------------------------------------------ *)

  (* A global well-foundedness check. *)

  exception IllFounded

  let wf () : unit =

    (* First, let us construct a graph that corresponds to the proof net,
       deprived of its word-decreasing edges. *)

    let module G1 = struct

      type node =
	  Goal.t

      let n =
	n ()

      let index =
	index

      let successors f goal =
	successors_if_undetermined (fun edge subgoal ->
	  match edge with
	  | EWordDecreasing ->
	      () (* edge intentionally ignored *)
	  | EDerivationDecreasing
	  | ENeutral
	  | EDangerous ->
	      f subgoal
        ) goal

      let iter =
	iter

    end in

    (* Identify the strongly connected components of this graph. This tells
       us which cycles do not contain a word-decreasing edge. *)

    let module SCC1 = Tarjan.Run (G1) in

    (* Among these cycles, check that none contains a dangerous (that
       is, derivation-increasing) edge. That is, every such edge must
       relate two distinct strongly connected components. *)

    let () =
      iter (fun goal ->
	let scc1 = SCC1.scc goal in
	successors_if_undetermined (fun edge subgoal ->
	  match edge with
	  | EWordDecreasing
	  | EDerivationDecreasing
	  | ENeutral ->
	      ()
	  | EDangerous ->
	      let scc2 = SCC1.scc subgoal in
	      if SCC1.equal scc1 scc2 then begin
		if true then (* toggle *)
		  w (fun b ->
		    bprintf b "There is a dangerous edge from %a to %a,\n"
		      print_goal_index goal
		      print_goal_index subgoal;
		    bprintf b "and these goals participate in a non-word-decreasing cycle:\n%a\n\n"
		      (seplist space print_goal_index) (SCC1.members scc1)
                  );
		raise IllFounded
	      end
        ) goal
      )
    in

    (* Now, let us construct a graph that corresponds to the proof net,
       deprived of its word-decreasing and derivation-decreasing edges,
       and check that it contains no cycles.

       Equivalently, we can check that there are no cycles of neutral
       edges. (A mixed cycle of neutral and dangerous edges has
       already been ruled out.) *)

    let module G2 = struct

      include G1

      let successors f goal =
	successors_if_undetermined (fun edge subgoal ->
	  match edge with
	  | EWordDecreasing
	  | EDerivationDecreasing
	  | EDangerous ->
	      () (* edge intentionally ignored *)
	  | ENeutral ->
	      f subgoal
	) goal

    end in

    let module SCC2 = Tarjan.Run (G2) in

    G2.iter (fun goal ->
      let scc1 = SCC2.scc goal in
      G2.successors (fun subgoal ->
	let scc2 = SCC2.scc subgoal in
	if SCC2.equal scc1 scc2 then begin
	  if true then (* toggle *)
	    w (fun b ->
	      bprintf b "The following goals participate in a neutral cycle:\n%a\n\n"
		(seplist space print_goal_index) (SCC2.members scc1)
	    );
	  raise IllFounded
	end
      ) goal
    )

  let wf () : bool =
    try
      wf ();
      true
    with IllFounded ->
      false

  (* ------------------------------------------------------------------------ *)

  (* In a well-founded proof net, every cycle must either (i) contain at least
     one word-decreasing edge, or (ii) contain no derivation-increasing edge
     and contain at least one derivation-decreasing edge.

     This pattern can be recognized by a deterministic finite automaton. The
     following functions implement such an automaton. This allows detecting
     whether a newly-created cycle is well-founded.

     The automaton has four states. [S0] is the initial state. [S1] is
     an accepting sink where all [EWordDecreasing] transitions
     lead. [S2] is a rejecting sink where [EDangerous] transitions
     lead (except from [S1], that is); one can escape from it only to
     [S1] via a [EWordDecreasing] transition. [S3] is a state that is
     reached after at least one [EDerivationDecreasing] transition is
     taken. *)

  type state =
    | S0
    | S1
    | S2
    | S3

  let initial =
    S0

  let delta state input =
    match state, input with

    (* [ENeutral transitions have no effect. *)

    | _, ENeutral ->
	state

    (* [S1] is an accepting sink where all [EWordDecreasing]
       transitions lead. *)

    | _, EWordDecreasing
    | S1, _ ->
	S1

    (* [S2] is a rejecting sink where all [EDangerous] transitions
       lead, except as for the rules above. *)

    | _, EDangerous
    | S2, _ ->
	S2

    (* [S3] is a state that is reached after at least one
       [EDerivationDecreasing] transition is taken, except as for the
       rules above. *)

    | _, EDerivationDecreasing ->
	S3

  let accepting = function
    | S0
    | S2 ->
	false
    | S1
    | S3 ->
	true

  (* The automaton has totally ordered states -- if a word is accepted
     when beginning from a certain state, then it is also accepted
     when beginning from a higher state.

     [S1] is clearly the ordering's top element, because it accepts
     every word. [S2] is the bottom element, because it accepts only
     the words that contain [EWordDecreasing], and those words are
     accepted by every state. Furthermore, [S0] accepts fewer words
     than [S3], because it has exactly the same outgoing transitions,
     but is not itself accepting. So, the total order is [S2 < S0 < S3
     < S1]. *)

  let leq s1 s2 =
    match s1, s2 with
    | S2, _
    | _, S1
    | S0, S0
    | S0, S3
    | S3, S3 ->
	true
    | _, _ ->
	false

  let top =
    S1

  let print b state =
    Buffer.add_string b
      begin match state with
      | S0 ->
	  "S0"
      | S1 ->
	  "S1"
      | S2 ->
	  "S2"
      | S3 ->
	  "S3"
      end

  (* ------------------------------------------------------------------------ *)

  (* An accessibility check is used in order to prevent the creation
     of ill-founded cycles. The check is implemented by a depth-first
     search. One must verify that every path from a fixed source goal
     to a fixed destination goal carries an edge string that is
     accepted by the automaton.

     The automaton has totally ordered states -- if a word is accepted
     when beginning from a certain state, then it is also accepted
     when beginning from a higher state. Thus, the depth-first search
     maintains a mapping from each goal to the lowest state at which
     this goal could be reached.

     Note that the well-foundedness criterion is such that if two
     cycles are well-founded, then their combination (assuming that
     they share a vertex) is also well-founded. This means that we
     need check only simple cycles -- we do not need to cross the
     destination vertex several times in order to detect
     ill-foundedness. *)

  exception Rejected

  let check edge source destination : unit =

    (* Initialize the mapping of goals to states. *)

    let lowest : state Goal.Map.t ref =
      ref Goal.Map.empty
    in

    (* Define the function that looks up and updates this mapping.

       Note that a goal reached at state [top] is automatically
       considered visited, so that we stop examining its successors
       -- whatever they are, the word would be accepted. *)

    let visited state goal =
      if false then (* toggle *)
	w (fun b ->
	  bprintf b "Touching goal %a at state %a.\n"
	    print_goal_index goal
	    print state
	);
      let previous : state =
	try
	  Goal.Map.find goal !lowest
	with Not_found ->
	  top
      in
      if leq previous state then
	true
      else begin
	lowest := Goal.Map.add goal state !lowest;
	false
      end
    in

    (* Define depth-first search. *)

    let rec visit state goal =

      if not (visited state goal) then begin

	if Goal.equal goal destination then begin

	  (* The destination goal was reached. Check that the path that was
	     followed is accepted. *)

	  if not (accepting state) then
	    raise Rejected

	end
	else begin

	  (* An ordinary goal was reached. Traverse its outgoing edges. *)

	  successors_if_undetermined (fun edge successor ->
	    visit (delta state edge) successor
          ) goal

	end

      end

    in

    (* Initiate the search. *)

    let state =
      delta initial edge
    in

    if false then (* toggle *)
      w (fun b ->
	bprintf b "Checking all paths from %a to %a, beginning in state %a.\n"
	  print_goal_index source
	  print_goal_index destination
	  print state
      );

    visit state source;

    if false then (* toggle *)
      w (fun b ->
	bprintf b "Checking all paths from %a to %a took %d steps.\n"
	  print_goal_index source
	  print_goal_index destination
	  (Goal.Map.fold (fun _ _ n -> n + 1) !lowest 0)
      )

  (* ------------------------------------------------------------------------ *)

  (* [weakly_unreachable source destination] tests whether every path
     from [source] to [destination] contains a word-decreasing
     edge. This means that, if we start in [source] in state [S2],
     then we cannot reach [destination] in state [S2]. *)

  module Memo =
    SetMemo.Make(Goal.Set)

  let weakly_unreachable source destination : bool =

    let visited = Memo.create() in

    let rec visit goal : unit =
      if not (visited goal) then
	if Goal.equal goal destination then
	  raise Rejected
	else
	  successors_if_undetermined (fun edge successor ->
	    match edge with
	    | EWordDecreasing ->
		()
	    | EDerivationDecreasing
	    | ENeutral
	    | EDangerous ->
		visit successor
	  ) goal
    in

    try
      visit source;
      true (* acceptable *)
    with Rejected ->
      false (* invalid path exists *)

  (* ------------------------------------------------------------------------ *)

  (* [break source destination] ensures that no invalid path begins
     with [EDerivationDecreasing] and continues with a path from
     [source] to [destination]. It does so by destroying zero or more
     dangerous edges along the way. (Because the first edge is
     [EDerivationDecreasing], any invalid path must contain at least
     one dangerous edge.) *)

  let break source destination : unit =

    let visited = Memo.create() in

    (* [visit goal] ensures that there are no invalid paths starting
       from [goal] in state [S3]. *)

    let rec visit goal =
      if not (visited goal) && not (Goal.equal goal destination) then
	filter_successors_if_undetermined (fun edge successor ->
	  match edge with
	  | EWordDecreasing ->
	      true (* keep edge *)
	  | EDerivationDecreasing
	  | ENeutral ->
	      visit successor;
	      true (* keep edge *)
	  | EDangerous ->
	      weakly_unreachable successor destination (* keep edge only if cannot reach destination through it *)
	      || begin
		  if false then (* toggle *)
		    w (fun b ->
		      bprintf b "Breaking a dangerous edge from %a to %a.\n"
			Goal.print goal
			Goal.print successor
		    );
		  false
	      end
        ) goal
    in

    (* Initiate the search. *)

    visit source

end

