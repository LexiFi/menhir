(* This module proves or refutes goals whose left-hand side contains
   no transparent non-terminal symbols, by using an Earley parser, and
   attempts to prove or refute other goals, by enumerating expansions
   of their left-hand sides. *)

(* Note that, although the Earley parser is probably not very efficient, it is
   not a bottleneck. Some tests show that few of the goals explored by the
   refuter require parsing (typically less than 5%), and we are parsing very
   short sentences anyway. *)

open Printf
open Print
open Sigs
open Bool3

module Make (Proof : PROOF) = struct

  open Proof
  open Grammar
  open Goal
  open Word

  module Earley =
    Earley.Make (Grammar)

  (* ------------------------------------------------------------------------ *)

  (* Expansion of a goal on the left-hand side. *)

  let expand goal : (bool * goal) list =
    let lhs = Normal.left goal
    and rhs = Normal.right goal in

    (* Try expanding every symbol on the left-hand side. *)

    let rec loop i subgoals =
      try
	let i' = Left.next lhs i in 
	loop i' (
	  let symbol = Left.get lhs i in

	  (* Examine the symbol found between indices [i] and [i']. *)

	  match Grammar.classify symbol with
	  | T
	  | NO _ ->
	      subgoals (* symbol cannot be expanded *)
	  | NT productions ->
	      let prefix = Left.prefix lhs i
	      and suffix = Left.suffix lhs i' in
	      List.fold_left (fun subgoals (decreasing, production) ->
		let subgoal =
		  Normal.make (Word.concat3 prefix production suffix) rhs
		in
		(decreasing, subgoal) :: subgoals
	      ) subgoals productions

        )
      with Left.AtBoundary ->
	subgoals
    in

    loop (Left.start lhs) []

  (* ------------------------------------------------------------------------ *)

  (* A goal either has been refuted or has been explored down to some
     depth without finding a contradiction. A goal that has been
     proved is considered explored down to infinity. *)

  type status =
    | SContradicted
    | SExplored of int

  let proved =
    SExplored max_int

  let conjunction status1 status2 =
    match status1 with
    | SContradicted ->
	SContradicted
    | SExplored depth1 ->
	match Lazy.force status2 with
	| SContradicted ->
	    SContradicted
	| SExplored depth2 ->
	    SExplored (min depth1 depth2)

  let successor status =
    match status with
    | SContradicted ->
	SContradicted
    | SExplored depth ->
	if depth = max_int then
	  status
	else
	  SExplored (depth + 1)

  let leq status1 status2 =
    match status1, status2 with
    | _, SContradicted ->
	true
    | SContradicted, SExplored _ ->
	false
    | SExplored depth1, SExplored depth2 ->
	depth1 <= depth2

  let print_status b = function
    | SContradicted ->
	bprintf b "contradicted"
    | SExplored depth ->
	if depth = max_int then
	  bprintf b "proved"
	else
	  bprintf b "explored down to depth %d" depth

  (* ------------------------------------------------------------------------ *)

  (* A record of the goals that have been explored. *)

  let goals : status Goal.Map.t ref =
    ref Goal.Map.empty

  let get_status goal =
    try
      Goal.Map.find goal !goals
    with Not_found ->
      SExplored (-1)

  let set_status goal status =
    assert (leq (get_status goal) status); (* status improves over time *)
    goals := Goal.Map.add goal status !goals;
    status

  (* ------------------------------------------------------------------------ *)

  (* Counters. *)

  let invoked =
    ref 0

  let explored =
    ref 0

  let parsed =
    ref 0

  let conclusive =
    ref 0

  (* ------------------------------------------------------------------------ *)

  (* [explore depth goal] ensures that [goal] has been explored down to
     depth [depth]. It returns the goal's new status, which is either
     [SContradicted] or [SExplored depth'] where [depth' >= depth]. *)

  let rec explore depth goal =
    if false then (* toggle *)
      w (fun b ->
	bprintf b "Exploring %a (down to depth %d)...\n%!" Goal.print goal depth;
      );
    assert (depth >= 0);

   let status = get_status goal in
   let new_status = 
     match status with
     | SContradicted ->
	 SContradicted
     | SExplored k ->
	 if depth <= k then
	   status
	 else begin

	   (* Do not waste time studying goals whose status has been
	      determined already. The impact of this check seems
	      minimal, but one never knows. *)

	   match if known goal then Proof.status goal else BUndetermined with
	   | BFalse ->
	       set_status goal SContradicted
	   | BTrue ->
	       set_status goal proved
	   | BUndetermined ->

		(* Assert, ahead of time, that the goal has been explored
		   down to the desired depth, so as to avoid duplicating
		   work if we reach that goal again through a cycle.

		   In the presence of cycles, the status that is read at a
		   goal is not necessary stable yet, so it can be less
		   informative than it should be. This phenomenon should not
		   be problematic, especially considering that it cannot
		   take place at the root of the query -- the root result of
		   a query is [SContradicted] iff a refutable subgoal could be
		   reached within [depth] steps. *)

		let status = set_status goal (SExplored depth) in

		(* Examine the goal at hand. *)

		incr explored;

		match expand goal with
		| [] ->

		    (* The goal has no subgoals, which means that its left-hand side
		       contains no transparent non-terminal symbols. Evaluating such
		       a goal is a simple parsing problem. *)

		    (* Note that this code cannot be run twice for the same goal,
		       since the first run sets the goal status to [proved] or
		       [refuted] and the second run can only take place if the goal
		       status exhibits a finite depth. *)

		    let lhs = Normal.left goal
		    and rhs = Normal.right goal in

		    incr parsed;

		    set_status goal (
		      if Earley.parse lhs rhs then proved else SContradicted
		    )

		| subgoals ->

		    (* The goal has subgoals, which must be explored, [depth]
		       permitting. *)

		    (* When following a decreasing edge, the [depth] parameter
		       remains unchanged. When following a non-decreasing edge,
		       [depth] is decremented. This strategy ensures termination
		       (because there can be no infinite series of decreasing
		       edges) while ensuring good coverage where possible. In
		       particular, in areas where the grammar is not cyclic,
		       expansion is performed all the way down. *)

		    (* Another way of putting this is: depth counts only the
		       non-decreasing edges. *)

		    if depth > 0 then
		      set_status goal (successor (
			List.fold_left (fun status (decreasing, subgoal) ->
			  let depth =
			    if decreasing then depth else depth - 1
			  in
			  conjunction status (lazy (explore depth subgoal))
			) proved subgoals
		      ))
		    else begin
		      assert (status = SExplored 0);
		      status
		    end

	      end
	in
	if false then (* toggle *)
	  w (fun b ->
	    bprintf b "Exploring %a (down to depth %d) yields %a.\n%!" Goal.print goal depth print_status new_status;
	  );
	new_status
    
  (* ------------------------------------------------------------------------ *)

  (* Public interface. *)

  let explore =
    Monitor.monitor false Goal.print print_status "refuter" (explore Settings.depth)

  let explore goal =
    incr invoked;
    match explore goal with
    | SContradicted ->
	incr conclusive;
	BFalse
    | SExplored k ->
	if k = max_int then begin
	  incr conclusive;
	  BTrue
	end
	else
	  BUndetermined

  let stats () =
    fprintf stderr
      "The refuter was invoked %d times and produced %d conclusive answers (%d%%).\n"
      !invoked
      !conclusive
      (100 * !conclusive / !invoked);
    fprintf stderr
      "The refuter explored %d goals, out of which %d required parsing (%d%%).\n%!"
      !explored
      !parsed
      (100 * !parsed / !explored)

end

(* TEMPORARY the heuristics in [initial_status] could also be useful
   in the refuter to avoid expanding nodes that are clearly true or
   false. Actually, perhaps all of the code in [initial_status] could
   move into the refuter. More generally, is the duplication between
   the prover and the refuter reasonable? *)

(* TEMPORARY the refuter is really a prover with a particular strategy
   (namely, priority to left expansion, within a certain budget) and
   with quasi-immediate evaluation of goals. If those features were
   moved into the main prover, there would perhaps be no need for a
   separate refuter. How about keeping track of each goal's budget in
   the main prover? And how about eager evaluation of statuses? *)

(* TEMPORARY note, however, that the refuter is more geared towards
   refuting, in the sense that it explores fewer subgoals for each
   goal (a sufficient number of subgoals for refuting, although
   perhaps not sufficient for proving). Merging the prover and refuter
   would require distinguishing sufficient and necessary subgoals. *)

(* TEMPORARY the refuter eats up gobs of memory as soon as depth=2,
   due to its excessive memoization. Memoizing only during single runs
   seems to lead to unreasonable running times. So? *)

(* TEMPORARY is it wise to spend time attempting to refute lots of
   goals that eventually will all turn out to be true? *)

(* TEMPORARY how do we guarantee that at least one ground subgoal is
   tested for every goal that is submitted to the refuter? I mean,
   the parser should be invoked at least once, otherwise we are just
   wasting time. Measure the height of the initial word and ensure
   that the initial depth is at least that much? Use a notion of
   height that counts only the non-decreasing edges? *)

(* TEMPORARY isn't it inefficient to perform just one expansion at
   a time? Would it make sense to simultaneous expand all non-terminals
   on the left-hand side? *)

