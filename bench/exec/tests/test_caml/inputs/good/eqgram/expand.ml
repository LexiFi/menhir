open Sigs
open Bool3

module Make (Proof : PROOF) = struct

  open Proof
  open Grammar
  open Goal
  open Word
  open Symbol

  module type INDEX =
      INDEX with type symbol = symbol and type word = word

  module type PAIR =
      PAIR with type component = word and type pair = goal

  module Refuter =
    Refuter.Make (Proof)

  let refute, display_refutation_timer =
    Monitor.time "Refutation" Refuter.explore

  let stats () =
    display_refutation_timer();
    Refuter.stats()

  (* ------------------------------------------------------------------------ *)

  (* Allocation of unique stamps to steps. *)

  let stamp : unit -> int =
    let next = ref 0 in
    fun () ->
      let n = !next in
      next := n + 1;
      n

  (* ------------------------------------------------------------------------ *)

  (* Attack -- eager simplification of shared terminal or opaque symbols. *)

  module Attack (Index : INDEX) = struct

    (* [attack] removes a shared prefix of terminal or opaque non-terminal
       symbols on both sides of the goal. The accumulating parameter
       [successful] tells whether the removed prefix is non-empty. The
       accumulating parameter [nullable] tells whether it is nullable. *)

    let rec attack successful nullable i j lhs rhs =
      try
	let i' = Index.next lhs i
	and j' = Index.next rhs j in

	(* If the above calls succeed, then we have at least one letter
	   in each word, which we now retrieve. *)
	  
	let s = Index.get lhs i
	and t = Index.get rhs j in

	if Symbol.equal s t then

	  match Grammar.classify s with
	  | T
	  | NO _ ->

	      (* The leading symbol on the two sides is identical
		 and is terminal or opaque. Continue. *)

	      attack true (nullable && Grammar.nullable_symbol s) i' j' lhs rhs

	  | NT _ ->
	      raise Index.AtBoundary (* stop *)

	else
	  raise Index.AtBoundary (* stop *)

      with Index.AtBoundary ->
	successful, nullable, i, j (* stop *)

    let attack successful nullable lhs rhs =
      let successful, nullable, i, j =
	attack successful nullable (Index.start lhs) (Index.start rhs) lhs rhs
      in
      successful, nullable, Index.suffix lhs i, Index.suffix rhs j

  end

  (* ------------------------------------------------------------------------ *)

  (* Determining the edge status of an attack or simplification step,
     and constructing its subgoal. *)

  let simplification_edge reason nullable lhs rhs =

    (* If the symbols that are being discarded are non-nullable, then
       this is a word-decreasing step. If they are nullable, then this
       is a word-preserving (and derivation-decreasing, because at
       least one non-terminal symbol is being discarded) step. *)

    let edge =
      if nullable then EDerivationDecreasing else EWordDecreasing
    in

    {
      stamp = stamp();
      reason = reason;
      subgoals = [ edge, Normal.make lhs rhs ];
    }

  (* ------------------------------------------------------------------------ *)

  (* Determining the edge status of a step that discards a piece of the left
     context. *)

  (* If the context that was discarded is empty, then the edge is
     word-preserving and derivation-preserving.

     If the context is non-nullable, then the word that is under
     examination in the subgoal is a strict sub-word of the word that
     was under examination in the original goal, which means that the
     edge is word-decreasing.

     If neither of these conditions holds, then the context contains
     at least one non-terminal symbol, so the derivation under
     examination in the subgoal is strictly smaller than the one under
     examination in the original goal. The edge is
     derivation-decreasing. *)

  let discard_left_context context =
    if Word.is_epsilon context then
      ENeutral
    else if Grammar.nullable context then
      EDerivationDecreasing
    else
      EWordDecreasing

  let discard_left_context2 context1 context2 =
    if Word.is_epsilon context1 && Word.is_epsilon context2 then
      ENeutral
    else if Grammar.nullable context1 && Grammar.nullable context2 then
      EDerivationDecreasing
    else
      EWordDecreasing

  (* ------------------------------------------------------------------------ *)

  (* Attack from both ends. *)

  let attack goal : step option =

    let lhs = Normal.left goal
    and rhs = Normal.right goal in

    let module L = Attack (Left) in
    let module R = Attack (Right) in

    let successful, nullable, lhs, rhs = L.attack false true lhs rhs in
    let successful, nullable, lhs, rhs = R.attack successful nullable lhs rhs in
    
    if successful then
      Some (simplification_edge "attack" nullable lhs rhs)
    else
      None

  (* ------------------------------------------------------------------------ *)

  (* A few simple checks for immediately determining whether a newly created
     goal is true or false. *)

  let initial_status goal =

    let lhs = Normal.left goal
    and rhs = Normal.right goal in

    (* If the goal is an identity, then it definitely holds. *)

    if Word.equal lhs rhs then
      BTrue
    else

    (* TEMPORARY could also add a particular case for decidable
       situations, e.g. when no recursion is involved and enumeration
       suffices, or when both sides describe regular languages without
       opaque symbols *)

    (* TEMPORARY also keep in mind that making some symbols opaque can
       make a language appear regular, leading to a decidable (sufficient)
       check *)

    (* If the left-hand side is epsilon, then the goal holds iff the right-hand
       side is nullable. *)

    if Word.is_epsilon lhs then
      if Grammar.nullable rhs then BTrue else BFalse
    else

    (* If the right-hand side is epsilon, then the goal does not hold, because the
       left-hand side is not epsilon (as per the above test) and we have ruled out
       non-terminal symbols that generate a subset of the singleton epsilon set. *)

    if Word.is_epsilon rhs then
      BFalse
    else

    (* If the right-hand side is a transparent non-terminal symbol,
       check whether, by any chance, the left-hand side is one of its
       immediate expansions -- that is, whether the goal is in fact a
       production. *)

    (* TEMPORARY another (more efficient?) way of achieving the same
       effect would be to simply add all grammar productions as true
       goals initially *)

    if match Word.is_singleton rhs with
    | None ->
	false
    | Some symbol ->
	match Grammar.classify symbol with
	| NT productions ->
	    List.exists (fun (_, rhs) -> Word.equal lhs rhs) productions
	| NO _
	| T ->
	    false
    then BTrue
    else

    (* If abstract inclusion (that is, inclusion of approximations of
       the generated languages) does not hold, then the goal
       definitely does not hold. Otherwise, it is undetermined, for
       now. *)

    if not (Grammar.abstract_inclusion lhs rhs)
    then BFalse
    else

    (* If all these checks fail, invoke the refuter, which can refute
       or prove goals via enumeration of left expansions and parsing. *)

    refute goal

  (* ------------------------------------------------------------------------ *)

  (* Assigning a priority to a goal, based on the goal alone, without
     any knowledge of the global structure of the proof net.

     Our heuristic measures the size of the goal, and assigns greater
     weight to transparent non-terminal symbols, because, via
     expansion, they can lead to ever more complex subgoals. *)

  (* TEMPORARY greater weight to non-terminal is dangerous, as it
     can lead to exploring goals that contain growing numbers of
     terminals *)

  let weight w =
    Word.fold (fun s accu ->
      accu + match classify s with
      | NT _ ->
	  2 (* TEMPORARY changing this to 1 or 3 can turn a 20 second run into a diverging run! Cf. [paren-nullable] *)
      | NO _
      | T ->
	  1
    ) w 0

  let weight goal =
    let lhs = Normal.left goal
    and rhs = Normal.right goal in
    weight lhs + weight rhs (* TEMPORARY could conceivably use multiplication *)

  (* ------------------------------------------------------------------------ *)

  (* When the above checks find that a goal is proved or refuted, we let its
     structure consist of an empty conjunction or disjunction, and we set its
     priority to -1, which means that no further examination of this goal is
     required.

     If we find that a goal is susceptible to attack (eager
     simplification), then we assign it maximum priority, so that
     attack is performed immediately.

     In the general case, a goal's priority is set to its weight, as
     defined above. *)

  let obvious : step = {
    stamp = stamp();
    reason = "obvious";
    subgoals = [];
  }

  let proved : structure =
    Disjunction [ obvious ]

  let initial_structure_and_priority goal status =
    match status with
    | BTrue ->
	proved,
	-1
    | BFalse ->
	Disjunction [],
	-1
    | BUndetermined ->
	Absent,
	match attack goal with
	| Some step ->
	    0 (* maximum priority *)
	| None ->
	    weight goal (* TEMPORARY not consistent with [Driver.examine], which also takes depth into account *)

  (* ------------------------------------------------------------------------ *)

  (* When a new goal is discovered, we must decide whether it should be added
     to the dictionary. If we do add it, more inductive proofs become possible,
     but if we do not add it, speed is preserved. *)

  let should_add_to_dictionary is_initial goal =
    is_initial (* TEMPORARY *)

  (* ------------------------------------------------------------------------ *)

  (* The above functions combine into a few simple steps that are performed
     when a new goal is added to a proof net. *)

  exception Hopeless

  let add_new_goal is_initial (goal, status) =

    (* Determine the goal's initial structure and priority. Decide
       whether it should be inserted into the dictionary. *)

    let structure, priority = initial_structure_and_priority goal status
    and insert = should_add_to_dictionary is_initial goal in

    if false then (* toggle *)
      Print.w (fun b ->
	Printf.bprintf b "Adding new %sgoal %a (%a)%s%s\n"
	  (if is_initial then "initial " else "")
	  Goal.print goal
	  Bool3.print status
	  (if priority = (-1) then "" else Printf.sprintf " (priority %d)" priority)
	  (if insert then " (inserted into dictionary)" else "")
     );

    add goal is_initial status structure priority insert

  (* ------------------------------------------------------------------------ *)

  (* Simplification of a single (transparent, non-terminal) symbol. *)

  module Simplify (Index : INDEX) (R : sig val direction: string end) = struct

    (* It is assumed that neither side of the goal is the empty word. *)

    let reason =
      R.direction ^ " simplification"

    let simplify goal steps : step list =

      let lhs = Normal.left goal
      and rhs = Normal.right goal in

      let i = Index.start lhs
      and j = Index.start rhs in

      let i', j' =
	try
	  Index.next lhs i, Index.next rhs j
	with Index.AtBoundary ->
	  assert false (* neither side is the empty word *)
      in

      let s = Index.get lhs i
      and t = Index.get rhs j in

      if Symbol.equal s t then
	simplification_edge reason (Grammar.nullable_symbol s) (Index.suffix lhs i') (Index.suffix rhs j')
	:: steps
      else
	steps

  end

  (* ------------------------------------------------------------------------ *)

  (* Simplification of an initial symbol. Simplification of a final symbol. *)

  let initial_simplify =
    let module S = Simplify (Left) (struct let direction = "left" end) in
    S.simplify

  let final_simplify =
    let module S = Simplify (Right) (struct let direction = "right" end) in
    S.simplify

  (* ------------------------------------------------------------------------ *)

  (* Expansion of a transparent non-terminal symbol on the left-hand or
     right-hand side (shared code). *)

  module Expand (Pair : PAIR) (C : sig val construct: goal list -> step list -> step list end) = struct

    let expand goal steps : step list =

      (* Focus on one side. *)

      let focus = Pair.left goal
      and other = Pair.right goal in

      (* Try expanding every symbol on this side. *)

      let rec loop i steps : step list =
	try
	  let i' = Left.next focus i in 

	  let steps =
	    let symbol = Left.get focus i in

	    (* Examine the symbol found between indices [i] and [i']. *)

	    match Grammar.classify symbol with
	    | T
	    | NO _ ->
		steps (* symbol cannot be expanded *)
	    | NT productions ->
		let prefix = Left.prefix focus i
		and suffix = Left.suffix focus i' in
                let subgoals =
		  List.map (fun (_, production) ->
		    Pair.make (Word.concat3 prefix production suffix) other
		  ) productions
		in
		C.construct subgoals steps

	  in
	  loop i' steps

	with Left.AtBoundary ->
	  steps
      in

      loop (Left.start focus) steps

   end

  (* ------------------------------------------------------------------------ *)

  (* Expansion of a transparent non-terminal symbol on the left-hand side. *)

  let left_expand =

    (* Focus is on the left-hand side. *)

    let module E = Expand (Normal) (struct

      (* The subgoals form a conjunction. This step is word-preserving (and
	 derivation-decreasing). *)

      let construct subgoals steps =
	let subgoals = List.map (fun subgoal ->
	  EDerivationDecreasing,
	  subgoal
	) subgoals in
	{
	  stamp = stamp();
	  reason = "left expansion";
	  subgoals = subgoals;
        }
	:: steps

   end) in
   
   E.expand

  (* ------------------------------------------------------------------------ *)

  (* Expansion of a transparent non-terminal symbol on the right-hand side.

     This is very much like expansion on the left-hand side, except we are
     transforming the desired, not the given -- so the existence of multiple
     productions for a single non-terminal symbol gives rise to a disjunction,
     instead of a conjunction. Also, a right-expansion step is not derivation
     decreasing -- it does not affect the derivation of the left-hand side. *)

  let right_expand =

    (* Focus is on the right-hand side. *)

    let module E = Expand (Reversed) (struct

      (* The subgoals form a disjunction. This step is word-preserving (and
	 derivation-preserving). *)

      let construct subgoals steps =
	List.fold_right (fun subgoal steps ->
	  {
	    stamp = stamp();
	    reason = "right expansion";
	    subgoals = [ ENeutral, subgoal ]
	  } :: steps
	) subgoals steps

   end) in
   
   E.expand

   (* TEMPORARY expansion on the right-hand side is a special case of cut
      on the right-hand side, if productions are part of the dictionary *)

   (* TEMPORARY left-expansion could also share some code with cut? *)

  (* ------------------------------------------------------------------------ *)

  (* Cut on the left-hand side. *)

  let left_cut goal steps : step list =

    let lhs = Normal.left goal
    and rhs = Normal.right goal in

    (* Rewrite the left-hand side using the dictionary. Each possible
       rewrite gives rise to a possible proof step, which consists of
       a conjunction of two subgoals: the lemma (that is, the rewrite
       rule) that was used and a new goal formed of the transformed
       left-hand side and the unmodified right-hand side. *)

    List.fold_right (fun (rule, context1, reduct, context2) steps ->

      (* [rule] is the rewrite rule that was exploited, and becomes
	 the first subgoal. [reduct] is the right-hand side of the
	 rewrite rule. [context1] and [context2] are the parts of the
	 goal's left-hand side that were not subject to rewriting. *)

      let lemma =
	rule
      in

      (* Build the new left-hand side of the second subgoal. *)

      let new_lhs =
	Word.concat3 context1 reduct context2
      in

      (* The [lemma] subgoal is being used in a certain left context. 

	 The second subgoal is always used in a dangerous way, that
	 is, the word under examination in its left-hand side is
	 unchanged, and the derivation can be of arbitrary size. *)

      {
        stamp = stamp();
        reason = "left cut";
        subgoals = [
	  discard_left_context2 context1 context2, lemma;
	  EDangerous, Normal.make new_lhs rhs
        ]
      }
      :: steps

    ) (Dictionary.lookup (normal_dictionary()) lhs) steps

  (* ------------------------------------------------------------------------ *)

  (* Cut on the right-hand side. (No attempt at sharing code, because it would
     be too confusing.) *)

  let right_cut goal steps : step list =

    let lhs = Normal.left goal
    and rhs = Normal.right goal in

    (* Rewrite the right-hand side using the reversed dictionary. *)

    List.fold_right (fun (rule, context1, reduct, context2) steps ->

      (* [rule] is the rewrite rule that was exploited, so it is
	 in fact the reverse of the second subgoal. *)

      let lemma =
	Goal.exchange rule
      in

      (* Build the new right-hand side of the first subgoal. *)

      let new_rhs =
	Word.concat3 context1 reduct context2
      in

      (* Determine how the [lemma] subgoal is being used.

	 If the lemma was applied in a non-nullable context, then the
	 word that is under examination in the subgoal is a strict
	 sub-word of the word that was under examination in the
	 original goal, which means that the edge is word-decreasing.

	 Otherwise, there is no telling. *)

      let edge =
	if Grammar.nullable context1 && Grammar.nullable context2 then
	  EDangerous
	else
	  EWordDecreasing
      in

      (* The first subgoal is always used in a word-preserving (and
	 derivation-preserving) way, because the word under examination in it
	 is exactly the word that was examination in the original goal. *)

      {
        stamp = stamp();
        reason = "right cut";
        subgoals = [
	  ENeutral, Normal.make lhs new_rhs;
	  edge, lemma
        ]
      }
      :: steps

    ) (Dictionary.lookup (reversed_dictionary()) rhs) steps

  (* TEMPORARY several cuts at once can be required. Their "lemma" edges will
     correctly be seen as derivation-decreasing, whereas if several single cuts
     are chained, extra "dangerous" edges will appear in between. Cf. [paren].
     The "split" method below can help compensate for this problem. *)

  (* ------------------------------------------------------------------------ *)

  (* Split on both sides. Interesting, but the number of steps thus produced
     is quadratic in the length of the sides. *)

  let split goal steps : step list =

    let lhs = Normal.left goal
    and rhs = Normal.right goal in

    Left.fold (fun i steps ->
      let lprefix = Left.prefix lhs i
      and lsuffix = Left.suffix lhs i in
      let edge1 = discard_left_context lsuffix
      and edge2 = discard_left_context lprefix in
      Left.fold (fun j steps ->
	let rprefix = Left.prefix rhs j
	and rsuffix = Left.suffix rhs j in
	{
	  stamp = stamp();
	  reason = "split";
	  subgoals = [
	    edge1, Normal.make lprefix rprefix;
	    edge2, Normal.make lsuffix rsuffix
	  ]
        }
	:: steps
      ) rhs steps
    ) lhs steps

  (* ------------------------------------------------------------------------ *)

  (* Completion of an open goal -- production of a disjunction of steps. *)

  let complete goal : step list =

    (* Try an eager attack. If it succeeds in producing a simpler subgoal,
       make it the only subgoal. *)

    match attack goal with
    | Some step ->
	[ step ]
    | None ->

	(* Otherwise, apply all known strategies for producing
	   sufficient subgoals. Or just apply a subset of these
	   strategies. Some fine-tuning could be performed here. *)

        (* TEMPORARY think *)

	initial_simplify goal (
	final_simplify goal (
	left_expand goal (
	right_expand goal (
	left_cut goal (
	right_cut goal (
	split goal (
	[]
        )
        )
        )
        )
        )
        )
        )

  (* ------------------------------------------------------------------------ *)

  (* Detection of neutral or dangerous self-loops. These are always useless
     and can be eagerly suppressed. *)

  module Wf =
    Wf.Make (Proof)

  let self_loop_check goal edge subgoal =
    match edge with
    | EWordDecreasing
    | EDerivationDecreasing ->
	()
    | ENeutral
    | EDangerous ->
	if Goal.equal goal subgoal then
	  raise Wf.Rejected

  (* ------------------------------------------------------------------------ *)

  (* Completion of an open goal -- installation of the steps into the proof
     net. *)

  let complete goal =

    if false then (* toggle *)
      Print.w (fun b ->
	Printf.bprintf b "Expanding goal %a...\n"
	  Goal.print goal
     );

    (* Find which steps can in principle be performed. *)

    let steps = complete goal in

    (* Install each step, one after another. A step is a conjunction
       of subgoals. Install each subgoal, one after another. If some
       subgoal either (i) is immediately refuted or (ii) gives rise to
       a weak self-loop, then discard this step. *)

    let steps =
      List.fold_left (fun steps step ->
	try

	  (* First, determine whether this step should be discarded and, if
	     not, which new goals should be installed. *)

	  let new_goals : (goal * status) list =

	    List.fold_left (fun new_goals (edge, subgoal) ->

	      (* Evaluate the subgoal's status. If it was refuted already,
		 abort. This check is not necessary for soundness, but should
		 make an important difference in speed by preventing the
		 installation of many useless subgoals.*)

	      if known subgoal then

		let status = status subgoal in
		if status = BFalse then
		  raise Hopeless;

		(* Check against ill-founded self-loops. *)

		self_loop_check goal edge subgoal;

		(* If running in acyclic mode, check against the
		   creation of an ill-founded cycle. The cycle, if it
		   exists, begins with the edge from [goal] to
		   [subgoal], and continues with a path from [subgoal]
		   back to [goal].

		   Note that the well-foundedness check is performed
		   with respect to the current net, which does not yet
		   contain the new edges that we are about to
		   add. This could be problematic if one ill-founded
		   cycle exploits several of these edges. But since of
		   all these edges have the same source, that cycle
		   would not be simple. So, it seems that we are
		   safe.

		   If the present step is a left expansion step, we do
		   not want to discard it, because we want to preserve
		   completeness (the property that a goal that is true
		   in reality will lead either to a proof or to looping,
		   but never to apparent refutation). In that case, if
		   an ill-founded cycle, we break it not by discarding
		   the present step, but by breaking a dangerous edge
		   somewhere else (which must belong to a left or right
		   cut -- cuts are not needed for completeness). *)

		if not Settings.cyclic then
		  if step.reason = "left expansion" then (* TEMPORARY inelegant *)
		    Wf.break subgoal goal
		  else
		    Wf.check edge subgoal goal;

		new_goals

	      else

		let status = initial_status subgoal in
		if status = BFalse then
		  raise Hopeless;
		(subgoal, status) :: new_goals

            ) [] step.subgoals

	  in

	  (* Installation of the step can now proceed. The new subgoals are
	     added. Subgoals that were already known are unaffected, even
	     though new paths lead to them. Their priority will be
	     re-evaluated at some later point. *)

	  List.iter (add_new_goal false) new_goals;

	  (* Accept this step. *)

	  step :: steps

	with Hopeless | Wf.Rejected ->

	  (* Discard this step. *)

	  steps

      ) [] steps
    in

    (* Modify the structure of the goal. The goal's status is unaffected. It
       will be re-evaluated at some later time. *)

    set_structure goal (Disjunction steps)

  (* ------------------------------------------------------------------------ *)

  (* Creation of an initial proof net out of a set of initial goals. *)

  let create goals =
    List.iter (fun (lhs, rhs) ->
      let goal = Goal.Normal.make lhs rhs in
      let status = initial_status goal in
      add_new_goal true (goal, status)
    ) goals

end

(* TEMPORARY when a new goal is added, it could be immediately and
   eagerly expanded in a restricted mode when no new subgoals can be
   created.

   This would help discover more causality relationships between
   existing goals, instead of waiting until more expansion is done
   (not all of them, though) Discovering all of them would require
   pausing from time to time and re-expanding all non-fully-expanded
   nodes in restricted mode.

   When the node is later re-expanded in normal mode, the existing
   list of steps is replaced with a new, potentially larger
   one. Beware of the well-foundedness check, though, which might
   introduce non-monotonicity? *)

(* TEMPORARY a subgoal that has a proof that is open solely because it
   relies (through a word-decreasing edge) on one (or more)
   undetermined initial goal could be considered provisionally proved,
   that is, if the initial goal is eventually proved (as it should
   be), then our proof will turn out to be correct. So, there is no
   need to look for other proofs of that subgoal, even though the
   subgoal appears to be undetermined. *)

(* TEMPORARY if new goals are added to the dictionary as we go, then
   we might miss cut opportunities because some goal is not yet in
   the dictionary *)

(* TEMPORARY why does expansion take up 99% of the computation time?
   how much time is spent in the refuter? in the Earley parser? *)

