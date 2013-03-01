(* This module evaluates the status of goals in a proof net. *)

(* TEMPORARY could think about an intermediate mode where ill-founded
   cycles are allowed but [EDerivationDecreasing], [ENeutral] and
   [EDangerous] are collapsed, which means that expressive power is
   decreased but only two nested fixed points are required. *)

open Printf
open Print
open Hashcons
open Bool3
open Sigs

module Run (Proof : PROOF) = struct

  open Proof

  (* ------------------------------------------------------------------------ *)

  (* We wish to map each goal (and, for this purpose, each step) to a truth
     value. So, for the purposes of the computations that follow, a ``point''
     is either a goal (a disjunction) or a step (a conjunction). *)

  module Point = struct

    type t =
      | PGoal of Goal.goal
      | PStep of step

    let compare p1 p2 =
      match p1, p2 with
      | PGoal goal1, PGoal goal2 ->
	  Goal.compare goal1 goal2
      | PStep step1, PStep step2 ->
	  step1.stamp - step2.stamp
      | _, _ ->
	  compare p1 p2

    let print b = function
      | PGoal goal ->
	  Goal.print b goal
      | PStep step ->
	  seplist wedge (fun b (_, subgoal) -> Goal.print b subgoal) b step.subgoals

    let iter f =
      iter (fun goal ->
	f (PGoal goal);
	match structure goal with
	| Absent ->
	    ()
	| Disjunction steps ->
	    List.iter (fun step -> f (PStep step)) steps
      )

    let base =
      0

  end

  module PointMap =
    Map.Make (Point)

  module LFP =
    FixOnDemand.Make (PointMap) (FalseBottom)

  module GFP =
    FixOnDemand.Make (PointMap) (TrueBottom)

  type point =
      Point.t

  type valuation =
      point -> bool3

  open Point

  (* ------------------------------------------------------------------------ *)

  (* A generic transfer function. The class is parameterized by the
     fixed point's [get] function. *)

  class virtual gtransfer get = object (self)

    (* The truth value at a goal is fixed if that goal already has
       determined status, or if it has undetermined status and no
       structure.

       Otherwise, the truth value at a goal (which represents a
       disjunction) is evaluated directly in terms of the truth value
       at each disjunct. The truth value at a disjunct is obtained
       through a call to the current fixed point's [get] function. *)

    method transfer x =
      match x with
      | PStep _ ->
	  self#step x
      | PGoal goal ->
	  let status = status goal in
	  match status with
	  | BTrue
	  | BFalse ->
	      status
	  | BUndetermined ->
	      match structure goal with
	      | Absent ->
		  BUndetermined
	      | Disjunction steps ->
		  disjunction_map
		    (fun step -> get (PStep step))
		    steps

    (* The truth value at a step (which represents a conjunction) is
       obtained in one of several ways: (i) directly in terms of the
       truth value at each conjunct, (ii) via a nested fixed point
       computation, (iii) thanks to approximations. This choice is
       deferred. *)

    method virtual step: valuation

  end

  (* ------------------------------------------------------------------------ *)

  (* A mix-in specialization, where the truth value at a conjunction
     is computed directly in terms of the truth value at each
     conjunct. This is appropriate for single-level fixed point
     computations, or for the innermost level of multiple-level fixed
     point computations.*)

  class virtual step = object (self)

    method step x =
      match x with
      | PStep step ->
	  conjunction_map
	    (fun (edge, subgoal) -> self#select x edge (PGoal subgoal))
	    step.subgoals
      | PGoal _ ->
	  assert false

    (* How the truth value at each conjunct is computed can depend
       on what kind of edge was followed. This method is in charge
       of making this decision. *)

    method virtual select: point -> edge -> valuation

  end

  (* ------------------------------------------------------------------------ *)

  (* A specialized transfer function that combines the above two
     classes and ignores edge labels. This is appropriate for
     single-level fixed point computations. *)

  class sstransfer get = object (self)

    inherit gtransfer get
    inherit step

    method select source edge target =
      get target

  end

  (* ------------------------------------------------------------------------ *)

  (* Assuming that the proof net contains no ill-founded cycles, the
     truth value at each point is obtained via a straightforward
     greatest fixed point computation.

     Assuming that the proof net can contain ill-founded cycles, this
     computation provides an over-approximation -- so a node that is
     false according to [G] is definitely false.

     Assuming that the proof net can contain ill-founded cycles, a
     least fixed point computation provides an under-approximation --
     so a node that is true according to [L] is definitely true. *)

  module T = struct

    let transfer (get : valuation) : valuation =
      (new sstransfer get)#transfer

  end

  module G = GFP (T)

  module L = LFP (T)

  (* ------------------------------------------------------------------------ *)

  (* We can take advantage of the above under- and over-approximations
     as follows. If the two approximations coincide, then the exact
     value is already known.

     This refinement is made by overriding the method [step], as
     opposed to [transfer], because the code for pruning proofs
     depends on the fact that the truth value at a disjunction is
     evaluated in terms of the truth value at each disjunct. *)

  let refine f x =
    let under = L.get x
    and over = G.get x in
    assert (FalseBottom.leq under over);
    if under = over then
      under
    else
      f x

  (* ------------------------------------------------------------------------ *)

  let evaluate =
    if Settings.cyclic then

  (* ------------------------------------------------------------------------ *)

  (* Find out which edges participate in ill-founded cycles. To do so,
     compute the strongly connected components of the proof net,
     deprived of its word-decreasing edges. An edge can participate in
     an ill-founded cycle only if it relates two distinct strongly
     connected components. *)

  let module SCC = Tarjan.Run (struct
    type node = point
    include Number.Make (PointMap) (Point)
    let successors f = function
      | PStep step ->
	  List.iter (fun (edge, subgoal) ->
	    match edge with
	    | EWordDecreasing ->
		() (* edge intentionally ignored *)
	    | EDerivationDecreasing
	    | ENeutral
	    | EDangerous ->
		f (PGoal subgoal)
          ) step.subgoals
      | PGoal goal ->
	  match structure goal with
	  | Absent ->
	      ()
	  | Disjunction steps ->
	      List.iter (fun step -> f (PStep step)) steps
    let iter = iter
  end) in

  let safe source target =
    not (SCC.equivalent source target)
  in

  (* TEMPORARY measure how much time is spent evaluating proof nets
               and if that percentage is low enough, then move on and
               focus on search heuristics *)

  (* ------------------------------------------------------------------------ *)

  (* Assuming that the proof net can contain ill-founded cycles, the
     truth value at each point is obtained via a number of nested
     least and greatest fixed point computations. *)

  (* Outermost comes a greatest fixed point computation *) (* TEMPORARY explain *)

 let module GLGL = GFP (struct
    let transfer get1 x =
      let module LGL = LFP (struct
	let transfer get2 x =
	  let module GL = GFP (struct
	    let transfer get3 x =
	      let module L = LFP (struct
		let transfer get4 =
		  (object
		    inherit gtransfer get4
		    inherit step as super
		    method step = refine super#step
		    method select source edge target =
		      begin match edge with
		      | EWordDecreasing ->
			  get1
		      | EDangerous ->
			  if safe source target then get1 else get2
		      | EDerivationDecreasing ->
			  if safe source target then get1 else get3
		      | ENeutral ->
			  if safe source target then get1 else get4
		      end target
		  end)#transfer
	      end) in
	      (object
		inherit gtransfer get3
		method step = refine L.get
	      end)#transfer x
	  end) in
	  (object
	    inherit gtransfer get2
	    method step = refine GL.get 
	  end)#transfer x
      end) in
      (object
	inherit gtransfer get1
	method step = refine LGL.get
      end)#transfer x
  end) in

  (* ------------------------------------------------------------------------ *)

  (* Putting all of the above together. *)

      GLGL.get
    else
      G.get

  (* ------------------------------------------------------------------------ *)

  (* A number of simplification rules can be used to remove edges from a proof
     net without affecting soundness or completeness.

     After one branch of a disjunction evaluates to false, this branch
     can be dropped. This can make some of its conjuncts inaccessible.

     After one branch of a disjunction evaluates to true, the entire
     disjunction can be reduced to only the branches that have been
     evaluated to true. (There can be several such branches, but we do
     not know how to choose between them while preserving the validity
     of the proof net, so we keep all of them.) (Unevaluated branches
     can be discarded. This is true, even when we are computing
     multiple, nested fixed points, because the value at a disjunction
     at level [k] is computed directly in terms of the value at each
     disjunct at level [k].) *)

  (* TEMPORARY since I evaluate all nodes, pruning unevaluated branches
     is no longer possible, and that hurts -- we end up with many true
     branches *)

  let keep_true_branches steps =
    let branches =
      ListMisc.filter (fun step ->
	match evaluate (PStep step) with
	| BTrue ->
	    true
	| BUndetermined
	| BFalse ->
	    false
      ) steps
    in
    assert (branches <> []);
    branches

  let discard_false_branches steps =
    let branches =
      ListMisc.filter (fun step ->
	match evaluate (PStep step) with
	| BFalse ->
	    false
	| BUndetermined
	| BTrue ->
	    true
      ) steps
    in
    assert (branches <> []);
    branches

  let map f structure =
    match structure with
    | Absent ->
	Absent
    | Disjunction steps ->
	let steps' = f steps in
	if steps' == steps then
	  structure
	else
	  Disjunction steps'

  let update f goal =
    let old_structure = structure goal in
    let new_structure = map f old_structure in
    if old_structure != new_structure then
      set_structure goal new_structure

  (* ------------------------------------------------------------------------ *)

  (* Evaluate all (currently undetermined) goals and update their status. *)

  let () =
    Proof.iter (fun goal ->
      match status goal with
      | BTrue
      | BFalse ->
	  ()
      | BUndetermined ->
	  match evaluate (PGoal goal) with
	  | BUndetermined ->
	      update discard_false_branches goal
	  | BTrue ->
	      set_status goal BTrue;
	      update keep_true_branches goal
	  | BFalse ->
	      set_status goal BFalse;
	      set_structure goal (Disjunction [])
    )

end

