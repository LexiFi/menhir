open Printf
open Print
open Hashcons
open Sigs

(* -------------------------------------------------------------------------- *)

(* Goals are hash-consed pairs of words. *)

module Goal = struct

  module Word = Front.Grammar.Word
  
  type bare_goal =
      Word.word * Word.word

  type goal =
      bare_goal hash_consed

  type t =
      goal

  module T = Hashcons.Make (struct

    type t =
	bare_goal

    let equal (lhs1, rhs1) (lhs2, rhs2) =
      Word.equal lhs1 lhs2 &&
      Word.equal rhs1 rhs2

    let hash (lhs, rhs) =
      hash2 (Word.hash lhs) (Word.hash rhs)

  end)

  let table =
    T.create 1023

  let hashcons w =
    T.hashcons table w

  let equal g1 g2 =
    g1 == g2 (* goals are hash-consed *)

  let hash g =
    g.hkey

  let compare g1 g2 =
    compare g1.tag g2.tag

  let print b g =
    let lhs, rhs = g.node in
    bprintf b "%a < %a" Word.print lhs Word.print rhs

  let eprint b g =
    let lhs, rhs = g.node in
    bprintf b "%a \\< %a" Word.print lhs Word.print rhs

  module Normal = struct

    type component =
	Word.word

    type pair =
	goal

    let make lhs rhs =
      hashcons (lhs, rhs)

    let left g =
      let (lhs, _) = g.node in
      lhs

    let right g =
      let (_, rhs) = g.node in
      rhs

  end

  module Reversed = struct

    include Normal

    let make lhs rhs =
      hashcons (rhs, lhs)

    let left =
      Normal.right

    let right =
      Normal.left

  end

  let exchange g =
    let lhs, rhs = g.node in
    hashcons (rhs, lhs)

  let length g =
    let lhs, rhs = g.node in
    Word.length lhs + Word.length rhs

  module Self = struct
    type t = goal
    let equal = equal
    let compare = compare
    let hash = hash
  end

  module Set : Set.S with type elt = goal = (* TEMPORARY try representation based on Patricia maps? *)
    Set.Make (Self)

  module Map : Map.S with type key = goal =
    Map.Make (Self)

  module Table : Hashtbl.S with type key = goal =
    Hashtbl.Make (Self)

end

(* -------------------------------------------------------------------------- *)

(* Dictionaries. TEMPORARY more efficient implementation? *)

module Dictionary =
  ListDictionary.Make (Goal)

(* -------------------------------------------------------------------------- *)

(* Priority queues are implemented as binomial queues. *)

module Q = BinomialQueue.Make (struct

  type t =
      int * Goal.goal

  let compare (pri1, _) (pri2, _) =
    pri1 - pri2

end)

module PriorityQueue = struct

  type element =
      Goal.goal

  type t =
      Q.t

  let empty =
    Q.empty

  let insert pri x q =
    if pri < 0 then
      q
    else
      Q.insert (pri, x) q

  let extract q =
    try
      let (_, x), q = Q.extract q in
      Some (x, q)
    with Not_found ->
      None

end

(* -------------------------------------------------------------------------- *)

(* Proof nets are implemented in the most straightforward way. *)

module Proof = struct

  (* Repetition of definitions found in [Sigs]. *)

  module Goal = Goal
  open Goal

  module Grammar = Front.Grammar

  type structure =
    | Absent
    | Disjunction of step list

  and step = {

      (* Each step carries a unique stamp, that is, two physically
	 distinct steps carry distinct stamps. (Two steps that are
	 logically equal can carry distinct stamps.) *)

      stamp: int;

      (* A human-readable label explains the step. *)

      reason: string;

      (* A conjunction of subgoals. *)

      subgoals: (edge * goal) list;

    }

  and edge =
    | EWordDecreasing       (* word decreasing *)
    | EDerivationDecreasing (* word preserving and derivation decreasing *)
    | ENeutral              (* word preserving and derivation preserving *)
    | EDangerous            (* word preserving and derivation increasing *)

  type status = (* equality type; order type *)
    Bool3.bool3

  module Dictionary =
    Dictionary

  (* A proof net consists mainly of a mapping of goals to [info] records. The
     goals in a proof net are sequentially numbered; the [index] field keeps
     track of these numbers. The [structure] field keeps track of how each
     goal can be solved. The [status] field keeps track of each goal's truth
     value, as presently known. *)

  type info = {
      index: int;
      mutable structure: structure;
      mutable status: status;
    }

  (* In addition to the above-mentioned mapping, held in the field [goals], a
     proof net contains the following information. The [n] field holds the
     total number of goals. The [initial] field holds the set of initial
     goals, that is, the goals that the user would like us to prove. The
     [queue] field keeps track of all open (unresolved) goals, together with
     their priority. The fields [ndict] and [rdict] contain the normal and
     reversed dictionaries. *)

  (* TEMPORARY since goals are hash-consed, one could avoid using a hash table
     over goals and directly store information inside goals. *)

  type net = {
      mutable n: int;
      goals: info Goal.Table.t;
      mutable initial: Goal.Set.t;
      mutable queue: PriorityQueue.t;
      mutable ndict: Dictionary.dictionary;
      mutable rdict: Dictionary.dictionary;
    }

  (* There is only one, mutable, proof net. *)

  let net = {
    n = 0;
    goals = Goal.Table.create 100003;
    initial = Goal.Set.empty;
    queue = PriorityQueue.empty;
    ndict = Dictionary.empty;
    rdict = Dictionary.empty;
  }

  (* Most of the operations published as part of signature [PROOF] are
     simple getters and setters for the above data structure. There is
     no intelligence here. *)

  let known goal =
    Goal.Table.mem net.goals goal

  let lookup goal : info =
    try
      Goal.Table.find net.goals goal
    with Not_found ->
      assert false

  let n () =
    net.n

  let index goal =
    (lookup goal).index

  let structure goal =
    (lookup goal).structure

  let set_structure goal structure =
    (lookup goal).structure <- structure

  let status goal =
    (lookup goal).status

  let set_status goal status =
    (lookup goal). status <- status;
    if true then (* toggle *)
      w (fun b ->
	if Goal.Set.mem goal net.initial then
	  match status with
	  | Bool3.BUndetermined ->
	      ()
	  | Bool3.BFalse ->
	      bprintf b "Goal %a was refuted.\n" Goal.print goal
	  | Bool3.BTrue ->
	      bprintf b "Goal %a was proved.\n" Goal.print goal
      )

  let add goal is_initial status structure pri insert =
    assert (not (known goal));
    let n = net.n in
    let info = {
      index = n;
      structure = structure;
      status = status;
    } in
    net.n <- n + 1;
    Goal.Table.add net.goals goal info;
    if is_initial then
      net.initial <- Goal.Set.add goal net.initial;
    net.queue <- PriorityQueue.insert pri goal net.queue;
    if insert then begin
      net.ndict <- Dictionary.add goal net.ndict;
      net.rdict <- Dictionary.add (Goal.exchange goal) net.rdict
    end

  let iter f =
    Goal.Table.iter (fun goal info ->
      f goal
    ) net.goals

  let fold f accu =
    Goal.Table.fold (fun goal info accu ->
      f goal accu
    ) net.goals accu

  let iter_initial f =
    Goal.Set.iter f net.initial

  let fold_initial f accu =
    Goal.Set.fold f net.initial accu

  let successors f goal =
    match structure goal with
    | Absent ->
	()
    | Disjunction steps ->
	List.iter (fun step ->
	  List.iter (fun (edge, subgoal) ->
	    f edge subgoal
	  ) step.subgoals
	) steps

  let filter_successors f goal =
    match structure goal with
    | Absent ->
	()
    | Disjunction steps ->
	let rec filter steps =
	  match steps with
	  | [] ->
	      []
	  | head :: tail ->
	      let tail' = filter tail in
	      let keep =
		match head with
		| step ->
		    List.fold_left (fun keep (edge, subgoal) ->
		      keep && f edge subgoal
         	    ) true step.subgoals
	      in
	      if keep then
		if tail == tail' then steps else head :: tail'
	      else
		tail'
	in
	let steps' = filter steps in
	if steps != steps' then
	  set_structure goal (Disjunction steps')

  let normal_dictionary () =
    net.ndict

  let reversed_dictionary () =
    net.rdict

  module PriorityQueue =
    PriorityQueue

  let extract () =
    match PriorityQueue.extract net.queue with
    | None ->
	None
    | Some (goal, queue) ->
	net.queue <- queue;
	Some goal

  let set_priority_queue queue =
    net.queue <- queue

  let print_goal_index b goal =
    bprintf b "[%d]" (index goal)

end
