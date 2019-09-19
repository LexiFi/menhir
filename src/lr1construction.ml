(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This module constructs an LR(1) automaton for the grammar described by the
   module [Grammar]. *)

(* Depending on Menhir's settings, one of four methods is used:

   - Canonical mode: no merging of states at all.

   - Inclusion only: a new state can be merged into a larger existing state.
     The reverse is forbidden, though: a smaller existing state will not be
     grown if a new larger state must be created. This mode is undocumented
     and may be removed in the future.

   - Normal mode: a version of Pager's weak compatibility criterion is used to
     determine which states can be merged. Merging is performed on the fly,
     during construction. A proposed new state can be merged with an existing
     state, which is then grown.

   - LALR mode: two states are merged as soon as they have the same LR(0) core.

  *)

open Grammar

type lr1state =
  Lr0.lr1state

module Run () = struct

(* -------------------------------------------------------------------------- *)

(* Nodes. *)

type node = {

  (* An internal node number assigned during construction. This number
     appears in Menhir's output when [--follow-construction] is set.
     This number is also exposed to the client so as to allow building
     efficient maps over nodes. It otherwise has no use. *)

  number: int;

  (* Each node is associated with a state. This state can change during
     construction as nodes are merged. *)

  mutable state: lr1state;

  (* Each node carries information about its outgoing transitions towards
     other nodes. *)

  mutable transitions: node SymbolMap.t;

}

(* -------------------------------------------------------------------------- *)

(* Output debugging information if [--follow-construction] is enabled. *)

let follow_transition
  (again : bool) (source : node) (symbol : Symbol.t) (state : lr1state)
=
  if Settings.follow then
    Printf.fprintf stderr
      "%s transition out of state r%d along symbol %s.\n\
       Proposed target state:\n%s"
      (if again then "Re-examining" else "Examining")
      source.number
      (Symbol.print symbol)
      (Lr0.print_closure "" state)

let follow_state (msg : string) (node : node) (print : bool) =
  if Settings.follow then
    Printf.fprintf stderr
      "%s: r%d.\n%s\n"
      msg
      node.number
      (if print then Lr0.print_closure "" node.state else "")

(* -------------------------------------------------------------------------- *)

(* The following two mutually recursive functions are invoked when the state
   associated with an existing node grows. The node's descendants are examined
   and grown until a fixpoint is reached.

   This work is performed in an eager manner: we do not attempt to build any
   new transitions until all existing nodes have been suitably grown. Indeed,
   building new transitions requires making merging decisions, and such
   decisions cannot be made on a sound basis unless all existing nodes have
   been suitably grown. Otherwise, one could run into a dead end where two
   successive, incompatible merging decisions are made, because the
   consequences of the first decision (growing descendant nodes) were not made
   explicit before the second decision was taken. This was a bug in versions
   of Menhir ante 20070520.

   Although I wrote this code independently, I later found out that it seems
   quite similar to the code in Karl Schimpf's Ph.D. thesis (1981), page 35.

   It is necessary that all existing transitions be explicit before the [grow]
   functions are called. In other words, if it has been decided that there
   will be a transition from [node1] to [node2], then [node1.transitions] must
   be updated before [grow] is invoked. *)

(* [grow node state] grows the existing node [node], if necessary, so that its
   associated state subsumes [state]. If this represents an actual (strict)
   growth, then [node]'s descendants are grown as well. *)

let rec grow node state =
  if Lr0.subsume state node.state then
    follow_state "Target state is unaffected" node false
   else begin

     (* In versions of Menhir prior to June 2008, I wrote this:

          If I know what I am doing, then the new state that is being
          merged into the existing state should be compatible, in
          Pager's sense, with the existing node. In other words,
          compatibility should be preserved through transitions.

        and the code contained this assertion:

          assert (Lr0.compatible state node.state);
          assert (Lr0.eos_compatible state node.state);

        However, this was wrong. See, for instance, the sample grammars
        cocci.mly and boris-mini.mly. The problem is particularly clearly
        apparent in boris-mini.mly, where it only involves inclusion of
        states -- the definition of Pager's weak compatibility does not
        enter the picture. Here is, roughly, what is going on.

        Assume we have built some state A, which, along some symbol S,
        has a transition to itself. This means, in fact, that computing
        the successor of A along S yields a *subset* of A, that is,
        succ(A, S) <= A.

        Then, we wish to build a new state A', which turns out to be a
        superset of A, so we decide to grow A. (The fact that A is a
        subset of A' implies that A and A' are Pager-compatible.) As
        per the code below, we immediately update the state A in place,
        to become A'. Then, we inspect the transition along symbol S.
        We find that the state succ(A', S) must be merged into A'.

        In this situation, the assertions above require succ(A', S)
        to be compatible with A'. However, this is not necessarily
        the case. By monotonicity of succ, we do have succ(A, S) <=
        succ(A', S). But nothing says that succ(A', S) and A' are related
        with respect to inclusion, or even Pager-compatible. The
        grammar in boris-mini.mly shows that they are not.

     *)

    (* Grow [node]. *)

    node.state <- Lr0.union state node.state;
    follow_state "Growing existing state" node true;

    (* Grow [node]'s successors. *)

    grow_successors node

  end

(* [grow_successors node] grows [node]'s successors. *)

(* Note that, if there is a cycle in the graph, [grow_successors] can be
   invoked several times at a single node [node], with [node.state] taking on
   a new value every time. In such a case, this code should be correct,
   although probably not very efficient. *)

and grow_successors node =
  SymbolMap.iter (fun symbol (successor_node : node) ->
    let successor_state = Lr0.transition symbol node.state in
    follow_transition true node symbol successor_state;
    grow successor_node successor_state
  ) node.transitions

(* -------------------------------------------------------------------------- *)

(* Data structures maintained during the construction of the automaton. *)

(* A queue of pending nodes, whose outgoing transitions have not yet
   been built. *)

let queue : node Queue.t =
  Queue.create()

(* A mapping of LR(0) node numbers to lists of nodes. This allows us to
   efficiently find all existing nodes that are core-compatible with a
   newly found state. *)

let map : node list array =
  Array.make Lr0.n []

(* A counter that allows assigning raw numbers to nodes. *)

let num =
  ref 0

(* A (reversed) list of all nodes that we have allocated. At the end of the
   process, this list is turned into an array, and allows us to expose an
   efficient mapping of node numbers back to nodes. *)

let nodes =
  ref []

(* -------------------------------------------------------------------------- *)

(* [create state] creates a new node that stands for the state [state].
   It is expected that [state] does not subsume, and is not subsumed by,
   any existing state. *)

let create (state : lr1state) : node =

  (* Allocate a new node. *)

  let node = {
    state = state;
    transitions = SymbolMap.empty;
    number = Misc.postincrement num;
  } in

  nodes := node :: !nodes;

  (* Update the mapping of LR(0) cores to lists of nodes. *)

  let k = Lr0.core state in
  assert (k < Lr0.n);
  map.(k) <- node :: map.(k);

  (* Enqueue this node for further examination. *)

  Queue.add node queue;

  (* Debugging output. *)

  follow_state "Creating a new state" node false;

  (* Return the freshly created node. *)

  node

(* -------------------------------------------------------------------------- *)

(* Materializing a transition turns its target state into a (fresh or
   existing) node. There are three scenarios: the proposed new state can be
   subsumed by an existing state, compatible with an existing state, or
   neither. *)

exception Subsumed of node

exception Compatible of node

let materialize (source : node) (symbol : Symbol.t) (target : lr1state) : unit =
  try

    (* Debugging output. *)

    follow_transition false source symbol target;

    (* Find all existing core-compatible states. *)

    let k = Lr0.core target in
    assert (k < Lr0.n);
    let similar = map.(k) in

    (* Check whether we need to create a new node or can reuse an existing
       state. *)

    (* 20120525: the manner in which this check is performed depends on
       [Settings.construction_mode]. There are now three modes. *)

    (* 20150204: there are now four modes. *)

    begin match Settings.construction_mode with
    | Settings.ModeCanonical ->

        (* In a canonical automaton, two states can be merged only if they
           are identical. *)

        List.iter (fun node ->
          if Lr0.subsume target node.state &&
             Lr0.subsume node.state target then
            raise (Subsumed node)
        ) similar

    | Settings.ModeInclusionOnly
    | Settings.ModePager ->

        (* A more aggressive approach is to take subsumption into account:
           if the new candidate state is a subset of an existing state,
           then no new node needs to be created. Furthermore, the existing
           state does not need to be enlarged. *)

        (* 20110124: require error compatibility in addition to subsumption. *)

        List.iter (fun node ->
          if Lr0.subsume target node.state &&
             Lr0.error_compatible target node.state then
            raise (Subsumed node)
        ) similar

    | Settings.ModeLALR ->
        ()

    end;

    begin match Settings.construction_mode with
    | Settings.ModeCanonical
    | Settings.ModeInclusionOnly ->
        ()

    | Settings.ModePager ->

        (* One can be even more aggressive and check whether the existing state is
           compatible, in Pager's sense, with the new state. If so, there is no
           need to create a new state: just merge the new state into the existing
           one. The result is a state that may be larger than each of the two
           states that have been merged. *)

        (* 20110124: require error compatibility in addition to the existing
           compatibility criteria. *)

        List.iter (fun node ->
          if Lr0.compatible target node.state &&
             Lr0.eos_compatible target node.state &&
             Lr0.error_compatible target node.state then
            raise (Compatible node)
        ) similar

    | Settings.ModeLALR ->

        (* In LALR mode, as soon as there is one similar state -- i.e. one
           state that shares the same LR(0) core -- we merge the new state
           into the existing one. *)
        List.iter (fun node ->
          raise (Compatible node)
        ) similar

    end;

    (* The above checks have failed. Create a new node. Two states that are in
       the subsumption relation are also compatible. This implies that the
       newly created node does not subsume any existing states. *)

    source.transitions <- SymbolMap.add symbol (create target) source.transitions

  with

  | Subsumed node ->

      (* Join an existing target node. *)

      follow_state "Joining existing state" node false;
      source.transitions <- SymbolMap.add symbol node source.transitions

  | Compatible node ->

      (* Join and grow an existing target node. It seems important that the
         new transition is created before [grow_successors] is invoked, so
         that all transition decisions made so far are explicit. *)

      node.state <- Lr0.union target node.state;
      follow_state "Joining and growing existing state" node true;
      source.transitions <- SymbolMap.add symbol node source.transitions;
      grow_successors node

(* -------------------------------------------------------------------------- *)

(* The actual construction process. *)

(* Populate the queue with the start nodes and store them in an array. *)

let entry : node ProductionMap.t =
  ProductionMap.map (fun (k : Lr0.node) ->
    create (Lr0.start k)
  ) Lr0.entry

(* Pick a node in the queue, that is, a node whose transitions have not yet
   been built. Build these transitions, and continue. *)

(* Note that building a transition can cause existing nodes to grow, so
   [node.state] is not necessarily invariant throughout the inner loop. *)

let () =
  Misc.qiter (fun node ->
    List.iter (fun symbol ->
      materialize node symbol (Lr0.transition symbol node.state)
    ) (Lr0.outgoing_symbols (Lr0.core node.state))
  ) queue

(* Record how many nodes were constructed. *)

let n =
  !num

(* Allocate an array of all nodes. *)

let nodes =
  Array.of_list (List.rev !nodes)

let () =
  assert (Array.length nodes = n)

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let number node =
  node.number

let node i =
  assert (0 <= i && i < n);
  nodes.(i)

let state node =
  node.state

let transitions node =
  node.transitions

(* -------------------------------------------------------------------------- *)

end
