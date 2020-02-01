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

(* This module constructs an LALR automaton for the grammar described by the
   module [Grammar]. *)

(* In LALR mode, two LR(1) states are merged as soon as they have the same
   LR(0) core. *)

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
   and grown until a fixpoint is reached. *)

(* [grow node state] grows the existing node [node], if necessary, so that its
   associated state subsumes [state]. If this represents an actual (strict)
   growth, then [node]'s descendants are grown as well. *)

let rec grow node state =
  if Lr0.subsume state node.state then
    follow_state "Target state is unaffected" node false
  else begin

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

(* A mapping of LR(0) node numbers to at most one node. This allows us to
   efficiently find the unique node (if it exists) that is core-compatible
   with a newly found state. *)

let map : node option array =
  Array.make Lr0.n None

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
  assert (map.(k) = None);
  map.(k) <- Some node;

  (* Enqueue this node for further examination. *)

  Queue.add node queue;

  (* Debugging output. *)

  follow_state "Creating a new state" node false;

  (* Return the freshly created node. *)

  node

(* -------------------------------------------------------------------------- *)

(* Materializing a transition turns its target state into a (fresh or
   existing) node. There are two scenarios: the proposed new state may
   or may not be subsumed by an existing state. *)

let materialize (source : node) (symbol : Symbol.t) (target : lr1state) : unit =

  (* Debugging output. *)

  follow_transition false source symbol target;

  (* Find all existing core-compatible states. *)

  let k = Lr0.core target in
  assert (k < Lr0.n);

  (* Check whether we must create a new node or reuse an existing one. *)

  (* In LALR mode, as soon as there is one similar state -- i.e. one state
     that shares the same LR(0) core -- we merge the new state into the
     existing one. *)

  match map.(k) with
  | None ->
      (* There is no similar state. Create a new node. *)
      source.transitions <- SymbolMap.add symbol (create target) source.transitions
  | Some node ->
      (* There is an existing node. Join it and grow it if necessary. *)
      source.transitions <- SymbolMap.add symbol node source.transitions;
      grow node target

(* -------------------------------------------------------------------------- *)

(* The actual construction process. *)

(* Populate the queue with the start nodes and store them in an array. *)

let entry : node ProductionMap.t =
  ProductionMap.map (fun k ->
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
