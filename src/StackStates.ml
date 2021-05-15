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

open Grammar

module Run (S : sig

  (**[stack_height s] is the height of the known suffix of the stack
     at state [s]. *)
  val stack_height: Lr1.node -> int

  (**[production_height prod] is the height of the known suffix of the stack
     at a state where production [prod] can be reduced. *)
  val production_height: Production.index -> int

  (**[goto_height nt] is the height of the known suffix of the stack at a
     state where an edge labeled [nt] has just been followed. *)
  val goto_height: Nonterminal.t -> int

end) = struct

open S

(* We now wish to compute, at each state [s], a vector of sets of states,
   whose length is [stack_height s].  *)

(* Vectors of sets of states. *)

module StateSetVector = struct

  (* We use arrays whose right end represents the top of the stack. *)

  (* The index 0 corresponds to the cell that lies deepest in the stack. *)

  let empty, push =
    MArray.(empty, push)

  let truncate k v =
    assert (k <= Array.length v);
    MArray.truncate k v

  type property =
    Lr1.NodeSet.t array

  let bottom height =
    Array.make height Lr1.NodeSet.empty

  let leq_join v1 v2 =
    MArray.leq_join Lr1.NodeSet.leq_join v1 v2
    (* Because all heights are known ahead of time, we are able (and careful)
       to compare and join only vectors of equal length. *)

end

open StateSetVector

(* Define the data flow graph. *)

(* Its vertices are the nodes of the LR(1) automaton. *)

module G = struct

  type variable = Lr1.node

  type property = StateSetVector.property

  (* At each start state of the automaton, the stack is empty. *)

  let foreach_root contribute =
    Lr1.entry |> ProductionMap.iter (fun _prod root ->
      assert (stack_height root = 0);
      contribute root empty
    )

  (* The edges of the data flow graph are the transitions of the automaton. *)

  let foreach_successor source stack contribute =
    Lr1.transitions source |> SymbolMap.iter (fun _symbol target ->
      (* The contribution of [source], through this edge, to [target], is the
         stack at [source], extended with a new cell for this transition, and
         truncated to the stack height at [target], so as to avoid obtaining a
         vector that is longer than expected/necessary. *)
      let cell = Lr1.NodeSet.singleton source
      and height = stack_height target in
      let stack = push stack cell in
      contribute target (truncate height stack)
    )

end

(* Compute the least fixed point. *)

let stack_states : Lr1.node -> property option =
  let module F = Fix.DataFlow.Run(Lr1.ImperativeNodeMap)(StateSetVector)(G) in
  F.solution

(* If every state is reachable, then the least fixed point must be non-[None]
   everywhere, so we may view it as a function that produces a vector of sets
   of states. *)

let stack_states (node : Lr1.node) : property =
  match stack_states node with
  | None ->
      (* Apparently this node is unreachable. *)
      assert false
  | Some v ->
      v

(* [truncate_join height f nodes] computes a join of the images through [f] of
   the nodes in the set [nodes], truncated at height [height]. *)

let truncate_join height f nodes =
  Lr1.NodeSet.fold (fun node accu ->
    leq_join (truncate height (f node)) accu
  ) nodes (bottom height)

(* From the above information, deduce, for each production, the shape
   of the stack when this production is reduced. *)

(* We produce a vector of states whose length is [production_height prod].
   It is up to the user to provide an appropriate height oracle. *)

let production_states : Production.index -> property =
  Production.tabulate (fun prod ->
    let sites = Lr1.production_where prod in
    let height = production_height prod in
    truncate_join height stack_states sites
  )

(* Compute the shape of the stack when a transition on the nonterminal
   symbol [nt] is taken. *)

(* We produce a vector of states whose length is [goto_height nt].
   It is up to the user to provide an appropriate height oracle. *)

let goto_states : Nonterminal.t -> property =
  Nonterminal.tabulate (fun nt ->
    let symbol = Symbol.N nt in
    (* Compute the join of the stack shapes at every target of an edge
       labeled with [nt]. *)
    let targets = Lr1.all_targets symbol in
    let height = goto_height nt in
    truncate_join height stack_states targets
  )

type property =
  Lr1.NodeSet.t array

(* Debugging output. *)

let print (v : property) =
  if Array.length v = 0 then
    "epsilon"
  else
    Misc.separated_list_to_string Lr1.NodeSet.print "; " (Array.to_list v)

let dump (prefix : string) f =
  Lr1.iter (fun node ->
    Printf.fprintf f "%sstack(%s) = %s\n"
      prefix
      (Lr1.print node)
      (print (stack_states node))
  );
  Production.iterx (fun prod ->
    Printf.fprintf f "%sprodstack(%s) = %s\n"
      prefix
      (Production.print prod)
      (print (production_states prod))
  )

end (* Run *)
