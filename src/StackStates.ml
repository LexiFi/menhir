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

  (**This flag indicates whether the user wishes to compute a short
     or a long invariant. *)
  val long: bool

end) = struct

open S

(* We now wish to compute, at each state [s], a vector of sets of states,
   whose length is [stack_height s].  *)

(* Vectors of sets of states. *)

module StateSetVector = struct

  (* We use arrays whose right end represents the top of the stack. *)

  (* The index 0 corresponds to the cell that lies deepest in the stack. *)

  let empty, push, truncate =
    MArray.(empty, push, truncate)

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
      assert (height <= Array.length stack);
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

(* [minimum f nodes] computes the minimum of the images through [f] of
   the nodes in the nonempty set [nodes]. *)

let minimum f nodes =
  assert (not (Lr1.NodeSet.is_empty nodes));
  Lr1.NodeSet.fold (fun node accu ->
    min (f node) accu
  ) nodes max_int

(* [truncate_join height f nodes] computes a join of the images through [f] of
   the nodes in the set [nodes], truncated at height [height]. *)

let truncate_join height f nodes =
  Lr1.NodeSet.fold (fun node accu ->
    leq_join (truncate height (f node)) accu
  ) nodes (bottom height)

(* From the above information, deduce, for each production, the shape
   of the stack when this production is reduced. *)

(* If [long] is false, then we are careful to produce a vector of
   states whose length is exactly that of the production [prod]. *)

(* If [long] is true, then we *can* produce a vector whose length is
   greater than that of the production [prod]. *)

let production_states : Production.index -> property =
  Production.tabulate (fun prod ->
    let sites = Lr1.production_where prod in
    let height =
      if long && not (Lr1.NodeSet.is_empty sites) then
        minimum stack_height sites
      else
        Production.length prod
    in
    truncate_join height stack_states sites
  )

(* Compute the shape of the stack when a transition on the nonterminal
   symbol [nt] is taken. *)

let goto_states : Nonterminal.t -> property =
  Nonterminal.tabulate (fun nt ->
    let symbol = Symbol.N nt in
    (* Compute the join of the stack shapes at every target of an edge
       labeled with [nt]. *)
    let targets = Lr1.all_targets symbol in
    let height =
      if long && not (Lr1.NodeSet.is_empty targets) then
        minimum stack_height targets
      else
        1
    in
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
