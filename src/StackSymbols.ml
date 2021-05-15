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

module type STACK_SYMBOLS = sig

  (**[stack_height s] is [Array.length (stack_symbols s)]. *)
  val stack_height: Lr1.node -> int

  (**[stack_symbols s] is the known suffix of the stack at state [s]. It
     is represented as an array of symbols. By convention, the top of
     the stack is the end of the array. *)
  val stack_symbols: Lr1.node -> Symbol.t array

  (**[production_symbols s] is the known suffix of the stack at a point
     where production [prod] is about to be reduced. *)
  val production_symbols: Production.index -> Symbol.t array

end

(* We compute a lower bound on the height of the stack at every state, and at
   the same time, we compute which symbols are held in this stack prefix. *)

(* In order to compute a lower bound on the height of the stack at a state
   [s], we examine the LR(0) items that compose [s]. For each item, if the
   bullet is at position [pos], then we can be assured that the height of the
   stack is at least [pos]. Thus, we compute the maximum of [pos] over all
   items (of which there is at least one). *)

(* The set of items that we use is not closed, but this does not matter; the
   items that would be added by the closure would not add any information
   regarding the height of the stack, since the bullet is at position 0 in
   these items. *)

(* Instead of computing just the stack height, we compute, in the same manner,
   which symbols are on the stack at a state [s]. This is an array of symbols
   whose length is the height of the stack at [s]. By convention, the top of
   the stack is the end of the array. *)

(* This analysis is extremely fast: on an automaton with over 100,000 states,
   it takes under 0.01 second. *)

module Run () = struct

  (* Compute and tabulate this information at the level of the LR(0)
     automaton. *)

  let stack_symbols : Lr0.node -> Symbol.t array =
    let dummy =
      Array.make 0 (Symbol.T Terminal.sharp)
    in
    Misc.tabulate Lr0.n (fun node ->
      Item.Set.fold (fun item accu ->
        let _prod, _nt, rhs, pos, _length = Item.def item in
        if pos > Array.length accu then Array.sub rhs 0 pos else accu
      ) (Lr0.items node) dummy
    )

  (* Extend it to the LR(1) automaton. *)

  let stack_symbols (node : Lr1.node) : Symbol.t array =
    stack_symbols (Lr0.core (Lr1.state node))

  let stack_height (node : Lr1.node) : int =
    Array.length (stack_symbols node)

  (* Add a trivial definition of [production_symbols]. *)

  let production_symbols =
    Production.rhs

end

(* ------------------------------------------------------------------------ *)

(* The submodule [Long] computes the known suffix of the stack in each state,
   as a vector of symbols, and it computes a suffix that is as long as
   possible, in contrast with the above code, which computes a suffix whose
   length can be predicted by based on the LR(0) items in each state. *)

module Long () = struct

  (* Vectors of symbols. *)

  module SymbolVector = struct

    type property =
      Symbol.t array

    let empty, push =
      MArray.(empty, push)

    (* Given two arrays [v1] and [v2] of lengths [n1] and [n2], the function
       call [lcs v1 v2 n1 n2 (min n1 n2) 0] computes the greatest [k] such
       that [truncate k v1] and [truncate k v2] are equal. *)

    let rec lcs v1 v2 n1 n2 n k =
      (* [n] is [min n1 n2]. *)
      if k = n || v1.(n1 - 1 - k) <> v2.(n2 - 1 - k) then k
      else lcs v1 v2 n1 n2 n (k + 1)

    let leq_join v1 v2 =
      let n1 = Array.length v1
      and n2 = Array.length v2 in
      let n = min n1 n2 in
      let k = lcs v1 v2 n1 n2 n 0 in
      if k = n2 then v2
      else if k = n1 then v1
      else MArray.truncate k v1

  end

  open SymbolVector

  (* Define the data flow graph. *)

  (* We perform the data flow analysis at the level of the LR(0) automaton. *)

  module G = struct

    type variable = Lr0.node

    type property = SymbolVector.property

    (* At each start state of the automaton, the stack is empty. *)

    let foreach_root contribute =
      Lr0.entry |> ProductionMap.iter (fun _prod root ->
        contribute root empty
      )

    (* The edges of the data flow graph are the transitions of the automaton. *)

    let foreach_successor source stack contribute =
      Lr0.outgoing_edges source |> SymbolMap.iter (fun symbol target ->
        (* The contribution of [source], through this edge, to [target], is the
           stack at [source], extended with a new cell for this transition. *)
        contribute target (push stack symbol)
      )

  end

  (* Compute the least fixed point. *)

  let stack_symbols : Lr0.node -> property option =
    let module F = Fix.DataFlow.Run(Lr0.ImperativeNodeMap)(SymbolVector)(G) in
    F.solution

  (* If every state is reachable, then the least fixed point must be non-[None]
     everywhere, so we may view it as a function that produces a vector of
     symbols. *)

  let stack_symbols (node : Lr0.node) : property =
    match stack_symbols node with
    | None ->
        (* Apparently this node is unreachable. *)
        assert false
    | Some v ->
        v

  (* Move up to the level of the LR(1) automaton. *)

  let stack_symbols (node : Lr1.node) : Symbol.t array =
    stack_symbols (Lr0.core (Lr1.state node))

  let stack_height (node : Lr1.node) : int =
    Array.length (stack_symbols node)

  (* From the above information, deduce, for each production, the shape
     of the stack when this production is reduced. *)

  (* We *can* produce a vector whose length is greater than that
     of the production [prod]. *)

  let production_symbols : Production.index -> property =
    Production.tabulate (fun prod ->
      let nodes = Lr1.production_where prod in
      if Lr1.NodeSet.is_empty nodes then
        (* This production is never reduced. It is not clear what vector
           should be returned. Using the right-hand side of the production
           seems reasonable. *)
        Production.rhs prod
      else
        (* Compute a join over the nonempty set of nodes where this
           production is reduced. *)
        let node = Lr1.NodeSet.choose nodes in
        let nodes = Lr1.NodeSet.remove node nodes in
        Lr1.NodeSet.fold (fun node accu ->
          leq_join (stack_symbols node) accu
        ) nodes (stack_symbols node)
    )

end

(* ------------------------------------------------------------------------ *)

(* Printing. *)

let buffer =
  Buffer.create 1024

let print_symbols symbols =
  symbols |> Array.iter (fun symbol ->
    Printf.bprintf buffer " %s" (Symbol.print symbol)
  );
  let s = Buffer.contents buffer in
  Buffer.clear buffer;
  s
