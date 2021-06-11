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
open Infix

let minimal_symbol = function
  | Symbol.N nt ->
      Analysis.minimal nt
  | Symbol.T _ ->
      1

module ProductionSet = ProductionMap.Domain

let maximal nt =
  let visited_symbols = ref SymbolSet.empty in
  let visited_nt = ref NonterminalSet.empty in
  let visited_prods = ref ProductionSet.empty in
  let rec aux_symbol symbol =
    if SymbolSet.mem symbol !visited_symbols then CompletedNat.Infinity
    else (
      visited_symbols := SymbolSet.add symbol !visited_symbols ;
      match symbol with
      | Symbol.N nt ->
          aux_nt nt
      | Symbol.T _ ->
          CompletedNat.one )
  and aux_prod prod =
    if ProductionSet.mem prod !visited_prods then CompletedNat.Infinity
    else (
      visited_prods := ProductionSet.add prod !visited_prods ;
      let symbols = Production.rhs prod in
      Array.fold_left
        (fun acc symbol ->
          CompletedNat.add_lazy acc (fun () -> aux_symbol symbol))
        CompletedNat.zero symbols )
  and aux_nt nt =
    if NonterminalSet.mem nt !visited_nt then CompletedNat.Infinity
    else (
      visited_nt := NonterminalSet.add nt !visited_nt ;
      Production.foldnt nt
        (fun prod acc -> CompletedNat.max_lazy acc (fun () -> aux_prod prod))
        CompletedNat.zero )
  in
  aux_nt nt

let maximal_symbol = function
  | Symbol.N nt ->
      maximal nt
  | Symbol.T _ ->
      CompletedNat.one

let sentence ?(log = false) nt budget : Terminal.t array =
  let log = if log then Printf.eprintf "%s" else ignore in
  log @@ Printf.sprintf "Starting to generate sentence with %i budget\n" budget ;
  let array = Array.make budget Terminal.sharp in
  let global_budget = budget in
  let ri = ref 0 in
  (* This auxilliary function build a non-terminal with a specified budget *)
  let rec aux nt budget =
    log
    @@ Printf.sprintf
         "Building nonterminal %s with budget %i. Maximal cost is %s and \
          minimal cost is %i\n"
         (Nonterminal.print false nt)
         budget
         ( match maximal nt with
         | CompletedNat.Infinity ->
             "Infinite"
         | CompletedNat.Finite i ->
             string_of_int i )
         (Analysis.minimal nt) ;
    (* A list of possible choices *)
    let choices =
      Production.foldnt nt
        (fun index acc ->
          if Production.error_free index then index :: acc else acc)
        []
    in
    (* A list of pair of cost.
       The first element of the pair is the maximal cost of the choice, which can be infinite,
        the second is the minimum. *)
    let costs =
      List.map
        (fun production_index ->
          let symbols = Production.rhs production_index in
          let max_cost =
            Array.fold_left
              (fun n symbol ->
                CompletedNat.add_lazy n (fun () -> maximal_symbol symbol))
              CompletedNat.zero symbols
          in
          let min_cost =
            Array.fold_left
              (fun n symbol -> n + minimal_symbol symbol)
              0 symbols
          in
          (max_cost, min_cost))
        choices
    in
    let priced_choices = List.combine choices costs in
    (* Very reasonables choices are the best choices,
       in the sense that their minimal cost is inferior or equal to the budget,
       and their maximal cost is superior or equal to the budget,
       which means that we have a chance to use up all our budget on them. *)
    let very_reasonable_choices =
      Array.of_list
        (List.filter
           (fun (_, (max_price, min_price)) ->
             min_price <= budget && CompletedNat.(max_price >= Finite budget))
           priced_choices)
    in
    (* Reasonable choices are choices such that we will not go over budget.
       They are included in very reasonable choices. *)
    let reasonable_choices =
      Array.of_list
        (List.filter
           (fun (_, (_, min_price)) -> min_price <= budget)
           priced_choices)
    in
    if reasonable_choices = [||] then (
      log @@ Printf.sprintf "  No choice at all :/\n" ;
      assert false )
    else (
      log
      @@ Printf.sprintf "  Number of very reasonable choices : %i"
           (Array.length very_reasonable_choices) ;
      log
      @@ Printf.sprintf "  Number of reasonable choices : %i\n"
           (Array.length reasonable_choices) ;
      (* We randomly pick a production among the [very_reasonable_choices],
         and if failing, among [reasonable_choices]. *)
      let choice, choice_min_price =
        let choice, (_, min_price) =
          if Array.length very_reasonable_choices > 0 then
            very_reasonable_choices.(Random.int
                                       (Array.length very_reasonable_choices))
          else reasonable_choices.(Random.int (Array.length reasonable_choices))
        in
        (Production.rhs choice, min_price)
      in
      let minimal_costs = Array.map minimal_symbol choice in
      let maximal_costs = Array.map maximal_symbol choice in
      (* Check that the choice is in fact reasonable. *)
      assert (budget >= choice_min_price) ;
      let spendable = budget - choice_min_price in
      let budget_allocation = Array.copy minimal_costs in
      let rspendable = ref spendable in
      let count_maxed_out = ref 0 in
      (* maxed_out is a bool array that tells, for every symbol, whether we can
         still allocate more budget to it. We can never allocate more budget
         than 1 to a nonterminal. For a terminal, we cannot allocate more
         budget than the minimum. *)
      let maxed_out =
        Array.mapi
          (fun i symbol ->
            let r =
              match symbol with
              | Symbol.T _ ->
                  true
              | Symbol.N _ ->
                  CompletedNat.(
                    compare (Finite minimal_costs.(i)) maximal_costs.(i))
                  >= 0
            in
            if r then count_maxed_out := !count_maxed_out + 1 ;
            r)
          choice
      in
      let count_symbol = Array.length choice in
      (* While we still have budget to spend, and not every symbol is maxed out. *)
      while !rspendable > 0 && !count_maxed_out < count_symbol do
        let spendable_for_this_round = !rspendable in
        (* We will try to give every non-maxed-out symbol this much budget.
           Hopefully this means that they will share the budget. *)
        let desired_increase =
          spendable_for_this_round / (count_symbol - !count_maxed_out)
        in
        Array.iteri
          (fun i current_cost ->
            (* We do not change anything for maxed-out symbols *)
            if not maxed_out.(i) then (
              let maximal_cost = maximal_costs.(i) in
              (* We never give more budget to a symbol than it can handle. *)
              let actual_increase =
                CompletedNat.(
                  to_int
                  @@ min (Finite desired_increase)
                       (sub maximal_cost current_cost))
              in
              (* If we did not allocate all of our budget to the symbol,
                 it means it is maxed-out *)
              if actual_increase < desired_increase || actual_increase = 0 then (
                maxed_out.(i) <- true ;
                count_maxed_out += 1 ) ;
              (* Increase the budget. *)
              budget_allocation.(i) <- current_cost + actual_increase ;
              (* Decrease the budget *)
              rspendable := !rspendable - actual_increase ))
          budget_allocation
      done ;
      if not (!rspendable = 0 || !count_maxed_out = count_symbol) then
        (* The exit of the while loop puts limits on [rspendable] and [count_maxed_out].
           The limits should be reached exactly. *)
        log
        @@ Printf.sprintf "  /!\\ RSPENDABLE = %i COUNT_MAXED_OUT = %i"
             !rspendable !count_maxed_out ;
      if !rspendable <> 0 then
        log
        @@ Printf.sprintf "  Did not use all the budget : %i left\n" !rspendable ;
      let over_budget = ref false in
      (* For every symbol,
         if it is a terminal we append it to the array,
         if it is a non-terminal we call aux on it with its allocated budget.*)
      Array.iter2
        (fun allocation -> function Symbol.T t ->
              if !ri < global_budget then (
                array.(!ri) <- t ;
                ri += 1 )
              else over_budget := true | Symbol.N nt ->
              log
              @@ Printf.sprintf "Nonterminal %S starting on line %i\n"
                   (Nonterminal.print false nt)
                   !ri ;
              aux nt allocation)
        budget_allocation choice ;
      (* If we go over budget we log information. This should never happen. *)
      if !over_budget then
        log
        @@ Printf.sprintf
             "Went over budget. Array length is %i, budget was %i, predicted \
              cost was %i\n\
             \  Symbols : %s\n"
             (Array.length choice) budget choice_min_price
             (String.concat " " (Array.to_list (Array.map Symbol.print choice)))
      )
  in
  aux nt budget ; Array.sub array 0 !ri
