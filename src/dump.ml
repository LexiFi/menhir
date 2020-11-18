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

open Printf
open Grammar

let dump_node out node =

  (* Print the state number. *)

  fprintf out "State %d:\n"
    (Lr1.number node);

  (* Print the items. *)

  fprintf out "%s"
    (Lr0.print "" (Lr1.state node));

  (* Print the transitions. *)

  SymbolMap.iter (fun symbol node ->
    fprintf out "-- On %s shift to state %d\n"
      (Symbol.print symbol) (Lr1.number node)
  ) (Lr1.transitions node);

  (* Print the reductions. *)

  (* One might wish to group all symbols that
     lead to reducing a common production. *)

  TerminalMap.iter (fun tok prods ->
    List.iter (fun prod ->
      fprintf out "-- On %s " (Terminal.print tok);
      match Production.classify prod with
      | Some nt ->
          fprintf out "accept %s\n" (Nonterminal.print false nt)
      | None ->
          fprintf out "reduce production %s\n" (Production.print prod)
    ) prods
  ) (Lr1.reductions node);

  (* Print the conflicts. *)

  if not (TerminalSet.is_empty (Lr1.conflict_tokens node)) then
    fprintf out "** Conflict on %s\n"
      (TerminalSet.print (Lr1.conflict_tokens node));

  (* Print the end-of-stream conflicts. *)

  Lr1.has_eos_conflict node |> Option.iter begin fun (prods, toks) ->

    (* If this function is invoked before conflict resolution has been
       performed, then the list [prods] could have several elements.
       We pick one. *)
    assert (prods <> []);
    let prod = List.hd prods in

    fprintf out "** End-of-stream conflict on %s\n"
      (TerminalSet.print toks);
    fprintf out
      "**   There is a tension between\n\
       **   (1) %s\n\
       **   without even requesting a lookahead token, and\n\
       **   (2) testing whether the lookahead token is a member of the above set.\n"
      (Production.describe prod)

  end;

  (* Skip a line. *)

  fprintf out "\n"

let dump filename =
  let out = open_out filename in
  Lr1.iter (dump_node out);
  close_out out;
  Time.tick "Dumping the LR(1) automaton"
