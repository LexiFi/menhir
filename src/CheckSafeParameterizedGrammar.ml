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

open Positions
open Syntax
open Misc

(* Inside its own definition, a parameterized nonterminal symbol foo(X, Y)
   can be applied only to its formal parameters:  that is, using foo(X, Y)
   is permitted, but using foo(Y, X) or foo(X, list(Y)) is not.

   If foo(X, Y) is defined in a mutually recursive manner with bar(X, Y),
   then this restriction extends to both foo and bar. Furthermore, in such
   a case, foo and bar must have the same arity, that is, the same formal
   parameters.

   These conditions are sufficient to ensure the termination of expansion.

   For example:
   C(x)   : ...            // This definition does not involve A or B.
   A(x,y) : B(x,y) C(Y)    // This mutually recursive definition is ok.
   B(x,y) : A(x,y)
   D(x)   : E(D(x))        // This one is incorrect.
   E(y)   : D(y)

*)

let check (p_grammar : Syntax.grammar) =
  (* [n] is the grammar size. *)
  let n        = StringMap.cardinal p_grammar.p_rules in

  (* The successors of the non terminal [N] are its producers. It
     induce a graph over the non terminals and its successor function
     is implemented by [successors]. Non terminals are indexed using
     [nt].
  *)
  let nt, conv, _iconv = index_map p_grammar.p_rules in
  let parameters, name, branches, positions =
    (fun n -> (nt n).pr_parameters), (fun n -> (nt n).pr_nt),
    (fun n -> (nt n).pr_branches), (fun n -> (nt n).pr_positions)
  in

  (* The successors function is implemented as an array using the
     indexing previously created. *)
  let successors =
    Array.init n (fun node ->
      (* We only are interested by parameterized non terminals. *)
      if parameters node <> [] then
        List.fold_left (fun succs { pr_producers = symbols } ->
          List.fold_left (fun succs (_, p, _) ->
            let symbol, _ = Parameters.unapp p in
            try
              let symbol_node = conv symbol.value in
                (* [symbol] is a parameterized non terminal, we add it
                   to the successors set. *)
                if parameters symbol_node <> [] then
                  IntSet.add symbol_node succs
                else
                  succs
            with Not_found ->
              (* [symbol] is a token, it is not interesting for type inference
                 purpose. *)
              succs
          ) succs symbols
        ) IntSet.empty (branches node)
      else
        Misc.IntSet.empty
    )
  in

  (* The successors function and the indexing induce the following graph
     module. *)
  let module RulesGraph =
      struct

        type node = int

        let n = n

        let index node =
          node

        let successors f node =
          IntSet.iter f successors.(node)

        let iter f =
          for i = 0 to n - 1 do
            f i
          done

      end
  in
  let module ConnectedComponents = Tarjan.Run (RulesGraph) in
    (* We check that:
       - all the parameterized definitions of a particular component
       have the same number of parameters.
       - every parameterized non terminal definition always uses
       parameterized definitions of the same component with its
       formal parameters.

       Components are marked during the traversal:
       -1 means unvisited
       n with n > 0 is the number of parameters of the clique.
    *)
  let unseen = -1 in
  let marked_components = Array.make n unseen in

  (* [actual_parameters_as_formal] is the well-formedness checker for
     parameterized non terminal application. *)
  let actual_parameters_as_formal actual_parameters formal_parameters =
    List.for_all2 (fun y -> (function ParameterVar x -> x.value = y
                              | _ -> false))
      formal_parameters actual_parameters
  in

  (* We traverse the graph checking each parameterized non terminal
     definition is well-formed. *)
  RulesGraph.iter (fun i ->
    let params    = parameters i
    and iname     = name i
    and repr      = ConnectedComponents.representative i
    and positions = positions i
    in

    (* We check the number of parameters. *)
    let check_parameters () =
      let parameters_len = List.length params in
        (* The component is visited for the first time. *)
        if marked_components.(repr) = unseen then
          marked_components.(repr) <- parameters_len
        else (* Otherwise, we check that the arity is homogeneous
                in the component. *)
          if marked_components.(repr) <> parameters_len then
            Error.error positions
                 "mutually recursive definitions must have the same parameters.\n\
                  This is not the case for %s and %s."
                    (name repr) iname
    in

    (* In each production rule, the parameterized non terminal
       of the same component must be instantiated with the same
       formal arguments. *)
    let check_producers () =
      List.iter
        (fun { pr_producers = symbols } -> List.iter
           (function (_, p, _) ->
              (* If it is in the same component, check in addition that
                 the arguments are the formal arguments. *)
              let symbol, actuals = Parameters.unapp p in
              try
                let idx = conv symbol.value in
                  if ConnectedComponents.representative idx = repr then
                    if not (actual_parameters_as_formal actuals params)
                    then
                      Error.error [ symbol.position ]
                           "mutually recursive definitions must have the same \
                            parameters.\n\
                            This is not the case for %s."
                            (let name1, name2 = (name idx), (name i) in
                               if name1 <> name2 then name1 ^ " and "^ name2
                               else name1)
              with _ -> ())
               symbols) (branches i)
    in

    check_parameters();
    check_producers()
  )
