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
open UnparameterizedSyntax

(* -------------------------------------------------------------------------- *)

(* Auxiliary functions for expansion. *)

let rec subst_parameter subst = function
  | ParameterVar x ->
      (try
        List.assoc x.value subst
      with Not_found ->
        ParameterVar x)

  | ParameterApp (x, ps) ->
      (try
        match List.assoc x.value subst with
          | ParameterVar y ->
              ParameterApp (y, List.map (subst_parameter subst) ps)

          | ParameterApp _ ->
              (* Type-checking ensures that we cannot do partial
                 application. Consequently, if a higher-order nonterminal
                 is an actual argument, it cannot be the result of a
                 partial application. *)
              assert false

          | ParameterAnonymous _ ->
              (* Anonymous rules are eliminated early on. *)
              assert false

      with Not_found ->
          ParameterApp (x, List.map (subst_parameter subst) ps))

  | ParameterAnonymous _ ->
      (* Anonymous rules are eliminated early on. *)
      assert false


let dummy : rule =
  { branches = [];
    positions = [];
    inline_flag = false;
    attributes = [];
  }

(* [mangle p] chooses a name for the new nonterminal symbol that corresponds
   to the parameter [p]. *)

(* We include parentheses and commas in this name, because that is readable
   and acceptable in many situations. We replace them with underscores in
   situations where these characters are not valid. *)

let mangle p =
  Parameters.print false p

(* -------------------------------------------------------------------------- *)

(* Expansion. *)

module Expand (X : sig
  val p_grammar: Syntax.grammar
end) = struct
  open X

  (* Set up a table that stores the rules of the new grammar. *)
  let expanded_rules =
    Hashtbl.create 13


  (* On top of the function [mangle], we set up a mechanism that checks that
     every (normalized) mangled name is unique. (Indeed, in principle, there
     could be clashes, although in practice this is unlikely.) We must check
     that every application of [mangle] to a *new* argument yields a *new*
     (normalized) result. This is succinctly expressed by combining a claim
     and a memoizer. *)

  let name_of : parameter -> string =
    let ensure_fresh = Misc.new_claim() in
    let module M = Memoize.MakeViaHashtbl(Parameters) in
    M.memoize (fun param ->
      let name = mangle param in
      ensure_fresh (Misc.normalize name);
      name
    )

  (* Now is the time to eliminate (desugar) %attribute declarations. We build
     a table of these declarations, and look up this table so as to place
     appropriate attributes on terminal and nonterminal symbols. *)

  module InstanceTable =
    Hashtbl.Make (Parameters)

  let symbol_attributes_table =
    InstanceTable.create 7

  let () =
    List.iter (fun (actuals, attributes) ->
      List.iter (fun actual ->
        let attributes', used =
          try InstanceTable.find symbol_attributes_table actual
          with Not_found -> [], ref false
        in
        InstanceTable.replace symbol_attributes_table actual
          (attributes @ attributes', used)
      ) actuals
    ) p_grammar.p_symbol_attributes

  let symbol_attributes (actual : parameter) : attributes =
    try
      let attrs, used = InstanceTable.find symbol_attributes_table actual in
      used := true;
      attrs
    with Not_found ->
      []

  let symbol_attributes_warnings () =
    InstanceTable.iter (fun actual (attributes, used) ->
      if not !used then
        List.iter (fun (id, _payload) ->
          Error.warning [Positions.position id]
            "this attribute could not be transferred to the symbol %s"
            (Parameters.print true actual)
        ) attributes
    ) symbol_attributes_table

  (* This auxiliary function transfers information from the
     table [symbol_attributes] towards terminal symbols. *)

  let decorate tok prop : token_properties =
    let attrs = symbol_attributes (ParameterVar (Positions.unknown_pos tok)) in
    { prop with tk_attributes = attrs @ prop.tk_attributes }

  (* Given the substitution [subst] from parameters to non terminal, we
     instantiate the parameterized branch. *)
  let rec expand_branch subst pbranch =
    let new_producers = List.map (fun (ido, p, attrs) ->
      let p = subst_parameter subst p in
      let sym, actual_parameters = Parameters.unapp p in
      (* Instantiate the definition of the producer. *)
      { producer_identifier = Positions.value ido;
        producer_symbol = expand_branches subst sym actual_parameters;
        producer_attributes = attrs }
    ) pbranch.pr_producers
  in
  {
    branch_position          = pbranch.pr_branch_position;
    producers                = new_producers;
    action                   = pbranch.pr_action;
    branch_prec_annotation   = pbranch.pr_branch_prec_annotation;
    branch_production_level  = pbranch.pr_branch_production_level;
  }

  (* Instantiate the branches of sym for a particular set of actual
     parameters. *)
  and expand_branches subst sym actual_parameters : symbol =
    match StringMap.find (Positions.value sym) p_grammar.p_rules with
    | exception Not_found ->
        (* [sym] is a terminal symbol. Expansion is not needed. *)
        Positions.value sym
    | prule ->
        let nsym = name_of (ParameterApp (sym, actual_parameters)) in
        (* Check up front if [nsym] is marked, so as to deal with it just once. *)
        if Hashtbl.mem expanded_rules nsym then
          nsym
        else begin
          (* Type checking ensures that parameterized nonterminal symbols
             are applied to an appropriate number of arguments. *)
          assert (List.length prule.pr_parameters =
                  List.length actual_parameters);
          let subst =
            List.combine prule.pr_parameters actual_parameters @ subst
          in
          (* Mark [nsym] up front, so as to avoid running in circles. *)
          Hashtbl.add expanded_rules nsym dummy;
          (* The attributes carried by the expanded symbol [nsym] are those
             carried by the original parameterized symbol [sym], plus those
             found in %attribute declarations for [nsym], plus those found
             in %attribute declarations for [sym]. *)
          let attributes =
            symbol_attributes (ParameterApp (sym, actual_parameters)) @
            symbol_attributes (ParameterVar sym) @
            prule.pr_attributes
          in
          Hashtbl.replace expanded_rules nsym {
            branches    = List.map (expand_branch subst) prule.pr_branches;
            positions   = prule.pr_positions;
            inline_flag = prule.pr_inline_flag;
            attributes  = attributes;
          };
          nsym
        end

  (* Process %type declarations. *)
  let rec types_from_list
      (ps : (Syntax.parameter * 'a Positions.located) list)
    : 'a StringMap.t =
    match ps with
    | [] -> StringMap.empty
    | (nt, ty)::q ->
        let accu = types_from_list q in
        let mangled = mangle nt in
        if StringMap.mem mangled accu then
          Error.error [Parameters.position nt]
               "there are multiple %%type declarations for nonterminal %s."
               mangled;
        StringMap.add mangled (Positions.value ty) accu

  (* Process %on_error_reduce declarations. *)
  let rec on_error_reduce_from_list (ps : (Syntax.parameter * 'p) list) : 'p StringMap.t =
    match ps with
    | [] ->
        StringMap.empty
    | (nt, prec) :: ps ->
        let accu = on_error_reduce_from_list ps in
        let mangled = mangle nt in
        if StringMap.mem mangled accu then
          Error.error [Parameters.position nt]
               "there are multiple %%on_error_reduce declarations for nonterminal %s."
               mangled;
        StringMap.add mangled prec accu

  (* The entry points are the nonparameterized nonterminal symbols. (Not just
     the start symbols, as we haven't run the reachability analysis, and the
     grammar may contain unreachable parts, which we still want to expand.)
     Because a start symbol cannot be parameterized (which the type analysis
     guarantees), all of the start symbols are entry points. *)
  let () =
    StringMap.iter (fun nt prule ->
      if prule.pr_parameters = [] then
        let (_ : symbol) = expand_branches [] (Positions.unknown_pos nt) [] in
        ()
    ) p_grammar.p_rules

  let grammar = {
    preludes        = p_grammar.p_preludes;
    postludes       = p_grammar.p_postludes;
    parameters      = p_grammar.p_parameters;
    start_symbols   = StringMap.domain (p_grammar.p_start_symbols);
    types           = types_from_list p_grammar.p_types;
    on_error_reduce = on_error_reduce_from_list p_grammar.p_on_error_reduce;
    tokens          = StringMap.mapi decorate p_grammar.p_tokens;
    gr_attributes   = p_grammar.p_grammar_attributes;
    rules           = Hashtbl.fold StringMap.add expanded_rules StringMap.empty
  }

  (* If some %attribute declarations are unused, warn about it. *)
  let () =
    symbol_attributes_warnings()

end

let expand p_grammar =
  let module E = Expand(struct let p_grammar = p_grammar end) in
  E.grammar
