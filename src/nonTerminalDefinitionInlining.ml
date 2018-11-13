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

let position = Positions.position
open Keyword
open UnparameterizedSyntax
open ListMonad

(* [index2id] converts a 0-based index (into a list of producers) to
   an identifier (the name of the producer). *)

let index2id producers i =
  try
    producer_identifier (List.nth producers i)
  with Failure _ ->
    assert false (* should not happen *)

(* [rename_sw_outer] transforms the keywords in the outer production (the
   caller) during inlining. It replaces [$startpos(x)] and [$endpos(x)], where
   [x] is the name of the callee, with [startpx] and [endpx], respectively. *)

let rename_sw_outer (x, startpx, endpx) (subject, where) : (subject * where) option =
  match subject, where with
  | Before, _ ->
      None
  | RightNamed x', _ ->
      if x' = x then
        match where with
        | WhereStart -> Some startpx
        | WhereEnd   -> Some endpx
        | WhereSymbolStart -> assert false (* has been expanded away *)
      else
        None
  | Left, _ ->
      (* [$startpos], [$endpos], and [$symbolstartpos] have been expanded away
         earlier; see [KeywordExpansion]. *)
      assert false

(* [rename_sw_inner] transforms the keywords in the inner production (the callee)
   during inlining. It replaces [$endpos($0)] with [beforeendp]. *)

let rename_sw_inner beforeendp (subject, where) : (subject * where) option =
  match subject, where with
  | Before, _ ->
      assert (where = WhereEnd);
      Some beforeendp
  | RightNamed _, _ ->
      None
  | Left, _ ->
      (* [$startpos] and [$endpos] have been expanded away earlier; see
         [KeywordExpansion]. *)
      assert false

(* This auxiliary function checks that a use site of an %inline symbol does
   not carry any attributes. *)

let check_no_producer_attributes producer =
  match producer_attributes producer with
  | [] ->
      ()
  | (id, _payload) :: _attributes ->
      Error.error
        [position id]
        "the nonterminal symbol %s is declared %%inline.\n\
         A use of it cannot carry an attribute."
        (producer_symbol producer)

(* [names producers] is the set of names of the producers [producers].
   The name of a producer is the OCaml variable that is used to name the
   semantic value. *)

(* At the same time, this function checks that no two producers carry the
   same name. This check should never fail if we have performed appropriate
   renamings. It is a debugging aid. *)

let names (producers : producers) : StringSet.t =
  List.fold_left (fun ids producer ->
    let id = producer_identifier producer in
    assert (not (StringSet.mem id ids));
    StringSet.add id ids
  ) StringSet.empty producers

(* [fresh names x] returns a fresh name that is not in the set [names].
   The new name is based on [x] in an unspecified way. *)
let rec fresh names x =
  if StringSet.mem x names then
    let x =
      (* Propose a new candidate name. A fairly arbitrary construction
         can be used here; we just need it to produce an infinite sequence
         of names, so that eventually we fall outside of [names]. We also
         need it to produce reasonably concise names, as this construction
         can be iterated several times in practice; I have observed up to
         9 iterations in real-world grammars. *)
      let x, n = ChopInlined.chop (Lexing.from_string x) in
      let n = n + 1 in
      Printf.sprintf "%s_inlined%d" x n
    in
    fresh names x
  else
    x

(* We have to rename the producers [producers] of the inlined production
   if they clash with the set [used] of the names used by the producers
   of the host branch. (Note that [used] need not contain the name of the
   producer that is inlined away.)
   This function produces a pair of:
   1. a substitution [phi], which represents the renaming that we have
      performed, and which must be applied to the inner semantic action;
   2. the renamed [producers]. *)
let rename (used : StringSet.t) producers: Action.subst * producers =

  (* Compute a renaming and the new names of the inlined producers. *)
  let phi, _used, producers' =
    List.fold_left (fun (phi, used, producers) producer ->
      let x = producer_identifier producer in
      if StringSet.mem x used then
        let x' = fresh used x in
        (x, x') :: phi,
        StringSet.add x' used,
        { producer with producer_identifier = x' } :: producers
      else
        (phi, StringSet.add x used, producer :: producers)
    ) ([], used, []) producers
  in
  phi, List.rev producers'

(* [is_inline_nonterminal grammar symbol] tells whether [symbol] is a nonterminal
   symbol (as opposed to a terminal symbol) and is marked %inline. *)

let is_inline_nonterminal grammar symbol : bool =
  match StringMap.find symbol grammar.rules with
  | rule ->
      (* This is a nonterminal symbol. Test its %inline flag. *)
      rule.inline_flag
  | exception Not_found ->
      (* This is a terminal symbol. *)
      false

(* [find grammar symbol] looks up the definition of [symbol], which must be
   a valid nonterminal symbol. *)

let find grammar symbol : rule =
  try
    StringMap.find symbol grammar.rules
  with Not_found ->
    assert false

(* [find_inlining_site grammar (prefix, suffix)] traverses a list of producers
   that is already decomposed as [List.rev prefix @ suffix]. It looks for the
   first nonterminal symbol that can be inlined away. If it does not find one,
   it raises [NoInlining]. If it does find one, it returns a decomposition of
   the whole list of producers under the form [prefix @ producer :: suffix]. *)

type site =
  producers * producer * producers

let rec find_inlining_site grammar (prefix, suffix : producers * producers) : site option =
  match suffix with
  | [] ->
      None
  | producer :: suffix ->
      let symbol = producer_symbol producer in
      if is_inline_nonterminal grammar symbol then begin
        (* We have checked earlier than an %inline symbol does not carry
           any attributes. In addition, we now check that the use site of
           this symbol does not carry any attributes either. Thus, we need
           not worry about propagating these attributes through inlining. *)
        check_no_producer_attributes producer;
        Some (List.rev prefix, producer, suffix)
      end
      else
        find_inlining_site grammar (producer :: prefix, suffix)

(* Inline the branch [callee] into the branch [caller] at the site
   determined by [prefix, producer, suffix]. *)

let inline_branch caller (prefix, producer, suffix) (callee : branch) : branch =

  (* 2015/11/18. The interaction of %prec and %inline is not documented.
     It used to be the case that we would disallow marking a production
     both %inline and %prec. Now, we allow it, but we check that (1) it
     is inlined at the last position of the host production and (2) the
     host production does not already have a %prec annotation. *)
  callee.branch_prec_annotation |> Option.iter (fun callee_prec ->
    (* The callee has a %prec annotation. *)
    (* Check condition 1. *)
    if List.length suffix > 0 then begin
      let nt = producer_symbol producer in
      Error.error [ position callee_prec; caller.branch_position ]
        "this production carries a %%prec annotation,\n\
         and the nonterminal symbol %s is marked %%inline.\n\
         For this reason, %s can be used only in tail position."
        nt nt
    end;
    (* Check condition 2. *)
    caller.branch_prec_annotation |> Option.iter (fun caller_prec ->
      let nt = producer_symbol producer in
      Error.error [ position callee_prec; position caller_prec ]
        "this production carries a %%prec annotation,\n\
         and the nonterminal symbol %s is marked %%inline.\n\
         For this reason, %s cannot be used in a production\n\
         which itself carries a %%prec annotation."
        nt nt
    )
  );

  (* These are the names of the producers in the host branch,
     minus the producer that is being inlined away. *)
  let used = StringSet.union (names prefix) (names suffix) in
  (* Rename the producers of this branch if they conflict with
     the name of the host's producers. *)
  let phi, inlined_producers = rename used callee.producers in

  (* After inlining, the producers are as follows. *)
  let producers = prefix @ inlined_producers @ suffix in
  (* For debugging: check that each producer carries a unique name. *)
  let (_ : StringSet.t) = names producers in

  let index2id = index2id producers in

  let prefix = List.length prefix
  and inlined_producers = List.length inlined_producers in

  (* Define how the start and end positions of the inner production should
     be computed once it is inlined into the outer production. These
     definitions of [startp] and [endp] are then used to transform
     [$startpos] and [$endpos] in the inner production and to transform
     [$startpos(x)] and [$endpos(x)] in the outer production. *)

  (* 2015/11/04. We ensure that positions are computed in the same manner,
     regardless of whether inlining is performed. *)

  let startp =
    if inlined_producers > 0 then
      (* If the inner production is non-epsilon, things are easy. The start
         position of the inner production is the start position of its first
         element. *)
      RightNamed (index2id prefix), WhereStart
    else if prefix > 0 then
      (* If the inner production is epsilon, we are supposed to compute the
         end position of whatever comes in front of it. If the prefix is
         nonempty, then this is the end position of the last symbol in the
         prefix. *)
      RightNamed (index2id (prefix - 1)), WhereEnd
    else
      (* If the inner production is epsilon and the prefix is empty, then
         we need to look up the end position stored in the top stack cell.
         This is the reason why we need the keyword [$endpos($0)]. It is
         required in this case to preserve the semantics of $startpos and
         $endpos. *)
      Before, WhereEnd

    (* Note that, to contrary to intuition perhaps, we do NOT have that
       if the prefix is empty, then the start position of the inner
       production is the start production of the outer production.
       This is true only if the inner production is non-epsilon. *)

  in

  let endp =
    if inlined_producers > 0 then
      (* If the inner production is non-epsilon, things are easy: its end
         position is the end position of its last element. *)
      RightNamed (index2id (prefix + inlined_producers - 1)), WhereEnd
    else
      (* If the inner production is epsilon, then its end position is equal
         to its start position. *)
      startp

  in

  (* We must also transform [$endpos($0)] if it used by the inner
     production. It refers to the end position of the stack cell
     that comes before the inner production. So, if the prefix is
     non-empty, then it translates to the end position of the last
     element of the prefix. Otherwise, it translates to [$endpos($0)]. *)

  let beforeendp =
    if prefix > 0 then
      RightNamed (index2id (prefix - 1)), WhereEnd
    else
      Before, WhereEnd
  in

  (* [c] is the identifier under which the callee is known inside the caller. *)
  let c = producer_identifier producer in

  (* Rename the outer and inner semantic action. *)
  let outer_action =
    Action.rename (rename_sw_outer (c, startp, endp)) [] caller.action
  and action' =
    Action.rename (rename_sw_inner beforeendp) phi callee.action
  in

  (* 2015/11/18. If the callee has a %prec annotation (which implies
     the caller does not have one, and the callee appears in tail
     position in the caller) then the annotation is inherited. This
     seems reasonable, but remains undocumented. *)
  let branch_prec_annotation =
    match callee.branch_prec_annotation with
    | (Some _) as annotation ->
        assert (caller.branch_prec_annotation = None);
        annotation
    | None ->
        caller.branch_prec_annotation
  in

  { caller with
    producers;
    action = Action.compose c action' outer_action;
    branch_prec_annotation;
  }

(* Inline a list of branches [callees] into the branch [caller] at [site]. *)

let inline_branches caller site (callees : branches) : branches =
  List.map (inline_branch caller site) callees

(* Inline a grammar. The resulting grammar does not contain any definitions
   that can be inlined. *)
let inline grammar =

  (* Inline the non terminals that can be inlined in [caller]. We use the
     ListMonad to combine the results. *)
  let rec expand_branch expand_symbol (caller : branch) : branch ListMonad.m =
    match find_inlining_site grammar ([], caller.producers) with
    | None ->
        return caller
    | Some ((_prefix, producer, _suffix) as site) ->
        let symbol = producer_symbol producer in
        let rule = expand_symbol symbol in
        inline_branches caller site rule.branches >>= expand_branch expand_symbol
  in

  let expand_symbol expand_symbol symbol : rule =
    let rule = find grammar symbol in
    { rule with branches = rule.branches >>= expand_branch expand_symbol }
  in

  let expand_symbol : Syntax.symbol -> rule =
    Memoize.String.defensive_fix expand_symbol
  in

  let expand_symbol symbol =
    try
      expand_symbol symbol
    with Memoize.String.Cycle (_, symbol) ->
      let rule = find grammar symbol in
      Error.error rule.positions
        "there is a cycle in the definition of %s." symbol
      (* TEMPORARY we can now give a better message. *)
  in

  (* If we are in Coq mode, %inline is forbidden. *)
  let _ =
    if Settings.coq then
      StringMap.iter
        (fun _ r ->
           if r.inline_flag then
             Error.error r.positions
               "%%inline is not supported by the Coq back-end.")
        grammar.rules
  in

  (* To expand a grammar, we expand all its rules and remove
     the %inline rules. *)
  let rules =
    grammar.rules
    |> StringMap.filter (fun _ r -> not r.inline_flag)
    |> StringMap.mapi (fun symbol _rule -> expand_symbol symbol) (* a little wasteful, but simple *)
  in

  let useful (k : string) : bool =
    try
      not (StringMap.find k grammar.rules).inline_flag
    with Not_found ->
      true (* could be: assert false? *)
  in

  (* Remove %on_error_reduce declarations for symbols that are expanded away,
     and warn about them, at the same time. *)
  let useful_warn (k : string) : bool =
    let u = useful k in
    if not u then
      Error.grammar_warning []
        "the declaration %%on_error_reduce %s\n\
          has no effect, since this symbol is marked %%inline and is expanded away." k;
    u
  in

  { grammar with
      rules;
      types = StringMap.filter (fun k _ -> useful k) grammar.types;
      on_error_reduce = StringMap.filter (fun k _ -> useful_warn k) grammar.on_error_reduce;
  }
