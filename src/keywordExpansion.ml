open UnparameterizedSyntax
open Keyword
open IL
open CodeBits

(* [posvar_ keyword] constructs the conventional name of the variable
   that stands for the position keyword [keyword]. *)

let posvar_ = function
  | Position (subject, where, flavor) ->
      posvar subject where flavor
  | _ ->
      assert false (* [posvar_] should be applied to a position keyword *)

(* [symbolstartpos producers i n] constructs an expression which, beginning at
   index [i], looks for the first non-empty producer and returns its start
   position. If none is found, this expression returns the end position of the
   right-hand side. This computation is modeled after the function
   [Parsing.symbol_start_pos] in OCaml's standard library. *)

(* This cascade of [if] constructs may be quite big, so in terms of code size,
   it is not great. If we knew, at this point, which symbols are nullable and
   which symbols generate the singleton language {epsilon}, then we could
   optimize this code by computing, ahead of time, the outcome of certain
   comparisons. (That is, assuming a token cannot have the same start and end
   positions.) Unfortunately, at this point, (before inlining,) we do not have
   this information yet. *)

(* Although this code is modeled after [Parsing.symbol_start_pos], we compare
   positions using physical equality, whereas they use structural equality. If
   for some reason a symbol has start and end positions that are structurally
   equal but physically different, then a difference will be observable.
   However, this is very unlikely. It would mean that a token has the same start
   and end positions (and furthermore, this position has been re-allocated). *)

(* The reason why we expand [$symbolstartpos] away prior to inlining is that we
   want its meaning to be preserved by inlining. If we tried to preserve this
   keyword through the inlining phase, then (I suppose) we would have to introduce
   a family of keywords [$symbolstartpos(i, j)], computing over the interval from
   [i] to [j], and the preservation would not be exact -- because a nonempty
   symbol, once inlined, can be seen to be a sequence of empty and nonempty
   symbols. *)

let rec symbolstartpos producers i n : IL.expr * KeywordSet.t =
  if i = n then
    (* Return [$endpos]. *)
    let keyword = Position (Left, WhereEnd, FlavorPosition) in
    EVar (posvar_ keyword), KeywordSet.singleton keyword
  else
    (* Compare [$startpos($i)] and [$endpos($i)]. If they differ, return
       [$startpos($i)]. Otherwise, continue. *)
    let _, x = List.nth producers i in
    let startp = Position (RightNamed x, WhereStart, FlavorPosition)
    and   endp = Position (RightNamed x, WhereEnd,   FlavorPosition) in
    let continue, keywords = symbolstartpos producers (i + 1) n in
    EIfThenElse (
      EApp (EVar "Pervasives.(!=)", [ EVar (posvar_ startp); EVar (posvar_ endp) ]),
      EVar (posvar_ startp),
      continue
    ),
    KeywordSet.add startp (KeywordSet.add endp keywords)

(* [define keyword1 f keyword2] macro-expands [keyword1] as [f(keyword2)],
   where [f] is a function of expressions to expressions. *)

let define keyword1 f keyword2 =
  Action.define
    keyword1
    (KeywordSet.singleton keyword2)
    (mlet
       [ PVar (posvar_ keyword1) ]
       [ f (EVar (posvar_ keyword2)) ])

(* An [ofs] keyword is expanded away. It is defined in terms of the
   corresponding [pos] keyword. *)

let expand_ofs keyword action =
  match keyword with
  | Position (subject, where, FlavorOffset) ->
      define keyword
        (fun e -> ERecordAccess (e, "Lexing.pos_cnum"))
        (Position (subject, where, FlavorPosition))
        action
  | _ ->
      action

(* [$symbolstartpos] is expanded into a cascade of [if] constructs, modeled
   after [Parsing.symbol_start_pos]. *)

let expand_symbolstartpos producers n keyword action =
  match keyword with
  | Position (Left, WhereSymbolStart, FlavorPosition) ->
      let expansion, keywords = symbolstartpos producers 0 n in
      Action.define keyword keywords
        (mlet [ PVar (posvar_ keyword) ] [ expansion ])
        action
  | Position (RightNamed _, WhereSymbolStart, FlavorPosition) ->
      (* [$symbolstartpos(x)] does not exist. *)
      assert false
  | _ ->
      action

(* [$startpos] and [$endpos] are expanded away.  *)

let expand_startend producers n keyword action =
  match keyword with
  | Position (Left, WhereStart, flavor) ->

      (* [$startpos] is defined as [$startpos($1)] if this production has
         nonzero length and [$endpos($0)] otherwise. *)
      define keyword (fun e -> e) (
        if n > 0 then
          let _, x = List.hd producers in
          Position (RightNamed x, WhereStart, flavor)
        else
          Position (Before, WhereEnd, flavor)
      ) action

  | Position (Left, WhereEnd, flavor) ->

      (* [$endpos] is defined as [$endpos($n)] if this production has
         nonzero length and [$endpos($0)] otherwise. *)
      define keyword (fun e -> e) (
        if n > 0 then
          let _, x = List.hd (List.rev producers) in
          Position (RightNamed x, WhereEnd, flavor)
        else
          Position (Before, WhereEnd, flavor)
      ) action

  | _ ->
      action

(* [expand_round] performs one round of expansion on [action], using [f] as a
   rewriting rule. *)

let expand_round f action =
  KeywordSet.fold f (Action.keywords action) action

(* [expand_action] performs macro-expansion in [action]. We do this in several
   rounds: first, expand the [ofs] keywords away; then, expand [symbolstart]
   away; then, expand the rest. We do this in this order because each round
   can cause new keywords to appear, which must eliminated by the following
   rounds. *)

let expand_action producers action =
  let n = List.length producers in

  (* The [ofs] keyword family is defined in terms of the [pos] family by
     accessing the [pos_cnum] field. Expand these keywords away first. *)

  let action = expand_round expand_ofs action in

  (* Expand [$symbolstartpos] away. *)

  let action = expand_round (expand_symbolstartpos producers n) action in

  (* Then, expand away the non-[ofs] keywords. *)

  let action = expand_round (expand_startend producers n) action in

  action

let expand_branch branch =
  { branch with action = expand_action branch.producers branch.action }

let expand_rule rule =
  { rule with branches = List.map expand_branch rule.branches }

let expand_grammar grammar =
  { grammar with rules = StringMap.map expand_rule grammar.rules }

