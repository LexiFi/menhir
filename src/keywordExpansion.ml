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

(* [define keyword1 f keyword2] macro-expands [keyword1] as [f(keyword2)],
   where [f] is a function of expressions to expressions. *)

let define keyword1 f keyword2 =
  Action.define
    keyword1
    (KeywordSet.singleton keyword2)
    (mlet
       [ PVar (posvar_ keyword1) ]
       [ f (EVar (posvar_ keyword2)) ])

(* [expand_action producers action] macro-expands certain keywords away
   in the semantic action [action]. The list [producers] tells us how
   many elements appear in this production. *)

let expand_action producers action =
  let n = List.length producers in
  KeywordSet.fold (fun keyword action ->
    match keyword with
    | Position (subject, where, FlavorOffset) ->

        (* The [ofs] keyword family is defined in terms of the [pos] family by
           accessing the [pos_cnum] field. *)
       define keyword
         (fun e -> ERecordAccess (e, "Lexing.pos_cnum"))
         (Position (subject, where, FlavorPosition))
         action

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

    | Position (Before, _, _)
    | Position (RightNamed _, _, _)
    | SyntaxError ->
        action
  ) (Action.keywords action) action

let expand_branch branch =
  { branch with action = expand_action branch.producers branch.action }

let expand_rule rule =
  { rule with branches = List.map expand_branch rule.branches }

let expand_grammar grammar =
  { grammar with rules = StringMap.map expand_rule grammar.rules }

