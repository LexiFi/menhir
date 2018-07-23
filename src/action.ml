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

open Keyword

type t = {

  (* The code for this semantic action. *)
  expr: IL.expr;

  (* The files where this semantic action originates. Via inlining,
     several semantic actions can be combined into one, so there can
     be several files. *)
  filenames: string list;

  (* The set of keywords that appear in this semantic action. They can be thought
     of as free variables that refer to positions. They must be renamed during
     inlining. *)
  keywords  : keywords;

}

let keyword_best_position pos1 pos2 =
  let l1 = Positions.start_of_position pos1 in
  let l2 = Positions.start_of_position pos2 in
  if l1 = Lexing.dummy_pos || l2.Lexing.pos_cnum > l1.Lexing.pos_cnum
  then pos2
  else pos1

let keyword_add keyword pos keywords =
  match KeywordMap.find keyword keywords with
  | pos' ->
    KeywordMap.add keyword (keyword_best_position pos pos') keywords
  | exception Not_found ->
    KeywordMap.add keyword pos keywords

let keyword_union _keyword pos1 pos2 =
  Some (keyword_best_position pos1 pos2)

let keywords_union k1 k2 = KeywordMap.union keyword_union k1 k2

(* Creation. *)

let from_stretch s = {
  expr      = IL.ETextual s;
  filenames = [ s.Stretch.stretch_filename ];
  keywords  =
    List.fold_left
      (fun keywords (keyword, pos) -> keyword_add keyword pos keywords)
      KeywordMap.empty s.Stretch.stretch_keywords
}

(* Defining a keyword in terms of other keywords. *)

let define keyword keywords f action =
  assert (KeywordMap.mem keyword action.keywords);
  { action with
    expr     = f action.expr;
    keywords = keywords_union keywords
                 (KeywordMap.remove keyword action.keywords)
  }

(* Composition, used during inlining. *)

let compose x a1 a2 =
  (* 2015/07/20: there used to be a call to [parenthesize_stretch] here,
     which would insert parentheses around every stretch in [a1]. This is
     not necessary, as far as I can see, since every stretch that represents
     a semantic action is already parenthesized by the lexer. *)
  {
    expr      = IL.ELet ([ IL.PVar x, a1.expr ], a2.expr);
    keywords  = keywords_union a1.keywords a2.keywords;
    filenames = a1.filenames @ a2.filenames;
  }

(* Substitutions, represented as association lists.
   In principle, no name appears twice in the domain. *)

type subst =
  (string * string) list

let apply (phi : subst) (s : string) : string =
  try
    List.assoc s phi
  with Not_found ->
    s

let apply_subject (phi : subst) (subject : subject) : subject =
  match subject with
  | Before
  | Left ->
      subject
  | RightNamed s ->
      RightNamed (apply phi s)

let extend x y (phi : subst ref) =
  assert (not (List.mem_assoc x !phi));
  if x <> y then
    phi := (x, y) :: !phi

(* Renaming of keywords, used during inlining. *)

type sw =
  Keyword.subject * Keyword.where

(* [rename_keyword f phi keyword] applies the function [f] to possibly change
   the keyword [keyword]. If [f] decides to change this keyword (by returning
   [Some _]) then this decision is obeyed. Otherwise, the keyword is renamed
   by the substitution [phi]. In either case, [phi] is extended with a
   renaming decision. *)

let rename_keyword (f : sw -> sw option) (phi : subst ref) keyword : keyword =
  match keyword with
  | SyntaxError ->
      SyntaxError
  | Position (subject, where, flavor) ->
      let subject', where' =
        match f (subject, where) with
        | Some (subject', where') ->
            subject', where'
        | None ->
            apply_subject !phi subject, where
      in
      extend
        (Keyword.posvar subject where flavor)
        (Keyword.posvar subject' where' flavor)
        phi;
      Position (subject', where', flavor)

(* [rename f phi a] applies to the semantic action [a] the renaming [phi] as
   well as the transformations decided by the function [f]. The function [f] is
   applied to each (not-yet-renamed) keyword and may decide to transform it, by
   returning [Some _], or to not transform it, by returning [None]. (In the
   latter case, [phi] still applies to the keyword.) *)

let rename f phi a =

  (* Rename all keywords, growing [phi] as we go. *)
  let keywords = a.keywords in
  let phi = ref phi in
  let keywords = KeywordMap.map_keyword (rename_keyword f phi) keywords in
  let phi = !phi in

  (* Construct a new semantic action, where [phi] is translated into
     a series of [let] bindings. *)
  let phi = List.map (fun (x, y) -> IL.PVar x, IL.EVar y) phi in
  let expr = IL.ELet (phi, a.expr) in

  {
    expr      = expr;
    filenames = a.filenames;
    keywords  = keywords;
  }

let to_il_expr action =
  action.expr

let filenames action =
  action.filenames

let keywords action =
  action.keywords

let keyword_position action keyword =
  KeywordMap.find keyword action.keywords

let has_syntaxerror action =
  KeywordMap.mem SyntaxError (keywords action)

let has_beforeend action =
  KeywordMap.mem (Position (Before, WhereEnd, FlavorPosition)) action.keywords
