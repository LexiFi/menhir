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

  (* This Boolean flag indicates whether this semantic action originates from
     Menhir's standard library. Via inlining, several semantic actions can be
     combined into one; in that case, we take a conjunction. *)
  standard: bool;

  (* The set of keywords that appear in this semantic action. They can be
     thought of as free variables that refer to positions. They must be
     renamed during inlining. *)
  keywords  : KeywordSet.t;

}

(* -------------------------------------------------------------------------- *)

(* Constructors. *)

let from_stretch s =
  {
    expr      = IL.ETextual s;
    standard  = s.Stretch.stretch_filename = Settings.stdlib_filename;
    keywords  = KeywordSet.of_list s.Stretch.stretch_keywords
  }

let from_il_expr e =
  {
    expr      = e;
    standard  = true;
    keywords  = KeywordSet.empty;
  }

(* -------------------------------------------------------------------------- *)

(* Composition. *)

let compose x a1 a2 =
  (* 2015/07/20: there used to be a call to [parenthesize_stretch] here,
     which would insert parentheses around every stretch in [a1]. This is
     not necessary, as far as I can see, since every stretch that represents
     a semantic action is already parenthesized by the lexer. *)
  {
    expr      = CodeBits.blet ([ IL.PVar x, a1.expr ], a2.expr);
    keywords  = KeywordSet.union a1.keywords a2.keywords;
    standard  = a1.standard && a2.standard;
  }

(* Building [let p = x in a]. *)

let bind p x a =
  {
    expr      = CodeBits.blet ([ p, IL.EVar x ], a.expr);
    keywords  = a.keywords;
    standard  = a.standard;
  }

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let to_il_expr action =
  action.expr

let is_standard action =
  action.standard

let keywords action =
  action.keywords

let has_syntaxerror action =
  KeywordSet.mem SyntaxError action.keywords

let has_beforeend action =
  KeywordSet.mem (Position (Before, WhereEnd, FlavorPosition)) action.keywords

(* -------------------------------------------------------------------------- *)

(* Defining a keyword in terms of other keywords. *)

let define keyword keywords f action =
  assert (KeywordSet.mem keyword action.keywords);
  { action with
    expr     = f action.expr;
    keywords = KeywordSet.union
                 keywords (KeywordSet.remove keyword action.keywords)
  }

(* -------------------------------------------------------------------------- *)

(* Simultaneous substitutions are represented as association lists, where no
   name appears twice in the domain. *)

type subst =
  (string * string) list

let empty =
  []

let extend x y phi =
  assert (not (List.mem_assoc x phi));
  if x <> y then (x, y) :: phi else phi

let apply (phi : subst) (x : string) : string =
  try List.assoc x phi with Not_found -> x

let apply_subject (phi : subst) (subject : subject) : subject =
  match subject with
  | Before
  | Left ->
      subject
  | RightNamed s ->
      RightNamed (apply phi s)

(* -------------------------------------------------------------------------- *)

(* [rename_keyword f phi keyword] applies [f] to possibly transform the
   keyword [keyword]. If [f] decides to change this keyword (by returning
   [Some _]) then this decision is obeyed. Otherwise, the keyword is renamed
   by the substitution [phi]. In either case, [phi] is extended with a
   renaming decision. *)

let rename_keyword f (phi : subst ref) keyword : keyword =
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
      phi :=
        extend
          (Keyword.posvar subject where flavor)
          (Keyword.posvar subject' where' flavor)
          !phi;
      Position (subject', where', flavor)

let rename f phi a =

  (* Rename all keywords, growing [phi] as we go. *)
  let keywords = a.keywords in
  let phi = ref phi in
  let keywords = KeywordSet.map (rename_keyword f phi) keywords in
  let phi = !phi in

  let standard = a.standard in

  (* Construct a new semantic action, where [phi] is translated into
     a set of *simultaneous* [let] bindings. (We cannot use a series
     of nested [let] bindings, as that would cause a capture if the
     domain and codomain of [phi] have a nonempty intersection.) *)
  let phi = List.map (fun (x, y) -> IL.PVar x, IL.EVar y) phi in
  let expr = CodeBits.eletand (phi, a.expr) in

  { expr; standard; keywords }
