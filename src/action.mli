(** Semantic action's type. *)
type t

(** [compose x a1 a2] builds the action [let x = a1 in a2]. This
    feature is used during the processing of the %inline keyword. *)
val compose : string -> t -> t -> t

(* TEMPORARY document this: *)

type subst =
  (string * string) list

type sw =
  Keyword.subject * Keyword.where

(** [rename f phi a] applies to the semantic action [a] the renaming [phi] as
    well as the transformations decided by the function [f]. The function [f] is
    applied to each (not-yet-renamed) keyword and may decide to transform it, by
    returning [Some _], or to not transform it, by returning [None]. (In the
    latter case, [phi] still applies to the keyword.) *)
val rename:
  (sw -> sw option) ->
  subst ->
  t ->
  t

(** Semantic actions are translated into [IL] code using the
    [IL.ETextual] and [IL.ELet] constructors. *)
val to_il_expr: t -> IL.expr

(** A semantic action might be the inlining of several others. The
    filenames of the different parts are given by [filenames a]. This
    can be used, for instance, to check whether all parts come from
    the standard library. *)
val filenames: t -> string list

(** [pkeywords a] returns a list of all keyword occurrences in [a]. *)
val pkeywords: t -> Keyword.keyword Positions.located list

(** [keywords a] is the set of keywords used in the semantic action [a]. *)
val keywords: t -> Keyword.KeywordSet.t

(** [print f a] prints [a] to channel [f]. *)
val print: out_channel -> t -> unit

(** [from_stretch s] builds an action out of a textual piece of code. *)
val from_stretch: Stretch.t -> t

(** Check whether the keyword $syntaxerror is used in the action. *)
val has_syntaxerror: t -> bool

(** Check whether the keyword $start is used in the action. *)
val has_leftstart: t -> bool

(** Check whether the keyword $end is used in the action. *)
val has_leftend: t -> bool

