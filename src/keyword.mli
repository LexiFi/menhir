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

(* This module provides some type and function definitions
   that help deal with the keywords that we recognize within
   semantic actions. *)

(* The user can request position information either at type
   [int] (a simple offset) or at type [Lexing.position]. *)

type flavor =
  | FlavorOffset
  | FlavorPosition

(* The user can request position information about the $start or $end
   of a symbol. Also, $symbolstart requests the computation of the
   start position of the first nonempty element in a production. *)

type where =
  | WhereSymbolStart
  | WhereStart
  | WhereEnd

(* The user can request position information about a production's
   left-hand side or about one of the symbols in its right-hand
   side, which he must refer to by name. (Referring to its symbol
   by its position, using [$i], is permitted in the concrete
   syntax, but the lexer eliminates this form.)

   We add a new subject, [Before], which corresponds to [$endpos($0)]
   in concrete syntax. We adopt the (slightly awkward) convention that
   when the subject is [Before], the [where] component must be [WhereEnd]. *)

type subject =
  | Before
  | Left
  | RightNamed of string

(* Keywords inside semantic actions. They allow access to semantic
   values or to position information. *)

type keyword =
  | Position of subject * where * flavor
  | SyntaxError

(* This maps a [Position] keyword to the name of the variable that the
   keyword is replaced with. *)

val posvar: subject -> where -> flavor -> string

(* Sets of keywords. *)

module KeywordSet : sig
  include Set.S with type elt = keyword
  val map: (keyword -> keyword) -> t -> t
end
