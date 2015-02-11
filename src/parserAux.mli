(* This module provides utilities that are shared by the two versions
   of the parser. *)

open Syntax

(* TEMPORARY document *)

val current_token_precedence: Lexing.position -> Lexing.position -> precedence_level
val current_reduce_precedence: unit -> precedence_level

(* [check_disjunctive_production] accepts production group and checks
   that they all productions in the group define the same set of
   identifiers. It also checks that the semantic action does not use
   any [$i] keyword. *)

val check_production_group:
  (producer list * 'a * 'b * 'c) list ->
  Lexing.position -> Lexing.position -> Action.t -> unit

(* [override pos oprec1 oprec2] decides which of the two optional
   %prec declarations [oprec1] and [oprec2] applies to a
   production. It signals an error if the two are present. *)

val override: Positions.t -> 'a option -> 'a option -> 'a option

(* Support for on-the-fly expansion of anonymous rules. When such a
   rule is encountered, invoke [anonymous], which creates a fresh
   non-terminal symbol, records the definition of this symbol to a
   global variable, and returns this symbol. In the end, invoke
   [rules], so as to obtain a list of all recorded definitions. *)

val anonymous: Positions.t -> parameterized_branch list -> string
val rules: unit -> parameterized_rule list
