open Syntax

(* The new rule syntax is desugared to the old rule syntax.
   The translation exploits anonymous rules, so it must be
   performed before anonymous rules are eliminated. *)

val rule: rule -> parameterized_rule
