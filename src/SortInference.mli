open Syntax
open SortUnification

(* [infer_grammar g] performs sort inference for the grammar [g],
   rejecting the grammar if it is ill-sorted. It returns a map of
   (terminal and nonterminal) symbols to ground sorts. *)

val infer_grammar: grammar -> ground_sort StringMap.t
