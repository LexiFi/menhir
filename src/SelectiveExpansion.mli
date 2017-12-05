open Syntax
open SortInference

(* [expand sorts g] expands away some or all of the parameterized
   nonterminal symbols in the grammar [g], producing a new grammar.
   [sorts] is the sort environment produced by [SortInference]. *)

(* At this time, expansion is complete: all parameters are expanded
   away. *)

val expand: sorts -> grammar -> grammar
