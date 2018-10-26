(* Token aliases are quoted strings that are used to provide syntactic sugar
   for terminal symbols, for example, to allow "+" to be used in grammar rules
   instead of PLUS, or to allow ")" instead of RPAREN. *)

(* This transformation eliminates all references to token aliases in a list of
   partial grammars. (An alias declared in one partial grammar can be used in
   another partial grammar.) Declarations of token aliases are preserved, and
   could be used if desired (e.g. for printing). *)

open Syntax

val dealias_grammars: partial_grammar list -> partial_grammar list
