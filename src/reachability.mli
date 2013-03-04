(* This extremely simple analysis restricts a grammar to the set
   of nonterminals that are reachable, via productions, from the
   start nonterminals. *)

val trim: UnparameterizedSyntax.grammar -> UnparameterizedSyntax.grammar

