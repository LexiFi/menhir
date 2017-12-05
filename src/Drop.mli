(* This function translates a grammar from the [Syntax] format
   to the [UnparameterizedSyntax] format. Naturally, the grammar
   must not have any parameterized symbols, since these are not
   allowed by the latter format. *)

val drop: Syntax.grammar -> UnparameterizedSyntax.grammar
