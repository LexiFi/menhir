(* This module drives the first half of the front-end. It opens and
   parses the input files, which yields a number of partial
   grammars. It joins these grammars, expands them to get rid of
   parameterized nonterminals, and performs reachability
   analysis. This yields a single unified grammar.

   More transformations over this grammar are performed in the second
   half of the front-end, which is driven by [Front]. The modules
   [PreFront] and [Front] are separated because it is convenient to
   insert auxiliary modules, such as [TokenType] and [Infer], in
   between the two.  *)

val grammar: UnparameterizedSyntax.grammar

