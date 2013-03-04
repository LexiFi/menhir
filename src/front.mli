(* This module drives the second half of the front-end. It starts
   where [PreFront] left off, and performs type inference. This yields
   the grammar that the back-end works with (through the interface
   provided by module [Grammar]). *)

val grammar: UnparameterizedSyntax.grammar

