(* Build and print the forward reference graph of the grammar. There is an edge
   of a nonterminal symbol [nt1] to every nonterminal symbol [nt2] that occurs
   in the definition of [nt1]. *)

val print_dependency_graph: unit -> unit

