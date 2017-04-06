open Grammar

(* [has_default_reduction s] tells whether state [s] has a default reduction,
   and, if so, upon which set of tokens. *)

val has_default_reduction : Lr1.node -> (Production.index * TerminalSet.t) option
