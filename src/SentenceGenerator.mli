open Grammar

(** [sentence ~log nt budget] is a sentence generated by non terminal [nt] of
    length at most [budget]

    This sentence is valid according to the grammar, but is not necessarily
    accepted by the LR(1) automaton, because the construction of the automaton
    takes precedence declarations into account, which (strictly speaking) are
    not part of the grammar.
*)
val sentence: ?log:bool -> Nonterminal.t -> int -> Terminal.t array