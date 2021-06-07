open StackLangBasics

(** [registers p] is the set of registers defined by the pattern [p]. *)
val registers : pattern -> registers

(** [restrict regs p] replace every register in [p] that is not in [regs] by a
    wildcard *)
val restrict : registers -> pattern -> pattern