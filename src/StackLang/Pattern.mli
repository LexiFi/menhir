open StackLangBasics

type t = pattern

(** [Pattern.registers p] is the set of registers defined by pattern [p] *)
val registers : t -> registers
