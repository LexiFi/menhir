open StackLangBasics

type t = value

val registers : t -> registers
(** Returns the set of register that appear in a value *)
