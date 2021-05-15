open StackLangBasics

type t = value

val registers : t -> registers
(** [registers v] is the set of register that appear in value [v] *)

val registers_of_list : t list -> StringSet.t
(** [registers vs] is the set of register that appear in values [vs] *)