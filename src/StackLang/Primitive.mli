open StackLang

type t = primitive

val registers : t -> registers

val action : ?bindings:bindings -> action -> t
