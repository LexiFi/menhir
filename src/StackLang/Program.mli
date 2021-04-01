
type t = StackLang.program

val map: (StackLang.typed_block -> StackLang.typed_block) -> t -> t

val iter: (StackLang.register -> StackLang.typed_block -> unit) -> t -> unit