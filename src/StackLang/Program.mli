open StackLang

type t = program

val map : (typed_block -> typed_block) -> t -> t

val mapi : (register -> typed_block -> typed_block) -> t -> t

val iter : (register -> typed_block -> unit) -> t -> unit

val filter : (register -> typed_block -> bool) -> t -> t
