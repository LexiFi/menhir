open StackLang

type t = program

val map : (typed_block -> typed_block) -> t -> t

val mapi : (label -> typed_block -> typed_block) -> t -> t

val iter : (label -> typed_block -> unit) -> t -> unit

val filter : (label -> typed_block -> bool) -> t -> t
