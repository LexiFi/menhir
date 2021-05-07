open StackLang

(** [optimize program] perform optimization on program and return a transformed
    version with the same semantic.
    The specific nature of the optimizations depend on the [Settings] module. *)
val optimize: program -> program

val test: unit -> unit