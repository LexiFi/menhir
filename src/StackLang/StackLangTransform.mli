open StackLang

val optimize : program -> program
(** [optimize program] perform optimization on program and return a transformed
    version with the same semantic.
    The specific nature of the optimizations depend on the [Settings] module. *)

val test : unit -> unit
