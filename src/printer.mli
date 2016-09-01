(* A pretty-printer for [IL]. *)

module Make (X : sig

  (* This is the channel that is being written to. *)

  val f: out_channel

  (* This controls the way we print OCaml stretches (types and
     semantic actions). We either surround them with #line directives
     (for better error reports if the generated code is ill-typed) or
     don't (for better readability). The value is either [None] -- do
     not provide #line directives -- or [Some filename] -- do provide
     them. [filename] is the name of the file that is being written. *)

  val locate_stretches: string option

end) : sig

  val program: IL.program -> unit

  val expr: IL.expr -> unit

  val interface: IL.interface -> unit

end

(* Common instantiations. *)

val print_expr: out_channel -> IL.expr -> unit (* no #line directives *)
