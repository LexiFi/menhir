(* A debugging pretty-printer for [IL]. Newlines are used liberally, so as to
   facilitate diffs. *)

module Make (X : sig

  (* This is the channel that is being written to. *)

  val f: out_channel

end) : sig

  val expr: IL.expr -> unit

end

