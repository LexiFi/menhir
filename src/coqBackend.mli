(* The coq code generator. *)

module Run (T: sig end) : sig

  val write_all: out_channel -> unit

end
