module Make
  (I : MenhirLib.IncrementalEngine.INSPECTION)
  (User : sig
    val arrow: string (* should include space on both sides *)
    val dot: string
    val space: string
    val print_symbol: I.xsymbol -> string
  end)
: sig

  (* Printing an item. *)

  val buffer_item: Buffer.t -> I.item -> unit
  val print_item: I.item -> string

  (* Printing a production. *)

  val buffer_production: Buffer.t -> I.production -> unit
  val print_production: I.production -> string

end

