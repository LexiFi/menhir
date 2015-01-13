module Make
  (I : MenhirLib.IncrementalEngine.EVERYTHING)
  (User : sig
    val arrow: string (* should include space on both sides *)
    val dot: string
    val space: string
    val print_symbol: I.xsymbol -> string
  end)
: sig

  (* Printing an element as a symbol. This prints just the symbol
     that this element represents; nothing more. *)

  val buffer_element_as_symbol: Buffer.t -> I.element -> unit
  val print_element_as_symbol: I.element -> string

  (* Printing a stack or an environment. These functions are parameterized
     over an element printer. [print_element_as_symbol] can be used for
     this purpose; but the user can define other printers if desired. *)

  val buffer_stack: (Buffer.t -> I.element -> unit) -> Buffer.t -> I.element I.stream -> unit
  val print_stack: (I.element -> string) -> I.element I.stream -> string

  val buffer_env: (Buffer.t -> I.element -> unit) -> Buffer.t -> I.env -> unit
  val print_env: (I.element -> string) -> I.env -> string

  (* Printing an item. *)

  val buffer_item: Buffer.t -> I.item -> unit
  val print_item: I.item -> string

  (* Printing a production. *)

  val buffer_production: Buffer.t -> I.production -> unit
  val print_production: I.production -> string

end

