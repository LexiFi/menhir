module Make
  (E : MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE)
  (I : MenhirLib.IncrementalEngine.INSPECTION
   with type 'a lr1state = 'a E.lr1state)
  (User : sig
    val arrow: string (* should include space on both sides *)
    val dot: string
    val space: string
    val print_symbol: I.xsymbol -> string
  end)
: sig

  (* Printing an element as a symbol. This prints just the symbol
     that this element represents; nothing more. *)

  val buffer_element_as_symbol: Buffer.t -> E.element -> unit
  val print_element_as_symbol: E.element -> string

  (* Printing a stack or an environment. These functions are parameterized
     over an element printer. [print_element_as_symbol] can be used for
     this purpose; but the user can define other printers if desired. *)

  val buffer_stack: (Buffer.t -> E.element -> unit) -> Buffer.t -> E.element E.stream -> unit

  (* Printing an item. *)

  val buffer_item: Buffer.t -> I.item -> unit
  val print_item: I.item -> string

  (* Printing a production. *)

  val buffer_production: Buffer.t -> I.production -> unit
  val print_production: I.production -> string

end

