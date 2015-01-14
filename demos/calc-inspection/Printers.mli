module Make
  (I : MenhirLib.IncrementalEngine.EVERYTHING)
  (User : sig
    val arrow: string (* should include space on both sides *)
    val dot: string
    val space: string
    val print_symbol: I.xsymbol -> string
    val print_element: (I.element -> string) option
  end)
: sig

  (* Printing an element as a symbol. This prints just the symbol
     that this element represents; nothing more. *)

  val buffer_element_as_symbol: Buffer.t -> I.element -> unit
  val print_element_as_symbol: I.element -> string

  (* Printing a stack or an environment. These functions need an element
     printer. They use [print_element] if provided by the user; otherwise
     they use [print_element_as_symbol]. *)

  val buffer_stack: Buffer.t -> I.element I.stream -> unit
  val print_stack: I.element I.stream -> string

  val buffer_env: Buffer.t -> I.env -> unit
  val print_env: I.env -> string

  (* Printing an item. *)

  val buffer_item: Buffer.t -> I.item -> unit
  val print_item: I.item -> string

  (* Printing a production. *)

  val buffer_production: Buffer.t -> I.production -> unit
  val print_production: I.production -> string

end

