module Make

  (I : MenhirLib.IncrementalEngine.EVERYTHING)

  (User : sig

    (* [print s] is supposed to send the string [s] to some output channel. *)

    val print: string -> unit

    (* [print_symbol s] is supposed to print a representation of the symbol [s]. *)

    val print_symbol: I.xsymbol -> unit

    (* [print_element e] is supposed to print a representation of the element [e].
       This function is optional; if it is not provided, [print_element_as_symbol]
       (defined below) is used instead. *)

    val print_element: (I.element -> unit) option

  end)

: sig

  (* Printing a list of symbols. *)

  val print_symbols: I.xsymbol list -> unit

  (* Printing an element as a symbol. This prints just the symbol
     that this element represents; nothing more. *)

  val print_element_as_symbol: I.element -> unit

  (* Printing a stack as a list of elements. This function needs an element
     printer. It uses [print_element] if provided by the user; otherwise
     it uses [print_element_as_symbol]. *)

  val print_stack: I.stack -> unit

  (* Printing an item. (Ending with a newline.) *)

  val print_item: I.item -> unit

  (* Printing a production. (Ending with a newline.) *)

  val print_production: I.production -> unit

  (* Printing the current LR(1) state. The current state is first displayed
     as a number; then the list of its LR(0) items is printed. (Ending with
     a newline.) *)

  val print_current_state: I.env -> unit

  (* TEMPORARY move and document *)
  val past: 'a I.lr1state -> I.xsymbol list

end

