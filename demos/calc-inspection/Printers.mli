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

  open I

  (* Printing a list of symbols. *)

  val print_symbols: xsymbol list -> unit

  (* Printing an element as a symbol. This prints just the symbol
     that this element represents; nothing more. *)

  val print_element_as_symbol: element -> unit

  (* Printing a stack as a list of elements. This function needs an element
     printer. It uses [print_element] if provided by the user; otherwise
     it uses [print_element_as_symbol]. (Ending with a newline.) *)

  val print_stack: stack -> unit

  (* Printing an item. (Ending with a newline.) *)

  val print_item: item -> unit

  (* Printing a production. (Ending with a newline.) *)

  val print_production: production -> unit

  (* Printing the current LR(1) state. The current state is first displayed
     as a number; then the list of its LR(0) items is printed. (Ending with
     a newline.) *)

  val print_current_state: env -> unit

  (* Printing a summary of the stack and current state. This function just
     calls [print_stack] and [print_current_state] in succession. *)

  val print_env: env -> unit

end

