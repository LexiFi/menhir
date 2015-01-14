module Make
  (I : MenhirLib.IncrementalEngine.EVERYTHING)
  (User : sig
    val print: string -> unit
    val print_symbol: I.xsymbol -> unit
    val print_element: (I.element -> unit) option
  end)
= struct

  let arrow = " -> "
  let dot = "."
  let space = " "

  open User

  (* Printing a list of symbols. An optional dot is printed at offset
     [i] into the list [symbols], if this offset lies between [0] and
     the length of the list (included). *)

  let rec print_symbols i symbols =
    if i = 0 then begin
      print dot;
      print space;
      print_symbols (-1) symbols
    end
    else begin
      match symbols with
      | [] ->
          ()
      | symbol :: symbols ->
          print_symbol symbol;
          print space;
          print_symbols (i - 1) symbols
    end

  (* Printing an item. *)

  let print_item (prod, i) =
    print_symbol (I.lhs prod);
    print arrow;
    print_symbols i (I.rhs prod)

  (* Printing a production (without a dot). *)

  let print_production prod =
    print_item (prod, -1)

  (* Printing an element as a symbol. *)

  let print_element_as_symbol element =
    match element with
    | I.Element (s, _, _, _) ->
        print_symbol (I.X (I.incoming_symbol s))

  (* Some of the functions that follow need an element printer. They use
     [print_element] if provided by the user; otherwise they use
     [print_element_as_symbol]. *)

  let print_element =
    match print_element with
    | Some print_element ->
        print_element
    | None ->
        print_element_as_symbol

  (* Printing a stack as a list of symbols. *)

  let print_stack stack =
    I.foldr (fun element () ->
      print_element element;
      print space
    ) stack ()

end

