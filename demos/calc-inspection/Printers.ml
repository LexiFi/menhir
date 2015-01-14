module Make
  (I : MenhirLib.IncrementalEngine.EVERYTHING)
  (User : sig
    val arrow: string (* should include space on both sides *)
    val dot: string
    val space: string
    val print_symbol: I.xsymbol -> string
    val print_element: (I.element -> string) option
  end)
= struct

  open User

  (* Buffer and string utilities. *)

  let out =
    Buffer.add_string

  let with_buffer f x =
    let b = Buffer.create 128 in
    f b x;
    Buffer.contents b

  let into_buffer f b x =
    out b (f x)

  (* Printing a list of symbols. An optional dot is printed at offset
     [i] into the list [symbols], if this offset lies between [0] and
     the length of the list (included). *)

  let rec buffer_symbols b i symbols =
    if i = 0 then begin
      out b dot;
      out b space;
      buffer_symbols b (-1) symbols
    end
    else begin
      match symbols with
      | [] ->
          ()
      | symbol :: symbols ->
          out b (print_symbol symbol);
          out b space;
          buffer_symbols b (i - 1) symbols
    end

  (* Printing an item. *)

  let buffer_item b (prod, i) =
    out b (print_symbol (I.lhs prod));
    out b arrow;
    buffer_symbols b i (I.rhs prod)

  (* Printing a production (without a dot). *)

  let buffer_production b prod =
    buffer_item b (prod, -1)

  let print_item =
    with_buffer buffer_item

  let print_production =
    with_buffer buffer_production

  (* Printing an element as a symbol. *)

  let print_element_as_symbol element =
    match element with
    | I.Element (s, _, _, _) ->
        print_symbol (I.X (I.incoming_symbol s))

  let buffer_element_as_symbol =
    into_buffer print_element_as_symbol

  (* Some of the functions that follow need an element printer. They use
     [print_element] if provided by the user; otherwise they use
     [print_element_as_symbol]. *)

  let print_element =
    match print_element with
    | Some print_element ->
        print_element
    | None ->
        print_element_as_symbol

  let buffer_element =
    into_buffer print_element

  (* Printing a stack or an environment. *)

  let buffer_stack b stack =
    I.foldr (fun element () ->
      buffer_element b element;
      out b space
    ) stack ()

  let buffer_env b env =
    buffer_stack b (I.stack env)

  let print_stack stack =
    with_buffer buffer_stack stack

  let print_env env =
    with_buffer buffer_env env

end

