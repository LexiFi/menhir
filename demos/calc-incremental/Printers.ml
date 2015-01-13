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
    | E.Element (s, _, _, _) ->
        print_symbol (I.X (I.incoming_symbol s))

  let buffer_element_as_symbol =
    into_buffer print_element_as_symbol

  (* Folding over a stream. *)

  let rec foldr f xs accu =
    match Lazy.force xs with
    | E.Nil ->
        accu
    | E.Cons (x, xs) ->
        f x (foldr f xs accu)

  (* Printing a stack or an environment. These functions are parameterized
     over an element printer. [print_element_as_symbol] can be used for
     this purpose; but the user can define other printers if desired. *)

  let buffer_stack buffer_element b stack =
    foldr (fun element () ->
      buffer_element b element;
      out b space
    ) stack ()

  let buffer_env buffer_element b env =
    buffer_stack buffer_element b (E.view env)

  let print_stack print_element stack =
    with_buffer (buffer_stack (into_buffer print_element)) stack

  let print_env print_element env =
    with_buffer (buffer_env (into_buffer print_element)) env

end

