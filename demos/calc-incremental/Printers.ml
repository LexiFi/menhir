module Make
  (I : MenhirLib.IncrementalEngine.INSPECTION)
  (User : sig
    val arrow: string (* should include space on both sides *)
    val dot: string
    val space: string
    val print_symbol: I.xsymbol -> string
  end)
= struct

  open User

  let out =
    Buffer.add_string

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

  let with_buffer f x =
    let b = Buffer.create 128 in
    f b x;
    Buffer.contents b

  let print_item =
    with_buffer buffer_item

  let print_production =
    with_buffer buffer_production

end

