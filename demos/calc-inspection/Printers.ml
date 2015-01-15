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
  let newline = "\n"

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

  (* Printing an item. *)

  let print_item (prod, i) =
    print_symbol (I.lhs prod);
    print arrow;
    print_symbols i (I.rhs prod);
    print newline

  (* Printing a list of symbols (public version). *)

  let print_symbols symbols =
    print_symbols (-1) symbols

  (* Printing a production (without a dot). *)

  let print_production prod =
    print_item (prod, -1)

  (* The past of an LR(0) item is the first part of the right-hand side,
     up to the point. We represent it as a reversed list, right to left.
     Thus, the past corresponds to a prefix of the stack. *)

  let rec take n xs =
    match n, xs with
    | 0, _ ->
        []
    | _, [] ->
        (* [n] is too large *)
        assert false
    | _, x :: xs ->
        x :: take (n - 1) xs

  let past (prod, index) =
    let rhs = I.rhs prod in
    List.rev (take index rhs)

  (* The LR(0) items that form the core of an LR(1) state have compatible
     pasts. If we pick the one with the longest past, we obtain the past
     of this state, i.e., the longest statically known prefix of the stack
     in this state. *)

  let past s =
    let (max_index, max_past) =
      List.fold_left (fun ((max_index, max_past) as accu) ((_, index) as item) ->
        if max_index < index then
          index, past item
        else
          accu
      ) (0, []) (I.items s)
    in
    max_past

  (* Printing the current LR(1) state. *)

  let print_current_state env =
    print "Current LR(1) state: ";
    match Lazy.force (I.stack env) with
    | I.Nil ->
        print "<some initial state>";
        print newline
    | I.Cons (I.Element (current, _, _, _), _) ->
        print (string_of_int (Obj.magic current)); (* TEMPORARY safe conversion needed *)
        print newline;
        List.iter print_item (I.items current)

end

