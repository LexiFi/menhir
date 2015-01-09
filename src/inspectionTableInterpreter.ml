module Make (
  T : InspectionTableFormat.TABLES
      with type 'a lr1state = int
) = struct

  type 'a symbol = 'a T.symbol =
    | T : 'a T.terminal -> 'a symbol
    | N : 'a T.nonterminal -> 'a symbol

  (* The type [xsymbol] is an existentially quantified version of the type
     ['a symbol]. *)

  type xsymbol = T.xsymbol =
    | X : 'a symbol -> xsymbol

  (* This auxiliary function decodes a packed linearized array, as created by
     [TableBackend.linearize_and_marshal1]. Here, we read a row all at once. *)

  let read_packed_linearized ((data, entry) : PackedIntArray.t * PackedIntArray.t) (i : int) : int list =
    LinearizedArray.read_row_via
      (PackedIntArray.get data)
      (PackedIntArray.get entry)
      i

  (* This auxiliary function decodes a symbol. The encoding was done by
     [encode_symbol] or [encode_symbol_option] in the table back-end. *)

  let decode_symbol (symbol : int) : T.xsymbol =
    (* If [symbol] is 0, then we have no symbol. This could mean e.g.
       that the function [incoming_symbol] has been applied to an
       initial state. In principle, this cannot happen. *)
    assert (symbol > 0);
    (* The low-order bit distinguishes terminal and nonterminal symbols. *)
    let kind = symbol land 1 in
    let symbol = symbol lsr 1 in
    if kind = 0 then
      T.terminal (symbol - 1)
    else
      T.nonterminal symbol

  (* The function [incoming_symbol] goes through the tables [lr0_core] and
     [lr0_incoming]. This yields a representation of type [xsymbol], out of
     which we strip the [X] quantifier, so as to get a naked symbol. This last
     step is ill-typed and potentially dangerous. It is safe only because this
     function is used at type ['a lr1state -> 'a symbol], which forces an
     appropriate choice of ['a]. *)

  let incoming_symbol (s : 'a T.lr1state) : 'a T.symbol =
    let core = PackedIntArray.get T.lr0_core s in
    let symbol = decode_symbol (PackedIntArray.get T.lr0_incoming core) in
    match symbol with
    | T.X symbol ->
        Obj.magic symbol

  (* The function [lhs] reads the table [lhs] and uses [T.nonterminal]
     to decode the symbol. *)

  let lhs prod =
    T.nonterminal (PackedIntArray.get T.lhs prod)

  (* The function [rhs] reads the table [rhs] and uses [decode_symbol]
     to decode the symbol. *)

  let rhs prod =
    List.map decode_symbol (read_packed_linearized T.rhs prod)

  (* The function [items] maps the LR(1) state [s] to its LR(0) core,
     then uses [core] as an index into the table [lr0_items]. The
     items are then decoded by the function [export] below, which is
     essentially a copy of [Item.export]. *)

  type item =
      int * int

  let export t : item =
    (t lsr 7, t mod 128)

  let items s =
    (* Map [s] to its LR(0) core. *)
    let core = PackedIntArray.get T.lr0_core s in
    (* Now use [core] to look up the [lr0_items] table. *)
    List.map export (read_packed_linearized T.lr0_items core)

end
