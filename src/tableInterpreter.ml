(* This module instantiates the generic [Engine] with a thin decoding layer
   for the generated tables. Like [Engine], it is part of [MenhirLib]. *)

(* The exception [Accept] is pre-declared here: this obviates the need
   for generating its definition. The exception [Error] is declared
   within the generated parser. This is preferable to pre-declaring it
   here, as it ensures that each parser gets its own, distinct [Error]
   exception. This is consistent with the code-based back-end. *)

exception Accept of Obj.t

(* This functor is invoked by the generated parser. *)

module Make (T : TableFormat.TABLES)

= Engine.Make (struct

  type state =
      int

  type token =
      T.token

  type terminal =
      int

  type semantic_value =
      Obj.t
	  
  let token2terminal =
    T.token2terminal
	
  let token2value =
    T.token2value
	
  let error_terminal =
    T.error_terminal

  let error_value =
    Obj.repr ()
  
  type production =
      int
  
  let default_reduction state defred nodefred env =
    let code = PackedIntArray.get T.default_reduction state in
    if code = 0 then
      nodefred env
    else
      defred env (code - 1)
  
  (* This auxiliary function helps access a compressed, two-dimensional
     matrix, like the action and goto tables. *)

  let unmarshal2 table i j =
    RowDisplacement.getget
      PackedIntArray.get
      PackedIntArray.get
      table
      i j

  (* This auxiliary function helps access a flattened, two-dimensional
     matrix, like the error bitmap. *)

  let unflatten (n, data) i j =
    PackedIntArray.get1 data (n * i + j)

  let action state terminal value shift reduce fail env =
    match unflatten T.error state terminal with
    | 1 ->
	let action = unmarshal2 T.action state terminal in
	let opcode = action land 0b11
	and param = action lsr 2 in
	if opcode >= 0b10 then
	  (* 0b10 : shift/discard *)
	  (* 0b11 : shift/nodiscard *)
	  let please_discard = (opcode = 0b10) in
	  shift env please_discard terminal value param
	else
	  (* 0b01 : reduce *)
	  (* 0b00 : cannot happen *)
	  reduce env param
    | c ->
	assert (c = 0);
	fail env
  
  let goto state prod =
    let code = unmarshal2 T.goto state (PackedIntArray.get T.lhs prod) in
    (* code = 1 + state *)
    code - 1

  exception Accept =
	Accept

  exception Error =
	T.Error

  type semantic_action =
      (state, semantic_value, token) EngineTypes.env ->
      (state, semantic_value)        EngineTypes.stack
	
  let semantic_action prod =
    T.semantic_action.(prod)
  
  (* If [T.trace] is [None], then the logging functions do nothing. *)

  let log =
    match T.trace with Some _ -> true | None -> false

  module Log = struct
    
    open Printf
    
    let state state =
      match T.trace with
      | Some _ ->
          fprintf stderr "State %d:\n%!" state
      | None ->
	  ()
    
    let shift terminal state =
      match T.trace with
      | Some (terminals, _) ->
          fprintf stderr "Shifting (%s) to state %d\n%!" terminals.(terminal) state
      | None ->
	  ()
    
    let reduce_or_accept prod =
      match T.trace with
      | Some (_, productions) ->
          fprintf stderr "%s\n%!" productions.(prod)
      | None ->
	  ()
    
    let lookahead_token token startp endp =
      match T.trace with
      | Some (terminals, _) ->
          fprintf stderr "Lookahead token is now %s (%d-%d)\n%!"
            terminals.(token)
            startp.Lexing.pos_cnum
            endp.Lexing.pos_cnum
      | None ->
	  ()
    
    let initiating_error_handling () =
      match T.trace with
      | Some _ ->
          fprintf stderr "Initiating error handling\n%!"
      | None ->
	  ()
    
    let resuming_error_handling () =
      match T.trace with
      | Some _ ->
          fprintf stderr "Resuming error handling\n%!"
      | None ->
	  ()
    
    let handling_error state =
      match T.trace with
      | Some _ ->
          fprintf stderr "Handling error in state %d\n%!" state
      | None ->
	  ()
    
  end
  
end)

(* This functor constructs the inspection API. *)

module MakeInspection (T : TableFormat.INSPECTION_TABLES) = struct

  let symbol =
    T.symbol

  let lhs prod =
    let nt = PackedIntArray.get T.lhs prod in
    T.nonterminal nt

  let rhs prod =
    T.production_defs.(prod)

  (* This is a copy of [Item.export]. *)

  let export t =
    (t lsr 7, t mod 128)

  let items s =
    let core = PackedIntArray.get T.lr0_core s in
    List.map export T.lr0_items.(core)

end
