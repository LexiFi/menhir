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

: EngineTypes.ENGINE with type state = int
                           and type token = T.token
			   and type semantic_value = Obj.t
                           and type production = int

(* This functor is also invoked inside the generated parser. It
   constructs the inspection API on top of the inspection tables. *)

module MakeInspection (T : TableFormat.INSPECTION_TABLES
                       with type 'a lr1state = int)

: IncrementalEngine.INSPECTION
  with type 'a lr1state := 'a T.lr1state
   and type 'a symbol := 'a T.symbol
   and type xsymbol := T.xsymbol
   and type production := int

