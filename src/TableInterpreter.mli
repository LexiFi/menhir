(* This module instantiates the generic [Engine] with a thin decoding layer
   for the generated tables. Like [Engine], it is part of [MenhirLib]. *)

(* The exception [Error] is declared within the generated parser. This is
   preferable to pre-declaring it here, as it ensures that each parser gets
   its own, distinct [Error] exception. This is consistent with the code-based
   back-end. *)

(* This functor is invoked by the generated parser. *)

module Make (T : TableFormat.TABLES)

: EngineTypes.ENGINE with type state = int
                           and type token = T.token
                           and type semantic_value = Obj.t
                           and type production = int

