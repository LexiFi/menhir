(* This module defines the interface of the generated parser. *)

(* This is the [Error] exception. *)

val excname: string
val excdef: IL.excdef
val excredef: IL.excdef

(* The type of the entry point for the nonterminal start symbol
   [symbol]. *)

val entrytypescheme: string -> IL.typescheme

(* This writes the interface of the generated parser to the [.mli]
   file. *)

val write: unit -> unit

