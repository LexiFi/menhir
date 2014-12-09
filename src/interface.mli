(* This module defines the interface of the generated parser. *)

(* This is the [Error] exception. *)

val excname: string
val excdef: IL.excdef
val excredef: IL.excdef

(* The type of the entry point for the start symbol [nt]. *)

val entrytypescheme: string -> IL.typescheme

(* The name of the interpreter sub-module, when the table back-end
   is used. *)

val interpreter: string

(* The type ['a result], defined in the interpreter sub-module. *)

val result: IL.typ -> IL.typ

(* The name of the incremental entry point for the start symbol [symbol]. *)

val incremental: string -> string

(* The type of the incremental entry point for the start symbol [symbol]. *)

val entrytypescheme_incremental: string -> IL.typescheme

(* This writes the interface of the generated parser to the [.mli]
   file. *)

val write: unit -> unit

