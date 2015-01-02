(* This module defines the interface of the generated parser. *)

(* This is the [Error] exception. *)

val excname: string
val excdef: IL.excdef

(* The type of the entry point for the start symbol [nt]. *)

val entrytypescheme: UnparameterizedSyntax.grammar -> string -> IL.typescheme

(* The name of the interpreter sub-module, when the table back-end
   is used. *)

val interpreter: string

(* The type ['a result], defined in the interpreter sub-module. *)

val result: IL.typ -> IL.typ

(* The type ['a lr1state], defined by the interpreter sub-module. *)

val lr1state: string
val tlr1state: IL.typ -> IL.typ
val lr1state_redef: IL.interface_item

(* The name of the sub-module that contains the incremental entry points. *)

val incremental: string

(* The name of the sub-module that contains the inspection API. *)

val inspection: string

(* This writes the interface of the generated parser to the [.mli] file. *)

val write: UnparameterizedSyntax.grammar -> unit -> unit

