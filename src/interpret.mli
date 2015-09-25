(* This module is in charge of handling several command line options, namely
   [--interpret], [--interpret-error], [--compile-errors], [--compare-errors].
   If any of these options is present, the execution of Menhir stops here. *)

(* This default error message is produced by [--list-errors] when it creates a
   [.messages] file, and is recognized by [--compare-errors] when it compares
   two such files. *)

val default_message: string

