(* This module is in charge of handling several command line options, namely
   [--interpret], [--interpret-error], [--compile-errors], [--compare-errors].
   If any of these options is present, the execution of Menhir stops here. *)

(* This default error message is produced by [--list-errors] when it creates a
   [.messages] file, and is recognized by [--compare-errors] when it compares
   two such files. *)

val default_message: string

(* [print_messages_item] displays one data item. The item is of the form [nt,
   sentence, s'], which means that beginning at the start symbol [nt], the
   sentence [sentence] ends in an error in state [s']. The display obeys the
   [.messages] file format. *)

open Grammar

val print_messages_item: Nonterminal.t * Terminal.t list * Lr1.node -> unit

