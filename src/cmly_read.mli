(* The functor [Read] reads a .cmly file. If the file is unreadable,
   the exception [Error] is raised. Otherwise, the functor builds a
   module of type [Cmly_api.GRAMMAR], which gives access to a description
   of the grammar and automaton. *)

exception Error of string

module Read (X : sig val filename : string end) : Cmly_api.GRAMMAR
