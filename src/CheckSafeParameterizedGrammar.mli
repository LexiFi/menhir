(* [check] determines whether a parameterized grammar is safe for expansion,
   that is, whether the process of expanding away its parameterized symbols is
   guaranteed to terminate. *)

val check: Syntax.grammar -> unit
