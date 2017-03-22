(* [write filename] queries the module [Grammar] for information about the
   grammar and queries the modules [Lr0] and [Lr1] for information about the
   automaton. It writes this information to the .cmly file [filename]. *)

val write: string -> unit
