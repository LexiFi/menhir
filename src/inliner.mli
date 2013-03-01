(* This transformer inlines every function that is called at most
   once. It also inlines some functions whose body consists of a
   single function call. At the same time, every function that is
   never called is dropped. Public functions are never inlined or
   dropped. *)

val inline: IL.program -> IL.program


