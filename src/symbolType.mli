(* The symbol GADT is the union of the terminal and nonterminal GADTs. *)

val tsymbolgadt: IL.typ -> IL.typ

(* The conventional names of the data constructors. *)

val dataT: string
val dataN: string

(* The definition of the symbol GADT. This definition can be produced only if
   we are successfully able to construct the nonterminal GADT first. *)

val symbolgadtdef: unit -> IL.interface

