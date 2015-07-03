(* Sequences with constant time concatenation and linear-time conversion
   to an ordinary list. *)

type 'a seq

val empty: 'a seq
val singleton: 'a -> 'a seq
val append: 'a seq -> 'a seq -> 'a seq
val elements: 'a seq -> 'a list
