include module type of Array

val rev : 'a t -> 'a t
val rev_of_list : 'a list -> 'a t
val pop : 'a t -> 'a t
val push : 'a t -> 'a -> 'a t
val rev_to_list : 'a t -> 'a list
val test : unit -> unit
