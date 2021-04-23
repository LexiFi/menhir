(** This module is an extension of Stdlib.Array *)

include module type of Array

(** Array reversal.
   [rev [|1; 2; 3; 4|] = [|4; 3; 2; 1|]]*)
val rev : 'a t -> 'a t

(** Convert a list to an array with the list's head at the end of the array.
    [rev_of_list [1; 2; 3; 4; 5] = [|5; 4; 3; 2; 1|]] *)
val rev_of_list : 'a list -> 'a t

(** [pop a] is [a] with its last element removed.
    [pop [|1; 2; 3; 4|] = [|1; 2; 3|]] *)
val pop : 'a t -> 'a t

(** [push a e] is [a] with [e] added at its end.
    [push [|1; 2; 3|] 4 = [|1; 2; 3; 4|]] *)
val push : 'a t -> 'a -> 'a t

(** Convert an array to list with the list head being the end of the array.
    [rev_to_list [|1; 2; 3; 4; 5|] = [5; 4; 3; 2; 1]] *)
val rev_to_list : 'a t -> 'a list

val test : unit -> unit
