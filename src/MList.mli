(** This module is an extension of Stdlib.List *)

include module type of List

(** A list subject to a condition. (Be careful, though: the list is of course
    constructed even if the condition is false.) *)
val ifn : bool -> 'a t -> 'a t

(** A list subject to a condition. (Be careful, though: the list is of course
    constructed even if the condition is false.) *)
val if1 : bool -> 'a -> 'a t

(** A lazy version of [ifn], where the list is constructed only if the condition
    is true. *)
val ifnlazy : bool -> (unit -> 'a t) -> 'a t

(** The sum of a list of integers. *)
val sum : int t -> int
