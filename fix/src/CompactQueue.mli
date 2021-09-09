(** This module implements a mutable queue, like [Queue]. However it has a
    more compact representation, storing elements contiguously in an array.

    This has a nice impact on performance (both time and memory consumption)
    when adding and removing many elements.
    A drawback of this implementation is that some values will be kept alive
    longer than necessary, as they might still be reachable from the internal
    array.
*)

type 'a t
(** The type of a queue. *)

val create : unit -> 'a t
(** [create ()] creates an empty queue. *)

val is_empty : 'a t -> bool
(** [is_empty q] tests whether the queue [q] is empty. *)

val push : 'a -> 'a t -> unit
(** [push x q] adds the element [x] at th end of the queue [q]. *)

exception Empty
(** Raised when {!pop} is applied to an empty queue. *)

val pop : 'a t -> 'a
(** [pop q] removes and returns the first element in queue [q],
   or raises {!Empty} if the queue is empty. *)
