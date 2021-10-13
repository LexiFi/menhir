(** This module implements a mutable FIFO queue, like [Queue]. However it has a
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

val add : 'a -> 'a t -> unit
(** [add x q] adds the element [x] at th end of the queue [q]. *)

exception Empty
(** Raised when {!take} is applied to an empty queue. *)

val take : 'a t -> 'a
(** [take q] removes and returns the first element in queue [q],
   or raises {!Empty} if the queue is empty. *)
