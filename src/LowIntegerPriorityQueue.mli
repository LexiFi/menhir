(** This module implements a simple-minded priority queue, under the
    assumption that priorities are low nonnegative integers. *)

(** The type of priority queues. *)
type 'a t

(** [create()] creates an empty priority queue. *)
val create: unit -> 'a t

(** [add q x p] inserts the element [x], with priority [p], into the queue [q]. *)
val add: 'a t -> 'a -> int -> unit

(** [remove q] extracts out of [q] and returns an element with minimum priority. *)
val remove: 'a t -> 'a option

(** [is_empty q] tests whether the queue [q] is empty. *)
val is_empty: 'a t -> bool

(** [cardinal q] returns the number of elements in the queue [q]. *)
val cardinal: 'a t -> int

(** [repeat q f] repeatedly extracts an element with minimum priority out of [q]
    and passes it to [f] (which may insert new elements into [q]), until [q] is
    exhausted. *)
val repeat: 'a t -> ('a -> unit) -> unit
