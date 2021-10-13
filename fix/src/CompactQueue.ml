(** This module implements a mutable FIFO queue, like [Queue]. However it has a
    more compact representation, storing elements contiguously in an array. *)

type 'a t = {

  mutable buffer: 'a array;
  (* The array that store all elements. *)

  mutable first: int;
  (* Index of the first element in the queue, if any (the one that will be
     popped next). It has to be taken modulo (Array.length buffer). *)

  mutable size: int;
  (* The number of elements in the queue. *)

  (* Invariants:
     - [Array.length q.buffer] is a power of 2, to speed-up modulo
     - [0 <= q.size <= Array.length q.buffer]
  *)

  (* The following code iterate on all queued elements:

     for i = 0 to q.size - 1 do
       f q.buffer.((q.first + i) mod (Array.length q.buffer))
     done

     Since length is a power of 2, it is equivalent to:

     for i = 0 to q.size - 1 do
       f q.buffer.((q.first + i) land (Array.length q.buffer - 1))
     done
  *)
}

let create () = {
  (* The buffer is initially an empty array. It will be re-allocated when a
     first element is pushed. *)
  buffer = [||];
  first = 0;
  size = 0;
}

let is_empty q =
  q.size = 0

let add x q =
  let buffer = q.buffer in
  let length = Array.length buffer in
  if q.size < length then (
    (* Queue still has some room left *)
    buffer.((q.first + q.size) land (length - 1)) <- x;
    q.size <- q.size + 1
  ) else if length > 0 then (
    (* Buffer is full *)
    (* 1. Reallocate a buffer twice the size *)
    let buffer' = Array.make (length * 2) x in
    (* 2. Move existing elements *)
    let first = q.first land (length - 1) in
    Array.blit buffer first buffer' 0 (length - first);
    Array.blit buffer 0 buffer' (length - first) first;
    q.buffer <- buffer';
    q.first <- 0;
    q.size <- length + 1;
  ) else (
    (* Queue has a buffer of size 0. Allocate an initial array. *)
    q.buffer <- Array.make 8 x;
    q.size <- 1;
  )

exception Empty

let take q =
  if q.size = 0 then
    raise Empty
  else (
    q.size <- q.size - 1;
    let result = q.buffer.(q.first land (Array.length q.buffer - 1)) in
    q.first <- q.first + 1;
    result
  )
