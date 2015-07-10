(* This module implements a simple-minded priority queue, under the assumption
   that priorities are low nonnegative integers. *)

module InfiniteArray = MenhirLib.InfiniteArray

type 'a t = {

  (* A priority queue is represented as an array of lists, indexed by
     priorities. There is no a priori bound on the size of this array -- its
     size is increased if needed. It is up to the user to use priorities of
     reasonable magnitude. *)
  a: 'a list InfiniteArray.t;

  (* Index of lowest nonempty list, if there is one; or lower (sub-optimal,
     but safe). If the queue is empty, [best] is arbitrary. *)
  mutable best: int;

  (* Current number of elements in the queue. Used in [remove] to stop the
     search for a nonempty bucket. *)
  mutable cardinal: int;

}

let create () = {
  a = InfiniteArray.make [];
  best = 0;
  cardinal = 0;
}

let add q x priority =
  assert (0 <= priority);
  q.cardinal <- q.cardinal + 1;
  let xs = InfiniteArray.get q.a priority in
  InfiniteArray.set q.a priority (x :: xs);
  (* Decrease [q.best], if necessary, so as not to miss the new element. In
     the special case of Dijkstra's algorithm or A*, this never happens. *)
  if priority < q.best then
    q.best <- priority

let is_empty q =
  q.cardinal = 0

let cardinal q =
  q.cardinal

let rec remove_nonempty q =
  (* Look for the next nonempty bucket. We know there is one. This may seem
     inefficient, because it is a linear search. However, in applications
     where [q.best] never decreases, the cumulated cost of this loop is the
     maximum priority ever used, which is good. *)
  match InfiniteArray.get q.a q.best with
  | [] ->
      q.best <- q.best + 1;
      remove_nonempty q
  | x :: xs ->
      q.cardinal <- q.cardinal - 1;
      InfiniteArray.set q.a q.best xs;
      Some x

let remove q =
  if q.cardinal = 0 then
    None
  else
    remove_nonempty q

let rec repeat q f =
  match remove q with
  | None ->
      ()
  | Some x ->
      f x;
      repeat q f
