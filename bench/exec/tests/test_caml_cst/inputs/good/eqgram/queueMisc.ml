(* This module adds some functionality to the standard [Queue]
   implementation. *)

(* ------------------------------------------------------------------------ *)

(* [iter f q] repeatedly takes an element [x] off the queue [q] and
   applies [f] to [x], until [q] becomes empty. Of course, [f] can add
   elements to [q] as a side-effect. *)

let iter f q =
  try
    while true do
      f (Queue.take q)
    done
  with Queue.Empty ->
    ()

(* ------------------------------------------------------------------------ *)

(* [elements q] is the list of all elements of the queue [q]. *)

let elements q =
  List.rev (Queue.fold (fun t h -> h :: t) [] q)

(* ------------------------------------------------------------------------ *)

(* [exists p q] tells whether some element of the queue [q] satisfies
   the predicate [p]. *)

exception Stop

let stop =
  Stop

let exists p q =
  try
    Queue.iter (fun x ->
      if p x then
	raise stop
    ) q;
    false
  with Stop ->
    true

