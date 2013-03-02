(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/name.ml,v 1.1 2002/05/30 09:45:58 fpottier Exp $ *)

(* This module provides basic support for associating names with elements of
   an arbitrary type, provided it admits a notion of equality. *)

module Make (X : sig

  type t
  val equal: t -> t -> bool

end) = struct

  (* The state consists of an integer counter, which gives the next available
     name, and a mapping from terms to internal names, i.e.  integers. The
     latter is implemented as a list of terms; this is a bit inefficient
     (linear time lookup), but simple. *)

  let next =
    ref 0

  let known : X.t list ref =
    ref []

  (* [index elem i l] returns the index of the element [elem] in the list [l],
     considering that the first element is numbered [i], and further elements
     have decreasing indices. [Not_found] is raised if [elem] does not appear
     in [l]. *)

  let rec index elem i = function
    | [] ->
	raise Not_found
    | elem' :: rest ->
	if X.equal elem elem' then i
	else index elem (i-1) rest

  (* [name elem] returns a name for the element [elem]. If the element was
     encountered before, its previous name is returned and the state is
     unchanged; otherwise, a new name is picked and the state is updated to
     record this fact. Names are integers numbered from 0 and up. *)

  let name elem =
    try
      index elem (!next-1) !known
    with Not_found ->
      let i = !next in
      incr next;
      known := elem :: !known;
      i

end

(* [i2s base limit i] converts the integer [i] to a human-readable
   name. A letter is picked between [base] and [limit], and a number
   is appended to it, if necessary. *)

let i2s base limit =
  let base = Char.code base
  and limit = Char.code limit in
  let range = limit - base + 1 in
  assert (range > 0);
  function i ->
    (String.make 1 (Char.chr (base + (i mod range)))) ^
    (if i < range then "" else string_of_int (i / range - 1))

let lowercase =
  i2s 'a' 'z'

let uppercase =
  i2s 'A' 'Z'

