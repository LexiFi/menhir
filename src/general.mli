(* This module offers general-purpose functions on lists and streams. *)

(* --------------------------------------------------------------------------- *)

(* Lists. *)

(* [take n xs] returns the [n] first elements of the list [xs]. It is
   acceptable  for the list [xs] to have length less than [n], in
   which case [xs] itself is returned. *)

val take: int -> 'a list -> 'a list

(* [drop n xs] returns the list [xs], deprived of its [n] first elements.
   It is acceptable for the list [xs] to have length less than [n], in
   which case an empty list is returned. *)

val drop: int -> 'a list -> 'a list

(* [uniq cmp xs] assumes that the list [xs] is sorted according to the
   ordering [cmp] and returns the list [xs] deprived of any duplicate
   elements. *)

val uniq: ('a -> 'a -> int) -> 'a list -> 'a list

(* [weed cmp xs] returns the list [xs] deprived of any duplicate elements. *)

val weed: ('a -> 'a -> int) -> 'a list -> 'a list

(* If [cmp] is an ordering on elements, then [compare cmp] is the
   lexicographic ordering on lists. *)

val compare: ('a -> 'a -> int) -> 'a list -> 'a list -> int

(* --------------------------------------------------------------------------- *)

(* A stream is a list whose elements are produced on demand. *)

type 'a stream =
    'a head Lazy.t

and 'a head =
  | Nil
  | Cons of 'a * 'a stream

(* The length of a stream. *)

val length: 'a stream -> int

(* Folding over a stream. *)

val foldr: ('a -> 'b -> 'b) -> 'a stream -> 'b -> 'b

