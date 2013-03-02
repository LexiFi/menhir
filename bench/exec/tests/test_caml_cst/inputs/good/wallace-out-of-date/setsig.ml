(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/Attic/setsig.ml,v 1.1.2.5 1999/04/05 16:45:40 francois Exp $ *)
(*

This is an interface for sets over ordered types. No implementation is provided here; several implementations are
provided in other modules.

*)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Orderings.

An ordering is a two-argument function f such that (f e1 e2) is zero if the elements e1 and e2 are equal, strictly
negative if e1 is smaller than e2, and strictly positive if e1 is greater than e2. For instance, a suitable ordering
function for integers is (-). You can also use the generic structural comparison function Pervasives.compare.

*)

type 'a ordering = 'a -> 'a -> int
type 'a equality = 'a -> 'a -> bool

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The interface.

*)

module type SetSig = sig

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The type of sets containing elements of type 'a.

*)

type 'a t

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Elementary operations.

*)

val empty: 'a ordering -> 'a t
    (* The empty set. *)

val is_empty: 'a t -> bool
    (* Emptiness test. *)

val mem: 'a -> 'a t -> bool
    (* Membership test. *)
val memp: ('a -> int) -> 'a t -> 'a
    (* A refined version of the above function. Instead of looking for a specific element, the search is driven by
       a callback function, which is essentially a partial application of the set's ordering to the element being
       looked for. The function raises Not_found if no such element exists. *)
val memp_greatest: ('a -> int) -> 'a t -> 'a
    (* A more specific version of the above function. If several elements satisfy the supplied callback function,
       then the greatest one is returned. *)

val add: 'a -> 'a t -> 'a t
    (* Addition of an element to a set. *)
val isolate: 'a t -> 'a * 'a t
    (* Isolates an unspecified element out of a non-empty set. *)

exception StrictAdd

val strict_add: 'a -> 'a t -> 'a t
    (* Addition of a new element to a set. StrictAdd is raised if the element is already present in the set. *)

val smart_add: ('a -> 'a -> 'a) -> 'a -> 'a t -> 'a t
    (* Addition of an element to a set. The decision function is requested to make a choice if the element
       already exists. It is allowed to return any equal element. *)

val remove: 'a -> 'a t -> 'a t
    (* Removal of an element. If the element is absent, the set is returned unchanged. *)

val choose: 'a t -> 'a
    (* Returns an unspecified element of the given set. Raises Not_found if the set is empty. *)
val choosep: ('a -> bool) -> 'a t -> 'a
    (* Returns an unspecified element of the given set which satisfies the given predicate. Raises Not_found
       if the set is empty. *)

val smallest: 'a t -> 'a
    (* Returns the smallest element of the set. Raises Not_found if the set is empty. *)

val elements: 'a t -> 'a list
    (* Returns the list of all elements of the given set. The order in which elements appear in the list is
       unspecified, but is the same for two equal sets. *)

val cardinality: 'a t -> int
    (* Returns the number of elements of the given set. *)

val get_order: 'a t -> 'a ordering
    (* Returns a set's ordering relation. *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

More advanced operations.

*)

val union: 'a t -> 'a t -> 'a t
    (* Set union. *)
val smart_union: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    (* When an element appears in both operand sets, the above function arbitrarily chooses one of them to be made
       part of the result set. This refined function allows finer control over the choice, by calling the client back
       whenever a pair of equal elements shows up; the callback function must produce the element which shall be made
       part of the result set. *)

val inter: 'a t -> 'a t -> 'a t
    (* Set intersection. *)
val smart_inter: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    (* Same refinement as above. *)

val diff: 'a t -> 'a t -> 'a t
    (* Set difference. *)
val disjoint: 'a t -> 'a t -> bool
    (* Checking whether two sets are disjoint. *)

val subset: 'a t -> 'a t -> bool
    (* Set inclusion. *)
val smart_subset: ('a -> 'b -> 'b -> 'a) -> ('b -> 'a) -> 'a -> 'b t -> 'b t -> 'a
    (* A refinement of the above function, which accepts two callback functions. The first one is presented with
       each pair of elements corresponding to the injection. It may maintain private data through an accumulator.
       If the subset relationship does not hold, then this process is aborted and the second callback function is
       invoked, with a witness as argument, i.e. an element of the first set which is not present in the second set.
       This function must terminate by raising an exception. *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Comparisons.

*)

val equal: ('a t) equality
    (* Set equality. *)
val smart_equal: 'a equality -> ('a t) equality
    (* A refinement of the above function. Instead of using the sets' own equality relationship, a user-supplied
       relationship is used. It must of course be more precise than the former. *)

val compare: ('a t) ordering
    (* A total ordering between sets. *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Iterators.

*)

val iter: ('a -> unit) -> 'a t -> unit
    (* Applies the specified action to all elements of the set. The order in which elements are presented to the
       function is unspecified, but is the same for two equal sets. *)
val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    (* Applies the specified action to all elements of the set, while maintaining an accumulator. The order in which
       elements are presented to the function is unspecified, but is the same for two equal sets. *)

val apply_assoc_op1: ('a -> 'a -> 'a) -> 'a t -> 'a
    (* The first argument to this function must be an associative binary operator. The function computes the result
       of applying this operator to the whole set. The set is assumed to contain at least one element, so there is no
       need to specify a neutral element. Raises Not_found if the set is empty. *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Maps.

*)

val map: 'b ordering -> ('a -> 'b) -> 'a t -> 'b t
    (* Applies the given function to each element of the given set. The first argument is the ordering on elements
       of the destination set. *)
val endo_map: ('a -> 'a) -> 'a t -> 'a t
    (* A refinement of map, where the target set is assumed to have the same ordering as the source one. *)
val monotonous_map: 'b ordering -> ('a -> 'b) -> 'a t -> 'b t
    (* A refinement of map, where the map is assumed to be monotonous, i.e. to preserve the ordering of elements.
       This often allows a more efficient implementation, since the underlying data structure retains its shape. *)
val monotonous_endo_map: ('a -> 'a) -> 'a t -> 'a t
    (* A combination of the above two refinements. *)

val filter: ('a -> bool) -> 'a t -> 'a t
    (* Returns the set of elements which match the given predicate. *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Freezing a set is necessary before writing it out to a file using output_value, because the data structure may contain
an ordering function. Reciprocally, unfreezing is necessary after reading a set from a file.

*)

val freeze: 'a t -> unit
    (* Prepares the data structure for output. *)
val unfreeze: 'a t -> 'a ordering -> unit
    (* Prepares the data structure for use, after input. One must of course specify a proper ordering. *)

(* ----------------------------------------------------------------------------------------------------------------- *)

end
