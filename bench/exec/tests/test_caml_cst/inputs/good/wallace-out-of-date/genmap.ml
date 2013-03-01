(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/Attic/genmap.ml,v 1.1.2.6 1999/04/05 16:45:38 francois Exp $ *)
(*

An implementation of maps, parameterized by an implementation of sets.

*)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

We begin by mimicking our interface.

*)

open Setsig

module type S = sig

  type 'a domain
    (* The type of sets of keys, used to represent a map's domain. *)

  type ('a, 'b) t
    (* The type of maps from keys of type 'a to values of type 'b. *)

  val empty: 'a ordering -> ('a, 'b) t
    (* An empty map can be obtained by specifying an ordering on keys. *)
  val is_empty: ('a, 'b) t -> bool
    (* Determines whether a map contains any bindings. *)

  exception StrictAdd
  val add: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    (* Adds a new binding to a map. If a binding already exists for this key, StrictAdd is raised. *)

  val override: 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
    (* Creates a new binding, or overrides an existing binding for this key. *)

  val find: 'a -> ('a, 'b) t -> 'b
    (* Finds the value associated to a key. Raises Not_found if the binding is undefined. *)

  val cardinality: ('a, 'b) t -> int
    (* Returns the number of bindings in the map, i.e. its domain's cardinality. *)

  val domain: 'a ordering -> ('a, 'b) t -> 'a domain
    (* Returns the map's domain, i.e. the set of keys for which bindings have been defined. *)
  val lift: ('a -> 'b) -> 'a domain -> ('a, 'b) t
    (* Given a domain and a function which associates a value to each element of the domain, build the corresponding
       map. *)

  val equal: 'b equality -> ('a, 'b) t -> ('a, 'b) t -> bool
    (* Determines whether two maps are equal. The first argument is an equality function on values. *)

  val disjoint: ('a, 'b) t -> ('a, 'b) t -> bool
    (* Determines whether two maps have disjoint domains. *)

  val union: ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    (* Takes the union of two maps of disjoint domains. If the maps' domains overlap, the result is unspecified over
       common keys. *)

  val concat: ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t
    (* Concatenates two maps. The result is the union of the two maps, where if the map's domains overlap, then the
       second map's binding prevails. *)

  val map: ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t
    (* Composes a map with a function from values to values, thus yielding a new map. *)

  val restrict: ('a, 'b) t -> 'a domain -> ('a, 'b) t
    (* Restricts a map to a subset of its domain. Raises Not_found if the supplied set is not a subset of the
       supplied map's domain. *)
  val split: ('a, 'b) t -> 'a domain -> ('a, 'b) t * ('a, 'b) t
    (* Splits a map with respect to a set of keys. The first result is the map formed of all bindings whose keys
       are members of the supplied set; the second result is the map formed of all bindings whose keys are not
       members of the supplied set. *)

  val iter: ('b -> unit) -> ('a, 'b) t -> unit
    (* Applies the specified action to all values stored in the map. The action function does not have access to
       the corresponding keys. *)
  val iter2: ('b -> 'b -> unit) -> ('a, 'b) t -> ('a, 'b) t -> unit
    (* Same as above, except that two maps are walked at the same time. Their domains must be identical. *)
  val iter3: ('b -> 'b -> 'b -> unit) -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t -> unit
    (* Same as above, except that three maps are walked at the same time. Their domains must be identical. *)
  val fold: ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
    (* Applies the specified action to all keys and values in the map. The action function is allowed to maintain
       an accumulator. *)
  val fold2: ('a -> 'b -> 'b -> 'c -> 'c) -> ('a, 'b) t -> ('a, 'b) t -> 'c -> 'c
    (* Same as above, except that two maps are walked at the same time, so the action function receives one key,
       two values and the accumulator as parameters. The maps' domains must be identical. *)

  val freeze: ('a, 'b) t -> unit
    (* Prepares the data structure for output. *)
  val unfreeze: ('a, 'b) t -> 'a ordering -> unit
    (* Prepares the data structure for use, after input. One must of course specify a proper ordering. *)

end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The actual implementation. Maps are simply implemented as sets of pairs, ordered according to their first components.

*)

module Make (Set : SetSig) = struct

  type 'a domain = 'a Set.t
  type ('a, 'b) t = ('a * 'b) Set.t

  let empty key_order =
    Set.empty (fun (key1, _) (key2, _) -> key_order key1 key2)

  let is_empty m =
    Set.is_empty m

  let cardinality =
    Set.cardinality

  (* Domain requires the user to provide the ordering on keys again. To avoid this, we would need to store it
     inside the data structure, which would be a loss of space. Hopefully, this shouldn't be too much of a bother. *)

  let domain key_order m =
    Set.monotonous_map key_order fst m

  let lift generator domain =
    let key_order = Set.get_order domain in
    Set.monotonous_map (fun (key1, _) (key2, _) -> key_order key1 key2) (fun key -> (key, generator key)) domain

  exception StrictAdd

  let add key value m =
    try
      Set.strict_add (key, value) m
    with Set.StrictAdd ->
      raise StrictAdd

  let override key value m =
    Set.smart_add (fun x x' -> x') (key, value) m

  let find key m =
    let pair_order = Set.get_order m in (* Hack *)
    let _, value = Set.memp (fun ((key', value') as pair') -> pair_order (key, value') (* Hack *) pair') m in
    value

  let union =
    Set.union

  let concat m1 m2 =
    Set.smart_union (fun x x' -> x') m1 m2

  let iter action m =
    Set.iter (fun (_, value) -> action value) m

  let iter2 action m1 m2 =
    List.iter2 (fun (_, e1) (_, e2) -> action e1 e2) (Set.elements m1) (Set.elements m2)

  let iter3 action m1 m2 m3 =
    Standard.list_iter3 (fun (_, e1) (_, e2) (_, e3) ->
      action e1 e2 e3
    ) (Set.elements m1) (Set.elements m2) (Set.elements m3)

  let fold action m accu =
    Set.fold (fun (key, value) accu -> action key value accu) m accu

  let fold2 action m1 m2 accu =
    Standard.list_fold_right2 (fun (key, e1) (_, e2) accu ->
				 action key e1 e2 accu
    ) (Set.elements m1) (Set.elements m2) accu

  let equal comparison m1 m2 =
    Set.smart_equal (fun (key1, e1) (key2, e2) -> (key1 = key2) & (comparison e1 e2)) m1 m2

  let map f m =
    Set.monotonous_endo_map (fun (key, v) -> (key, f v)) m

  let restrict m d =
    Set.monotonous_map (Set.get_order m) (fun key -> (key, find key m)) d

  let split m d =
    let empty_map = Set.empty (Set.get_order m) in
    Set.fold (fun ((key, elem) as binding) (m1, m2) ->
      if Set.mem key d then
	(Set.add binding m1, m2)
      else
	(m1, Set.add binding m2)
    ) m (empty_map, empty_map)

  let disjoint =
    Set.disjoint

  let freeze =
    Set.freeze

  let unfreeze m key_order =
    Set.unfreeze m (fun (key1, _) (key2, _) -> key_order key1 key2)

end

