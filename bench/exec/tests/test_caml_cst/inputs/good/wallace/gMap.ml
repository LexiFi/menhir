(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/gMap.ml,v 1.11 2000/02/11 16:15:48 fpottier Exp $ *)

(* This is a generic map interface. In other words, these signatures document the operations which should be provided
   by any general-purpose implementation of maps. Three signatures are in fact provided. The first one documents
   additive maps, i.e. maps which do not support removing bindings. The second one, a superset of the first one, adds
   more advanced operations. Lastly, the third one is a specialization of the second one, which assumes the type of
   keys, as well as the ordering on keys, to be fixed. *)

module type Additive = sig

  (* The type of maps whose keys have type ['a] and whose data have type ['b]. *)

  type ('a, 'b) t

  (* An ordering is a two-argument function [f] such that [f e1 e2] is zero if the elements [e1] and [e2] are equal,
     strictly negative if [e1] is smaller than [e2], and strictly positive if [e1] is greater than [e2]. For instance,
     a suitable ordering function for integers is [(-)]. The generic comparison function [Pervasives.compare] is also
     a suitable ordering. *)

  type 'a ordering = 'a -> 'a -> int

  (* The empty map. *)

  val empty: ('a, 'b) t

  (* [lookup o k m] looks up the value associated to the key [k] in the map [m], and raises [Not_found] if no value is
     bound to [k]. The ordering [o] must be consistent with the map [m]. *)

  val lookup: 'a ordering -> 'a -> ('a, 'b) t -> 'b

  (* [add o k d m] returns a map whose bindings are all bindings in [m], plus a binding of the key [k] to the datum
     [d]. If a binding already exists for [k], it is overridden. The ordering [o] must be consistent with the map
     [m]. *)

  val add: 'a ordering -> 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t

  (* [singleton k d] returns a map whose only binding is from [k] to [d]. *)

  val singleton: 'a -> 'b -> ('a, 'b) t

end

module type S = sig

  include Additive

  (* [is_empty m] returns [true] if and only if the map [m] defines no bindings at all. *)

  val is_empty: ('a, 'b) t -> bool

  (* [cardinal m] returns [m]'s cardinal, that is, the number of keys it binds, or, in other words, its domain's
     cardinal. *)

  val cardinal: ('a, 'b) t -> int

  (* [lookup_and_remove o k m] looks up the value [v] associated to the key [k] in the map [m], and raises [Not_found]
     if no value is bound to [k]. The ordering [o] must be consistent with the map [m]. The call returns the value
     [v], together with the map [m] deprived from the binding from [k] to [v]. *)

  val lookup_and_remove: 'a ordering -> 'a -> ('a, 'b) t -> 'b * ('a, 'b) t

  (* [union o m1 m2] returns the union of the maps [m1] and [m2]. As usual, the ordering [o] must be consistent with
     the map [m]. *)

  val union: 'a ordering -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  (* [fine_union o decide m1 m2] returns the union of the maps [m1] and [m2]. If a key [k] is bound to [x1]
     (resp. [x2]) within [m1] (resp. [m2]), then [decide] is called. It is passed [x1] and [x2], and must return the
     value which shall be bound to [k] in the final map. As usual, the ordering [o] must be consistent with the map
     [m]. *)

  type 'a decision = 'a -> 'a -> 'a

  val fine_union: 'a ordering -> 'b decision -> ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

  (* [iter f m] invokes [f k x], in turn, for each binding from key [k] to element [x] in the map [m]. Keys are
     presented to [f] in increasing order according to the map's ordering. *)

  val iter: ('a -> 'b -> unit) -> ('a, 'b) t -> unit

  (* [fold f m seed] invokes [f k d accu], in turn, for each binding from key [k] to datum [d] in the map
     [m]. Keys are presented to [f] in increasing order according to the map's ordering. The initial value of
     [accu] is [seed]; then, at each new call, its value is the value returned by the previous invocation of [f]. The
     value returned by [fold] is the final value of [accu]. *)

  val fold: ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

  (* [fold_rev] performs exactly the same job as [fold], but presents keys to [f] in the opposite order. *)

  val fold_rev: ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

  (* It is valid to evaluate [iter2 f m1 m2] if and only if [m1] and [m2] have the same domain, according to their
     natural key ordering(s). Doing so invokes [f k x1 x2], in turn, for each key [k] bound to [x1] in [m1] and
     to [x2] in [m2]. Bindings are presented to [f] in increasing order according to the maps' key ordering(s). *)

  val iter2: ('a -> 'b -> 'c -> unit) -> ('a, 'b) t -> ('a, 'c) t -> unit

  (* [map f m] returns the map obtained by composing the map [m] with the function [f]; that is, the map
     $k\mapsto f(m(k))$. *)

  val map: ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t

  (* [endo_map] is similar to [map], but attempts to physically share its result with its input. This saves
     memory when [f] is the identity function. *)

  val endo_map: ('b -> 'b) -> ('a, 'b) t -> ('a, 'b) t

  (* A map's domain is a set. Thus, to be able to perform operations on domains, we need set operations, provided by
     the [Domain] sub-module. The two-way connection between maps and their domains is given by two additional
     functions, [domain] and [lift]. [domain m] returns [m]'s domain. [lift f s] returns the map $k\mapsto f(k)$,
     where $k$ ranges over a set of keys [s]. *)

  module Domain : GSet.S
  
  val domain: ('a, 'b) t -> 'a Domain.t
  val lift: ('a -> 'b) -> 'a Domain.t -> ('a, 'b) t

  (* [corestrict o m d] performs a co-restriction of the map [m] to the domain [d]. That is, it returns the map
     $k\mapsto m(k)$, where $k$ ranges over all keys bound in [m] but \emph{not} present in [d]. As usual, the
     ordering [o] must be consistent with the domain [d] and the map [m]. *)

  val corestrict: 'a ordering -> ('a, 'b) t -> 'a Domain.t -> ('a, 'b) t

end

module type Fixed = sig

  (* In this signature, the ordered type of keys is fixed. *)

  type key

  (* The type of maps whose data have type ['a]. *)

  type 'a t

  (* Each operation's semantics is as described above, except no ordering on keys has to be given. *)

  val empty: 'a t
  val lookup: key -> 'a t -> 'a
  val add: key -> 'a -> 'a t -> 'a t
  val singleton: key -> 'a -> 'a t

  type 'a decision = 'a -> 'a -> 'a

  val is_empty: 'a t -> bool
  val cardinal: 'a t -> int
  val lookup_and_remove: key -> 'a t -> 'a * 'a t
  val union: 'a t -> 'a t -> 'a t
  val fine_union: 'a decision -> 'a t -> 'a t -> 'a t
  val iter: (key -> 'a -> unit) -> 'a t -> unit
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val fold_rev: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter2: (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit
  val map: ('a -> 'b) -> 'a t -> 'b t
  val endo_map: ('a -> 'a) -> 'a t -> 'a t

  module Domain : GSet.Fixed with type element = key

  val domain: 'a t -> Domain.t
  val lift: (key -> 'a) -> Domain.t -> 'a t
  val corestrict: 'a t -> Domain.t -> 'a t

end

