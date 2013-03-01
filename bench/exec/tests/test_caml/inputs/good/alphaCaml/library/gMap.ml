module type S = sig

  (* Keys are assumed to have a natural total order. *)

  type key

  (* The type of maps whose data have type ['a]. *)

  type 'a t

  (* The empty map. *)

  val empty: 'a t

  (* [lookup k m] looks up the value associated to the key [k] in the
     map [m], and raises [Not_found] if no value is bound to [k]. *)

  val lookup: key -> 'a t -> 'a
  val find: key -> 'a t -> 'a

  (* [add k d m] returns a map whose bindings are all bindings in [m],
     plus a binding of the key [k] to the datum [d]. If a binding
     already exists for [k], it is overridden. *)

  val add: key -> 'a -> 'a t -> 'a t

  (* [strict_add k d m] returns a map whose bindings are all bindings
     in [m], plus a binding of the key [k] to the datum [d]. If a
     binding already exists for [k] then [Unchanged] is raised. *)

  exception Unchanged

  val strict_add: key -> 'a -> 'a t -> 'a t

  (* [fine_add decide k d m] returns a map whose bindings are all
     bindings in [m], plus a binding of the key [k] to the datum
     [d]. If a binding from [k] to [d0] already exists, then the
     resulting map contains a binding from [k] to [decide d0 d]. *)

  type 'a decision = 'a -> 'a -> 'a

  val fine_add: 'a decision -> key -> 'a -> 'a t -> 'a t

  (* [add_or_lookup k d m] adds a binding of [k] to [d] if no binding
     for [k] exists in [m]. It returns the existing datum associated
     with [k] if a binding already exists. *)

  type 'a add_or_lookup =
    | Added of 'a t
    | LookedUp of 'a

  val add_or_lookup: key -> 'a -> 'a t -> 'a add_or_lookup

  (* [mem k m] tells whether the key [k] appears in the domain of the
     map [m]. *)

  val mem: key -> 'a t -> bool

  (* [singleton k d] returns a map whose only binding is from [k] to [d]. *)

  val singleton: key -> 'a -> 'a t

  (* [is_empty m] returns [true] if and only if the map [m] defines no
     bindings at all. *)

  val is_empty: 'a t -> bool

  (* [is_singleton s] returns [Some x] if [s] is a singleton
     containing [x] as its only element; otherwise, it returns
     [None]. *)

  val is_singleton: 'a t -> (key * 'a) option

  (* [cardinal m] returns [m]'s cardinal, that is, the number of keys
     it binds, or, in other words, the cardinal of its domain. *)

  val cardinal: 'a t -> int

  (* [choose m] returns an arbitrarily chosen binding in [m], if [m]
     is nonempty, and raises [Not_found] otherwise. *)

  val choose: 'a t -> key * 'a

  (* [lookup_and_remove k m] looks up the value [v] associated to the
     key [k] in the map [m], and raises [Not_found] if no value is
     bound to [k]. The call returns the value [v], together with the
     map [m] deprived from the binding from [k] to [v]. *)

  val lookup_and_remove: key -> 'a t -> 'a * 'a t
  val find_and_remove: key -> 'a t -> 'a * 'a t

  (* [remove k m] is the map [m] deprived from any binding for [k]. *)

  val remove: key -> 'a t -> 'a t

  (* [union m1 m2] returns the union of the maps [m1] and
     [m2]. Bindings in [m2] take precedence over those in [m1]. *)

  val union: 'a t -> 'a t -> 'a t

  (* [fine_union decide m1 m2] returns the union of the maps [m1] and
     [m2]. If a key [k] is bound to [x1] (resp. [x2]) within [m1]
     (resp. [m2]), then [decide] is called. It is passed [x1] and
     [x2], and must return the value that shall be bound to [k] in the
     final map. *)

  val fine_union: 'a decision -> 'a t -> 'a t -> 'a t

  (* [inter decide m1 m2] returns the intersection of the maps [m1]
     and [m2]. For each key [k] that is bound to [x1] within [m1] and
     to [x2] within [m2], [decide] is called. It is passed [x1] and
     [x2], and must return the value which shall be bound to [k] in
     the final map. The operation returns [m2] itself (as opposed to a
     copy of it) when its result is equal to [m2]. *)

  val inter: 'a decision -> 'a t -> 'a t -> 'a t

  (* [diff m1 m2] returns the map [m1] deprived of any bindings for
     keys that appear in the domain of [m2]. *)

  val diff: 'a t -> 'b t -> 'a t

  (* [disjoint m1 m2] returns [true] if and only if the maps [m1] and
     [m2] have disjoint domains. *)

  val disjoint: 'a t -> 'b t -> bool

  (* [subset m1 m2] returns [true] if and only if the domain of [m1]
     is a subset of the domain of [m2]. *)

  val subset: 'a t -> 'b t -> bool

  (* [iter f m] invokes [f k x], in turn, for each binding from key
     [k] to element [x] in the map [m]. Keys are presented to [f] in
     increasing order. *)

  val iter: (key -> 'a -> unit) -> 'a t -> unit

  (* [fold f m seed] invokes [f k d accu], in turn, for each binding
     from key [k] to datum [d] in the map [m]. Keys are presented to
     [f] in increasing order. The initial value of [accu] is [seed];
     then, at each new call, its value is the value returned by the
     previous invocation of [f]. The value returned by [fold] is the
     final value of [accu]. *)

  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (* [fold_rev] performs exactly the same job as [fold], but presents
     keys to [f] in the opposite order. *)

  val fold_rev: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (* It is valid to evaluate [iter2 f m1 m2] if and only if [m1] and
     [m2] have equal domains. Doing so invokes [f k x1 x2], in turn,
     for each key [k] bound to [x1] in [m1] and to [x2] in
     [m2]. Bindings are presented to [f] in increasing order. *)

  val iter2: (key -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit

  (* [iterator m] returns a stateful iterator over the map [m]. That
     is, if $m = \{ x_1 \mapsto d_1, x_2 \mapsto d_2, \ldots, x_n
     \mapsto d_n \}$, where $x_1 < x_2 < \ldots < x_n$, then [iterator
     s] is a function which, when invoked for the $k^{\text{th}}$
     time, returns [Some ]$(x_k, d_k)$, if $k\leq n$, and [None]
     otherwise. *)

  val iterator: 'a t -> (unit -> (key * 'a) option)

  (* [map f m] returns the map obtained by composing the map [m] with
     the function [f]; that is, the map $k\mapsto f(m(k))$. *)

  val map: ('a -> 'b) -> 'a t -> 'b t

  (* [endo_map] is similar to [map], but attempts to physically share
     its result with its input. This saves memory when [f] is the
     identity function. *)

  val endo_map: ('a -> 'a) -> 'a t -> 'a t

  (* If [dcompare] is an ordering over data, then [compare dcompare]
     is an ordering over maps. *)

  val compare: ('a -> 'a -> int) -> 'a t -> 'a t -> int

  (* A map's domain is a set. Thus, to be able to perform operations
     on domains, we need set operations, provided by the [Domain]
     sub-module. The two-way connection between maps and their domains
     is given by two additional functions, [domain] and
     [lift]. [domain m] returns [m]'s domain. [lift f s] returns the
     map $k\mapsto f(k)$, where $k$ ranges over a set of keys [s]. *)

  module Domain : GSet.S with type element = key

  val domain: 'a t -> Domain.t
  val lift: (key -> 'a) -> Domain.t -> 'a t

  (* [corestrict m d] performs a co-restriction of the map [m] to the
     domain [d]. That is, it returns the map $k\mapsto m(k)$, where
     $k$ ranges over all keys bound in [m] but \emph{not} present in
     [d]. *)

  val corestrict: 'a t -> Domain.t -> 'a t

end

