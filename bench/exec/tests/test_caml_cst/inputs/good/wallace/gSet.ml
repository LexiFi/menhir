(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/gSet.ml,v 1.12 2000/04/12 09:46:35 fpottier Exp $ *)

(* This is a generic set interface. In other words, this signature documents the operations which should be provided
   by any general-purpose implementation of sets. *)

module type S = sig

  (* The type of sets whose elements have type ['a]. *)

  type 'a t

  (* An ordering is a two-argument function [f] such that [f e1 e2] is zero if the elements [e1] and [e2] are equal,
     strictly negative if [e1] is smaller than [e2], and strictly positive if [e1] is greater than [e2]. For instance,
     a suitable ordering function for integers is [(-)]. The generic comparison function [Pervasives.compare] is also
     a suitable ordering. *)

  type 'a ordering = 'a -> 'a -> int

  (* The empty set. *)

  val empty: 'a t

  (* [is_empty s] tells whether [s] is the empty set. *)

  val is_empty: 'a t -> bool

  (* [singleton x] returns a singleton set containing [x] as its only element. *)

  val singleton: 'a -> 'a t

  (* [is_singleton s] returns [x] if [s] is a singleton containing [x] as its only element; otherwise, it raises
     [NotSingleton]. *)

  exception NotSingleton

  val is_singleton: 'a t -> 'a

  (* [make2 o x y] creates a set whose elements are [x] and [y]. [x] and [y] need not be distinct according
     to the ordering [o]. *)

  val make2: 'a ordering -> 'a -> 'a -> 'a t

  (* [cardinal s] returns the cardinal of [s]. *)

  val cardinal: 'a t -> int

  (* [mem o x s] returns [true] if and only if some value equal to [x] modulo [o] appears in the set [s]. The ordering
     [o] must be consistent with the set [s]. *)

  val mem: 'a ordering -> 'a -> 'a t -> bool

  (* [memp ox s] looks for an element equal to [x] within the set [s], if [ox] is the partial application of the set's
     ordering, [o], to some value [x]. If such an element exists, it is returned. Otherwise, the call raises
     [Not_found]. *)

  val memp: ('a -> int) -> 'a t -> 'a

  (* [add o x s] returns a set whose elements are all elements of [s], plus [x]. The ordering [o] must be consistent
     with the set [s]. *)

  val add: 'a ordering -> 'a -> 'a t -> 'a t

  (* [add o x s] returns a set whose elements are all elements of [s], plus [x]. The ordering [o] must be consistent
     with the set [s]. If the set [s] already contains an element equal to [x] (according to [o]) then [Unchanged] is
     raised. *)

  exception Unchanged

  val strict_add: 'a ordering -> 'a -> 'a t -> 'a t

  (* [fine_add o decide x s] returns a set whose elements are all elements of [s], plus [x]. If an element $x_0$ equal
     to [x] (according to [o]) already existed in [s], then [decide] is called. It is passed both elements (first
     $x_0$, then [x]) and must return the element which shall appear in the final set. This element may be different
     from [x] and from $x_0$, but must be equal to both according to [o]. As usual, the ordering [o] must be
     consistent with the set [s]. *)

  type 'a decision = 'a -> 'a -> 'a

  val fine_add: 'a ordering -> 'a decision -> 'a -> 'a t -> 'a t

  (* [remove o x s] returns a set whose elements are all elements of [s], except [x]. The ordering [o] must be
     consistent with the set [s]. *)

  val remove: 'a ordering -> 'a -> 'a t -> 'a t

  (* [removep ox s] looks for an element equal to [x] within the set [s], if [ox] is the partial application of the
     set's ordering, [o], to some value [x]. If such an element exists, it is returned, together with the set deprived
     of it. Otherwise, the call raises [Not_found]. *)

  val removep: ('a -> int) -> 'a t -> 'a * 'a t

  (* [union o s1 s2] returns the union of the sets [s1] and [s2]. The ordering [o] must be consistent with both
     of these sets. *)

  val union: 'a ordering -> 'a t -> 'a t -> 'a t

  (* [fine_union o decide s1 s2] returns the union of the sets [s1] and [s2]. If equal (according to [o]) elements
     [x1] and [x2] appear in [s1] and [s2], respectively, then [decide] is called. It is passed both elements (first
     [x1], then [x2]) and must return the element which shall appear in the final set. This element may be different
     from [x1] and from [x2], but must be equal to both according to [o]. As usual, the ordering [o] must be
     consistent with the set [s]. *)

  val fine_union: 'a ordering -> 'a decision -> 'a t -> 'a t -> 'a t

  (* [diff o s t] returns the set difference of [s] and [t], that is, $s\setminus t$. The ordering [o] must
     be consistent with both of these sets. *)

  val diff: 'a ordering -> 'a t -> 'a t -> 'a t

  (* [corestrict o s1 s2] assumes [s1] is a function graph (i.e. a set of pairs), and [s2] is some subset of [s1]'s
     domain space. It returns the graph of the co-restriction of [s1] with respect to [s2]. In other words, it
     returns the set of all pairs in [s1] whose first component is \emph{not} in [s2]. The ordering [o] must
     be consistent with both of these sets. *)

  val corestrict: 'a ordering -> ('a * 'b) t -> 'a t -> ('a * 'b) t

  (* [disjoint o s1 s2] returns [true] if and only if the sets [s1] and [s2] are disjoint, i.e. iff their intersection
     is empty. The ordering [o] must be consistent with both of these sets. *)

  val disjoint: 'a ordering -> 'a t -> 'a t -> bool

  (* [iter f s] invokes [f x], in turn, for each element [x] of the set [s]. Elements are presented to [f] in
     increasing order according to the set's ordering. *)

  val iter: ('a -> unit) -> 'a t -> unit

  (* [fold f s seed] invokes [f x accu], in turn, for each element [x] of the set [s]. Elements are presented to [f] in
     increasing order according to the set's ordering. The initial value of [accu] is [seed]; then, at each new call,
     its value is the value returned by the previous invocation of [f]. The value returned by [fold] is the final
     value of [accu]. In other words, if $s = \{ x_1, x_2, \ldots, x_n \}$, where $x_1 < x_2 < \ldots < x_n$, then
     [fold f s seed] computes $([f]\,x_n\,\ldots\,([f]\,x_2\,([f]\,x_1\,[seed]))\ldots)$. *)

  val fold: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (* [fold_rev] performs exactly the same job as [fold], but presents elements to [f] in the opposite order. *)

  val fold_rev: ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  (* It is valid to evaluate [iter2 f s1 s2] if and only if [s1] and [s2] are equal sets, according to their natural
     ordering(s). Doing so invokes [f x1 x2], in turn, for each pair of equal elements [x1] and [x2]. Elements are
     presented to [f] in increasing order according to the sets' ordering(s). *)

  val iter2: ('a -> 'b -> unit) -> 'a t -> 'b t -> unit

  (* [iterator s] returns a stateful iterator over the set [s]. That is, if $s = \{ x_1, x_2, \ldots, x_n \}$, where
     $x_1 < x_2 < \ldots < x_n$, then [iterator s] is a function which, when invoked for the $k^{\text{th}}$ time,
     returns [Some ]$x_k$, if $k\leq n$, and [None] otherwise. *)

  val iterator: 'a t -> unit -> 'a option

  (* [exists p s] returns [true] if and only if some element of [s] matches the predicate [p]. *)

  val exists: ('a -> bool) -> 'a t -> bool

  (* [compare o] is an ordering over sets whose ordering is [o]. In other words, if [s1] and [s2] have ordering [o],
     then [compare o s1 s2] compares them and returns an integer code. *)

  val compare: 'a ordering -> ('a t) ordering

  (* [equal o] implements equality over sets whose ordering is [o]. In other words, if [s1] and [s2] have ordering
     [o], then [equal o s1 s2] compares them and returns [true] if and only if they have the same elements. *)

  val equal: 'a ordering -> ('a t -> 'a t -> bool)

  (* [subset o] implements the subset predicate over sets whose ordering is [o]. In other words, if [s] and [t] have
     ordering [o], then [subset o s t] compares them and returns [true] if and only if $s\subseteq t$. *)

  val subset: 'a ordering -> ('a t -> 'a t -> bool)

  (* [filter o p s] returns the subset of [s] formed by all elements which satisfy the predicate [p]. The ordering [o]
     must be consistent with [s]. *)

  val filter: 'a ordering -> ('a -> bool) -> 'a t -> 'a t

  (* [map o f s] computes the image of [s] through [f]. [o] becomes the ordering of the result set. *)

  val map: 'b ordering -> ('a -> 'b) -> 'a t -> 'b t

  (* [monotone_map f s] computes the image of [s] through [f], which must be a monotone function, i.e. preserve the
     elements' \emph{strict} ordering. *)

  val monotone_map: ('a -> 'b) -> 'a t -> 'b t

  (* [endo_map] is similar to [map], but attempts to physically share its result with its input. This saves
     memory when [f] is the identity function. *)

  val endo_map: ('a -> 'a) -> 'a t -> 'a t

end

(* This signature provides exactly the same functionality as above, but assumes the ordered type of keys to be fixed,
   instead of requiring all operations to be polymorphic. *)

module type Fixed = sig

  (* Elements and sets. *)

  type element
  type t

  (* Basic creation and decomposition functions, involving 0, 1 or 2 elements. *)

  val empty: t
  val is_empty: t -> bool
  val singleton: element -> t
  exception NotSingleton
  val is_singleton: t -> element
  val make2: element -> element -> t

  val cardinal: t -> int

  (* Elementary set operations, involving one element and one set. *)

  val mem: element -> t -> bool
  val add: element -> t -> t
  type decision = element -> element -> element
  val fine_add: decision -> element -> t -> t
  val remove: element -> t -> t

  (* Advanced operations, involving two sets. *)

  val union: t -> t -> t
  val fine_union: decision -> t -> t -> t
  val diff: t -> t -> t
  val disjoint: t -> t -> bool

  (* Iterators. *)

  val iter: (element -> unit) -> t -> unit
  val fold: (element -> 'b -> 'b) -> t -> 'b -> 'b
  val fold_rev: (element -> 'b -> 'b) -> t -> 'b -> 'b
  val iter2: (element -> element -> unit) -> t -> t -> unit
  val iterator: t -> unit -> element option
  val exists: (element -> bool) -> t -> bool

  (* Miscellaneous. *)

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val subset: t -> t -> bool
  val filter: (element -> bool) -> t -> t

  (* Mapping one set to another. *)

  val map: (element -> element) -> t -> t
  val monotone_map: (element -> element) -> t -> t
  val endo_map: (element -> element) -> t -> t

end

