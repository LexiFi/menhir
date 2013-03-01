module type S = sig

  (* Elements are assumed to have a natural total order. *)

  type element

  (* Sets. *)

  type t

  (* The empty set. *)

  val empty: t

  (* [is_empty s] tells whether [s] is the empty set. *)

  val is_empty: t -> bool

  (* [singleton x] returns a singleton set containing [x] as its only
     element. *)

  val singleton: element -> t

  (* [is_singleton s] returns [Some x] if [s] is a singleton
     containing [x] as its only element; otherwise, it returns
     [None]. *)

  val is_singleton: t -> element option

  (* [make2 x y] creates a set whose elements are [x] and [y].
     [x] and [y] need not be distinct. *)

  val make2: element -> element -> t

  (* [cardinal s] returns the cardinal of [s]. *)

  val cardinal: t -> int

  (* [choose s] returns an arbitrarily chosen element of [s], if [s]
     is nonempty, and raises [Not_found] otherwise. *)

  val choose: t -> element

  (* [mem x s] returns [true] if and only if [x] appears in the set
     [s]. *)

  val mem: element -> t -> bool

  (* [add x s] returns a set whose elements are all elements of [s],
     plus [x]. *)

  val add: element -> t -> t

  (* [strict_add x s] returns a set whose elements are all elements of
     [s], plus [x]. If the set [s] already contains an element equal
     to [x] then [Unchanged] is raised. *)

  exception Unchanged

  val strict_add: element -> t -> t

  (* [fine_add decide x s] returns a set whose elements are all
     elements of [s], plus [x]. If an element [x0] equal to [x]
     already exists in [s], then [decide] is called. It is passed both
     elements (first [x0], then [x]) and returns the element that is
     to appear in the final set. This element may be physically
     distinct from [x] and from [x0], but must be logically equal to
     both. *)

  type decision = element -> element -> element

  val fine_add: decision -> element -> t -> t

  (* [remove x s] returns a set whose elements are all elements of
     [s], except [x]. *)

  val remove: element -> t -> t

  (* [union s1 s2] returns the union of the sets [s1] and [s2]. *)

  val union: t -> t -> t

  (* [fine_union decide s1 s2] returns the union of the sets [s1] and
     [s2]. If equal elements [x1] and [x2] appear in [s1] and [s2],
     respectively, then [decide] is called. It is passed both elements
     (first [x1], then [x2]) and must return the element which shall
     appear in the final set. This element may be physically distinct
     from [x1] and from [x2], but must be logically equal to both. *)

  val fine_union: decision -> t -> t -> t

  (* [diff s t] returns the set difference of [s] and [t], that is,
     $s\setminus t$. *)

  val diff: t -> t -> t

  (* [inter s t] returns the set intersection of [s] and [t], that is,
     $s\cap t$. *)

  val inter: t -> t -> t

  (* [disjoint s1 s2] returns [true] if and only if the sets [s1] and
     [s2] are disjoint, i.e. iff their intersection is empty. *)

  val disjoint: t -> t -> bool

  (* [iter f s] invokes [f x], in turn, for each element [x] of the
     set [s]. Elements are presented to [f] in increasing order. *)

  val iter: (element -> unit) -> t -> unit

  (* [fold f s seed] invokes [f x accu], in turn, for each element [x]
     of the set [s]. Elements are presented to [f] in increasing
     order. The initial value of [accu] is [seed]; then, at each new
     call, its value is the value returned by the previous invocation
     of [f]. The value returned by [fold] is the final value of
     [accu]. In other words, if $s = \{ x_1, x_2, \ldots, x_n \}$,
     where $x_1 < x_2 < \ldots < x_n$, then [fold f s seed] computes
     $([f]\,x_n\,\ldots\,([f]\,x_2\,([f]\,x_1\,[seed]))\ldots)$. *)

  val fold: (element -> 'b -> 'b) -> t -> 'b -> 'b

  (* [elements s] is a list of all elements in the set [s]. *)

  val elements: t -> element list

  (* [fold_rev] performs exactly the same job as [fold], but presents
     elements to [f] in the opposite order. *)

  val fold_rev: (element -> 'b -> 'b) -> t -> 'b -> 'b

  (* It is valid to evaluate [iter2 f s1 s2] if and only if [s1] and
     [s2] are equal sets. Doing so invokes [f x1 x2], in turn, for
     each pair of equal elements [x1] and [x2]. Elements are presented
     to [f] in increasing order according to the sets' ordering(s). *)

  val iter2: (element -> element -> unit) -> t -> t -> unit

  (* [iterator s] returns a stateful iterator over the set [s]. That
     is, if $s = \{ x_1, x_2, \ldots, x_n \}$, where $x_1 < x_2 <
     \ldots < x_n$, then [iterator s] is a function which, when
     invoked for the $k^{\text{th}}$ time, returns [Some ]$x_k$, if
     $k\leq n$, and [None] otherwise. *)

  val iterator: t -> (unit -> element option)

  (* [exists p s] returns [true] if and only if some element of [s]
     matches the predicate [p]. *)

  val exists: (element -> bool) -> t -> bool

  (* [compare] is an ordering over sets. *)

  val compare: t -> t -> int

  (* [equal] implements equality over sets. *)

  val equal: t -> t -> bool

  (* [subset] implements the subset predicate over sets. *)

  val subset: (t -> t -> bool)

  (* [filter p s] returns the subset of [s] formed by all elements
     that satisfy the predicate [p]. *)

  val filter: (element -> bool) -> t -> t

  (* [map f s] computes the image of [s] through [f]. *)

  val map: (element -> element) -> t -> t

  (* [monotone_map f s] computes the image of [s] through [f], which
     must be a monotone function, that is, preserve the elements'
     \emph{strict} ordering. *)

  val monotone_map: (element -> element) -> t -> t

  (* [endo_map] is similar to [map], but attempts to physically share
     its result with its input. This saves memory when [f] is the
     identity function. *)

  val endo_map: (element -> element) -> t -> t

end

