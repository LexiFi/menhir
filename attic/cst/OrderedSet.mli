(**This module offers an abstract type of ordered sets, or in other words,
   lists without duplicate elements. Its implementation is optimized for
   space, under the assumption that every set is very small (a handlful
   of elements). Do not use this code to construct medium or large sets! *)
module Make (X : sig
  type t
  val equal : t -> t -> bool
end) : sig

  (**The type of elements. *)
  type element = X.t

  (**The type of sets. *)
  type t

  (**[empty] is the empty set.*)
  val empty : t

  (**[singleton x] is the singleton set whose element is [x].*)
  val singleton : element -> t

  (**[mem x xs] tests whether [x] is a member of the set [xs]. *)
  val mem : element -> t -> bool

  (**[union xs ys] is the ordered union of the sets [xs] and [ys]. This
     operation is not symmetric. When sets are viewed as sequences, the
     resulting sequence is the concatenation of the sequence [xs] with the
     sequence [ys], deprived of the elements that appear in [xs] already. *)
  val union : t -> t -> t

  (**[elements xs] is the list of the elements of the set [xs]. The order
     of the elements in the set [xs] is respected. *)
  val elements : t -> element list

end
