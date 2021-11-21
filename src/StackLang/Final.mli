open StackLang

(**[sub] decides the subtyping relation on finals. This relation is defined
   by [None <= Some _]. This allows a StackLang routine that is polymorphic
   in its final type to be used in a context where the final type is known. *)
val sub : final -> final -> bool

(**[lub] computes the least upper bound of two finals.
   The least upper bound does not always exist: [lub (Some nt1) (Some nt2)]
   is defined only when the equality [nt1 = nt2] holds. *)
val lub : final -> final -> final option
