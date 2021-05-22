open StackLangBasics

(**A set of bindings is a finite map of registers to values. It can be thought
   of as a simultaneous assignment of values to registers; it is analogous to
   an OCaml [let/and] construct. *)
type t

val empty : t
(** An empty set of bindings. *)

val is_empty : t -> bool
(** [is_empty bs] is [true] if [bs] is empty. *)

val assign : pattern -> value -> t
(** [assign p v] represents the assignment [p <- v]. *)

val remove : t -> registers -> t
(**[remove bs regs] is the set of bindings [bs], deprived of the bindings
   that concern the registers [regs]. *)

val apply : t -> value -> value
(** [apply bs v] applies the bindings [bs], viewed as a substitution, to the
    value [v]. Thus, the instruction [IReturn (apply bs v)] is equivalent to
    the instruction [defs bs (IReturn v)]. *)

val compose : t -> t -> t
(** [compose bs1 bs2] returns a set of bindings [bs] such that
      [def bs1 (def bs2 block)] is equivalent to
      [def bs block] *)

val domain : t -> registers
(**[domain bs] is the domain of [bs], that is, the set of registers assigned
   by [bs]. *)

val codomain : t -> registers

val restrict : registers -> t -> t

val to_list : t -> (register * value) list
(**[to_list bs] is the set of bindings [bs], viewed as a list of
   register/value pairs, in an unspecified order. *)

val fold : (register -> value -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold] iterates over a set of bindings, in an unspecified order. *)
