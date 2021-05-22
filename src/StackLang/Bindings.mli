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

val extend : t -> register -> value -> t
(** [extend bs reg value] extends [bs] with a rule [reg <- value].
    [def (extend bs reg value) block] is equivalent to
    [def bs (def (singleton reg, value, block))]. *)

val extend_pattern : t -> pattern -> value -> t
(** [extend bs pattern value] return [bs'] such that [defs bs' block] is
    equivalent to [defs bs (IDef(pattern, value, block))]. *)

val remove : t -> pattern -> t
(** [remove s pattern] remove every rule of the shape [r := _] for every [r] a
    register occuring in [pattern] *)

val remove_registers : t -> registers -> t
(** [remove s regs] For every [r] in [regs], remove every rule of shape
    [r := _] from s *)

val remove_value : t -> value -> t
(** [remove s value] For every [r] referred by [value], remove every rule of
    shape [r := _] from s. *)

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

val restrict : t -> registers -> t

val to_list : t -> (register * value) list
(**[to_list bs] is the set of bindings [bs], viewed as a list of
   register/value pairs, in an unspecified order. *)

val fold : (register -> value -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold] iterates over a set of bindings, in an unspecified order. *)
