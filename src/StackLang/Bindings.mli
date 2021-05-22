open StackLangBasics

(**A set of bindings is a finite map of registers to values. It can be thought
   of as a simultaneous assignment of values to registers; it is analogous to
   an OCaml [let/and] construct. *)
type t

val empty : t
(** An empty set of bindings. *)

val is_empty : t -> bool
(** [is_empty b] is [true] if [b] is empty. *)

val singleton : register -> value -> t
(** [singleton r v] Is the singleton binding [r <- v]. *)

val singleton_pattern : pattern -> value -> t
(** [singleton_pat p v] Is the binding of a value to a pattern. *)

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
(** [apply s value] apply to rules of the substitution [s] to [value]
    recursively.
    [IReturn (apply s value)] is equivalent to [restore_defs s (IReturn value)]. *)

(* val apply_registers : t -> registers -> registers *)
(** [apply_registers s regs] returns the set of register [reg'] such that
    there exists [reg] in [regs] such that [reg'] is referred to in
    [apply s reg]. *)

val compose : t -> t -> t
(** [compose b1 b2] returns a bindings [b] such that :
      [def b1 (def b2 block)] is equivalent to
      [def b block] *)

val domain : t -> registers
(**[domain b] is the domain of [b], that is, the set of registers assigned by
   [b]. *)

val codomain : t -> registers

val restrict : t -> registers -> t

val to_list : t -> (register * value) list
(**[to_list b] is the set of bindings [b], viewed as a list of register/value
   pairs, in an unspecified order. *)

val fold : (register -> value -> 'a -> 'a) -> t -> 'a -> 'a
(** [fold] iterates over a set of bindings, in an unspecified order. *)
