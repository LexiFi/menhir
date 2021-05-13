open StackLangBasics

(** The type of bindings. *)
type t = private value RegisterMap.t

val empty : t
(** empty substitution *)

val singleton : register -> value -> t
(** Singleton binding *)

val simple : pattern -> value -> t
(** Binds a value to a pattern *)

val extend : register -> value -> t -> t
(** [extend reg value s] extends [s] with a rule [reg := value].
      [restore_defs (extend reg value s) block] is equivalent to
      [restore_defs s (IDef(PReg reg, value, block))].
      *)

val extend_pattern : t -> pattern -> value -> t
(** [extend s pattern value] return [s'] such that [restore_defs s' block] is
      equivalent to [restore_defs s (IDef(pattern, value, block))]. *)

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
      [IReturn (apply s value)] is equivalent to
      [restore_defs s (IReturn value)]. *)

val apply_pattern : t -> pattern -> pattern
(** [substitute s pattern] apply the rules of the substitution [s] to
      [pattern] recursively. It assumes that every relevant rule has shape
      [_ := VReg(_)]. *)

val apply_reg : t -> register -> register
(** [apply_reg s reg] if [apply s (VReg reg)] returns a value of shape
      [VReg reg'], returns [reg']. Fails if it is not the case. *)

val apply_registers : t -> registers -> registers
(** [apply_registers s regs] returns the set of register [reg'] such that
      there exists [reg] in [regs] such that [reg'] is referred to in
      [apply s reg]. *)

val fold : (register -> value -> 'a -> 'a) -> t -> 'a -> 'a
(** Fold over every rule. *)

val compose : t -> t -> t
(** [compose s1 s2] returns a substitution [s] such that :
      [restore_defs s1 (restore_defs s2 block)] is equivalent to
      [restore_defs s block] *)

val domain : t -> registers

val values : t -> value list

val codomain : t -> registers

val restrict : t -> registers -> t
