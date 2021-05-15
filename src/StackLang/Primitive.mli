open StackLang

type t = primitive

val registers : t -> registers
(** [Primitive.registers p] is the set of registers referred to by primitive [p]. *)

val action : ?bindings:bindings -> action -> t
(** [Primitive.action ?bindings a] is [PrimOCamlAction(bds, a)] if [bindings] is
    of shape [Some bds], [PrimOCamlAction(Bindings.empty, a)] else. *)
