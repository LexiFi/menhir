
type t = StackLang.block

(** [block_map f block] applies [f] to every direct children of [block],
    and return the same block with its child replaced by the return value of
    [f] *)
val map : (t -> t) -> t -> t

(** [block_iter f aggregate terminate block] applies [f] to every direct
    children of [block]. In case of a terminal block, [terminate] is returned.
    In case of a block with multiple children, [aggregate] is applied to the
    list of the return value of [f]. *)
val iter: (t -> 'a) -> ('a list -> 'a) -> 'a -> t -> 'a


(** [successors yield block] applies the function [yield] in turn to every
   label that is the target of a [jump] instruction in the block [block]. *)
val successors: (StackLang.label -> unit) -> t -> unit