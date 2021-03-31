open StackLang

(* A few constructors. *)

val vreg : string -> value

val vregs : string list -> value list

(* A few accessors. *)

val lookup : field -> 'a LabelMap.t -> 'a

val lookup_state : Lr1.node -> 'a TagMap.t -> 'a

val lookup_tag : tag -> 'a TagMap.t -> 'a

val entry_labels : program -> registers

val needed : typed_block -> RegisterSet.t
(** Returns the set of needed registers from a typed block. *)

val value_registers : value -> registers
(** Returns the set of register that appear in a value *)

val branch_iter : ('a -> 'b) -> 'c * 'a -> 'b

val branch_map : ('a -> 'b) -> 'c * 'a -> 'c * 'b

(** [block_map f block] applies [f] to every direct children of [block] *)
val block_map : (block -> block) -> block -> block

(** [successors yield block] applies the function [yield] in turn to every
   label that is the target of a [jump] instruction in the block [block]. *)
val successors: (label -> unit) -> block -> unit

val value_refers_to_register: register -> value -> bool

val state_info_intersection: state_info IntMap.t -> tag list -> state_info

(** Remove empty cells from an array of cell information. *)
val filter_stack: cell_info array -> cell_info array

val test: unit -> unit