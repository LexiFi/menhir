open StackLang

(* A few constructors. *)

val vreg : string -> value

val vregs : string list -> value list

(* A few accessors. *)

val lookup : label -> 'a LabelMap.t -> 'a

val lookup_state : Lr1.node -> 'a TagMap.t -> 'a

val lookup_tag : tag -> 'a TagMap.t -> 'a

val entry_labels : program -> registers

val needed : typed_block -> RegisterSet.t
(** Returns the set of needed registers from a typed block. *)

val branch_iter : ('a -> 'b) -> 'c * 'a -> 'b

val branch_map : ('a -> 'b) -> 'c * 'a -> 'c * 'b

val value_refers_to_register : register -> value -> bool

val is_suffix : Invariant.cell array -> Invariant.cell array -> bool

val state_info_intersection : state_info TagMap.t -> tag list -> state_info
(** [state_info_intersection states tags] d *)

val filter_stack : cell_info array -> cell_info array
(** Remove empty cells from an array of cell information. *)

val longest_known_cells : cell_info array list -> cell_info array
(** [longest_known_cells li] Returns the longest array from [li]. *)

val is_pattern_equivalent_to_value : pattern -> value -> bool
(** [is_pattern_equivalent_to_value pat value] is true if [pat, value] is of
    shape [x, x] or [_, x], and that recursively along tuples.  *)

val test : unit -> unit
