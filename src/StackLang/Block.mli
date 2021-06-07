open StackLang

type t = block

val map :
     (t -> t)
  -> ?push:(value -> cell_info -> t -> value * cell_info * t)
  -> ?pop:(pattern -> t -> pattern * t)
  -> ?def:(bindings -> t -> bindings * t)
  -> ?prim:(register -> primitive -> t -> register * primitive * t)
  -> ?trace:(trace -> t -> trace * t)
  -> ?comment:(string -> t -> string * t)
  -> ?die:(unit -> unit)
  -> ?return:(value -> value)
  -> ?jump:(label -> label)
  -> ?case_token:
       (   register
        -> (tokpat * t) list
        -> t option
        -> register * (tokpat * t) list * t option )
  -> ?case_tag:(register -> (tagpat * t) list -> register * (tagpat * t) list)
  -> ?typed_block:(typed_block -> typed_block)
  -> t
  -> t
(** [Block.map f block] applies [f] to every direct child of [block], and return
    the same block with its child replaced by the return value of [f], if the
    optional function corresponding to the block is not specified. If it is
    specified, then that function is called instead, allowing to change the
    fields of each block. *)

val iter :
     (t -> unit)
  -> ?push:(value -> cell_info -> t -> unit)
  -> ?pop:(pattern -> t -> unit)
  -> ?def:(bindings -> t -> unit)
  -> ?prim:(register -> primitive -> t -> unit)
  -> ?trace:(trace -> t -> unit)
  -> ?comment:(string -> t -> unit)
  -> ?die:(unit -> unit)
  -> ?return:(value -> unit)
  -> ?jump:(label -> unit)
  -> ?case_token:(register -> (tokpat * t) list -> t option -> unit)
  -> ?case_tag:(register -> (tagpat * t) list -> unit)
  -> ?typed_block:(typed_block -> unit)
  -> t
  -> unit
(** [Block.iter f block] applies [f] to every direct children of [block], if the
    optional function corresponding to the block is not specified.
    If it is specified, then that function is called instead, allowing to react
    to the fields of each block. *)

val reduce : (t -> 'a) -> ('a list -> 'a) -> t -> 'a
(** [block_iter f aggregate block] applies [f] to every direct
    child of [block]. [aggregate] is called on the list of results of every
    block : [[]] is the block is terminal, [[f b]] if it has a single child or
    the list of [f b] for every child if it has multiple. *)

val successors : (StackLang.label -> unit) -> t -> unit
(** [successors yield block] applies the function [yield] in turn to every
   label that is the target of a [jump] instruction in the block [block]. *)

val contains : t -> t -> bool
(** [contains block subblock] is true if [subblock] is among the successors of
    [block] *)

val push : value -> cell_info -> t -> t

val pop : pattern -> t -> t

val sdef : pattern -> value -> t -> t

val def : bindings -> t -> t

val prim : register -> primitive -> t -> t

val comment : string -> t -> t

val die : t

val return : value -> t

val jump : label -> t

val case_token : ?default:t -> register -> (tokpat * t) list -> t

val case_tag : register -> (tagpat * t) list -> t

val typed_block :
     cell_info array
  -> registers
  -> bool
  -> ?final_type:Stretch.ocamltype
  -> ?name:string
  -> t
  -> t
