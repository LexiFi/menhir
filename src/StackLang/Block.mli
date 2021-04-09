open StackLang


type t = block

(** [block_map f block] applies [f] to every direct children of [block],
    and return the same block with its child replaced by the return value of
    [f]. It is possible to give special-case functions for each member of the
    block ADT, in order to be able to replace any data contained in the block.
    It is not possible however to change the constructor of the block. *)
val map :
  (t -> t) ->
  ?need:(registers -> t -> registers * t) ->
  ?push:(value -> cell_info -> t -> value * cell_info * t) ->
  ?pop:(pattern -> t -> pattern * t) ->
  ?def:(pattern -> value -> t -> pattern * value * t) ->
  ?prim:(field -> primitive -> t -> field * primitive * t) ->
  ?trace:(field -> t -> field * t) ->
  ?comment:(field -> t -> field * t) ->
  ?die:(unit -> unit) ->
  ?return:(value -> value) ->
  ?jump:(field -> field) ->
  ?substituted_jump:(field -> substitution -> field * substitution) ->
  ?case_token:(field ->
               (tokpat * t) list ->
               t option -> field * (tokpat * t) list * t option) ->
  ?case_tag:(field -> (tagpat * t) list -> field * (tagpat * t) list) ->
  ?typed_block:(typed_block -> typed_block) ->
  t -> t

val iter_unit:
  (t -> unit) ->
  ?need:(registers -> t -> unit) ->
  ?push:(value -> cell_info -> t -> unit) ->
  ?pop:(pattern -> t -> unit) ->
  ?def:(pattern -> value -> t -> unit) ->
  ?prim:(field -> primitive -> t -> unit) ->
  ?trace:(field -> t -> unit) ->
  ?comment:(field -> t -> unit) ->
  ?die:(unit -> unit) ->
  ?return:(value -> unit) ->
  ?jump:(field -> unit) ->
  ?substituted_jump:(field -> substitution -> unit) ->
  ?case_token:(field -> (tokpat * t) list -> t option -> unit) ->
  ?case_tag:(field -> (tagpat * t) list -> unit) ->
  ?typed_block:(typed_block -> unit) ->
  t -> unit



(** [block_iter f aggregate terminate block] applies [f] to every direct
    children of [block]. In case of a terminal block, [terminate] is returned.
    In case of a block with multiple children, [aggregate] is applied to the
    list of the return value of [f]. *)
val iter: (t -> 'a) -> ('a list -> 'a) -> 'a -> t -> 'a


(** [successors yield block] applies the function [yield] in turn to every
   label that is the target of a [jump] instruction in the block [block]. *)
val successors: (StackLang.label -> unit) -> t -> unit

val need: registers -> t -> t

val push: value -> cell_info -> t -> t

val pop: pattern -> t -> t

val def: pattern -> value -> t -> t

val prim: register -> primitive -> t ->t

val comment: string -> t -> t

val die: t

val return: value -> t

val jump: label -> t

val substituted_jump: label -> Substitution.t -> t

val case_token: ?default:t -> register -> (tokpat * t) list -> t

val case_tag: register -> (tagpat * t) list -> t

val typed_block:
  cell_info array ->
  registers ->
  bool ->
  ?final_type:Stretch.ocamltype ->
  ?name:string ->
  t -> t
