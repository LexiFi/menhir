type register = string

(** A tag is an integer value. A tag can be used to encode a state of an LR
   automaton. *)
type tag

module RegisterSet = StringSet
module RegisterMap = StringMap

module TagMap : MMap.S with type key = tag

module TagSet : sig
  include Set.S with type elt = tag

  val all : t

  val to_string : t -> string
end

type registers = RegisterSet.t

val string_of_tag : tag -> string

val tag_of_node : Lr1.node -> tag

val tag_of_int : int -> tag
(** Unsafe : only for testing purposes *)

(** A code label is identified by its name. *)
type label = string

(** A terminal symbol. *)
type terminal = Grammar.Terminal.t

(** A set of terminal symbols. *)
type terminals = Grammar.TerminalSet.t

(** A value is a piece of data that can be pushed onto the stack. Values
   include tags, data loaded from a register, and tuples of values. *)
type value =
  | VTag of tag
  | VReg of register
  | VTuple of value list
  | VUnit

(** A pattern describes how to decompose and store a piece of data that is
   popped off the stack. Patterns include wildcards, registers, and tuples
   of patterns. *)
type pattern =
  | PWildcard
  | PReg of register
  | PTuple of pattern list
