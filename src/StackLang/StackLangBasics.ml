(* -------------------------------------------------------------------------- *)

(**A register is a named variable. When one thinks about StackLang as an
   imperative language, a register can be thought of as a mutable global
   variable. In the translation of StackLang through IL to OCaml, a register
   becomes an immutable local variable. *)
type register =
  string

module RegisterSet = StringSet
module RegisterMap = StringMap

type registers = RegisterSet.t

(* -------------------------------------------------------------------------- *)

include (struct
  type tag = int
  module TagMap = MMap.Make (Int)
  module TagSet = TagMap.Domain
  let string_of_tag = string_of_int
  let tag_of_node = Lr1.number
  let tag_of_int = Fun.id
end : sig

  (**A tag encodes a state of an LR automaton. *)
  type tag

  module TagMap : MMap.S with type key = tag
  module TagSet = TagMap.Domain

  (**[string_of_tag] converts a tag to a string. *)
  val string_of_tag : tag -> string

  (**[tag_of_node] converts an LR(1) state to a tag. *)
  val tag_of_node : Lr1.node -> tag

  (**[tag_of_int] converts an integer to a tag. It is unsafe and should be used
     only for testing purposes. *)
  val tag_of_int : int -> tag

end)

(* -------------------------------------------------------------------------- *)

(** A code label is identified by its name. *)
type label =
  string

(** A terminal symbol. *)
type terminal =
  Grammar.Terminal.t

(** A set of terminal symbols. *)
type terminals =
  Grammar.TerminalSet.t

(* -------------------------------------------------------------------------- *)

(**A value is a piece of data that can be pushed onto the stack. Values
   include the unit value, tags, data loaded from a register, tuples of
   values. *)
type value =
  | VUnit
  | VTag of tag
  | VReg of register
  | VTuple of value list

(* -------------------------------------------------------------------------- *)

(**A pattern describes how to decompose and store a piece of data that is
   popped off the stack. Patterns include wildcards, registers, and tuples
   of patterns. *)
type pattern =
  | PWildcard
  | PReg of register
  | PTuple of pattern list
