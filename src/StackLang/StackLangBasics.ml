type register = string

module RegisterSet = StringSet
module RegisterMap = StringMap
module TagMap = IntMap

module TagSet = struct
  include Set.Make (Int)

  let all = of_list (List.init (Lr1.n + 1) Fun.id)

  let to_string set =
    Printf.sprintf
      "{%s}"
      (String.concat "; " (List.map string_of_int (elements set)))
end

type registers = RegisterSet.t

(** A tag is an integer value. A tag can be used to encode a state of an LR
   automaton. *)
type tag = int

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
