(* Registers. *)

type register =
  string

module RegisterSet = StringSet
module RegisterMap = StringMap
type registers = RegisterSet.t

(* Tags. *)

type tag =
  int

module TagMap = MMap.Make (Int)
module TagSet = TagMap.Domain

let string_of_tag = string_of_int

let tag_of_node = Lr1.number

let tag_of_int = Fun.id

(* Labels. *)

type label =
  string

(* Symbols. *)

type terminal =
  Grammar.Terminal.t

type terminals =
  Grammar.TerminalSet.t

(* Values and patterns. *)

type value =
  | VUnit
  | VTag of tag
  | VReg of register
  | VTuple of value list

type pattern =
  | PWildcard
  | PReg of register
  | PTuple of pattern list
