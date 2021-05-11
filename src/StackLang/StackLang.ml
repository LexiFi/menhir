(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* A StackLang program can be thought of as a control flow graph, that is, a
   mapping of code labels to code blocks. The StackLang machine maintains a
   number of registers and a stack. The number and names of the registers is
   allowed to depend on the program point: that is, at different points in the
   program, different sets of registers may be defined. However, it is
   possible to statically determine which registers are defined at each
   program point. *)

(* -------------------------------------------------------------------------- *)

(* Basic type definitions. *)

(* A register is identified by its name. *)

type register = string

module RegisterSet = StringSet
module RegisterMap = StringMap

type registers = RegisterSet.t

(* A tag is an integer value. A tag can be used to encode a state of an LR
   automaton. *)

type tag = int

(* A code label is identified by its name. *)

type label = string

(* A terminal symbol. *)

type terminal = Grammar.Terminal.t

(* A set of terminal symbols. *)

type terminals = Grammar.TerminalSet.t

(* -------------------------------------------------------------------------- *)

(* A value is a piece of data that can be pushed onto the stack. Values
   include tags, data loaded from a register, and tuples of values. *)

type value =
  | VTag of tag
  | VReg of register
  | VTuple of value list

(* A pattern describes how to decompose and store a piece of data that is
   popped off the stack. Patterns include wildcards, registers, and tuples
   of patterns. *)

type pattern =
  | PWildcard
  | PReg of register
  | PTuple of pattern list

(* -------------------------------------------------------------------------- *)

(* A primitive operation involves the execution of some OCaml code. The
   primitive operations include OCaml function calls, read accesses to an
   OCaml record, accesses to a dummy position, and semantic actions. *)

(* The set of registers read by a semantic action [a] is [Action.vars a].
   By convention, in front in every semantic action, we generate an
   [INeed] instruction so as to make this requirement explicit. *)

type primitive =
  | PrimOCamlCall of string * register list
  | PrimOCamlFieldAccess of register * field
  | PrimOCamlDummyPos
  | PrimOCamlAction of action

and field = string

and action = Action.t

(* -------------------------------------------------------------------------- *)

(* In a case analysis on a token, each branch is guarded by a pattern that
   either selects a single terminal symbol (and stores its semantic value
   in a register) or selects multiple terminal symbols (and ignores their
   semantic value). There can also be a default branch, which is guarded
   by a wildcard pattern; that is implicit. *)

(* If the terminal symbol [tok] does not carry a semantic value, then the
   pattern [TokSingle (tok, r)] writes a unit semantic value into the register
   [r]. *)

type tokpat =
  | TokSingle of terminal * register
  | TokMultiple of terminals

(* -------------------------------------------------------------------------- *)

(* In a case analysis on a tag, each branch is guarded by a pattern that
   selects a set of tags. There is no default branch. *)

type tagpat = TagMultiple of tag list

(* -------------------------------------------------------------------------- *)

(* A block is a tree-shaped collection of instructions. (In classic compiler
   terminology, it could be known as an extended basic block.) The simplest
   instructions have exactly one successor. However, the case analysis
   instructions can have more than successor (this is where several tree
   branches become separate), and the control instructions have no successor
   (this is where a tree branch ends). *)

type block =
  (* Group 1: Instructions with exactly one successor. *)

  (* [INeed] is a special pseudo-instruction that is expected to appear at
     least at the beginning of every block. (It can also be used inside a
     block.) It indicates which registers are expected to be defined at this
     point, and it un-defines any registers that are not explicitly listed. *)
  | INeed of registers * block
  (* [IPush] pushes a value onto the stack. [IPop] pops a value off the stack.
     [IDef] can be viewed as a sequence of a push and a pop. It can be used to
     move data between registers or to load a value into a register. *)
  | IPush of value * block
  | IPop of pattern * block
  | IDef of pattern * value * block
  (* [IPrim] invokes a primitive operation and stores its result in a
     register. *)
  | IPrim of register * primitive * block
  (* [ITrace] logs a message on [stderr]. *)
  | ITrace of string * block
  (* [IComment] is a comment. *)
  | IComment of string * block (* Group 2: Instructions with zero successor. *)
  (* [IDie] causes an abrupt termination of the program. It is translated
     into OCaml by raising the exception [Error]. *)
  | IDie
  (* [IReturn] causes the normal termination of the program. A value read
     from a register is returned. *)
  | IReturn of register
  (* [IJump] causes a jump to a block identified by its label. The registers
     that are needed by the destination block must form a subset of the
     registers that are defined at the point of the jump. *)
  | IJump of label (* Group 3: Case analysis instructions. *)
  (* [ICaseToken] performs a case analysis on a token (which is held in a
     register). It carries a list of branches, each of which is guarded by
     a pattern, and an optional default branch. *)
  | ICaseToken of register * (tokpat * block) list * block option
  (* [ICaseTag] performs a case analysis on a tag (which is held in a
     register). It carries a list of branches, each of which is guarded by a
     pattern. There is no default branch; it is up to the user to ensure that
     the case analysis is exhaustive. *)
  | ICaseTag of register * (tagpat * block) list

(* -------------------------------------------------------------------------- *)

(* A control flow graph is a mapping of code labels to blocks. *)

module LabelSet = StringSet
module LabelMap = StringMap

type cfg = block LabelMap.t

(* A complete program is a control flow graph where some labels have been
   marked as entry points. There is in fact a mapping of the LR(1) start
   states to entry points. *)

type program =
  { cfg : cfg
  ; entry : label Lr1.NodeMap.t
  }

(* -------------------------------------------------------------------------- *)

(* A few constructors. *)

let vreg r = VReg r

let vregs rs = List.map vreg rs

(* A few accessors. *)

let lookup label map =
  try LabelMap.find label map with Not_found -> assert false


let entry_labels program =
  Lr1.NodeMap.fold
    (fun _s label accu -> LabelSet.add label accu)
    program.entry
    LabelSet.empty


(* We assume that every labeled block in a well-formed control flow graph
   begins with an [INeed] instruction that determines which registers are
   defined upon entry to this block. *)

let needed block = match block with INeed (rs, _) -> rs | _ -> assert false
