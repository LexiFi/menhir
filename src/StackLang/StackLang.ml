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

include StackLangBasics

type bindings = Bindings.t

(* -------------------------------------------------------------------------- *)

(* A primitive operation involves the execution of some OCaml code. The
   primitive operations include OCaml function calls, read accesses to an
   OCaml record, accesses to a dummy position, and semantic actions. *)

(* The set of registers read by a semantic action [a] is [Action.vars a].
   By convention, in front in every semantic action, we generate an
   [INeed] instruction so as to make this requirement explicit. *)

type primitive =
  | PrimOCamlCall of string * value list
  | PrimOCamlFieldAccess of value * field
  | PrimOCamlDummyPos
  | PrimOCamlAction of bindings * action

and field =
  string

and action =
  Action.t

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

type tagpat =
  | TagMultiple of tag list

(* -------------------------------------------------------------------------- *)

(** Type representing a stack cell *)
type cell_info =
  { typ: Stretch.ocamltype option
        (** Ocaml type of the semantic value. None means no type, sometime compiled as [unit]. *)
  ; hold_semv: bool  (** Whether the semantic value is represented. *)
  ; hold_state: bool  (** Whether the state is represented *)
  ; hold_startpos: bool  (** Whether the start position is represented *)
  ; hold_endpos: bool  (** Whether the end position is represented *) }

(** information on a state. *)
type state_info =
  { known_cells: cell_info array
        (** The known stack suffixes associated with the state *)
  ; sfinal_type: Stretch.ocamltype option
        (** The final type associated with the state*) }


(** A block is a tree-shaped collection of instructions. (In classic compiler
   terminology, it could be known as an extended basic block.) The simplest
   instructions have exactly one successor. However, the case analysis
   instructions can have more than successor (this is where several tree
   branches become separate), and the control instructions have no successor
   (this is where a tree branch ends). *)
type block =
  (* Group 1: Instructions with exactly one successor. *)
  | INeed of registers * block
  | IPush of value * cell_info * block
      (** [IPush] pushes a value onto the stack. *)
  | IPop of pattern * block  (** [IPop] pops a value off the stack. *)
  | IDef of bindings * block
      (** [IDef] can be viewed as a sequence of a push and a pop. It can be used
          to move data between registers or to load a value into a register. *)
  | IPrim of register * primitive * block
      (** [IPrim] invokes a primitive operation and stores its result in a
          register. *)
  | ITrace of string * block  (** [ITrace] logs a message on [stderr]. *)
  | IComment of string * block  (** [IComment] is a comment. *)
  (* Group 2: Instructions with zero successor. *)
  | IDie
      (** [IDie] causes an abrupt termination of the program. It is translated
          into OCaml by raising the exception [Error]. *)
  | IReturn of value
      (** [IReturn] causes the normal termination of the program. A value is
          returned. *)
  | IJump of label
      (** [IJump] causes a jump to a routine identified by its label. The
          registers that are needed by the destination block must form a subset
          of the registers that are defined at the point of the jump. *)
  (* Group 3: Case analysis instructions. *)
  | ICaseToken of register * (tokpat * block) list * block option
      (** [ICaseToken] performs a case analysis on a token (which is held in a
          register). It carries a list of branches, each of which is guarded by
          a pattern, and an optional default branch. *)
  | ICaseTag of register * (tagpat * block) list
      (** [ICaseTag] performs a case analysis on a tag (which is held in a
          register). It carries a list of branches, each of which is guarded by
          a pattern. There is no default branch; it is up to the user to ensure
          that the case analysis is exhaustive. *)
  | ITypedBlock of typed_block
      (** ITypedBlock introduces a typed block in the middle of a routine.
          This is used to inline a routine in another without loosing type
          information. *)

(** A typed block is a block annotated with information :
    The known suffixes of the stack, the final type returned by the block, the
    needed registers, and whether there is a match on tags inside the block. *)
and typed_block =
  { block: block
  ; stack_type: cell_info array
  ; name: string option
  ; final_type: Stretch.ocamltype option
  ; needed_registers: RegisterSet.t
  ; has_case_tag: bool }


(* -------------------------------------------------------------------------- *)

(* A control flow graph is a mapping of code labels to blocks. *)

module LabelSet = StringSet
module LabelMap = StringMap

type cfg =
  typed_block LabelMap.t

type states =
  state_info TagMap.t

(** A complete program is a control flow graph where some labels have been
   marked as entry points. There is in fact a mapping of the LR(1) start
   states to entry points.
   It is supplemented by a map encoding for every represented state, the typing
   information associated to said state. *)
type program =
  {cfg: cfg; entry: string StringMap.t; states: states}
