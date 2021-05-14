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

(* This module discovers and publishes information about the automaton.

   It determines the shape of the stack when a state is about to be
   entered, when a production is about to be reduced, and when a goto
   transition is about to be taken.

   It also determines which states should be represented (that is,
   need to physically exist on the stack at runtime) and which symbols
   need to keep track of (start or end) positions.

   It also determines which automaton states could have to deal with an
   [error] token.

   The information computed in this module is used in the code back-end,
   in the Coq back-end, and in the automated production of .messages files.
   It is not used in the table back-end. *)

open Grammar

(* ------------------------------------------------------------------------- *)
(* A representation of stack shapes. *)

(**A cell is a representation of a stack cell. *)
type cell = private {

  symbol: Symbol.t;
  (**The symbol associated with this cell. This symbol determines the
     presence and the type of the semantic value stored in this cell.
     It also determines whether a start position and an end position
     are stored in this cell. *)

  states: Lr1.NodeSet.t;
  (**A set of possible states such that the state that is stored in this
     cell (or would be stored in this cell) must be a member of this set.
     The states in this set have the property that either all of them are
     represented, in which case [holds_state] is [true], or none of them is
     represented, in which case [holds_state] is [false]. *)

  holds_semv: bool;
  (**Whether a semantic value is stored in this cell. By convention, if
     [symbol] is a nonterminal symbol, then a semantic value is stored.
     (We do not attempt to detect the situation where the semantic value
     could be omitted because it has type [unit], or the situation where
     it could be omitted because it is never used.) If [symbol] is a
     terminal symbol, then a semantic value is stored if and only if
     the [%token] declaration was annotated with a type. *)

  holds_state: bool;
  (**Whether a state is stored in this cell. *)

  holds_startp: bool;
  (**Whether a start position is stored in this cell. This decision
     is a function of [symbol]. *)

  holds_endp: bool;
  (**Whether an end position is stored in this cell. This decision
     is a function of [symbol]. *)

}

(**A word is a representation of a stack suffix. A word is an immutable
   array of cells, whose right end represents the top of the stack. Thus,
   the index 0 in the array corresponds to the cell that lies deepest in
   the stack. *)
type word =
  cell array

(**[pop w] is the stack [w], deprived of its top element (if it exists). *)
val pop: word -> word

(**[fold_top f default w] returns [f cell], where [cell] is the top cell
   in the stack [w], if [w] is nonempty. Otherwise, it returns [default]. *)
val fold_top: (cell -> 'a) -> 'a -> word -> 'a

(* ------------------------------------------------------------------------- *)
(* Information about the stack. *)

(* [stack s] is the structure of the stack at state [s]. *)

val stack: Lr1.node -> word

(* [prodstack prod] is the structure of the stack when production
   [prod] is about to be reduced. *)

(* Until 2020/11/20, it was forbidden to call this function if production
   [prod] is never reduced. It is now possible to do so. In that case, it
   returns a word where every cell contains an empty set of states. *)

val prodstack: Production.index -> word

(* [gotostack nt] is the structure of the stack when a shift
   transition over nonterminal [nt] is about to be taken. It
   consists of just one cell. *)

val gotostack: Nonterminal.t -> word

(* [rewind s] explains how to rewind the stack when dealing with an
   error in state [s]. It produces an instruction to either die
   (because no state on the stack can handle errors) or pop a suffix
   of the stack. In the latter case, one reaches a state that is
   either represented (its identity is physically stored in the
   bottommost cell that is popped) or unrepresented (its identity is
   statically known). *)

type instruction =
  | Die
  | DownTo of word * state

and state =
  | Represented
  | UnRepresented of Lr1.node

val rewind: Lr1.node -> instruction

(* ------------------------------------------------------------------------- *)
(* Information about which states and positions need to physically
   exist on the stack. *)

(* [represented s] tells whether state [s] must have an explicit
   representation, that is, whether it is pushed onto the stack. *)

val represented: Lr1.node -> bool

(* [startp symbol] and [endp symbol] tell whether start or end
   positions must be recorded for symbol [symbol]. *)

val startp: Symbol.t -> bool
val endp: Symbol.t -> bool

(* ------------------------------------------------------------------------- *)
(* Information about error handling. *)

(* [errorpeeker s] tells whether state [s] can potentially peek at an
   error. This is the case if, in state [s], an error token may be on
   the stream. *)

val errorpeeker: Lr1.node -> bool

(* ------------------------------------------------------------------------- *)
(* Miscellaneous. *)

(* [universal symbol] tells whether every represented state has an
   outgoing transition along [symbol]. *)

val universal: Symbol.t -> bool

(* ------------------------------------------------------------------------- *)
(* More information about the stack. *)

module Long : sig

  (* [Long.stack s] is the known suffix of the stack in state [s], presented
     as an array of symbols, where the rightmost end of the array represents
     the top of the stack (just as in the right-hand side of a production).
     This known suffix is as long as possible, based on an analysis of the
     automaton; it is possibly longer than the suffix obtained by [stack s],
     whose length is always the maximum position of the items in state [s]. *)

  val stack: Lr1.node -> Symbol.t array

end
