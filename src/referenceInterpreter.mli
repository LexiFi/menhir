open Grammar
open Cst

(* This reference interpreter animates the LR automaton. It uses the
   grammar and automaton descriptions, as provided by [Grammar] and
   [Lr1], as well as the generic LR engine in [MenhirLib.Engine]. *)

(* The first parameter to the interpreter is a Boolean flag that tells
   whether a trace should be produced on the standard error channel. *)

(* The interpreter requires a start symbol, a lexer, and a lexing
   buffer. It either succeeds and produces a concrete syntax tree, or
   fails. *)

val interpret:
  bool ->
  Nonterminal.t ->
  (Lexing.lexbuf -> Terminal.t) ->
  Lexing.lexbuf ->
  cst option

(* This variant of the reference interpreter is used internally by us. We use
   it to debug [LRijkstra]. It checks that a sentence leads to a syntax error
   in the expected state. *)

type check_error_path_outcome =
  (* Bad: the input was read past its end. *)
| OInputReadPastEnd
  (* Bad: a syntax error occurred before all of the input was read. *)
| OInputNotFullyConsumed
  (* Bad: the parser unexpectedly accepted (part of) this input. *)
| OUnexpectedAccept
  (* Good: a syntax error occurred after reading the last input token. *)
| OK of Lr1.node

val check_error_path:
  Nonterminal.t ->   (* initial non-terminal symbol *)
  Terminal.t list -> (* input  *)
  check_error_path_outcome

