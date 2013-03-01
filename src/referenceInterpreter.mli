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

