(* $Id$ *)

(** This module provides the typing environment used by {!MiniInfer}. *)
include TypingEnvironment.Make 
  (MiniKindInferencer)
  (MiniConstraint) 
  (MiniTypingExceptions)
