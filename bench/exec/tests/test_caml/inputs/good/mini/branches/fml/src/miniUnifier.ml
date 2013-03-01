(* $Id$ *)

(** This module instanciate a unifier for the Mini language. *)
include Unifier.Make (MiniMultiEquation) (MiniTypingExceptions)
