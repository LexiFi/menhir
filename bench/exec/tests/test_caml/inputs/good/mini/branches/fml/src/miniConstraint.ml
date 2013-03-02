(* $Id $*)

(** This modules instanciates {!Constraint} for the Mini language. *)
include Constraint.Make (MiniMultiEquation) (IntRank)

