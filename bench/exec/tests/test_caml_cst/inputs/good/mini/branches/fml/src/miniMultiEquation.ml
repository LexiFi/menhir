(* $Id$ *)

(** This module instantiates {!MultiEquation} for the Mini language. *)
include (MultiEquation.Make (MiniAlgebra) (IntRank) : 
	   Sig.MultiEquation 
	 with module Algebra = MiniAlgebra)
