(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/primitives.ml,v 1.3 2001/03/28 16:12:35 fpottier Exp $ *)

module type S = sig

  (* Names of primitive operations. *)

  type name

  (* Type nodes. *)

  type node

  (* Given the name of a primitive, this function returns its typing rule, as a list of types (which must be unified
     with the primitive's arguments) together with a type (which stands for the return type of the primitive). The
     first parameter to [rule] is a function [mono] which allows making a type monomorphic; it must be used when
     the primitive allocates fresh mutable storage. *)

  val rule: (node -> unit) -> name -> node list * node

end

