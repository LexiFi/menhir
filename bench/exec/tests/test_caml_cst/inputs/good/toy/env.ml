(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/toy/env.ml,v 1.2 2000/02/11 16:16:33 fpottier Exp $ *)

(* This signature describes the operations provided by environments. *)

module type S = sig

  (* Our typechecker requires identifiers to be unique throughout every scope. Because this does not hold in an
     actual (source) program, we define an internal notion of identifier, and set up a mechanism which maps source
     program identifiers to internal ones. *)

  type identifier

  (* The type of environments. Because the structure of type schemes is not relevant here, they are represented by
     a type parameter ['a]. *)

  type 'a t

  (* The empty environment. *)

  val empty: 'a t

  (* [lookup] looks up a source program identifier in an environment. *)

  exception Unbound

  type 'a binding =
    | BindingLet of identifier * 'a
    | BindingLambda of identifier

  val lookup: string -> 'a t -> 'a binding

  (* [bind_lambda] adds a new $\lambda$-bound identifier to the given environment. The function expects a source
     program identifier, and generates a new, unique internal identifier, which is returned together with the
     updated environment. *)

  val bind_lambda: string -> 'a t -> identifier * 'a t

  (* [bind_let] adds a new \verb+let+-bound identifier, together with its type scheme, to the given environment.
     The function expects a source program identifier, and returns an updated environment. *)

  val bind_let: string -> 'a -> 'a t -> 'a t

end

