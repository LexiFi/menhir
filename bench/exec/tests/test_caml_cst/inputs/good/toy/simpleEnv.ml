(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/toy/simpleEnv.ml,v 1.4 2000/02/11 16:16:34 fpottier Exp $ *)

(* This module provides a straightforward implementation of environments. It is parameterized by a general-purpose
   implementation of maps. One may simply use unordered lists, as is usually done, or provide a more evolved data
   structure. *)

module Make (Map : GMap.Additive) = struct

  (* Our typechecker requires identifiers to be unique throughout every scope. Because this does not hold in an
     actual (source) program, we define an internal notion of identifier, and set up a mechanism which maps source
     program identifiers to internal ones.

     Here, identifiers are simply integers. This allows a more efficient implementation of contexts. *)

  type identifier = int

  (* The type of environments. Because the structure of type schemes is not relevant here, they are represented by
     a type parameter ['a].

     Identifiers have to be unique within every scope, not throughout the whole program. This allows us to assign
     identifiers which correspond to their binder's nesting level, rather than to use a global counter. The environment
     carries a field which contains the next available number.

     The data itself is organized as a map from source program names to bindings. *)

  let cs : string Map.ordering =
    Pervasives.compare

  type 'a binding =
    | BindingLet of identifier * 'a
    | BindingLambda of identifier

  type 'a t = {
      env_next_identifier: int;
      env_map: (string, 'a binding) Map.t
    } 

  (* The empty environment. *)

  let empty = {
    env_next_identifier = 0;
    env_map = Map.empty
  } 

  (* [lookup] looks up a source program identifier in an environment. *)

  exception Unbound

  let lookup name env =
    try
      Map.lookup cs name env.env_map
    with Not_found ->
      raise Unbound

  (* [bind_lambda] adds a new $\lambda$-bound identifier to the given environment. The function expects a source
     program identifier, and generates a new, unique internal identifier, which is returned together with the
     updated environment. *)

  let bind_lambda name env =
    let identifier = env.env_next_identifier in
    identifier, {
    env_next_identifier = identifier + 1;
    env_map = Map.add cs name (BindingLambda identifier) env.env_map
  } 

  (* [bind_let] adds a new \verb+let+-bound identifier, together with its type scheme, to the given environment.
     The function expects a source program identifier, and returns an updated environment. *)

  let bind_let name scheme env =
    let identifier = env.env_next_identifier in
    {
      env_next_identifier = identifier + 1;
      env_map = Map.add cs name (BindingLet(identifier, scheme)) env.env_map
    }

end

