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

(* This module provides a number of tiny functions that help produce
   [IL] code. *)

open Positions
open IL

(** A smart constructor for [PVarLocated]. *)
val pvarlocated: string located -> pattern

(** A type name. *)
val tname: string -> typ

(* Tuples. *)

val etuple: expr list -> expr
val ptuple: pattern list -> pattern

(* Standard types. *)

val tunit: typ
val tbool: typ
val tint: typ
val tstring: typ
val texn: typ
val tposition: typ
val tlocation: typ
val tlexbuf: typ
val tobj : typ

(** Building a type variable. *)
val tvar: string -> typ

(** Building a type scheme. *)
val scheme: string list -> typ -> typescheme

(** Building a locally abstract type scheme. *)
val local_scheme: string list -> typ -> typescheme

val type2scheme: typ -> typescheme

(** Constraining an expression to have a (monomorphic) type. *)
val annotate: expr -> typ -> expr

(** Projecting out of a [PVar] pattern. *)
val pat2var: pattern -> string

(** Building a [let] construct, with on-the-fly simplification. These two
  functions construct a nested sequence of [let] definitions, or [let[@local]]
  if [?local] is set to true. *)
val blet: ?local:bool -> (pattern * expr) list * expr -> expr

(** See [blet]. *)
val mlet: ?local:bool ->  pattern list -> expr list -> expr -> expr

(** Simulating a [let/and] construct. *)
val eletand: ?local:bool -> (pattern * expr) list * expr -> expr

(** [eraisenotfound] is an expression that raises [Not_found]. *)
val eraisenotfound: expr

(** [bottom] is an expression that has every type. Its semantics is
    irrelevant. *)
val bottom: expr

(* Boolean constants. *)

val etrue: expr
val efalse: expr
val eboolconst: bool -> expr

(* Option constructors. *)

val enone: expr
val esome: expr -> expr

(* List constructors. *)

val elist: expr list -> expr

(* Integer constants as patterns. *)

val pint: int -> pattern

(* These help build function types. *)

val arrow: typ -> typ -> typ
val arrowif: bool -> typ -> typ -> typ
val marrow: typ list -> typ -> typ

(* These functions are used to generate names in menhir's namespace. *)
val prefix: string -> string
val dataprefix: string -> string
val tvprefix: string -> string
val tprefix: string -> string

(** Converting an interface to a structure. Only exception and type definitions
    go through. *)
val interface_to_structure: interface -> structure

(** Constructing a named module type together with a list of "with type"
    constraints. *)
val with_types: with_kind -> string -> (string list * string * typ) list -> module_type

(** Functor applications. *)
val mapp: modexpr -> modexpr list -> modexpr

(** Record fields. *)
val field: bool -> string -> typ -> fielddef

(** Branches. *)
val branch: pattern -> expr -> branch

val fresh_name : unit -> string

(** Variable *)
val evar: string -> expr

(** Variables *)
val evars: string list -> expr list


(** Pattern variable *)
val pvar: string -> pattern

(** Pattern variables *)
val pvars: string list -> pattern list