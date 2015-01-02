(* This module provides a number of tiny functions that help produce
   [IL] code. *)

open IL

(* A list subject to a condition. (Be careful, though: the list is
   of course constructed even if the condition is false.) *)

val listif: bool -> 'a list -> 'a list
val elementif: bool -> 'a -> 'a list

(* A lazy version of [listif], where the list is constructed only
   if the condition is true. *)

val listiflazy: bool -> (unit -> 'a list) -> 'a list

(* Standard types. *)

val tunit: typ
val tbool: typ
val tint: typ
val tstring: typ
val texn: typ
val tposition: typ
val tlexbuf: typ
val tobj : typ

(* Building a type variable. *)

val tvar: string -> typ

(* Building a type scheme. *)

val scheme: string list -> typ -> typescheme
val type2scheme: typ -> typescheme

(* Projecting out of a [PVar] pattern. *)

val pat2var: pattern -> string

(* Building a [let] construct, with on-the-fly simplification. *)

val blet: (pattern * expr) list * expr -> expr
val mlet: pattern list -> expr list -> expr -> expr

(* [bottom] is an expression that has every type. Its semantics is
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

(* Converting an interface to a structure. Only exception and type definitions
   go through. *)
val interface_to_structure: interface -> structure

