(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/internalSyntax.ml,v 1.20 2002/05/30 15:29:18 fpottier Exp $ *)

(* This module defines the abstract syntax of the core (internal)
   language. This language is significantly reduced. This makes
   type-checking more straightforward and avoids incorporating ad-hoc
   syntactic choices, such as various forms of syntactic sugar. *)

open Id
open KindUnification

(* The type of channels. *)

type channel =
    string

(* Abstract syntax for types. This is the (simply-typed)
   label-selective $\lambda$-calculus with rows. *)

type typ =
  | ITVar of id
  | ITApp of typ * channel * typ
  | ITAbs of channel * id * typ
  | ITRowCons of id * typ * typ
  | ITRowUniform of typ

  (* Built-in types. *)

  | ITForall of kind * typ
  | ITPresent of typ
  | ITAbsent
  | ITRecord of typ
  | ITArrow of typ * typ
  | ITTuple of typ list
  | ITInteger

type scheme =
    id list * typ

(* Abstract syntax for expressions and patterns.

   We use the same representation for expressions and patterns (even
   though some productions belong to expressions and not to patterns,
   and vice-versa), because expressions and patterns have some
   productions in common, which it is nice to identify. *)

type expression =

  (* These productions belong to both expressions and patterns. *)

  | IEVar of id                                         (* variable access *)
  | IETuple of expression list                          (* tuple *)
  | IERecordEmpty                                       (* the empty record *)
  | IERecordExtend of record_content list * expression  (* record extension *)
  | IETypeConstraint of expression * typ                (* type constraint *)
  | IDisplay of expression                              (* [display e] behaves as [e], but displays its type *)
  | IEChoice of expression list                         (* non-deterministic choice *)
                                                        (* if used as a pattern, requires all sub-patterns to match *)
  | IEInteger of int                                    (* integer constant *)

  (* These productions belong solely to expressions. *)

  | IEAbs of id * typ option * expression               (* term abstraction; optionally type-annotated *)
  | IEApp of expression * expression                    (* term application *)
  | IETypAbs of id option * kind * expression           (* type abstraction; optionally binds a type name *)
  | IETypApp of expression * kind                       (* type application *)
  | IEBinding of binding * expression                   (* local declaration *)
  | IERecordUpdate of expression * id * expression      (* imperative record update *)
  | IEExists of id list * expression                    (* flexible type introduction *)
  | IEForall of id list * expression                    (* rigid type introduction *)

  (* These productions belong solely to patterns. *)

  | IPWildcard                                          (* indefinite pattern *)
  | IPTypAbs of pattern * kind                          (* matches an explicit polymorphic abstraction *)
  | IPConstant of expression                            (* matches exactly this expression's value *)

and pattern =
    expression

and binding =
  | IBLetGen of generality ref * pattern * expression   (* ML-style polymorphic, non-recursive [let] binding *)
  | IBLetRec of (id * scheme * expression) list         (* Haskell-style polymorphic, recursive [let] binding *)
              * (id * expression) list                  (* mixing annotated and non-annotated bindings *)
  | IBLetType of (id * typ) list                        (* local type abbreviation *)

and generality =
  | Monomorphic                                         (* when applicable, more efficient; creates fewer equations *)
  | Polymorphic                                         (* more general *)

and record_content =

  (* This production belongs to both expressions and patterns. *)

  | IRElement of id * expression                        (* field definition *)

  (* This production belongs solely to expressions. *)

  | IRBinding of binding                                (* binding, does not define a field *)

(* The type of ``toplevel'' declarations. *)

type mutability =
  | Mutable
  | Immutable

type declaration =
  | IDeclareField of id * mutability                    (* declares a record field *)

(* A whole program is composed of a set of declarations,
   followed by an expression. *)

type program =
  | IProgram of declaration list * expression

