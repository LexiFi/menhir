(* This module deals with the definition of the type that describes the
   nonterminal symbols. *)

(* This is the conventional name of the [nonterminal] GADT. This is an indexed
   type (i.e., it has one type parameter). Its data constructors carry zero
   value arguments. *)

val tnonterminalgadt: IL.typ -> IL.typ

(* This is the definition of the [nonterminal] GADT, for use by the code
   generators. This definition can be constructed only if the type of every
   nonterminal symbol is known, either because the user has provided this
   information, or because [--infer] has been set and inference has been
   performed already. This definition is produced only in [--table] mode. *)

val nonterminalgadtdef: UnparameterizedSyntax.grammar -> IL.typedef list

(* When in [--depend] mode, we are asked to produce a mock [.mli] file before
   [--infer] has run, which means that we are usually not able to construct
   the definition of the [nonterminal] GADT. This implies that the mock [.mli]
   file is a subset of the final [.mli] file. It is not clear at this point
   whether this can cause a problem. *)

