(* This module deals with a few details regarding the definition of
   the [token] type. In particular, if [--only-tokens] was specified,
   it emits the type definition and exits. *)

(* This is the conventional name of the [token] type, for use by
   the code generator. *)

val tctoken: string
val ttoken: IL.typ

(* This is the type of lexers. It refers to the [token] type,
   which is why it is defined here. *)

val tlexer: IL.typ

(* If we were asked to only produce a type definition, then
   [produce_tokentype_and_stop] does so and stops Menhir.
   Otherwise, it does nothing. *)

val produce_tokentype: UnparameterizedSyntax.grammar -> unit

(* This is the definition of the type of tokens, for use by the
   code generator. *)

val tokentypedef: UnparameterizedSyntax.grammar -> IL.typedef list

(* This function prefixes the name of a token with an appropriate
   Objective Caml module name, if necessary. *)

val tokenprefix: string -> string

