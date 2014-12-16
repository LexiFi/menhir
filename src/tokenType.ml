(* This module deals with a few details regarding the definition of
   the [token] type. In particular, if [--only-tokens] was specified,
   it emits the type definition and exits. *)

open Syntax
open UnparameterizedSyntax
open IL
open CodeBits

(* This is the conventional name of the [token] type, with no
   prefix. A prefix is possibly appended to it below, where
   [tctoken] is redefined before being exported. *)

let tctoken =
  "token"

(* This is the definition of the type of tokens. (Regardless of
   [Settings.token_type_mode], which is examined below.) *)

let tokentypedef grammar =
  let datadefs =
    StringMap.fold (fun token properties defs ->

      (* Pseudo-tokens (used in %prec declarations, but never
	 declared using %token) are filtered out. *)

      if properties.tk_is_declared then
	let params =
	  match properties.tk_ocamltype with
	  | None ->
	      []
	  | Some t ->
	      [ TypTextual t ]
	in
	{
	  dataname = token;
	  datavalparams = params;
	  datatypeparams = None
	} :: defs
      else
	defs
    ) grammar.tokens []
  in
  {
    typename = tctoken;
    typeparams = [];
    typerhs = TDefSum datadefs;
    typeconstraint = None
  }

(* If we were asked to only produce a type definition, then
   do so and stop. *)

let produce_tokentype grammar =
  match Settings.token_type_mode with
  | Settings.TokenTypeOnly ->

      (* Create both an .mli file and an .ml file. This is made
	 necessary by the fact that the two can be different
	 when there are functor parameters. *)

      let module P = 
	Printer.Make (struct 
			let f = open_out (Settings.base ^ ".mli")
			let raw_stretch_action = false
			let locate_stretches = None 
		      end) 
      in
      P.interface [
        IIFunctor (grammar.parameters, [
	  IITypeDecls [ tokentypedef grammar ]
        ])
      ];
      let module P = 
	Printer.Make (struct 
			let f = open_out (Settings.base ^ ".ml")
			let raw_stretch_action = false
			let locate_stretches = None 
		      end) 
      in
      P.program {
        paramdefs = grammar.parameters;
        prologue = [];
        excdefs = [];
	typedefs = [ tokentypedef grammar ];
        nonrecvaldefs = [];
	valdefs = [];
	moduledefs = [];
        postlogue = [];
      };
      exit 0

  | Settings.CodeOnly _
  | Settings.TokenTypeAndCode ->
      ()

(* Redefine [tokentypedef], and define [tokenprefix], so as to tell the code
   generator whether it should include a definition of the token type in the
   code and how the token type is called. *)

let tokentypedef grammar =
  match Settings.token_type_mode with
  | Settings.CodeOnly _ ->
      []
  | Settings.TokenTypeAndCode ->
      [ tokentypedef grammar ]
  | Settings.TokenTypeOnly ->
      (* This should not happen, as [produce_tokentype] should
         have been called first. *)
      assert false

let tokenprefix id =
  match Settings.token_type_mode with
  | Settings.CodeOnly m ->
      m ^ "." ^ id
  | Settings.TokenTypeAndCode ->
      id
  | Settings.TokenTypeOnly ->
      id (* irrelevant, really *)

(* Redefine the name of the [token] type to take a possible
   prefix into account. *)

let tctoken =
  tokenprefix tctoken

let ttoken =
  TypApp (tctoken, [])

(* The type of lexers. *)

let tlexer =
  TypArrow (tlexbuf, ttoken)

