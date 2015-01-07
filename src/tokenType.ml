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

(* This is the conventional name of the token GADT, which describes
   the tokens. Same setup as above. *)

let tctokengadt =
  "terminal"

(* This is the conventional name of the data constructors of
   the token GADT. *)

let ttokengadtdata token =
  "T_" ^ token

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
  [
    IIComment "The type of tokens.";
    IITypeDecls [{
      typename = tctoken;
      typeparams = [];
      typerhs = TDefSum datadefs;
      typeconstraint = None
    }]
  ]

(* This is the definition of the token GADT. Here, the data
   constructors have no value argument, but have a type index. *)

(* The token GADT is produced only when [Settings.inspection] is true. Thus,
   when [Settings.inspection] is false, we remain compatible with old versions
   of OCaml, without GADTs. *)

(* Although the [token] type does not include the [error] token (because this
   token is never produced by the lexer), the token GADT must include the
   [error] token (because this GADT must describe all of the tokens that are
   allowed to appear in a production). *)

let tokengadtdef grammar =
  assert Settings.inspection;
  let errordata = {
    dataname = ttokengadtdata "error";
    datavalparams = [];
    datatypeparams = Some [ tunit ]
      (* the [error] token has a semantic value of type [unit] *)
  } in
  let datadefs =
    StringMap.fold (fun token properties defs ->
      if properties.tk_is_declared then
        let index =
          match properties.tk_ocamltype with
          | None ->
              tunit
          | Some t ->
              TypTextual t
        in
        {
          dataname = ttokengadtdata token;
          datavalparams = [];
          datatypeparams = Some [ index ]
        } :: defs
      else
        defs
    ) grammar.tokens [errordata]
  in
  [
    IIComment "The indexed type of terminal symbols.";
    IITypeDecls [{
      typename = tctokengadt;
      typeparams = [ "_" ];
      typerhs = TDefSum datadefs;
      typeconstraint = None
    }]
  ]

(* If we were asked to only produce a type definition, then
   do so and stop. *)

let produce_tokentypes grammar =
  match Settings.token_type_mode with
  | Settings.TokenTypeOnly ->

      (* Create both an .mli file and an .ml file. This is made
	 necessary by the fact that the two can be different
	 when there are functor parameters. *)

      let i =
        tokentypedef grammar @
        listiflazy Settings.inspection (fun () ->
          tokengadtdef grammar
        )
      in

      let module P = 
	Printer.Make (struct 
			let f = open_out (Settings.base ^ ".mli")
			let raw_stretch_action = false
			let locate_stretches = None 
		      end) 
      in
      P.interface [
        IIFunctor (grammar.parameters, i)
      ];
      let module P = 
	Printer.Make (struct 
			let f = open_out (Settings.base ^ ".ml")
			let raw_stretch_action = false
			let locate_stretches = None 
		      end) 
      in
      P.program [
        SIFunctor (grammar.parameters,
          interface_to_structure i
        )
      ];
      exit 0

  | Settings.CodeOnly _
  | Settings.TokenTypeAndCode ->
      ()

(* Define [tokentypedef] and [tokengadtdef], [tokenprefix]. *)

let tokentypedef grammar =
  match Settings.token_type_mode with
  | Settings.CodeOnly _ ->
      []
  | Settings.TokenTypeAndCode ->
      tokentypedef grammar
  | Settings.TokenTypeOnly ->
      (* This should not happen, as [produce_tokentype] should
         have been called first. *)
      assert false

let tokengadtdef grammar =
  match Settings.token_type_mode with
  | Settings.CodeOnly _ ->
      []
  | Settings.TokenTypeAndCode ->
      tokengadtdef grammar
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

let tokendata =
  tokenprefix

let tctokengadt =
  tokenprefix tctokengadt

let ttokengadt a =
  TypApp (tctokengadt, [ a ])

let tokengadtdata token =
  tokenprefix (ttokengadtdata token)

