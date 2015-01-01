open UnparameterizedSyntax
open IL
open CodeBits

(* -------------------------------------------------------------------------- *)

(* The [Error] exception. *)

let excname =
  "Error"

let excdef = {
  excname = excname;
  exceq = (if Settings.fixedexc then Some "Parsing.Parse_error" else None);
}

let excredef = {
  excdef with exceq = Some excname
}

(* -------------------------------------------------------------------------- *)

(* The type of the monolithic entry point for the start symbol [symbol]. *)

let entrytypescheme grammar symbol =
  let typ = TypTextual (ocamltype_of_start_symbol grammar symbol) in
  type2scheme (marrow [ arrow tlexbuf TokenType.ttoken; tlexbuf ] typ)

(* -------------------------------------------------------------------------- *)

(* When the table back-end is active, the generated parser contains,
   as a sub-module, an application of [Engine.Make]. This sub-module
   is named as follows. *)

let interpreter =
  "MenhirInterpreter"

let result t =
  TypApp (interpreter ^ ".result", [ t ])

(* -------------------------------------------------------------------------- *)

(* The name of the sub-module that contains the incremental entry points. *)

let incremental =
  "Incremental"

(* The type of the incremental entry point for the start symbol [symbol]. *)

let entrytypescheme_incremental grammar symbol =
  let t = TypTextual (ocamltype_of_start_symbol grammar symbol) in
  type2scheme (marrow [ tunit ] (result t))

(* -------------------------------------------------------------------------- *)

(* The monolithic (traditional) API: the type [token], the exception [Error],
   and the parser's entry points. *)

let monolithic_api grammar =

  TokenType.tokentypedef grammar @

  IIComment "This exception is raised by the monolithic API functions." ::
  IIExcDecls [ excdef ] ::

  IIComment "The monolithic API." ::
  IIValDecls (
    StringSet.fold (fun symbol decls ->
      (Misc.normalize symbol, entrytypescheme grammar symbol) :: decls
    ) grammar.start_symbols []
  ) ::

  []

(* -------------------------------------------------------------------------- *)

(* The incremental API. *)

let incremental_api grammar () =

  IIComment "The incremental API." ::
  IIModule (
    interpreter,
    MTWithType (
      MTNamedModuleType "MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE",
      "token", (* NOT [tctoken], which is qualified if [--external-tokens] is used *)
      WKDestructive,
      TokenType.ttoken
    )
  ) ::

  IIComment "The entry point(s) to the incremental API." ::
  IIModule (incremental, MTSigEnd [
    IIValDecls (
      StringSet.fold (fun symbol decls ->
        (symbol, entrytypescheme_incremental grammar symbol) :: decls
      ) grammar.start_symbols []
    )
  ]) ::

  []

(* -------------------------------------------------------------------------- *)

(* The inspection API. *)

let inspection_api grammar () =

  TokenType.tokengadtdef grammar @
  NonterminalType.nonterminalgadtdef grammar @
  SymbolType.symbolgadtdef() @

  (* TEMPORARY emit a comment *)
  IIValDecls [
    let ty =
      arrow (TypApp (interpreter ^ ".lr1state", [ TypVar "a" ]))
            (TypApp ("symbol", [ TypVar "a" ]))
    in
    (* TEMPORARY code sharing with tableBackend *)
    "symbol", type2scheme ty
  ] ::

  []

(* -------------------------------------------------------------------------- *)

(* The complete interface of the generated parser. *)

let interface grammar = [
  IIFunctor (grammar.parameters,
    monolithic_api grammar @
    listiflazy Settings.table (incremental_api grammar) @
    listiflazy Settings.inspection (inspection_api grammar)
  )
]

(* -------------------------------------------------------------------------- *)

(* Writing the interface to a file. *)

let write grammar () =
  assert (Settings.token_type_mode <> Settings.TokenTypeOnly);
  let mli = open_out (Settings.base ^ ".mli") in
  let module P = Printer.Make (struct
    let f = mli
    let locate_stretches = None
    let raw_stretch_action = false
  end) in
  P.interface (interface grammar);
  close_out mli

