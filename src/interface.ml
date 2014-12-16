open UnparameterizedSyntax
open IL
open CodeBits

(* In this module, we use [PreFront], not [Grammar], in order to avoid
   a circularity. [Interface] is used by [Infer], which runs before
   [Grammar]. *)

(* This is the [Error] exception. *)

let excname =
  "Error"

let excdef = {
  excname = excname;
  exceq = (if Settings.fixedexc then Some "Parsing.Parse_error" else None);
}

let excredef = {
  excdef with exceq = Some excname
}

(* Finding the type of a start symbol. *)

let ocamltype_of_start_symbol symbol =
  try
    TypTextual (StringMap.find symbol PreFront.grammar.types)
  with Not_found ->
    (* Every start symbol should have a type. *)
    assert false

(* The type of the monolithic entry point for the start symbol [symbol]. *)

let entrytypescheme symbol =
  let typ = ocamltype_of_start_symbol symbol in
  type2scheme (marrow [ arrow tlexbuf TokenType.ttoken; tlexbuf ] typ)

(* When the table back-end is active, the generated parser contains,
   as a sub-module, an application of [Engine.Make]. This sub-module
   is named as follows. *)

let interpreter =
  "MenhirInterpreter"

let result t =
  TypApp (interpreter ^ ".result", [ t ])

(* The name of the incremental entry point for the start symbol [symbol]. *)

let incremental symbol =
  Misc.normalize symbol ^ "_incremental" (* TEMPORARY better idea? *)

(* The type of the incremental entry point for the start symbol [symbol]. *)

let entrytypescheme_incremental symbol =
  let t = ocamltype_of_start_symbol symbol in
  type2scheme (marrow [ tunit ] (result t))

(* This is the interface of the generated parser -- only the part
   that is specific of the table back-end. *)

let table_interface grammar =
  if Settings.table then [
    IIComment "The incremental API.";
    IIModule (
      interpreter,
      MTWithType (
        MTNamedModuleType "MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE",
        "token", (* NOT [tctoken], which is qualified if [--external-tokens] is used *)
        WKDestructive,
        TokenType.ttoken
      )
    );
    IIComment "The entry point(s) to the incremental API.";
    IIValDecls (
      StringSet.fold (fun symbol decls ->
        (incremental symbol, entrytypescheme_incremental symbol) :: decls
      ) grammar.start_symbols []
    )
  ] else []

(* This is the interface of the generated parser. *)

let tokentypedef grammar =
  match TokenType.tokentypedef grammar with
  | [] ->
      []
  | def ->
      [ IIComment "The type of tokens."; IITypeDecls def ]

let interface grammar = [
  IIFunctor (grammar.parameters,
    tokentypedef grammar @ [
    IIComment "This exception is raised by the monolithic API functions.";
    IIExcDecls [ excdef ];
    IIComment "The monolithic API.";
    IIValDecls (
      StringSet.fold (fun symbol decls ->
        (Misc.normalize symbol, entrytypescheme symbol) :: decls
      ) grammar.start_symbols []
    )
  ] @ table_interface grammar)
]

(* Writing the interface to a file. *)

let write () =
  assert (Settings.token_type_mode <> Settings.TokenTypeOnly);
  let mli = open_out (Settings.base ^ ".mli") in
  let module P = Printer.Make (struct
    let f = mli
    let locate_stretches = None
    let raw_stretch_action = false
  end) in
  P.interface (interface PreFront.grammar);
  close_out mli

