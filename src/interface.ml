open UnparameterizedSyntax
open IL
open CodeBits
open TokenType

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
  type2scheme (marrow [ arrow tlexbuf ttoken; tlexbuf ] typ)

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

let table_interface =
  if Settings.table then [
    IIComment "The incremental API.";
    IIModule (
      interpreter,
      MTWithType (
        MTNamedModuleType "MenhirLib.IncrementalEngine.INCREMENTAL_ENGINE",
        "token", (* NOT [tctoken], which is qualified if [--external-tokens] is used *)
        WKDestructive,
        ttoken
      )
    );
    IIComment "The entry point(s) to the incremental API.";
    IIValDecls (
      StringSet.fold (fun symbol decls ->
        (incremental symbol, entrytypescheme_incremental symbol) :: decls
      ) PreFront.grammar.start_symbols []
    )
  ] else []

(* This is the interface of the generated parser. *)

let tokentypedef =
  match tokentypedef with
  | [] ->
      []
  | _ ->
      [ IIComment "The type of tokens."; IITypeDecls tokentypedef ]

let interface = [
  IIFunctor (PreFront.grammar.parameters,
    tokentypedef @ [
    IIComment "This exception is raised by the monolithic API functions.";
    IIExcDecls [ excdef ];
    IIComment "The monolithic API.";
    IIValDecls (
      StringSet.fold (fun symbol decls ->
        (Misc.normalize symbol, entrytypescheme symbol) :: decls
      ) PreFront.grammar.start_symbols []
    )
  ] @ table_interface)
]

(* Writing the interface to a file. *)

let write () =
  let mli = open_out (Settings.base ^ ".mli") in
  let module P = Printer.Make (struct
    let f = mli
    let locate_stretches = None
    let raw_stretch_action = false
  end) in
  P.interface interface;
  close_out mli

