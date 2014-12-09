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

(* The type of the entry point for the start symbol [symbol]. *)

let entrytypescheme symbol =
  let ocamltype =
    try
      StringMap.find symbol PreFront.grammar.types
    with Not_found ->
      (* Every start symbol should have a type. *)
      assert false
  in
  type2scheme (marrow [ arrow tlexbuf ttoken; tlexbuf ] (TypTextual ocamltype))

(* This is the interface of the generated parser. *)

let interface = [
  IIFunctor (PreFront.grammar.parameters, [
    IIExcDecls [ excdef ];
    IITypeDecls tokentypedef;
    IIValDecls (
      StringSet.fold (fun symbol decls ->
        (Misc.normalize symbol, entrytypescheme symbol) :: decls
      ) PreFront.grammar.start_symbols []
    )
  ])
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

