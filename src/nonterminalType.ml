open UnparameterizedSyntax
open IL

(* This is the conventional name of the nonterminal GADT, which describes the
   nonterminal symbols. *)

let tcnonterminalgadt =
  "nonterminal"

let tnonterminalgadt a =
  TypApp (tcnonterminalgadt, [ a ])

(* This is the conventional name of the data constructors of the nonterminal
   GADT. *)

let tnonterminalgadtdata nt =
  "N_" ^ Misc.normalize nt

(* This is the definition of the nonterminal GADT. Here, the data
   constructors have no value argument, but have a type index. *)

exception MissingOCamlType of string

let nonterminalgadtdef grammar =
  assert Settings.inspection;
  let comment, datadefs =
    try

      "The indexed type of nonterminal symbols.",
      List.fold_left (fun defs nt ->
        let index =
          match ocamltype_of_symbol grammar nt with
          | Some t ->
              TypTextual t
          | None ->
              raise (MissingOCamlType nt)
        in
        {
          dataname = tnonterminalgadtdata nt;
          datavalparams = [];
          datatypeparams = Some [ index ]
        } :: defs
      ) [] (nonterminals grammar)

    with MissingOCamlType nt ->
      (* If the type of some nonterminal symbol is unknown, give up
         and define ['a nonterminal] as an abstract type. This is
         useful when we are in [--(raw)-depend] mode and we do not
         wish to fail. Instead, we produce a mock [.mli] file that
         is an approximation of the real [.mli] file. When we are
         not in [--(raw)-depend] mode, though, this is a problem.
         We display an error message and stop. *)
      match Settings.depend with
      | Settings.OMRaw
      | Settings.OMPostprocess ->
          "The indexed type of nonterminal symbols (mock!).",
          []
      | Settings.OMNone ->
          Error.error [] (Printf.sprintf "\
            the type of the nonterminal symbol %s is unknown.\n\
            When --inspection is set, the type of every nonterminal symbol must be known.\n\
            Please use --infer or specify the type of every symbol via %%type declarations."
            nt
          )

  in
  [
    IIComment comment;
    IITypeDecls [{
      typename = tcnonterminalgadt;
      typeparams = [ "_" ];
      typerhs = TDefSum datadefs;
      typeconstraint = None
    }]
  ]
