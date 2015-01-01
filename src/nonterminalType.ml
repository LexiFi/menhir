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

exception MissingOCamlType

let nonterminalgadtdef grammar =
  assert Settings.table;
  try
    let datadefs =
      List.fold_left (fun defs nt ->
        let index =
          match ocamltype_of_symbol grammar nt with
          | Some t ->
              TypTextual t
          | None ->
              raise MissingOCamlType
        in
        {
          dataname = tnonterminalgadtdata nt;
          datavalparams = [];
          datatypeparams = Some [ index ]
        } :: defs
      ) [] (nonterminals grammar)
    in
    [
      IIComment "The indexed type of nonterminal symbols.";
      IITypeDecls [{
        typename = tcnonterminalgadt;
        typeparams = [ "_" ];
        typerhs = TDefSum datadefs;
        typeconstraint = None
      }]
    ]
  with MissingOCamlType ->
    (* If the type of some nonterminal symbol is unknown, give up
       on the whole thing. *)
    []
