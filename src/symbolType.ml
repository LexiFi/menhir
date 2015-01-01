open IL

(* The symbol GADT is the union of the terminal and nonterminal GADTs. *)

(* The conventional name of the symbol GADT. *)

let tcsymbolgadt =
  "symbol"

let tsymbolgadt a =
  TypApp (tcsymbolgadt, [ a ])

(* The conventional names of the data constructors. *)

let dataT =
  "T"

let dataN =
  "N"

(* The definition of the symbol GADT. *)

let symbolgadtdef grammar =
  assert Settings.table;
  (* This definition can be produced only if we are successfully able
     to construct the nonterminal GADT. *)
  match NonterminalType.nonterminalgadtdef grammar with
  | [] ->
      []
  | _ :: _ ->
      let a = "a" in
      let datadefs =
        {
          dataname = dataT;
          datavalparams = [ TokenType.ttokengadt (TypVar a) ];
          datatypeparams = Some [ TypVar a ]
        } ::
        {
          dataname = dataN;
          datavalparams = [ NonterminalType.tnonterminalgadt (TypVar a) ];
          datatypeparams = Some [ TypVar a ]
        } ::
        []
      in
      [ IIComment "The indexed type of terminal and nonterminal symbols.";
        IITypeDecls [{
          typename = tcsymbolgadt;
          typeparams = [ a ];
          typerhs = TDefSum datadefs;
          typeconstraint = None
        }]
      ]
