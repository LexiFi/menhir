open Strings
open Kind
open IL
open Bits
open InternalSyntax

(* Accessing our namespace. *)

let alphalib =
  PathVar "AlphaLib"

let sigs =
  PathDot (alphalib, "Signatures")

let privatesigs =
  PathDot (alphalib, "PrivateSignatures")

let mixins =
  PathDot (alphalib, "Mixins")

let replicate =
  PathDot (alphalib, "Replicate")

let toolbox =
  PathDot (alphalib, "Toolbox")

let abstraction =
  PathDot (alphalib, "Abstraction")

(* Building type definitions. *)

let mktypedef name params rhs =
  { typename = name; typeparams = params; typerhs = rhs }

(* Translating type definitions. *)

let rec translate_declaration (DeclType (params, name, _, rhs)) =
  mktypedef name params (Some (ADT.map_rhs translate_factor rhs))

and translate_modifier modifier t =
  match modifier with
  | MContainer container ->
      TypApp (PathVar container, [ t ])
  | MAbstraction ->
      TypApp (PathVar "abs", [ t ])
  | MNone
  | MInner
  | MOuter
  | MNeutral ->
      t

and translate_factor = function
  | FAtom sort ->
      tcon sort
  | FEscape t ->
      TypLiteral t
  | FTypCon (modifier, t, params) ->
      translate_modifier
	modifier
	(TypApp (PathVar t, List.map tvar params))

let usertypedefs =
  Declarations.map DAll translate_declaration

(* Building the signature [Data]. *)

let atomdecls =
  List.map (fun sort ->
    mktypedef sort [] None
  ) Sorts.sorts

let datadef =
  StructModuleTypeDef (
    "Data",
    MTESig [
      SigTypeDecls [ mktypedef "abs" [ "a" ] None ];
      SigTypeDecls [ mktypedef "abstractable" [ "a" ] None ];
      SigTypeDecls atomdecls;
      SigTypeDecls usertypedefs;
    ]
  )

let datacomment =
  StructComment
    "A module type describes the structure of our data. The types of atoms, \
     abstractions, and evidence are left unspecified, so this module \
     type serves as a common template for our raw, internal, and flat forms."

let data =
  mtvar "Data"

(* Building the signature [PrivateData]. *)

let atom : typ ADT.rhs option =
  Some (ADT.Tuple [TypApp (PathVar "atom", [])])

let atomdefs =
  List.map (fun sort ->
    mktypedef sort [] atom
  ) Sorts.sorts

let mkwith mt tds =
  List.fold_right (fun td mt ->
    MTEWith (mt, td)
  ) tds mt

let privatedatadef =
  StructModuleTypeDef (
    "PrivateData",
    MTESig [
      SigTypeDecls [ mktypedef "atom" [] None ];
      SigInclude (mkwith data atomdefs);
    ]
  )

(* Building the functor [Data]. *)

let flavor =
  mtvar "Flavor"

let mkdatadef =
  StructModuleDef (
    "Data",
    MEFunctor (
      "F",
      flavor,
      MEStruct [
	StructInclude (mvar "F");
	StructTypeDefs atomdefs;
	StructTypeDefs usertypedefs;
      ]
    )
  )

(* Building the selector functions. *)

let selector field =
  mkfun field [ vvar "self" ] (
    EMethodCall (
      vvar "self",
      field
    )
  )

let dictionary t =
  t ^ "_dictionary"

let selectorsdef =
  StructModuleDef (
    "Select",
    MEStruct (
      Declarations.map DAll (function (DeclType (_, id, _, _)) ->
	selector id (* TEMPORARY selector not needed at every type *)
      ) @
      Declarations.map DAbstraction (function (DeclType (_, id, _, _)) ->
	selector (dictionary id)
      )
    )
  )

let selector t =
  EVar (PathDot (PathVar "Select", t))

(* Building a block of dummy dictionary lookup methods. *)

let dictionariescomment =
  StructComment
    "These dummy methods should never be \
     called. Either they should be overridden, or the [abstraction] \
     method should be defined so as to never invoke a dictionary \
     method. Both of these styles are exploited further on."

let dummy_dictionary_method id =
  concrete_typed_method
    (dictionary id)
    (mono (tapp "abstractable" [ tcon id ]))
    assertfalse

let dictionariesdef =
  StructModuleDef (
    "Dictionaries",
    MEFunctor (
      "D",
      data,
      MEStruct [
	StructOpen (PathVar "D");
	StructClassDef (
	  NonVirtual,
	  [],
	  "dictionaries",
	  CObject (None,
	    Declarations.map DAbstraction (fun (DeclType (_, id, _, _)) ->
	      dummy_dictionary_method id
	    )
	  )
	)
      ]
    )
  )

(* Choosing names for factors. We could choose arbitrary distinct
   names, but we use a basename that is reminiscent of the factor
   for clarity. *)

let name_factor = function
  | FAtom sort ->
      sort
  | FEscape _ ->
      "foreign"
  | FTypCon (_, t, _) ->
      t

let name_factors factors =
  let c = ref 0 in
  List.map (fun factor ->
    incr c;
    vvar (Printf.sprintf "_%s'%d" (name_factor factor) !c)
  ) factors

(* Building the unary data classes. *)

module Map = struct

  let self =
    vvar "self"

  let mode2str = function
    | ModeExp ->
	"free_"
    | ModePat ->
	"bound_"

  let atom_method_name mode =
    mode2str mode ^ "atom"

  let sort_method_name mode sort =
    mode2str mode ^ sort

  let sort_method mode sort =
    concrete_method
      (sort_method_name mode sort)
      (app [
	EVar (PathDot (PathVar "Sort", "pipe"));
	Sorts.sortcon sort;
	EMethodCall (self, atom_method_name mode);
      ])

  let summand_method_name datacon =
    String.lowercase datacon

  let sum_type_method id summands =
    let root = vvar id in
    let branches =
      List.map (fun (datacon, factors) ->
	let parameters = name_factors factors in
	{
	  branchpat =
	    dcon datacon parameters;
	  branchbody =
	    app (
	      sharp self (summand_method_name datacon) ::
	      root :: parameters
	    )
	}
      ) summands
    in
    concrete_method id (EFun ([ root ], EMatch (root, branches)))

  let rec map_factor mode parameter = function
    | FAtom sort ->
	app [ sharp self (sort_method_name mode sort); parameter ]
    | FEscape _ ->
	parameter
    | FTypCon (MNone, t, _) ->
	app [ sharp self t;
	      parameter ]
    | FTypCon (MContainer container, t, _) ->
	app [ vvar "List.map"; (* TEMPORARY (Container.map container); *)
	      sharp self t;
	      parameter ]
    | FTypCon (MAbstraction, t, _) ->
	app [ sharp self "abstraction";
	      selector (dictionary t);
	      selector t;
	      parameter ]
    | FTypCon (MInner, t, _) ->
	app [ sharp self "inner";
	      selector t;
	      parameter ]
    | FTypCon ((MOuter | MNeutral), t, _) ->
	app [ sharp self "outer";
	      selector t;
	      parameter ]

  let tuple_method_body root mode rebuild factors =
    let parameters = name_factors factors in
    parameters,
    List.fold_right2 (fun parameter factor accu ->
      ELet (
	primed parameter,
	map_factor mode parameter factor,
	accu
      )
    ) parameters factors (
      EIfThenElse (
	conjunction (List.map (fun parameter ->
	  EInfixApp (parameter, "==", primed parameter)
	) parameters),
	root,
	rebuild (List.map primed parameters)
      )
    )

  let summand_method id mode (datacon, factors) =
    let root = vvar id in
    let parameters, body = tuple_method_body root mode (dcon datacon) factors in
    concrete_method (summand_method_name datacon) (EFun (root :: parameters, body))

  let tuple_method id mode factors =
    let root = vvar id in
    let parameters, body = tuple_method_body root mode tuple factors in
    concrete_method id (EFun ([ root ], ELet (tuple parameters, root, body)))

  let type_methods (DeclType (params, id, mode, rhs)) =
    match rhs with
    | ADT.Sum summands ->
	sum_type_method id summands ::
	List.map (summand_method id mode) summands
    | ADT.Tuple factors ->
	tuple_method id mode factors ::
	[]
    | _ ->
	assert false (* TEMPORARY *)

  let classdef =
    StructClassDef (
      Virtual,
      [],
      "cmap",
      CObject (
	Some ("self", None),
	[
	  CIInherit (CPath (PathDot (PathVar "D", "dictionaries")));
	  CIInherit (CPath (PathDot (PathVar "B", "cmap")));
	] @
	List.map (sort_method ModeExp) Sorts.sorts @
	List.map (sort_method ModePat) Sorts.sorts @
        Declarations.flat_map DAll type_methods
      )
    )

end

let unarydatadef =
  StructModuleDef (
    "UnaryDataClasses",
    MEFunctor (
      "Data",
      mtvar "PrivateData",
      MEStruct [
	StructOpen (PathVar "Data");
	StructComment "Default dictionary methods.";
	StructModuleDef ("D", MEApply (mvar "Dictionaries", mvar "Data"));
	StructComment "Declarations for the five virtual methods that \
		       deal with free atoms, bound atoms, abstractions, [inner] and [outer].";
	StructModuleDef ("B",
	  MEApply (mdot mixins "Declare",
		   MEApply (MEApply (mdot replicate "Flavor", mvar "Sort"), mvar "Data")));
	Map.classdef;
      ]
    )
  )

(* ------------------------------------------------------------------------- *)

(* We can now begin glueing things together. *)

let glue = [

  StructComment "Fix an implementation of identifiers.";
  StructModuleDef (
    "Identifier",
    mdot toolbox "String" (* TEMPORARY this is the default *)
  );

  StructComment "Create a toolbox -- an implementation of opaque atoms.";
  StructModuleDef (
    "Toolbox",
    MEApply (mdot toolbox "Make", mvar "Identifier")
  );

  StructComment "Create a sorted toolbox -- a variant of the toolbox \
		 where each atom is explicitly tagged with a sort.";
  StructModuleDef (
    "SortedToolbox",
    MEApply (MEApply (mdot replicate "Toolbox", mvar "Sort"), mvar "Toolbox")
  );
  StructOpen (PathVar "SortedToolbox");

  StructComment "Create an implementation of opaque abstractions on top of
		 sorted atoms.";
  StructModuleDef (
    "Opaque",
    MEApply (mdot abstraction "Make", mvar "Subst")
  );

  StructComment "Construct flavors for transparent atoms and abstractions. \
		 In one variant, atoms are raw, while in the other variant, \
		 atoms are tagged with sorts.";
  StructModuleDef (
    "Transparent",
    MEApply (MEApply (mdot privatesigs "TransparentFlavor", mvar "Identifier"), mvar "Sort")
  );
  StructModuleDef (
    "SortedTransparent",
    MEApply (MEApply (mdot replicate "Flavor", mvar "Sort"), mvar "Transparent")
  );

  StructComment "Instantiate the mixin classes provided by the library.";
  StructModuleDef (
    "Mixins",
    MEApply (MEApply (MEApply (mdot mixins "Make", mvar "SortedToolbox"), mvar "Opaque"), mvar "SortedTransparent")
  );

]

(* ------------------------------------------------------------------------- *)

(* Define internal form. *)

let internal =
  StructModuleDef (
    "Internal",
    MEStruct [

      StructComment "Export the implementation of opaque abstractions.";
      StructModuleDef ("Abstraction", mvar "Opaque");

      StructComment "Define the internal flavor of data.";
      StructModuleDef (
        "Data",
        MEApply (
          mvar "Data",
          MEStruct [
            StructInclude (mdot (PathVar "Toolbox") "Atom"); (* defines [atom] *)
            StructInclude (mvar "Opaque");       (* defines [abs] and [abstractable] *)
          ]
        )
      );
      StructInclude (mvar "Data");

      StructComment "Prepare for the construction of dictionaries.";
      StructModuleDef (
        "UDC",
        MEApply (mvar "UnaryDataClasses", mvar "Data")
      );
      StructClassDef (
        NonVirtual,
        [],
        "substitution",
        CFun (
          [ "outer"; "inner" ],
          CObject (
            None, [
              CIInherit (CPath (PathDot (PathVar "UDC", "cmap")));
              CIInherit (CApp (CPath (PathDot (PathVar "Mixins", "substitution")), [ vvar "outer"; vvar "inner" ]));
            ]
          )
        )
      )

    ]
  )

(* ------------------------------------------------------------------------- *)

(* Put it all together. *)

let hline =
  StructComment (String.make 73 '-')

let defs = [
  hline;
  datacomment;
  datadef;
  privatedatadef;
  StructOpen privatesigs;
  mkdatadef;

  hline;
  selectorsdef;

  hline;
  dictionariescomment;
  dictionariesdef;

  hline;
  StructOpen sigs;
  unarydatadef;

  hline
] @
  glue
@ [
  
  hline;
  internal;

]

