open Printf
open Print
open Source

(* ------------------------------------------------------------------------- *)

(* This signature describes symbol tables. *)

module type TABLE = sig

  type entity

  val identifier: entity -> Identifier.t
  val name: entity -> string
  val print: entity printer

  type entity_set

  val domain: entity_set

  type def

  val def: entity -> def
  val iter: (entity -> def -> unit) -> unit
  val fold: (entity -> def -> 'a -> 'a) -> 'a -> 'a

end

(* ------------------------------------------------------------------------- *)

(* Read the file and parse the program. *)

let program : Raw.program =
  let channel = open_in Settings.filename in
  let lexbuf = Lexing.from_channel channel in
  lexbuf.Lexing.lex_curr_p <-
      { 
	Lexing.pos_fname = Settings.filename; 
	Lexing.pos_lnum  = 1;
	Lexing.pos_bol   = 0; 
	Lexing.pos_cnum  = 0
      };
  try
    let program = Parser.program Lexer.main lexbuf in
    close_in channel;
    program
  with Parser.Error ->
    Error.error
      [ Some (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) ]
      "Syntax error."

(* ------------------------------------------------------------------------- *)

(* Convert the program to cooked form. *)

let unbound entity x =
  Error.errorv x (sprintf "Unbound %s: %s." entity (Annotation.content x))

let program : program =
  try
    let empty = Identifier.Map.empty in
    import_program (empty, empty, empty, empty, empty, empty) (Builtin.defs @ program)
  with
  | Sorte.UnboundIdentifier x ->
      unbound "sort" x
  | Datatype.UnboundIdentifier x ->
      unbound "data type" x
  | Datacon.UnboundIdentifier x ->
      unbound "data constructor" x
  | Var.UnboundIdentifier x ->
      unbound "variable" x
  | Exccon.UnboundIdentifier x ->
      unbound "exception constructor" x
  | Func.UnboundIdentifier x ->
      unbound "function" x

(* ------------------------------------------------------------------------- *)

(* Open the root abstraction, so as to assign names to all toplevel
   entities. *)

let defs : defs =
  open_defs program

let datacons, datatypes, exccons, funcs, sorts =
  bound_defs defs

(* TEMPORARY check equivariance of guards for exception constructor defs and for exceptional postconds *)

(* ------------------------------------------------------------------------- *)

(* Define a generic implementation of symbol tables. *)

module Table (A : AlphaLib.Signatures.Atom with type identifier = Identifier.t) (X : sig

  (* The textual categorical name of these entities. *)

  val nature: string

  (* The set of entities that we are interested in. *)

  val atoms: A.AtomSet.t

  (* The definition associated with each entity. *)

  type def

  (* Extraction of the definitions. *)

  val extract: Source.def -> (A.Atom.t * def) list

end) : TABLE with type entity = A.Atom.t and type entity_set = A.AtomSet.t and type def = X.def = struct

  type entity = A.Atom.t

  type entity_set = A.AtomSet.t

  type def = X.def

  open A.AtomIdMap

  let m =
    add_set X.atoms empty

  let identifier x =
    find x m

  let name x =
    Annotation.content (identifier x)

  let print b x =
    Buffer.add_string b (name x)

  let domain =
    X.atoms

  open A.AtomMap

  let table =
    try
      List.fold_left (fun table def ->
	List.fold_left (fun table (x, def) ->
	  strict_add x def table
        ) table (X.extract def)
      ) empty defs
    with Strict x ->
      let id = identifier x in
      Error.errorv id
	(sprintf "The %s %s is multiply defined." X.nature (Annotation.content id))

  let def x =
    try
      find x table
    with Not_found ->
      assert false

  let iter f =
    iter f table

  let fold f accu =
    fold f table accu

end

(* ------------------------------------------------------------------------- *)

(* Define one symbol table for each kind of entity. *)

module DataconTable =
  Table (Datacon) (struct
    let nature = "data constructor"
    let atoms = datacons
    type def = datatype * opaque_guarded_tuple
    let extract = function
      | DefDataType def ->
	  let t = def.datatype_name in
	  List.map (fun (tag, params) ->
	    tag, (t, params)
  	  ) def.datatype_constructors
      | _ ->
	  []
  end)

module DatatypeTable =
  Table (Datatype) (struct
    let nature = "data type"
    let atoms = datatypes
    type def = datatypedef
    let extract = function
      | DefDataType def ->
	  [ def.datatype_name, def ]
      | _ ->
	  []
  end)

module SortTable =
  Table (Sorte) (struct
    let nature = "sort"
    let atoms = sorts
    type def = unit
    let extract = function
      | DefSort sort ->
	  [ sort, () ]
      | _ ->
	  []
  end)

type function_definition =
  | FunDefUser of opaque_fundef
  | FunDefBuiltin

module FuncTable =
  Table (Func) (struct
    let nature = "function"
    let atoms = funcs
    type def = function_definition
    let extract = function
      | DefFun (f, def) ->
	  [ f, FunDefUser def ]
      | DefBuiltin f ->
	  [ f, FunDefBuiltin ]
      | _ ->
	  []
  end)

module ExcconTable =
  Table (Exccon) (struct
    let nature = "exception"
    let atoms = exccons
    type def = opaque_guarded_tuple
    let extract = function
      | DefException (xc, def) ->
	  [ xc, def ]
      | _ ->
	  []
  end)

(* ------------------------------------------------------------------------- *)

(* Lemmata are anonymous, so they are simply grouped in a list. *)

let lemmata =
  List.fold_left (fun accu def ->
    match def with
    | DefLemma lemma ->
	lemma :: accu
    | _ ->
	accu
  ) [] defs

