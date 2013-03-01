open Printf
open Source

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
    Error.error2
      (Lexing.lexeme_start_p lexbuf)
      (Lexing.lexeme_end_p lexbuf)
      "Syntax error."

(* ------------------------------------------------------------------------- *)

(* Convert the program to cooked form. *)

let program : program =
  try
    let empty = Identifier.Map.empty in
    import_program (empty, empty, empty, empty) program
  with
  | Var.UnboundIdentifier x
  | Datacon.UnboundIdentifier x
  | Datatype.UnboundIdentifier x
  | Valfun.UnboundIdentifier x ->
      Error.error x
	(sprintf "Unbound identifier: %s." (Annotation.content x))

(* ------------------------------------------------------------------------- *)

(* Open the root abstraction, so as to assign names to all
   toplevel entities. *)

let defs : defs =
   open_defs program

(* ------------------------------------------------------------------------- *)

(* Group definitions according to their nature. Open each of the
   function definitions. *)

let fundefs, typedefs, datacondefs =
  try
    List.fold_left (fun (fundefs, typedefs, datacondefs) def ->
      match def with
      | DefValFun (f, fundef) ->
	  Valfun.AtomMap.strict_add f (open_ofu fundef) fundefs,
	  typedefs,
	  datacondefs
      | DefDataType (t, typedef) ->
	  let _, datadefs = typedef in
	  fundefs,
	  Datatype.AtomMap.strict_add t typedef typedefs,
	  List.fold_left (fun datacondefs (tag, details) ->
	    let layout, guard = open_dcd details in
	    Datacon.AtomMap.strict_add tag (t, layout, guard) datacondefs
	  ) datacondefs datadefs
    ) (Valfun.AtomMap.empty, Datatype.AtomMap.empty, Datacon.AtomMap.empty) defs
  with
  | Valfun.AtomMap.Strict f ->
      let f = Valfun.Atom.basename f in
      Error.error f
	(sprintf "The function %s is multiply defined." (Annotation.content f))
  | Datatype.AtomMap.Strict t ->
      let t = Datatype.Atom.basename t in
      Error.error t
	(sprintf "The type %s is multiply defined." (Annotation.content t))
  | Datacon.AtomMap.Strict tag ->
      let tag = Datacon.Atom.basename tag in
      Error.error tag
	(sprintf "The data constructor %s is multiply defined." (Annotation.content tag))

(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)

(* Prepare for type inference. *)

(* ------------------------------------------------------------------------- *)

(* Unification (type) variables.

   We are really doing two-sorted unification: on the one hand, we
   infer types, and on the other hand, we infer arities (tuples of
   types). These are distinct: because we have no tuple constructor, a
   tuple of types is not a type. Because our [Unification] module does
   not support multi-sorted unification, we artificially mix the two
   sorts. *)

module Item : UnionFind.Item with type t = int = struct

  type t =
      int

  let equal : int -> int -> bool =
      (=)

  let n =
    ref 0

  let fresh () =
    let v = !n in
    n := v + 1;
    v

  module Map =
    AlphaLib.Patricia.Big

end

(* ------------------------------------------------------------------------- *)

(* Type unification. *)

module Term = struct

  type 'a t =
    | TyData of datatype
    | TyAtom
    | TyAtomSet
    | TyBool
    | TyArity of 'a list

  type 'a dt =
    | DTVar of 'a
    | DTTerm of 'a dt t

  let map f = function
    | TyData t ->
	TyData t
    | TyAtom ->
	TyAtom
    | TyAtomSet ->
	TyAtomSet
    | TyBool ->
	TyBool
    | TyArity xs ->
	TyArity (List.map f xs)

  let exomap _ = function
    | TyData t ->
	TData t
    | TyAtom ->
	TAtom
    | TyAtomSet ->
	TAtomSet
    | TyBool ->
	TBool
    | TyArity _ ->
	assert false

  exception Decompose

  let schedule x1 x2 pending =
    (x1, x2) :: pending

  let decompose term1 term2 pending =
    match term1, term2 with
    | TyData data1, TyData data2 when Datatype.Atom.equal data1 data2 ->
	pending
    | TyAtom, TyAtom
    | TyBool, TyBool
    | TyAtomSet, TyAtomSet ->
	pending
    | TyArity xs1, TyArity xs2 ->
	begin try
	  List.fold_right2 schedule xs1 xs2 pending
	with Invalid_argument _ ->
	  raise Decompose
	end
    | TyArity _, _
    | _, TyArity _ ->
	assert false
    | _, _ ->
	raise Decompose

end

module Unifier =
  Unification.ShallowMulti (Item) (Term)

(* ------------------------------------------------------------------------- *)

(* A global constraint store. *)

let partition =
  ref Unifier.empty

let (=) ty1 ty2 =
  partition := Unifier.unify !partition [ ty1, ty2 ]

(* ------------------------------------------------------------------------- *)

(* Types with type variables. *)

type ty =
    Unifier.variable

type arity =
    Unifier.variable

let make term : ty =
  let p, v = Unifier.make !partition term in
  partition := p;
  v

let atom : ty =
  make Term.TyAtom

let atomset : ty =
  make Term.TyAtomSet

let bool : ty =
  make Term.TyBool

let data t : ty =
  make (Term.TyData t)

let mkarity vs : arity =
  make (Term.TyArity vs)

let (==) arity tys =
  arity = mkarity tys

let rec import = function
  | TAtom ->
      atom
  | TAtomSet ->
      atomset
  | TBool ->
      bool
  | TData t ->
      data t

let unknown _ =
  Item.fresh() (* TEMPORARY que faire de la location? *)

let unknowns xs =
  List.map (fun x -> unknown (Var.Atom.basename x)) xs

(* ------------------------------------------------------------------------- *)

(* Printing types. *)

let print_type buffer = function
  | Term.DTVar v ->
      bprintf buffer "?%d" v
  | Term.DTTerm term ->
      match term with
      | Term.TyAtom ->
	  bprintf buffer "atom"
      | Term.TyAtomSet ->
	  bprintf buffer "atom_set"
      | Term.TyBool ->
	  bprintf buffer "bool"
      | Term.TyData t ->
	  bprintf buffer "%s" (Annotation.content (Datatype.Atom.basename t))
      | Term.TyArity _ ->
	  assert false

let print_types =
  Print.termlist Print.nl print_type

let print_arity buffer = function
  | Term.TyArity tys ->
      bprintf buffer "%d" (List.length tys)
  | _ ->
      assert false

let rec print_source_type buffer = function
  | TData t ->
      bprintf buffer "%s" (Annotation.content (Datatype.Atom.basename t))
  | TAtom ->
      bprintf buffer "atom"
  | TAtomSet ->
      bprintf buffer "atom_set"
  | TBool ->
      bprintf buffer "bool"

(* ------------------------------------------------------------------------- *)

(* A global environment. We exploit the fact that names are globally unique
   to simplify the handling of environments. *)

type env =
    ty Var.AtomMap.t

let env : env ref =
  ref Var.AtomMap.empty

let find x =
  try
    Var.AtomMap.find x !env
  with Not_found ->
    fprintf stderr "NOT FOUND: %s\n%!" (Annotation.content (Var.Atom.basename x)); (* TEMPORARY *)
    assert false

let extend x ty =
  try
    env := Var.AtomMap.strict_add x ty !env
  with Var.AtomMap.Strict _ ->
    let x = Var.Atom.basename x in
    Error.signal x
      (sprintf "The variable %s is multiply defined." (Annotation.content x))

let extends xs tys =
  List.iter2 extend xs tys

let extend_atom x =
  extend x atom

let extends_atom xs =
  List.iter extend_atom xs

(* ------------------------------------------------------------------------- *)

(* The function [ground] maps a type variable to its inferred value, a
   ground type. The function [typeof] maps a value variable to its
   inferred type, a ground type. 

   These two functions must not be called until all constraints have
   been successfully solved. They are defined here, however, because
   we will refer to them inside ``lazy'' thunks. *)

let rec ground v : typ =
  match Unifier.descriptor v !partition with
  | [] ->

      (* Undetermined value. Return an arbitrary type. *)

      (* We choose [atomset], as opposed to [atom], because the type
	 [atom] has specific properties -- e.g.  its values have
	 non-empty support. *)

      (* TEMPORARY avoid defaults -- perform generalization *)

      TAtomSet

  | [ term ] ->

      (* Determined value. *)

      Term.exomap ground term

  | _ :: _ :: _ ->

      (* Inconsistent value. This cannot happen, because errors have
	 been signaled above and are fatal. *)

      assert false

let typeof x : typ =
  ground (find x)

let groundx v : Typed.arity =
  match Unifier.descriptor v !partition with
  | [] ->

      (* Undetermined value. Return an arbitrary arity. *)

      []

  | [ Term.TyArity vs ] ->

      (* Determined value. *)

      List.map (fun v ->
	lazy (ground v)
      ) vs

  | [ Term.TyAtom ]
  | [ Term.TyAtomSet ]
  | [ Term.TyBool ]
  | [ Term.TyData _ ] ->

      assert false

  | _ :: _ :: _ ->

      (* Inconsistent value. This cannot happen, because errors have
	 been signaled above and are fatal. *)

      (* TEMPORARY this assertion failed once *)

      assert false

(* ------------------------------------------------------------------------- *)

(* Assigning (simple) type schemes to primitive operations. *)

let typeschemeofprim = function
  | PrimBoolAnd
  | PrimBoolOr ->
      [ bool; bool ], [ bool ]
  | PrimBoolNot ->
      [ bool ], [ bool ]
  | PrimAtomEquality ->
      [ atom; atom ], [ bool ]
  | PrimGenericSupport
  | PrimGenericOuter
  | PrimGenericInner
  | PrimGenericBound ->
      (* TEMPORARY restrict last three to pattern types *)
      [ unknown() ], [ atomset ]
  | PrimSetEmpty ->
      [], [ atomset ]
  | PrimSetMember ->
      [ atom; atomset ], [ bool ]
  | PrimSetAdd ->
      [ atom; atomset ], [ atomset ]
  | PrimSetUnion
  | PrimSetInter
  | PrimSetMinus ->
      [ atomset; atomset ], [ atomset ]
  | PrimSetIsEmpty ->
      [ atomset ], [ bool ]
  | PrimSetChoose ->
      [ atomset ], [ atom; atomset ]
  | PrimSingletonRename ->
      let v = unknown() in
      [ atom; atom; v ], [ v; bool]

(* ------------------------------------------------------------------------- *)

(* Operations on the (fixed) function environment.

   Each function's input and output arity is known, because its
   parameters and results are declared. *)

type fenv =
    (ty list * ty list) Valfun.AtomMap.t

let fenv : fenv =
  Valfun.AtomMap.map (fun (xs, _, opc, _) ->
    let ys, _ = open_opc opc in
    unknowns xs, unknowns ys
  ) fundefs

let ffind = function
  | CUser f ->
      Valfun.AtomMap.find f fenv
  | CPrim p ->
      let tuplein, tupleout = typeschemeofprim p in
      tuplein, tupleout

(* ------------------------------------------------------------------------- *)

(* A base name for the atoms that replace wildcard patterns. *)

let wildcard : Identifier.t =
  Annotation.make Location.dummy "_"

(* ------------------------------------------------------------------------- *)

(* Generating constraints, building the global environment, and translating
   the source abstract syntax tree into an open version. *) 

let rec arity layout =
  match Annotation.content layout with
  | LComponent _ ->
      1
  | LInner layout
  | LOuter layout
  | LAbstraction layout ->
      arity layout
  | LTuple layouts ->
      List.fold_left (fun accu layout ->
	accu + arity layout
      ) 0 layouts

let check_layout check_component components layout =
  let expected = arity layout
  and got = List.length components in
  if expected <> got then
    Error.error layout (* TEMPORARY find appropriate location *)
      (sprintf "Arity mismatch: are %d or %d value(s) transmitted here?" expected got)
      (* TEMPORARY try not to abort immediately -- use normal unification process *)
  else
    let next = Misc.iterator components in
    let rec check layout =
      match Annotation.content layout with
      | LComponent (x, ty) ->
	  let component = next() in
	  Typed.SComponent (x, lazy ty, check_component component (import ty))
      | LInner layout ->
	  Typed.SInner (check layout)
      | LOuter layout ->
	  Typed.SOuter (check layout)
      | LAbstraction layout ->
	  Typed.SAbstraction (check layout)
      | LTuple layouts ->
	  Typed.STuple (List.map check layouts)
    in
    check layout

let rec check_set_expression s =
  match s with
  | SEEmpty ->
      ()
  | SEApp (_, _) ->
      () (* TEMPORARY check that set function is applied to appropriate argument *)
  | SEAssocOp (_, ss) ->
      List.iter check_set_expression ss
  | SEConditional (c, s1, s2) ->
      check_constraint c;
      check_set_expression s1;
      check_set_expression s2

and check_constraint c =
  match c with
  | FTrue
  | FFalse ->
      ()
  | FBoolVar b ->
      bool = find b
  | FNot c ->
      check_constraint c
  | FBoolAssocOp (_, cs) ->
      List.iter check_constraint cs
  | FSetBinOp (s1, _, s2) ->
      check_set_expression s1;
      check_set_expression s2

let rec check_expression e (expected : arity) = 
  {
    Typed.loc = Annotation.get e;
    Typed.arity = groundx expected;
    Typed.content =
      match Annotation.content e with
      | EVar x ->
	  expected == [ find x ];
	  Typed.EVar x
      | EBool b ->
	  expected == [ bool ];
	  Typed.EBool b
      | ETagTuple (tag, es) ->
	  let datatype, layout, guard = Datacon.AtomMap.find tag datacondefs in
	  expected == [ data datatype ];
	  Typed.ETagTuple (tag, check_layout check_expression1 es layout, guard)
      | EFresh ofr ->
	  let xs, e = open_ofr ofr in
	  extends_atom xs;
	  Typed.EFresh (xs, check_expression e expected)
      | ECase (e, branches) ->
	  let tuple = unknown() in
	  Typed.ECase (check_expression e tuple, check_branches branches tuple expected)
      | ECall (callee, e) ->
	  let tuplein, tupleout = ffind callee in
	  expected == tupleout;
	  Typed.ECall (callee, check_expression e (mkarity tuplein))
      | EMulti es ->
	  let tuple = List.map unknown es in
	  expected == tuple;
	  Typed.EMulti (List.map2 check_expression1 es tuple)
      | ELetMulti (e1, olm) ->
	  let xs, guard, e2 = open_olm olm in
	  let tuple = unknowns xs in
	  extends xs tuple;
	  check_constraint guard;
	  Typed.ELetMulti (check_expression e1 (mkarity tuple), xs, guard, check_expression e2 expected)
      | EAssert (assertion, c, e) ->
	  check_constraint c;
	  Typed.EAssert (assertion, c, check_expression e expected)
      | EAbsurd ->
	  Typed.EAbsurd
      | EFail ->
	  Typed.EFail
      | ENextCase ->
	  Typed.ENextCase
  }

and check_expression1 e ty =
  check_expression e (mkarity [ ty ])

and check_branches branches tuplein tupleout =
  List.map (fun branch ->
    check_branch branch tuplein tupleout
  ) branches

and check_branch branch tuplein tupleout =
  let ps, e = open_obr branch in
  let ps = check_patterns ps tuplein in (* explicit sequencing required *)
  ps, check_expression e tupleout

and check_patterns ps tuplein =
  let tuple = List.map unknown ps in
  tuplein == tuple;
  List.map2 check_pattern ps tuple

and check_pattern p ty =
  match Annotation.content p with
  | PWildcard ->
      let x = Var.Atom.freshb wildcard in
      extend x ty;
      Typed.VVar x
  | PVar x ->
      extend x ty;
      Typed.VVar x
  | PBool b ->
      ty = bool;
      Typed.VBool b
  | PTagTuple (tag, ps) ->
      let datatype, layout, guard = Datacon.AtomMap.find tag datacondefs in
      ty = data datatype;
      Typed.VTagTuple (tag, check_layout check_pattern ps layout, Annotation.transfer p guard)

(* ------------------------------------------------------------------------- *)

(* Typecheck and translate all function definitions. *)

let fundefs =
  Valfun.AtomMap.mapi (fun f (xs, pre, opc, e) ->
    let ys, post = open_opc opc in
    let tuplein, tupleout = ffind (CUser f) in
    extends xs tuplein;
    check_constraint pre;
    extends ys tupleout;
    check_constraint post;
    xs, pre, ys, post, check_expression e (mkarity tupleout)
  ) fundefs

(* ------------------------------------------------------------------------- *)

(* Check whether any constraints are inconsistent. *)

(* TEMPORARY an occur check will be needed when types become more complex; add it to [Unification] *)

let () =
  Unifier.fold (fun _ desc () ->
    match desc with
    | []
    | [ _ ] ->
	()
    | (Term.TyArity _) :: _ :: _ ->
	Error.signal Location.vdummy (* TEMPORARY find location *)
	  (sprintf "Arity mismatch between the following arities:");
	fprintf stderr "%a" (Print.wrap (Print.seplist Print.comma print_arity)) desc
    | _ :: _ :: _ ->
	Error.signal Location.vdummy (* TEMPORARY find location *)
	  (sprintf "Type mismatch between the following types:");
	let terms = List.map (Unifier.unchop_term !partition) desc in
	fprintf stderr "%a"
	  (Print.wrap print_types) terms
  ) !partition ()

(* ------------------------------------------------------------------------- *)

(* Abort now if any errors were found above. *)

let () =
  Error.signaled()

(* ------------------------------------------------------------------------- *)

(* Accessors. *)

let fundef f =
  Valfun.AtomMap.find f fundefs

let typedef t =
  Datatype.AtomMap.find t typedefs

let tagdef tag =
  Datacon.AtomMap.find tag datacondefs

let tagtype tag =
  let datatype, _, _ = tagdef tag in
  TData datatype

let typename t =
  Annotation.content (Datatype.Atom.basename t)

let tagname tag =
  Annotation.content (Datacon.Atom.basename tag) (* TEMPORARY inelegant *)

let funname f =
  Annotation.content (Valfun.Atom.basename f)

