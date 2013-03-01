open Annotation
open Printf
open Source
open SymbolTable
open Facts

(* TEMPORARY document the constraints enforced by this module *)

(* TEMPORARY set up negative test suite *)

(* TEMPORARY the fbio axiom is true at type t and sort s only if
   t is a pattern type with respect to sort s. *)

(* ------------------------------------------------------------------------- *)

(* A mode is either [normal] or [binding]. The former means that
   occurrences of atoms are considered free occurrences; the latter
   means that occurrences of atoms are considered binding
   occurrences. *)

type mode =
  | MNormal
  | MBinding

let print_mode = function
  | MNormal ->
      "does not bind"
  | MBinding ->
      "binds"

(* ------------------------------------------------------------------------- *)

(* We focus on a single sort at a time, because this makes things
   simpler, obviating the need to manipulate sets of sorts. *)

module Check (F : sig val focus: sorte end) = struct

open F

let is_focus sort =
  Sorte.Atom.equal focus sort

let has_focus sorts =
  List.exists is_focus sorts

let mode_of sorts =
  if has_focus sorts then MBinding else MNormal

(* [check_raw_type] checks that each occurrence of a type is legal with
   respect to the context -- that is, with respect to an expected mode. *)

let check_raw_type mode loc = function
  | TData t ->

      (* If the focus sort is live in [t], then check that the
	 expected mode matches [t]'s declared mode for type [t]. *)

      let facts = Facts.datatype focus t in
      if facts.support.can then
	let def = DatatypeTable.def t in
	let declared_mode = mode_of def.datatype_sorts in
	if mode <> declared_mode then
	  Error.signal [ loc ]
	    (sprintf "The type %s does not have the expected kind.\n\
		      I expected a type that %s atoms of sort %s."
			  (DatatypeTable.name t)
			  (print_mode mode)
			  (SortTable.name focus))

  | TAtom _
  | TAtomSet _
  | TBool ->

      (* These types are legal in all contexts. *)

      ()

let check_type mode =
  Location.wrap (check_raw_type mode)

(* [check_sorts] is used to check that each of the sorts that are
   explicitly listed in binds clauses, abstractions, inner and outer
   constructs, is live. [sorts] is a list of sorts that appear in
   the data type declaration. [facts] provides information about
   which atoms of the focus sort are live at this point. *)

let check_sorts loc construct sorts facts =
  if has_focus sorts && not facts.support.can then
    Error.signal [ loc ]
      (sprintf "This use of %s mentions redundant sorts.\n\
		A value of this type contains no atoms of sort %s."
		    construct (SortTable.name focus))

let echo = function
  | TInner _ ->
      "\"inner\""
  | TOuter _ ->
      "\"outer\""
  | TAbstraction _ ->
      "< ... >"
  | TComponent _
  | TTuple _
  | TName _ ->
      assert false

(* [enter] and [exit] mirror the effect of entering or exiting an
   abstraction. *)

let enter mode loc construct sorts facts =
  if has_focus sorts then begin
    if mode = MBinding then
      Error.signal [ loc ]
	(sprintf "This use of %s does not make sense.\n\
		  The enclosing type already binds atoms of sort %s."
		      construct (SortTable.name focus))
    else if not facts.bound.can then
      Error.signal [ loc ]
	(sprintf "This use of %s does not make sense.\n\
		  No atoms of sort %s appear in a binding position."
		      construct (SortTable.name focus))
    else if not facts.inner.can then
      Error.signal [ loc ]
	(sprintf "This use of %s is odd.\n\
		  No atoms of sort %s appear within the scope of the abstraction."
		      construct (SortTable.name focus));
    MBinding
  end
  else
    mode

let exit mode loc construct sorts _ =
  if has_focus sorts then begin
    if mode = MNormal then
      Error.signal [ loc ]
	(sprintf "This use of %s does not make sense.\n\
		  The enclosing type does not bind atoms of sort %s."
		      construct (SortTable.name focus));
    MNormal
  end
  else
    mode

let effect = function
  | TAbstraction _ ->
      enter
  | TInner _
  | TOuter _ ->
      exit
  | TComponent _
  | TTuple _
  | TName _ ->
      assert false

(* Mode checking for tuples. *)

let rec check_raw_tuple mode loc raw_tuple =
  match raw_tuple with
  | TComponent typ ->
      check_type mode loc typ
  | TInner (sorts, tuple)
  | TOuter (sorts, tuple) 
  | TAbstraction (sorts, tuple) ->
      let construct = echo raw_tuple in
      let facts = Facts.tuple focus tuple in
      check_sorts loc construct sorts facts;
      let mode = (effect raw_tuple) mode loc construct sorts facts in
      check_tuple mode loc tuple
  | TTuple tuples ->
      List.iter (check_tuple mode loc) tuples
  | TName (_, tuple) ->
      check_tuple mode loc tuple

and check_tuple mode loc tuple =
  let loc = Location.get loc tuple in
  check_raw_tuple mode loc (Location.content tuple)

(* Mode checking for data type and data constructor definitions. *)

let check_datacondef _ (t, params) =
  let def = DatatypeTable.def t in
  let declared_mode = mode_of def.datatype_sorts in
  let tuple, _ = open_guarded_tuple params in
  check_tuple declared_mode None tuple

let check_datatypedef t def =
  let loc = Annotation.get (DatatypeTable.identifier t) in
  check_sorts loc "\"binds\"" def.datatype_sorts (Facts.datatype focus t)

(* Running. *)

let () =
  DatatypeTable.iter check_datatypedef;
  DataconTable.iter check_datacondef

end

(* ------------------------------------------------------------------------- *)

(* Mode checking at all sorts. *)

let () =
  SortTable.iter (fun sort () ->
    let module C = Check (struct let focus = sort end) in
    ()
  );
  Error.signaled()

(* ------------------------------------------------------------------------- *)
(* ------------------------------------------------------------------------- *)

(* Type checking for set expressions and constraints. *)

(* ------------------------------------------------------------------------- *)

(* Environments. *)

(* A variable within a set expression stands for either a value (in
   which case it has a type) or for a tuple (which is just like a
   value, except it does not have a memory representation of its
   own). *)

type classification =
  | CValueVar of typ
  | CTupleVar of tuple

type environment =
  classification Var.AtomMap.t

(* ------------------------------------------------------------------------- *)

(* This function builds an environment out of a tuple. It also checks
   that no variable is bound twice in the tuple. *)

let rec build env tuple =
  match Location.content tuple with
  | TComponent _ ->
      env
  | TName (x, tuple) ->
      begin match Location.content tuple with
      | TComponent typ ->
	  let env = Var.AtomMap.strict_add x (CValueVar typ) env in
	  env
      | _ ->
	  let env = Var.AtomMap.strict_add x (CTupleVar tuple) env in
	  build env tuple
      end
  | TInner (_, tuple)
  | TOuter (_, tuple)
  | TAbstraction (_, tuple) ->
      build env tuple
  | TTuple tuples ->
      List.fold_left build env tuples

let build tuple : environment =
  try
    build Var.AtomMap.empty tuple
  with
  | Var.AtomMap.Strict x ->
      let m = Var.AtomIdMap.add_set (bound_tuple tuple) Var.AtomIdMap.empty in
      let id = Var.AtomIdMap.find x m in
      Error.errorv id
	(sprintf "The variable %s is multiply bound." (Location.content id))

(* ------------------------------------------------------------------------- *)

(* These functions check set expressions and constraints within a fixed
   environment. *)

(* The following type checking constraints are enforced. The set
   expression [free(x)] is allowed regardless of the type of [x],
   since support is defined everywhere. The set expression [inner(x)],
   [outer(x)], [bound(x)] is allowed if either [x] is a value of
   algebraic data type or [x] is a tuple. The set constraint [x] is
   allowed if [x] has type [bool]. *)

let rec check_set_expression env loc s =
  let loc = Location.get loc s in
  match Location.content s with
  | SEmpty
  | SUniverse
  | SSort _
  | SApp (SFSupport, _, _) ->
      ()
  | SApp ((SFInner | SFOuter | SFBound), _, VTComponent (VVar x)) ->
     begin match Var.AtomMap.find x env with
     | CTupleVar _
     | CValueVar { content = TData _ } ->
	 ()
     | _ ->
	 Error.signal [ loc ]
	   "The set functions \"inner\", \"outer\", and \"bound\" can be\n\
	    applied only to variables whose type is an algebraic data type."
     end
  | SApp (_, _, _) ->
      assert false (* set function applied to non-variable *)
  | SAssocOp (_, ss) ->
      List.iter (check_set_expression env loc) ss
  | SConditional (c, s1, s2) ->
      check_constraint env loc c;
      check_set_expression env loc s1;
      check_set_expression env loc s2

and check_constraint env loc c =
  let loc = Location.get loc c in
  match Location.content c with
  | FTrue
  | FFalse ->
      ()
  | FBoolVar x ->
      begin match Var.AtomMap.find x env with
     | CValueVar { content = TBool } ->
	 ()
     | _ ->
	 Error.signal [ loc ]
	   "Only variables of type bool can be interpreted as constraints."
     end
  | FNot c ->
      check_constraint env loc c
  | FBoolAssocOp (_, cs) ->
      List.iter (check_constraint env loc) cs
  | FSetBinOp (s1, _, s2) ->
      check_set_expression env loc s1;
      check_set_expression env loc s2

(* ------------------------------------------------------------------------- *)

(* Using the above functions, we check that the constraint that appears
   within each data constructor definition and lemma is well-typed. *)

let check_datacondef _ (_, params) =
  let tuple, guard = open_guarded_tuple params in
  check_constraint (build tuple) None guard

let check_lemma lemma =
  let x, t, claim = open_lemma lemma in
  let env = Var.AtomMap.singleton x (CValueVar (Location.none (TData t))) in
  check_constraint env None claim

let () =
  DataconTable.iter check_datacondef;
  List.iter check_lemma lemmata;
  Error.signaled()

