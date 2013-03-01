open Printf
open Source

(* ------------------------------------------------------------------------- *)

(* Kind checking for data type declarations. *)

let check_typ loc kind = function
  | TData t ->
      let kindt, _ = SymbolTable.typedef t in
      if not (Kind.equal kind kindt) then
	let t = Datatype.Atom.basename t in
	Error.signal loc
	  (sprintf "I expected %s, but %s is %s."
	     (Kind.print kind)
	     (Annotation.content t)
	     (Kind.print kindt))
  | TAtom
  | TAtomSet
  | TBool ->
      ()

let rec check_layout kind ll =
  match Annotation.content ll, kind with
  | LComponent (_, typ), _ ->
      check_typ ll kind typ
  | LInner layout, KPattern
  | LOuter layout, KPattern ->
      check_layout KExpression layout
  | LInner _, KExpression ->
      Error.signal ll
	(sprintf "The \"inner\" keyword does not make sense within %s."
	   (Kind.print kind))
  | LOuter _, KExpression ->
      Error.signal ll
	(sprintf "The \"outer\" keyword does not make sense within %s."
	   (Kind.print kind))
  | LAbstraction layout, KExpression ->
      check_layout KPattern layout
  | LAbstraction _, KPattern ->
      Error.signal ll
	(sprintf "An abstraction does not make sense within %s."
	   (Kind.print kind))
  | LTuple layouts, _ ->
      List.iter (check_layout kind) layouts

let check_datacondetails kind details =
  let layout, _ = open_dcd details in
  check_layout kind layout

let check_datacondef kind (_, details) =
  check_datacondetails kind details

let check_datatypedef _ (kind, datacondefs) =
  List.iter (check_datacondef kind) datacondefs

let () =
  Datatype.AtomMap.iter check_datatypedef SymbolTable.typedefs;
  Error.signaled()

