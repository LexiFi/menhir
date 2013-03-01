open Source
open SymbolTable

(* ------------------------------------------------------------------------- *)

(* Type environments. *)

type environment =
    typ Var.AtomMap.t

(* ------------------------------------------------------------------------- *)

(* [suggestion typ] is a base name suggestion for fresh variables of
   type [typ]. *)

let suggestion typ : Identifier.t =
  Location.none ("$" ^
    match Location.content typ with
    | TAtom sort ->
	SortTable.name sort
    | TAtomSet sort ->
	SortTable.name sort ^ "s"
    | TBool ->
	"bool"
    | TData t ->
	DatatypeTable.name t
  )

(* ------------------------------------------------------------------------- *)

(* [manufacture tuple] constructs a generic instance of [tuple], where
   each component is a fresh variable. *)

(* The internal version of the code is parameterized with an optional
   base name suggestion, represented as an atom. *)

let rec manufacture (base : var option) tuple ((env, vs) as accu) =
  match Location.content tuple with
  | TComponent typ ->
      let x =
	match base with
	| None ->
	    Var.Atom.freshb (suggestion typ)
	| Some base ->
	    Var.Atom.fresha base
      in
      Var.AtomMap.add x typ env,
      VVar x :: vs
  | TInner (_, tuple)
  | TOuter (_, tuple)
  | TAbstraction (_, tuple) ->
      manufacture base tuple accu
  | TName (x, tuple) ->
      manufacture (Some x) tuple accu
  | TTuple tuples ->
      List.fold_right (manufacture None) tuples accu

let manufacture tuple =
  manufacture None tuple (Var.AtomMap.empty, [])

