open Source
open SymbolTable

(* ------------------------------------------------------------------------- *)

(* Converting values into value tuples. *)

(* [tuple_structure] accepts a type tuple [ttuple] and a list of
   values [vs], where [ttuple] and [vs] have a common arity. It
   returns a pair of [env] and [vtuple], where the value tuple
   [vtuple] is a view of the list [vs] under the structure provided by
   [ttuple], and where the environment [env] maps the variables bound
   in [ttuple] to appropriate sub-tuples of [vtuple]. *)

let tuple_structure ttuple vs =
  let vs = Iterator.make vs in
  let env = ref Var.AtomMap.empty in

  let rec map ttuple =
    match Location.content ttuple with
    | TComponent _ ->
	let v = Iterator.next vs in
	VTComponent v
    | TInner (sorts, ttuple) ->
	VTInner (sorts, map ttuple)
    | TOuter (sorts, ttuple) ->
	VTOuter (sorts, map ttuple)
    | TAbstraction (sorts, ttuple) ->
	VTAbstraction (sorts, map ttuple)
    | TTuple ttuples ->
	VTTuple (List.map map ttuples)
	  (* [List.map] evaluates left-to-right *)
    | TName (x, ttuple) ->
        let vtuple = map ttuple in
	env := Var.AtomMap.add x vtuple !env;
	vtuple
  in

  let vtuple = map ttuple in
  !env, vtuple

(* [tag_structure tag vs] returns a view of the value [VData (tag, vs)] under
   the structure associated with the declaration of the data constructor
   [tag]. *)

let tag_structure tag vs =
  let _, params = DataconTable.def tag in
  let tuple, _ = open_guarded_tuple params in
  let _, vtuple = tuple_structure tuple vs in
  (* dropping [env], since its domain is made up of fresh variables *)
  vtuple

(* ------------------------------------------------------------------------- *)

(* Determining the type of a (well-typed) value. *)

(* This function does not check that the value is well-typed. It assumes
   that the value is well-typed and re-computes its type. *)

let typeof env v =
  match v with
  | VVar x ->
      begin try
	Var.AtomMap.find x env
      with Not_found ->
	Printf.fprintf stderr
	  "NOT IN TYPE ENVIRONMENT: %s\n%!" (Location.content (Var.Atom.basename x)); (* TEMPORARY *)
	assert false
      end
  | VBool _ ->
      Location.none TBool
  | VData (tag, _) ->
      let t, _ = DataconTable.def tag in
      Location.none (TData t)

(* ------------------------------------------------------------------------- *)

(* Value unification. *)

(* I implement unification of deep terms, as opposed to shallow terms,
   because it is somewhat simpler -- it does not require creating
   fresh variables. *)

(* An equation between two abstractions cannot be decomposed, since
   that would require introducing a renaming. Such an equation is
   simply kept around in an unsimplified form. *) (* TEMPORARY not yet implemented *)

(* ------------------------------------------------------------------------- *)

(* Descriptors. *)

module Desc = struct

  (* A descriptor is an optional, non-variable value. *)

  type descriptor =
      value option

  let default =
    None

  (* Taking the union of two descriptors gives rise to new equations
     between values, hence the accumulator represents a conjunction of
     equations. We distinguish variable-variable equations and variable-value
     equations. *)

  type accumulator =
      (var * var) list * (var * (* non-variable *) value) list

  (* Decomposing an equation between two values. *)

  exception Decompose

  let rec decompose v1 v2 ((xxs, xvs) as accu) =
    match v1, v2 with

    (* Enqueue a new variable-variable equation. *)

    | VVar x1, VVar x2 ->

	(x1, x2) :: xxs, xvs

    (* Enqueue a new variable-term equation. *)

    | VVar x, v
    | v, VVar x ->

	xxs, (x, v) :: xvs

    (* Evaluate an equation between two (constant) Boolean values. *)

    | VBool b1, VBool b2 ->

	if b1 = b2 then
	  accu
	else
	  raise Decompose

    (* Decompose an equation between two structured values. *)

    | VData (tag1, vs1), VData (tag2, vs2) ->

	if Datacon.Atom.equal tag1 tag2 then
	  decompose_tuple (tag_structure tag1 vs1) (tag_structure tag2 vs2) accu
	else
	  raise Decompose

    | _, _ ->
	assert false (* impossible if value equations are well-typed *)

  (* Decomposing an equation between two value tuples. This is easy in
     all cases, except at abstractions, where decomposition is not
     possible -- introducing a renaming would be necessary -- which
     introduces a source of incompleteness. *)

  and decompose_tuple tuple1 tuple2 accu =
    match tuple1, tuple2 with
    | VTComponent v1, VTComponent v2 ->
	decompose v1 v2 accu
    | VTInner (_, tuple1), VTInner (_, tuple2)
    | VTOuter (_, tuple1), VTOuter (_, tuple2) ->
	decompose_tuple tuple1 tuple2 accu
    | VTAbstraction _, VTAbstraction _ ->
	(* TEMPORARY should iterate except under abstractions -- but that is a sort-specific notion. *)
	(* Introduce a focus sort and perform value unification independently at each sort? *)
	(* Problem: parameterizing [Desc] over a focus sort will lead to a dynamic number of
	   instantiations of [UnionFind], which will cause trouble. *)
	(* TEMPORARY at abstractions, suspend an equation between two value tuples *)
	(* TEMPORARY at abstractions, should still be able to go down into outer holes; only the inner/bound
	   parts should be suspended *)
	assert false (* TEMPORARY *)
    | VTTuple tuples1, VTTuple tuples2 ->
	begin try
	  List.fold_right2 decompose_tuple tuples1 tuples2 accu
	with Invalid_argument _ ->
	  assert false (* impossible if value equations are well-typed *)
	end
    | _, _ ->
	assert false (* impossible if value equations are well-typed *)

  let union desc1 desc2 accu =
    Option.concat_accu (fun v1 v2 accu ->
      v2, (* arbitrary choice *)
      decompose v1 v2 accu
    ) desc1 desc2 accu

end

(* ------------------------------------------------------------------------- *)

(* Instantiate [UnionFind] to map equivalence classes of variables to
   descriptors. *)

module U =
  UnionFind.Make (struct
    type t = var
    let equal = Var.Atom.equal
    module Map = Var.AtomMap
  end) (Desc)

(* ------------------------------------------------------------------------- *)

(* Unification. *)

let rec unify (state : U.state) accu =
  match accu with
  | [], [] ->
      state
  | [], (x1, v2) :: xvs ->
      let desc, accu = Desc.union (U.descriptor x1 state) (Some v2) ([], xvs) in
      let state = U.set x1 desc state in
      unify state accu
  | (x1, x2) :: xxs, xvs ->
      let state, accu = U.union x1 x2 state (xxs, xvs) in
      unify state accu

let unify state v1 v2 =
  unify state (Desc.decompose v1 v2 ([], []))

type equations =
  U.state option (* [None] means inconsistent *)

let truth : equations =
  Some U.initial

let unify (eqs : equations) v1 v2 : equations =
  match eqs with
  | None ->
      None
  | Some state ->
      try
	Some (unify state v1 v2)
      with Desc.Decompose ->
	None

(* ------------------------------------------------------------------------- *)

(* Presenting the outcome of unification (a set of value equations) to
   the client. *)

let decode (state : U.state) =

  let vd x1 desc accu =
    Option.fold (fun v2 accu ->
      (VVar x1, v2) :: accu (* emit a variable-value equation *)
    ) desc accu

  and vv x1 x2 accu =
    (VVar x1, VVar x2) :: accu (* emit a variable-variable equation *)

  in
  U.fold_all vd vv state []

let decode (eqs : equations) =
  Option.map decode eqs

