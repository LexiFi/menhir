(* $Id: constraint.ml 26 2007-09-28 13:56:54Z yann.regisgianas $ *)

open Position
open Misc
open MultiEquation
open CoreAlgebra

(** [sname] is the type of the names that are used to refer to type
    schemes inside constraints. These names are bound by [CLet]
    constraints and referred to by [CInstance] constraints. *)
type sname = PIdentifier.t

(** [type_constraint] defines a syntax for the constraints between 
    types. *)
type ('crterm, 'variable, 'syntax) type_constraint =
  | CTrue of position
  | CDump of position
  | CEquation of position * 'crterm * 'crterm
  | CConjunction of ('crterm, 'variable, 'syntax) type_constraint list
  | CLet of ('crterm, 'variable, 'syntax) scheme list 
      * ('crterm, 'variable, 'syntax) type_constraint
  | CInstance of position * sname * 'crterm
  | CDisjunction of ('crterm, 'variable, 'syntax) type_constraint list
  | CTerm of position 
      * (('crterm, 'variable, 'syntax) type_constraint list)
      * ('syntax list -> 'syntax) 
  | CGraft of position * sname * ('syntax -> 'variable -> int -> 'syntax)
  | CType of position * 'variable * ('variable -> int -> 'syntax)

(** A type scheme is a pair of a constraint [c] and a header [h],
    wrapped within two sets of universal quantifiers [rqs] and
    [fqs]. The former are considered rigid, while the latter are
    considered flexible. More precisely, for the type scheme to be
    considered consistent, the constraint [forall rqs.exists fqs.c]
    must hold. Rigid and flexible quantifiers otherwise play the same
    role, that is, they all end up universally quantified in the type
    scheme. A header is a mapping of names to types. *)
and ('crterm, 'variable, 'syntax) scheme =
  | Scheme of position * 'variable list * 'variable list 
      * ('crterm, 'variable, 'syntax) type_constraint
      * ('crterm, 'variable, 'syntax) header

and ('crterm, 'variable, 'syntax) header = 
    ('crterm * ('crterm, 'variable, 'syntax) type_constraint * position) 
      PIdentifier.Map.t

type variable = MultiEquation.variable

type variable_kind = MultiEquation.variable_kind

type crterm =
    variable arterm

type 'syntax tconstraint =
    (crterm, variable, 'syntax) type_constraint

type 'syntax theader = (crterm, variable, 'syntax) header

type 'syntax tscheme = 
    (crterm, variable, 'syntax) scheme

(** [variables_of_constraint c] returns the type variables of [c]. *)
let variables_of_constraint c =
  let rec chop acu = function
    | CTrue _ -> 
	acu

    | CType _ ->
	acu

    | CGraft _ ->
	acu

    | CTerm _ ->
	acu

    | CDump _ -> 
	acu

    | CEquation (_, t1, t2) -> 
	list_unionq acu (list_unionq (type_variables t1) (type_variables t2))

    | CConjunction ls -> 
	List.fold_left chop acu ls

    | CDisjunction ls -> 
	List.fold_left chop acu ls

    | CLet (schemes, c) -> 
	chop (List.fold_left chop_scheme acu schemes) c

  | CInstance (_, name, ty) -> 
      type_variables ty

  and chop_scheme acu = function
    | Scheme (_, rqs, fqs, c, h) -> 
	chop 
	  (PIdentifier.Map.fold 
	     (fun k (t, c, pos) acu -> 
		chop (list_unionq acu (type_variables t)) c) h acu)
	  c
  in
    chop [] c

(* TEMPORARY ne pas oublier d'expliquer que les rangs et les pools ne sont
   pas toujours d'accord entre eux *)

let rec expand_term = function
  | App (l, r) -> 
      TTerm (map (fun v -> TVariable v ) (App (l, r)))

  | _ -> assert false

let rec expand_term_in_depth t = 
  let expand v = 
    let desc = UnionFind.find v in
      match desc.structure with
	| None -> TVariable v
	| Some t -> expand_term_in_depth t
  in
    TTerm (map expand t)

let rec cposition = function
  | CTrue pos -> 
      pos

  | CType (pos, _, _) ->
      pos

  | CGraft (pos, _, _) ->
      pos

  | CTerm (pos, _, _) ->
      pos

  | CDump pos -> 
      pos

  | CLet ([], c) -> 
      cposition c

  | (CConjunction [] | CDisjunction []) -> 
      Position.dummy

  | (CConjunction l | CDisjunction l) -> 
      join (cposition (List.hd l)) (cposition (last l))

  | CLet (l, _) ->
      join (sposition (List.hd l)) (sposition (last l))

  | CEquation (p, _, _) -> 
      p

  | CInstance (p, _, _) -> 
      p

and sposition = function 
  | Scheme (p, _, _, _, _) -> 
      p

(* TEMPORARY expliquer qu' on emploie la pile native pour les let 
   et conj. frames, plus des pools s'epar'es pour les let frames *)

(** [bounded_variables_of_constraint c] returns the bounded 
 type variables of [c]. *)
let bounded_variables_of_constraint c =
  let rec chop acu = function

    | CTrue _ -> 
	acu

    | CType _ ->
	acu

    | CGraft _ ->
	acu

    | CDump _ -> 
	acu

    | CTerm _ ->
	acu

    | CEquation (_, _, _) | CInstance (_, _, _) -> 
	acu

    | CConjunction ls -> 
	List.fold_left chop acu ls

    | CDisjunction ls -> 
	List.fold_left chop acu ls

    | CLet (schemes, c) -> 
	chop (List.fold_left chop_scheme acu schemes) c

  and chop_scheme acu = function
    | Scheme (_, rqs, fqs, _, _) -> 
	list_unionq acu (rqs @ fqs)
  in
    chop [] c

(** [x <? t] is an instance constraint. *)
let (<?) x t pos =
  CInstance (pos, x, t)

(** [t1 =?= t2] is an equality constraint. *)
let (=?=) t1 t2 pos =
  CEquation (pos, t1, t2)

(** [c1 ^ c2] is a conjunction constraint. The implementation performs
    some simple (unnecessary) optimizations. *)
let (^) c1 c2 =
  match c1, c2 with
    | CTrue _, c
    | c, CTrue _ ->
      c

    | CConjunction cl1, CConjunction cl2 ->
	CConjunction (List.rev_append cl1 cl2)

    | CConjunction cl, c ->
	CConjunction (cl @ [ c ])

    | c, CConjunction cl ->
	CConjunction (c :: cl)

    | _, _ ->
	CConjunction [c1; c2]

let conj cs = 
  List.fold_left ( ^ ) (CTrue Position.dummy) cs

(** [ex qs c] returns the constraint [exists qs.c]. We encode existential
   constraints in terms of [let] constraints, since the latter are more
   general. *)
let ex ?pos qs c =
  CLet ([ Scheme (pos_or_undef pos, [], qs, c, PIdentifier.Map.empty) ], 
	CTrue (pos_or_undef pos))

(** [fl qs c] returns the constraint [forall qs.c]. We encode universal
   constraints in terms of [let] constraints, since the latter are more
   general. *)
let fl ?pos qs c =
  CLet ([ Scheme (pos_or_undef pos, qs, [], c, PIdentifier.Map.empty) ], 
	CTrue (pos_or_undef pos))

(** [exists f] creates a fresh variable [v] and returns the constraint
    [exists v.(f v)]. *)
let exists ?pos f =
  let v = variable Flexible () in
  let c = f (TVariable v) in
  ex ~pos:(pos_or_undef pos) [ v ] c

let exists3 ?pos f = 
  exists (fun x -> exists (fun y -> exists (fun z -> f x y z)))

(** [exists_list l f] associates a fresh variable with every element
    in the list [l], yielding an association list [m], and returns
    the constraint [exists m.(f m)]. *)
let exists_list ?pos l f =
  let l, m = variable_list Flexible l in
  ex ?pos:pos l (f m)

(** [forall_list l f] associates a fresh variable with every element
    in the list [l], yielding an association list [m], and returns
    the constraint [forall m.(f m)]. *)
let forall_list ?pos l f =
  let l, m = 
    List.fold_right (fun x (vs, xts) ->
		       let v = variable Rigid ~name:x () in
			 v :: vs, (x, TVariable v) :: xts
		    ) l ([], []) 
  in
  fl ~pos:(pos_or_undef pos) l (f m)

(** [variable_set xs] allocates a fresh variable for every element in the
    set [xs], and returns both a list of these variables and a map of
    elements to variables, viewed as types. *)
let variable_set kind xs =
  let pos = Position.dummy in
    PIdentifier.Set.fold 
      (fun x (vs, xts) ->
	 let k, n = kind (TName x) in
	 let v = variable k ?name:n () in
	   v :: vs, PIdentifier.Map.add x ((TVariable v), CTrue pos, pos) xts
      ) xs ([], PIdentifier.Map.empty)


(** [exists_set names f] associates a fresh variable with every name in
    the set [names], yielding a map [m] of names to variables, and returns
    the constraint [exists m.(f m)]. *)
let exists_set ?pos names f =
  let l, m = variable_set (const (Flexible, None)) names in
  ex ~pos:(pos_or_undef pos) l (f m)

(** [monoscheme header] turns [header] into a monomorphic type scheme. *)
let monoscheme ?pos header =
  Scheme (pos_or_undef pos, [], [], CTrue (pos_or_undef pos), header)

(** [scheme rqs names f] associates a fresh variable with every name in
    the set [names], yielding a map [m] of names to variables, and returns
    the type scheme [forall rqs m [f m] m], where the variables in [rqs]
    are rigid and the variables in [m] are flexible. *)
let scheme ?pos rqs names f =
  let l, m = variable_set (const (Flexible, None)) names in
    Scheme (pos_or_undef pos, rqs, l, f m, m)

(** [scheme' rqs rnames fnames f] associates a fresh variable with every 
  name in the set [fnames] and [rnames], yielding a map [m] of names to 
  variables, and returns the type scheme [forall (rqs @ rm) fm [f m] m], 
  where the variables in [rqs] and [rm] are rigid and the variables in [fm] 
  are flexible. *)
let scheme' ?pos rqs rnames fnames f =
  let fl, fm = variable_set (const (Flexible, None)) fnames in
  let rl, rm = variable_set (fun v -> (Rigid, Some v)) rnames in
  let m = PIdentifier.Map.union fm rm in
  Scheme (pos_or_undef pos, rqs @ rl, fl, f m, m)     

