(* This module performs kind checking, sanity checking,
   and infers information for use by the generator. *)

open Error
open Printf
open Syntax
open Strings

(* At most one "identifier module" declaration is allowed per file. *)

module Identifier = struct

  let state =
    ref None

  let set (pos1, pos2, x) =
    match !state with
    | None ->
	state := Some x
    | Some _ ->
	error2 pos1 pos2 "At most one \"identifier module\" declaration is allowed." (* kind26.mla *)

end

(* A table of the parameters associated with every type. Because O'Caml
   does not yet have a natural way of expressing polymorphic recursion,
   we avoid it, which forces us to put quite severe restrictions on the
   use of type parameters in the input language. In short, every occurrence
   of a type (including its defining occurrence) must carry the same
   parameters.

   We also collect a set of all type parameters in used in type definitions,
   for use by the generator. *)

module Params = struct

  let distinct params =
     let _ = List.fold_left (fun accu (pos1, pos2, x) ->
       if StringSet.mem x accu then
	 error2 pos1 pos2 "The parameters of a type constructor should be distinct type variables." (* kind19.mla *)
       else
	 StringSet.add x accu
     ) StringSet.empty params
     in
     ()

  let table =
    Hashtbl.create 3077

  let all =
    ref StringSet.empty

  let associate (pos1, pos2, t) params =
    let rec compare params0 params1 =
      match params0, params1 with
      | [], [] ->
	  ()
      | (_, _, v0) :: params0, (pos1, pos2, v1) :: params1 ->
	  if v0 = v1 then
	    compare params0 params1
	  else
	    error2 pos1 pos2 (sprintf "\
              All occurrences of the type \"%s\" should carry the same parameters.\n\
              Here, the type variable '%s was expected." t v0) (* kind20.mla kind21.mla *)
      | [], _ :: _
      | _ :: _, [] ->
	  error2 pos1 pos2 (sprintf "\
	    All occurrences of the type \"%s\" should carry\n\
	    the same number of parameters." t) (* kind22.mla *)
    in
    try
      compare (Hashtbl.find table t) params
    with Not_found ->
      distinct params;
      Hashtbl.add table t params;
      List.iter (fun (_, _, param) ->
	all := StringSet.add param !all
      ) params

  let scope t params =
    let params2set params =
      List.fold_left (fun accu (_, _, v) ->
	StringSet.add v accu
      ) StringSet.empty params
    in
    let valid = params2set (Hashtbl.find table t)
    and present = params2set params in
    try
      let param = StringSet.choose (StringSet.diff present valid) in
      let pos1, pos2, _ = List.find (fun (_, _, v) -> v = param) params in
      error2 pos1 pos2 (sprintf "The type variable '%s is unbound." param) (* kind23.mla kind24.mla *)
    with Not_found ->
      ()

end

(* A table of the known (external) container types. To each container
   are associated the names of its map and fold operators. The identity
   container is built-in and allows some code sharing. The list and option
   containers are known by default. *)

module Container = struct

  let containers =
     ref (
       StringMap.add "list"   ("List.map",   "List.fold_left", "List.fold_left2") (
       StringMap.add "option" ("option_map", "option_fold", "option_fold2") (
       StringMap.empty)))

  let add (pos1, pos2, x) (_, _, map) (_, _, fold) (_, _, fold2) =
    try
      let _ = StringMap.find x !containers in
      match x with
      | "list"
      | "option" ->
	  error2 pos1 pos2 (sprintf
	    "The container \"%s\" is already built-in; you cannot redeclare it." x) (* kind01.mla *)
      | _ ->
	  error2 pos1 pos2 (sprintf "The container \"%s\" is declared twice." x) (* kind02.mla *)
    with Not_found ->
      containers := StringMap.add x (map, fold, fold2) !containers

  let check = function
    | None ->
	()
    | Some (pos1, pos2, x) ->
	try
	  let _ = StringMap.find x !containers in
	  ()
	with Not_found ->
	  error2 pos1 pos2 (sprintf "The container \"%s\" is undeclared." x) (* kind03.mla *)

  let print = function
    | None ->
	""
    | Some (_, _, c) ->
	" " ^ c

  let suffix = function
    | None ->
	""
    | Some _ ->
	"s"

  let map = function
    | None ->
	""
    | Some (_, _, c) ->
	let map, _, _ = StringMap.find c !containers in
	map ^ " "

  let fold = function
    | None ->
	""
    | Some (_, _, c) ->
	let _, fold, _ = StringMap.find c !containers in
	fold ^ " "

  let fold2 = function
    | None ->
	""
    | Some (_, _, c) ->
	let _, _, fold2 = StringMap.find c !containers in
	fold2 ^ " "

end

(* This (global) environment allows keeping track of which identifiers
   are bound. There are distinct namespaces for sorts, types, record
   labels, and data constructors. Each namespace is implemented as a
   map of strings to the locations where the identifier is bound. *)

let sorts =
  ref StringMap.empty

let sortspace =
  "sort", sorts

let types =
  ref StringMap.empty

let typespace =
  "type", types

let labels =
  ref StringMap.empty

let labelspace =
  "record label", labels

let datas =
  ref StringMap.empty

let dataspace =
  "data constructor", datas

(* We distinguish expression types and pattern types. *)

let exptypes =
  ref StringSet.empty

let pattypes =
  ref StringSet.empty

let set_kind (_, _, t) k =
  k := StringSet.add t !k

let check_kind ((_, _, t) as id) k error =
  if not (StringSet.mem t !k) then
    error id

(* We keep track of the binds clause associated with every pattern
   type. *)

let binds =
  ref StringMap.empty

let getbinds t =
  StringMap.find t !binds

(* We also keep track of which (pattern) types are used in
   abstractions. *)

let abstractions =
  ref StringSet.empty

(* Inserting an identifier into a namespace. We check that no
   identifier is declared twice. *)

let insert (denomination, cell) (pos1, pos2, x) =
  let existing = !cell in
  try
    let _ = StringMap.find x existing in
    error2 pos1 pos2 (sprintf "The %s \"%s\" is declared twice." denomination x)
    (* kind04.mla kind05.mla kind17.mla kind18.mla *)
  with Not_found ->
    cell := StringMap.add x (pos1, pos2) existing

let inserto space = function
  | None ->
      ()
  | Some id ->
      insert space id

(* Checking that an identifier is declared. *)

let check (denomination, cell) (pos1, pos2, x) =
  try
    let _ = StringMap.find x !cell in
    ()
  with Not_found ->
    error2 pos1 pos2 (sprintf "The %s \"%s\" is undeclared." denomination x) (* kind06.mla kind07.mla *)

(* Build the environment. Record the binds clauses. *)

let inspect = function
  | Summand (ido, factors) ->
      inserto dataspace ido;
      List.iter (function (ido, _) ->
	    inserto labelspace ido
      ) factors

let () =
  List.iter (function
    | DeclSort id ->
	insert sortspace id
    | DeclExpType (params, id, summands) ->
	insert typespace id;
	set_kind id exptypes;
	Params.associate id params;
	List.iter inspect summands
    | DeclPatType (params, ((_, _, t) as id), ids, summands) ->
	insert typespace id;
	set_kind id pattypes;
	Params.associate id params;
	List.iter inspect summands;
	(* Turn the binds clause for this pattern type into a set
	   of sorts and record it. *)
	let sorts = List.fold_left (fun sorts (_, _, x) ->
	  StringSet.add x sorts
        ) StringSet.empty ids in
	binds := StringMap.add t sorts !binds
    | DeclContainer (c, map, fold, fold2) ->
	Container.add c map fold fold2
    | DeclIdentifier id ->
	Identifier.set id
  ) Front.declarations

let sorts =
  StringAux.domain !sorts

(* We wish to determine which sorts of atoms are directly or
   indirectly mentioned by each declaration. For expression types
   (types that bind no atoms), we refer to them as the live atoms. For
   pattern types (types that do bind atoms), we distinguish between
   atoms that are live in a binding position, live in outer scope, or
   live in inner scope. By construction, the atoms that are live in
   inner scope form a subset of the pattern type's binds
   clause. Indeed, there is no such thing as an inner scope for atoms
   that are not declared to be bound by the pattern. *)

let live =
  ref (StringAux.set_to_map !exptypes StringSet.empty)

let getlive t =
  StringMap.find t !live

let add_live_atom t sort =
  StringAux.modify live t (StringSet.add sort) 

let boundlive =
  ref (StringAux.set_to_map !pattypes StringSet.empty)

let getboundlive t =
  StringMap.find t !boundlive

let add_boundlive_atom t sort =
  StringAux.modify boundlive t (StringSet.add sort)

let innerlive =
  ref (StringAux.set_to_map !pattypes StringSet.empty)

let getinnerlive t =
  StringMap.find t !innerlive

let outerlive =
  ref (StringAux.set_to_map !pattypes StringSet.empty)

let getouterlive t =
  StringMap.find t !outerlive

(* These sets will be computed via a fixpoint computation. To perform
   the computation efficiently, we build a directed graph where every
   type identifier is a vertex and there is an edge from every type
   identifier [t1] to every type identifier [t2] whose definition
   mentions [t1]. Edges are labeled according to the keywords that
   they cross. *)

type label =
  | LExpExp
  | LInner of Lexing.position * Lexing.position
  | LOuter of Lexing.position * Lexing.position
  | LNeutral of Lexing.position * Lexing.position
  | LPatPat
  | LAbstraction

module EdgeSet = Set.Make (struct
  type t = label * string
  let compare = compare
end)

let edges =
  ref (StringMap.map (fun _ -> EdgeSet.empty) !types)

let add t1 label t2 =
  StringAux.modify edges t1 (EdgeSet.add (label, t2))

(* Check that every identifier is bound. Record which abstractions
   exist. Build the graph. Detect obvious inconsistencies between
   the definitions and the binds clauses. *)

let psorts sorts =
  match StringSet.cardinal sorts with
  | 0 ->
      "no atoms"
  | 1 ->
      sprintf "atoms of sort %s" (StringSet.choose sorts)
  | _ ->
      sprintf "atoms of sorts %s" (StringAux.print sorts)

let exp_refers_to_pat_error t (pos1, pos2, t') = (* kind08.mla *)
  error2 pos1 pos2 (sprintf "\
Type \"%s\" appears to be an expression type,
since it does not have a binds clause. Yet, its
definition refers to the pattern type \"%s\"." t t')

let pat_refers_to_exp_error t (pos1, pos2, t') = (* kind09.mla *)
  error2 pos1 pos2 (sprintf "\
Type \"%s\" is a pattern type. Yet, its definition
refers to type \"%s\", which appears to be an expression
type, since it does not have a binds clause." t t')

let abs_refers_to_exp_error (pos1, pos2, t) = (* kind10.mla *)
  error2 pos1 pos2 (sprintf "\
Type \"%s\" appears to be an expression type,
since it does not have a binds clause.
So, it cannot occur inside an abstraction." t)

let inner_refers_to_pat_error (pos1, pos2, t) = (* kind11.mla *)
  error2 pos1 pos2 (sprintf "\
\"%s\" is a pattern type, so it is not a valid
argument to \"inner\", \"outer\", or \"neutral\"." t)

let check_expfactor t (_, factor) =
  match factor with
  | EAtom ((_, _, sort) as id) ->
      (* Check that this sort identifier is bound. *)
      check sortspace id;
      (* Mark this atom as live in the current type. *)
      add_live_atom t sort
  | EEscape _ ->
      ()
  | ETypRef (container, params, ((_, _, t') as id)) ->
      (* Check that this container is known. *)
      Container.check container;
      (* Check that this type identifier is bound. *)
      check typespace id;
      (* Check that it is an expression type. *)
      check_kind id exptypes (exp_refers_to_pat_error t);
      (* Check that its parameters are as expected and in scope. *)
      Params.scope t params;
      Params.associate id params;
      (* Create an edge. *)
      add t' LExpExp t
  | EAbstraction (params, ((_, _, t') as id)) ->
      (* Check that this type identifier is bound. *)
      check typespace id;
      (* Check that it is a pattern type. *)
      check_kind id pattypes abs_refers_to_exp_error;
      (* Check that its parameters are as expected and in scope. *)
      Params.scope t params;
      Params.associate id params;
      (* Record that an abstraction at this type is used. *)
      abstractions := StringSet.add t' !abstractions;
      (* Create an edge. *)
      add t' LAbstraction t

let atom_pat_edge_error (pos1, pos2, sort) t = (* kind12.mla *)
  error2 pos1 pos2 (sprintf "\
The definition of the pattern type \"%s\"
refers to an atom of sort %s.
Yet, type \"%s\" is not declared to bind such atoms." t sort t)

let pat_pat_edge_error (pos1, pos2, t') t = (* kind13.mla *)
  let binds = getbinds t
  and binds' = getbinds t' in
  let sorts = StringSet.diff binds' binds in
  error2 pos1 pos2 (sprintf "\
The definition of type \"%s\" refers to type \"%s\".
Their \"binds\" clauses should be identical.
Yet, the latter is declared to bind %s,
while the former is not." t t' (psorts sorts))

let inverse_pat_pat_edge_error (pos1, pos2, t') t = (* kind14.mla *)
  let binds = getbinds t
  and binds' = getbinds t' in
  let sorts = StringSet.diff binds binds' in
  error2 pos1 pos2 (sprintf "\
The definition of type \"%s\" refers to type \"%s\".
Their \"binds\" clauses should be identical.
Yet, the former is declared to bind %s,
while the latter is not." t t' (psorts sorts))

let check_patfactor t (_, factor) =
  match factor with
  | PAtom ((_, _, sort) as id) ->
      (* Check that this sort identifier is bound. *)
      check sortspace id;
      (* Check that it is consistent with the enclosing pattern type's binds clause. *)
      if not (StringSet.mem sort (getbinds t)) then
	atom_pat_edge_error id t;
      (* Mark this atom as bound in the current type. *)
      add_boundlive_atom t sort
  | PEscape _ ->
      ()
  | PTypRef (modifier, container, params, ((pos1, pos2, t') as id)) ->
      (* Check that this container is known. *)
      Container.check container;
      (* Check that this type identifier is bound. *)
      check typespace id;
      (* Check that its parameters are as expected and in scope. *)
      Params.scope t params;
      Params.associate id params;
      (* Create an edge. *)
      let label =
	match modifier with
	| MRef ->
	    (* Check that [t] is a pattern type. *)
	    check_kind id pattypes (pat_refers_to_exp_error t);
	    (* Check that the two binds clauses are consistent.
	       It is important to require the two binds clause to
	       be identical. Allowing [getbinds t'] to be a subset
	       of [getbinds t] would subtly alter the meaning of
	       the [inner] keyword, which is interpreted relative
	       to the binds clause of the enclosing type, and would
	       probably lead to misinterpretation by the user. *)
	    if not (StringSet.subset (getbinds t') (getbinds t)) then
	      pat_pat_edge_error id t;
	    if not (StringSet.subset (getbinds t) (getbinds t')) then
	      inverse_pat_pat_edge_error id t;
	    (* Choose an appropriate label. *)
	    LPatPat
	| MInner ->
	    (* Check that [t] is an expression type. *)
	    check_kind id exptypes inner_refers_to_pat_error;
	    LInner (pos1, pos2)
	| MOuter ->
	    (* Check that [t] is an expression type. *)
	    check_kind id exptypes inner_refers_to_pat_error;
	    LOuter (pos1, pos2)
	| MNeutral ->
	    (* Check that [t] is an expression type. *)
	    check_kind id exptypes inner_refers_to_pat_error;
	    LNeutral (pos1, pos2)
      in
      add t' label t

let () =
  List.iter (function
    | DeclExpType (_, (_, _, t), summands) ->
	List.iter (function Summand (_, factors) ->
	  List.iter (check_expfactor t) factors
	) summands
    | DeclPatType (_, (_, _, t), ids, summands) ->
	List.iter (check sortspace) ids;
	List.iter (function Summand (_, factors) ->
	  List.iter (check_patfactor t) factors
	) summands
    | DeclSort _
    | DeclContainer _
    | DeclIdentifier _ ->
	()
  ) Front.declarations

let abstractions =
  !abstractions

let allparams =
  !Params.all

(* Perform the fixpoint computation. *)

let neutral_error pos1 pos2 binds_above forbidden t t' = (* kind15.mla *)
  error2 pos1 pos2 (sprintf "\
Type \"%s\" contains %s, so it is not
neutral with respect to the pattern type \"%s\",
which binds %s.
You should perhaps use \"inner\" or \"outer\" instead of
\"neutral\", or fix the binds clause for type \"%s\"." t' (psorts forbidden) t (psorts binds_above) t)

let should_use_neutral_error specifier pos1 pos2 binds_above live_under t t' = (* TEMPORARY.mla *)
  error2 pos1 pos2 (sprintf "\
Type \"%s\" contains %s, so it is
neutral with respect to the pattern type \"%s\",
which binds %s.
You should perhaps use \"neutral\" instead of \"%s\"
or fix the binds clause for type \"%s\"." t' (psorts live_under) t (psorts binds_above) specifier t)

let types =
  StringAux.domain !types

let pending =
  ref types

let pend t =
  pending := StringSet.add t !pending

(* map[key] := map[key] U s *)
let update key map s =
  StringAux.merge map key s pend

(* map[key] := map[key] U map'[key'] *)
let transfer key map key' map' =
  update key map (StringMap.find key' !map')

let rec iterate () =
  try
    let t = StringSet.choose !pending in
    pending := StringSet.remove t !pending;
    EdgeSet.iter (fun (label, t') ->
      match label with
      | LExpExp ->
	  (* [t] and [t'] are expression types. *)
	  transfer t' live t live
      | LPatPat ->
          (* [t] and [t'] are pattern types. *)
	  transfer t' boundlive t boundlive;
	  transfer t' outerlive t outerlive;
	  transfer t' innerlive t innerlive
      | LAbstraction ->
          (* [t] is a pattern type and [t'] is an expression type. *)
	  transfer t' live t outerlive;
	  transfer t' live t innerlive
      | LInner _ ->
	  (* [t] is an expression type and [t'] is a pattern type. *)
	  let live_under = getlive t
	  and binds_above = getbinds t' in
	  update t' innerlive (StringSet.inter live_under binds_above);
	  update t' outerlive (StringSet.diff live_under binds_above)
      | LOuter _
      | LNeutral _ ->
	  (* [t] is an expression type and [t'] is a pattern type. *)
	  transfer t' outerlive t live
    ) (StringMap.find t !edges);
    iterate()
  with Not_found ->
    ()

let () =
  iterate()

(* Once the fixpoint computation is over, check that the [inner],
   [outer], and [neutral] specifiers are used appropriately. *)

let () =
  StringSet.iter (fun t ->
    EdgeSet.iter (fun (label, t') ->
      match label with
      | LExpExp
      | LPatPat
      | LAbstraction ->
	  ()
      | LInner (pos1, pos2)
      | LOuter (pos1, pos2)
      | LNeutral (pos1, pos2) ->

	  (* The meaning of [inner] and [outer] only differs if there
	     are sorts that are both declared to be bound by the
	     pattern [t'] and live inside [t]. We check that [inner]
	     and [outer] are used only in that case, and that
	     [neutral] is used otherwise. In other words, [neutral] is
	     valid exactly when [outer] and [inner] would produce the
	     same effect. *)

	  let live_under = getlive t
	  and binds_above = getbinds t' in
	  let void = StringSet.is_empty (StringSet.inter live_under binds_above) in

	  match label with
	  | LInner _ ->
	      if void then
		should_use_neutral_error "inner" pos1 pos2 binds_above live_under t' t
	  | LOuter _ ->
	      if void then
		should_use_neutral_error "outer" pos1 pos2 binds_above live_under t' t
	  | LNeutral _ ->
	      if not void then
		neutral_error pos1 pos2 binds_above live_under t' t
	  | _ ->
	      assert false

    ) (StringMap.find t !edges);
  ) types

(* Verbosity. *)

let () =
  if !Error.verbose then begin
    StringMap.iter (fun t sorts ->
      if not (StringSet.is_empty sorts) then
	fprintf stderr "Type \"%s\" refers to %s.\n" t (psorts sorts)
    ) !live;
    StringMap.iter (fun t sorts ->
      if not (StringSet.is_empty sorts) then
	fprintf stderr "Type \"%s\" refers to %s in binding position.\n" t (psorts sorts)
    ) !boundlive;
    StringMap.iter (fun t sorts ->
      if not (StringSet.is_empty sorts) then
	fprintf stderr "Type \"%s\" refers to %s in outer scope.\n" t (psorts sorts)
    ) !outerlive;
    StringMap.iter (fun t sorts ->
      if not (StringSet.is_empty sorts) then
	fprintf stderr "Type \"%s\" refers to %s in inner scope.\n" t (psorts sorts)
    ) !innerlive
  end

(* For each pattern type that is used in an abstraction, check that
   the set of declared bound atoms coincides with the set of live
   bound atoms. If this is not true, then some atoms are declared
   as being bound but can in fact never be bound, which is strange.

   For each pattern type that is used in an abstraction, check that
   the set of bound atoms equals the set of atoms that are live in
   inner scope. If this is not true, then some atoms are being bound
   but can never be referenced, which is strange. *)

let failed =
  ref false

let signal (_, cell) t message =
  let pos1, pos2 = StringMap.find t !cell in
  signal2 pos1 pos2 message;
  failed := true

let () =
  StringSet.iter (fun t ->
    let binds = getbinds t in
    let boundlive = getboundlive t in
    let innerlive = getinnerlive t in

    (* [binds] is the set of atom sorts that are declared to be bound
       by this abstraction. [boundlive] is the set of atom sorts that
       can in fact be bound by this abstraction. [innerlive] is the
       set of atom sorts that are live in inner scopes. Thanks to
       earlier checks, [boundlive] is a subset of [binds].
       Furthermore, by construction, [innerlive] is also a subset of
       [binds]. We check that, at abstractions, the three coincide. *)

    assert (StringSet.subset boundlive binds);
    assert (StringSet.subset innerlive binds);

    if not (StringSet.subset binds boundlive) then (* kind16.mla *)
      signal typespace t (sprintf "\
Type \"%s\" is declared to bind %s
but only contains %s in a binding position.
That is, %s never appear in a binding position.\
" t (psorts binds) (psorts boundlive) (psorts (StringSet.diff binds boundlive)));

    if not (StringSet.subset binds innerlive) then (* kind16.mla *)
      signal typespace t (sprintf "\
Type \"%s\" is declared to bind %s
but only refers to %s in inner scope.
That is, %s in a binding position
are never referred to.\
" t (psorts binds) (psorts innerlive) (psorts (StringSet.diff binds innerlive)))

  ) abstractions

(* Check that type identifiers and sort identifiers do not clash.
   This is made necessary by the fact that both sorts and types
   are translated down to types. *)

let () =
  StringSet.iter (fun t ->
    signal typespace t (sprintf "The identifier \"%s\" is defined both as a sort and as a type." t) (* kind25.mla *)
  ) (StringSet.inter sorts types)

(* Check that type identifiers, field label identifiers, and
   (lowercase versions of) data constructor identifiers do not
   clash. This is made necessary by the fact that all three are
   translated down to method names. *)

let labels =
  StringAux.domain !labels

let datas =
  StringAux.domain !datas

let () =
  StringSet.iter (fun t ->
    signal typespace t
      (sprintf "The identifier \"%s\" is defined both as a type and as a field label." t) (* kind27.mla *)
  ) (StringSet.inter types labels);
  StringSet.iter (fun d ->
    let t = String.lowercase d in
    if StringSet.mem t types then
      signal dataspace d (sprintf "The identifier \"%s\" is defined as a type and\n\
                                  the identifier \"%s\" as a data constructor.\n\
                                  Their lowercase versions should be distinct." t d); (* kind27.mla *)
    if StringSet.mem t labels then
      signal dataspace d (sprintf "The identifier \"%s\" is defined as a field label and\n\
                                   the identifier \"%s\" as a data constructor.\n\
                                   Their lowercase versions should be distinct." t d); (* kind27.mla *)
  ) datas

(* Exit if one of the above checks failed. *)

let () =
  if !failed then
    exit 1

(* Export the identifier module that was chosen. *)

let identifier_module =
  match !Identifier.state with
  | None ->
      "AlphaLib.Atom.String" (* default implementation *)
  | Some x ->
      x

