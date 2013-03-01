open Printf
open Print
open Annotation
open Source
open SymbolTable

(* ------------------------------------------------------------------------- *)

(* Facilities for building source-level set expressions and constraints. *)

(* These smart constructors perform some simplifications on the fly. The goal
   is not to gain efficiency, but to construct simpler constraints, which is
   useful because these constraints may be displayed to the user. *)

(* All locations are ignored in the constraints that we manipulate. It would
   perhaps seem nicer to define a new type of constraints without locations,
   but that would introduce the need for a translation, which is cumbersome. *)

let empty =
  Location.none SEmpty

let univ =
  Location.none SUniverse

let univ_at sort =
  Location.none (SSort sort)

let union ss =
  let ss =
    Misc.flat_map (function
      | { content = SAssocOp (OpUnion, ss) } ->
	  ss
      | { content = SEmpty } ->
	  []
      | s ->
	  [ s ]
    ) ss
  in
  match ss with
  | [] ->
      empty
  | [ s ] ->
      s
  | _ :: _ :: _ ->
      assert (not (List.exists (function { content = SEmpty } -> true | _ -> false) ss)); (* TEMPORARY *)
      Location.none (SAssocOp (OpUnion, ss))

let intersection xs =
  match xs with
  | [] ->
      assert false
  | [ x ] ->
      x
  | _ :: _ :: _ ->
      Location.none (SAssocOp (OpIntersection, xs))

let app_v sf v =
  Location.none (SApp (sf, None, VTComponent v))

let app_v_at sort sf v =
  Location.none (SApp (sf, Some sort, VTComponent v))

let app sf x =
  app_v sf (VVar x)

let app_at sort sf x =
  app_v_at sort sf (VVar x)

let difference xs =
  match xs with
  | [] ->
      assert false
  | { content = SEmpty } as x :: _ ->
      x
  | x :: xs ->
      let xs = List.filter (function { content = SEmpty } -> false | _ -> true) xs in
      match xs with
      | [] ->
	  x
      | _ ->
	  Location.none (SAssocOp (OpDifference, x :: xs))

let sop = function
  | OpUnion ->
      union
  | OpIntersection ->
      intersection
  | OpDifference ->
      difference

let conditional c s1 s2 =
  match c with
  | { content = FTrue } ->
      s1
  | { content = FFalse } ->
      s2
  | _ ->
      Location.none (SConditional (c, s1, s2))

let truth =
  Location.none FTrue

let falsity =
  Location.none FFalse

let subset s1 s2 =
  match s1 with
  | { content = SEmpty } ->
      truth
  | _ ->
      Location.none (FSetBinOp (s1, OpSubset, s2))

let equal s1 s2 =
  match s1, s2 with
  | { content = SEmpty }, { content = SEmpty } ->
      truth
  | _ ->
      Location.none (FSetBinOp (s1, OpEqual, s2))

let notequal s1 s2 =
  Location.none (FSetBinOp (s1, OpNotEqual, s2))

let nonempty s =
  match s with
  | { content = SEmpty } ->
      falsity
  | _ ->
      notequal s empty

let disjoint s1 s2 =
  match s1, s2 with
  | { content = SEmpty }, _
  | _, { content = SEmpty } ->
      truth
  | _ ->
      Location.none (FSetBinOp (s1, OpDisjoint, s2))

let acop = function
  | OpSubset ->
      subset
  | OpEqual ->
      equal
  | OpNotEqual ->
      notequal
  | OpDisjoint ->
      disjoint

let negation c =
  match c with
  | { content = FFalse } ->
      truth
  | { content = FTrue } ->
      falsity
  | _ ->
      Location.none (FNot c)

let conjunction cs =
  if List.exists (function { content = FFalse } -> true | _ -> false) cs then
    falsity
  else
    let cs =
      Misc.flat_map (function
	| { content = FBoolAssocOp (OpConjunction, cs) } ->
	    cs
	| { content = FTrue } ->
	    []
	| c ->
	    [ c ]
      ) cs
    in
    match cs with
    | [] ->
	truth
    | [ c ] ->
	c
    | _ ->
	Location.none (FBoolAssocOp (OpConjunction, cs))

let disjunction cs =
  match cs with
  | [] ->
      falsity
  | [ c ] ->
      c
  | _ ->
      Location.none (FBoolAssocOp (OpDisjunction, cs))

let implication cs =
  match cs with
  | [] ->
      assert false
  | [ c ] ->
      c
  | _ ->
      Location.none (FBoolAssocOp (OpImplication, cs))

let iff cs =
  match cs with
  | []
  | [ _ ] ->
      assert false
  | _ ->
      Location.none (FBoolAssocOp (OpEquivalence, cs))

let cop = function
  | OpConjunction ->
      conjunction
  | OpDisjunction ->
      disjunction
  | OpImplication ->
      implication
  | OpEquivalence ->
      iff

(* ------------------------------------------------------------------------- *)

(* Substitution of value tuples for variables within constraints. *)

type substitution =
    value_tuple Var.AtomMap.t

class subst (subst : substitution) = object

  inherit map as super

  (* Substituting values for variables within values. *)

  method vvar x =
    try
      begin match Var.AtomMap.find x subst with
      | VTComponent v ->
	  v
      | _ ->
	  assert false
            (* A variable that appears deep inside a value cannot be
	       substituted with a non-single-value tuple. *)
      end
    with Not_found ->
      VVar x

  (* Substituting value tuples for variables within value tuples. *)

  method vtcomponent v =
    match v with
    | VVar x ->
	(* A variable that appears at the top level of a value can
	   be substituted with an arbitrary tuple. *)
	begin try
	  Var.AtomMap.find x subst
	with Not_found ->
	  VTComponent v
	end
    | VBool _
    | VData _ ->
	super#vtcomponent v

  (* Substituting values for variables within constraints. *)

  method fboolvar x =
    try
      match Var.AtomMap.find x subst with
      | VTComponent (VVar y) ->
	  FBoolVar y
      | VTComponent (VBool true) ->
	  FTrue
      | VTComponent (VBool false) ->
	  FFalse
      | VTComponent _ ->
	  assert false (* no other value form has type [bool] *)
      | _ ->
	  assert false (* a Boolean variable cannot be substituted with a non-single-value tuple. *)
    with Not_found ->
      FBoolVar x

end

let substitute_constraint subst =
  (new subst subst)#contrainte

(* ------------------------------------------------------------------------- *)

(* Pretty-printers. *)

let print_set_function = function
  | SFSupport ->
      "free"
  | SFOuter ->
      "outer"
  | SFInner ->
      "inner"
  | SFBound ->
      "bound"

module Print (P : sig

  val print: var printer

end) = struct

let print_setsetset_operator op b =
  bprintf b "%s" (
    match op with
    | OpUnion ->
	" U "
    | OpIntersection ->
	" @ "
    | OpDifference ->
	" \\ "
  )

let rec print_value b = function
  | VVar x ->
      P.print b x
  | VBool bo ->
      bprintf b "%b" bo
  | VData (tag, []) ->
      bprintf b "%s" (DataconTable.name tag)
  | VData (tag, vs) ->
      bprintf b "%s(%a)" (DataconTable.name tag) (seplist comma print_value) vs

let rec print_value_tuple b = function
  | VTComponent v ->
      print_value b v
  | VTInner (_, tuple)
  | VTOuter (_, tuple)
  | VTAbstraction (_, tuple) ->
      print_value_tuple b tuple
  | VTTuple tuples ->
      seplist comma print_value_tuple b tuples

let print_substitution b subst =
  Var.AtomMap.iter (fun x tuple ->
    bprintf b "%a --> %a\n"
      P.print x
      print_value_tuple tuple
  ) subst

let rec print_atomic_set_expression b s =
  match Location.content s with
  | SEmpty ->
      bprintf b "empty"
  | SSort sort ->
      bprintf b "%s" (SortTable.name sort)
	(* cannot be parsed back in -- the parser offers no concrete syntax for this construct *)
  | SUniverse ->
      bprintf b "universe"
	(* cannot be parsed back in -- the parser offers no concrete syntax for this construct *)
  | SApp (sf, sort, tuple) ->
      bprintf b "%s%s(%a)"
	(print_set_function sf)
	(match sort with Some sort -> sprintf "@%s" (SortTable.name sort) | None -> "")
	print_value_tuple tuple
  | SAssocOp _ ->
      bprintf b "(%a)" print_set_expression s
  | SConditional (c, s1, { content = SEmpty }) ->
      bprintf b "if %a then %a end" print_constraint c print_set_expression s1
  | SConditional (c, s1, s2) ->
      bprintf b "if %a then %a else %a end" print_constraint c print_set_expression s1 print_set_expression s2

and print_set_expression b s =
  match Location.content s with
  | SEmpty
  | SSort _
  | SUniverse
  | SApp _
  | SConditional _ ->
      print_atomic_set_expression b s
  | SAssocOp (op, ss) ->
      seplist (print_setsetset_operator op) print_atomic_set_expression b ss

and print_setsetbool_operator = function
  | OpSubset ->
      "<="
  | OpEqual ->
      "=="
  | OpNotEqual ->
      "!="
  | OpDisjoint ->
      "#"

and print_boolboolbool_operator op b =
  bprintf b "%s" (
    match op with
    | OpConjunction ->
	" and "
    | OpDisjunction ->
	" or "
    | OpImplication ->
	" -> "
    | OpEquivalence ->
	" <-> "
  )

and print_atomic_constraint b c =
  match Location.content c with
  | FTrue ->
      bprintf b "true"
  | FFalse ->
      bprintf b "false"
  | FBoolVar x ->
      P.print b x
  | FNot c ->
      bprintf b "not %a" print_atomic_constraint c
  | FBoolAssocOp _ ->
      bprintf b "(%a)" print_constraint c
  | FSetBinOp (s1, op, s2) ->
      bprintf b "%a %s %a" print_set_expression s1 (print_setsetbool_operator op) print_set_expression s2

and print_constraint b c =
  match Location.content c with
  | FTrue
  | FFalse
  | FBoolVar _
  | FNot _
  | FSetBinOp _ ->
      print_atomic_constraint b c
  | FBoolAssocOp (op, cs) ->
      seplist (print_boolboolbool_operator op) print_atomic_constraint b cs

let rec print_toplevel_constraint b c =
  match Location.content c with
  | FTrue ->
      ()
  | FBoolAssocOp (OpConjunction, cs) ->
      list print_toplevel_constraint b cs
  | _ ->
      bprintf b "%a%t" print_constraint c nl

end

let print_substitution print =
  let module P = Print (struct let print = print end) in
  P.print_substitution

let print_toplevel_constraint print =
  let module P = Print (struct let print = print end) in
  P.print_toplevel_constraint

