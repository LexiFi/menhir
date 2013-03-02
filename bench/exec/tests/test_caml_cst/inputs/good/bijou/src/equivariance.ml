open Printf
open Source
open SymbolTable
open Facts
open Typedefs

(* ------------------------------------------------------------------------- *)

(* [decompose c] decomposes the constraint [c] into atomic set
   constraints, represented as pairs of set expressions. This includes
   the constraints that can appear inside set expressions, as part of
   conditionals. Each atomic set constraint will be independently
   checked for equivariance. *)

let rec decompose_s loc accu s =
  let loc = Location.get loc s in
  match Location.content s with
  | SEmpty
  | SUniverse
  | SSort _
  | SApp _ ->
      accu
  | SAssocOp (_, ss) ->
      List.fold_left (decompose_s loc) accu ss
  | SConditional (c, s1, s2) ->
      decompose_s loc (decompose_s loc (decompose loc accu c) s1) s2

and decompose loc accu c =
  let loc = Location.get loc c in
  match Location.content c with
  | FTrue
  | FFalse
  | FBoolVar _ ->
      accu
  | FNot c ->
      decompose loc accu c
  | FBoolAssocOp (_, cs) ->
      List.fold_left (decompose loc) accu cs
  | FSetBinOp (s1, _, s2) ->
      (loc, s1, s2) :: (decompose_s loc (decompose_s loc accu s1) s2)

let decompose c =
  decompose None [] c

(* ------------------------------------------------------------------------- *)

(* [project facts mode sf] projects an application of the set function
   [sf] to some variable [x] onto a focus sort. The parameters [facts]
   and [mode] characterize the type of [x] at the focus sort. *)

let rec project facts mode sf : set_function list =
  match mode, sf with
  | MNormal, SFSupport ->
      (* If the type of [x] has empty support at the focus sort,
	 project this application down to the empty set. *)
      if facts.support.can then [ SFSupport ] else []
  | MNormal, (SFInner | SFOuter | SFBound) ->
      (* The type of [x] is a normal type at the focus sort, so
	 it has empty [inner], [outer], and [bound] sets. *)
      []
  | MBinding, SFSupport ->
      (* The type of [x] is a binding type at the focus sort.
	 Translate [support] to the union of [inner], [outer],
	 and [bound]. *)
      project facts mode SFInner @
      project facts mode SFOuter @
      project facts mode SFBound
  | MBinding, SFInner ->
      if facts.inner.can then [ SFInner ] else []
  | MBinding, SFOuter ->
      if facts.outer.can then [ SFOuter ] else []
  | MBinding, SFBound ->
      if facts.bound.can then [ SFBound ] else []

(* ------------------------------------------------------------------------- *)

(* [fold facts mode x f s accu] iterates over all applications of set
   functions to the variable [x] within the set expression [s]. This
   excludes the constraints that can appear as part of conditionals
   within [s]. The parameters [facts] and [mode] characterize the type
   of [x] at the focus sort. *)

let fold facts mode x f s accu =
  let rec fold s accu =
    match Location.content s with
    | SEmpty
    | SUniverse
    | SSort _ ->
	accu
    | SApp (sf, _, VTComponent (VVar y)) ->
	if Var.Atom.equal x y then
	  List.fold_right f (project facts mode sf) accu
	else
	  accu
    | SApp (_, _, _) ->
	assert false (* set function applied to non-variable *)
    | SAssocOp (_, ss) ->
	List.fold_right fold ss accu
    | SConditional (_, s1, s2) ->
	fold s1 (fold s2 accu)
  in
  fold s accu

(* [fold facts mode x f ac accu] iterates over all applications of set
   functions to the variable [x] within the atomic set constraint [ac]. *)

let fold facts mode x f (_, s1, s2) accu =
  let fold = fold facts mode x f in
  fold s1 (fold s2 accu)

(* ------------------------------------------------------------------------- *)

(* The equivariance check focuses on a single atomic constraint and on
   a single sort at a time. *)

module Check (Focus : sig

  val atomic_constraint: Location.location * set_expression * set_expression

  val sort: sorte

end) = struct

let is_focus sort =
  Sorte.Atom.equal Focus.sort sort

let has_focus sorts =
  List.exists is_focus sorts

let mode_of sorts =
  if has_focus sorts then MBinding else MNormal

(* ------------------------------------------------------------------------- *)

(* The equivariance check abstracts away the structure of a tuple into
   a so-called shape. Shapes are trees whose nodes reflect the binding
   structure (abstractions, uses of [inner] and [outer]) and whose
   leaves reflect applications of set functions -- that is, if a leaf
   hangs off a certain node [n] in the shape, then a set function is
   being applied to the sub-tuple that corresponds to [n] in the
   original tuple. *)

type shape =
  | SLeaf of set_function
  | SInner of shape
  | SOuter of shape
  | SAbstraction of shape
  | SConjunction of shape list (* of at least two elements *)

(* ------------------------------------------------------------------------- *)

(* Auxiliary functions for building shapes. *)

let inner s =
  SInner s

let outer s =
  SOuter s

let abstraction s =
  SAbstraction s

let apply (f : shape -> shape) : shape list -> shape list = function
  | [] ->
      []
  | [ s ] ->
      [ f s ]
  | ss (* of at least two elements *) ->
      [ f (SConjunction ss) ]

(* ------------------------------------------------------------------------- *)

(* To begin, we turn tuples into shapes. *)

(* [shapify mode tuple] turns [tuple] into a list of shapes, which is
   interpreted as a conjunction. The [mode] parameter specifies
   whether [tuple] is a normal or binding type with respect to the
   focus sort. *)

(* When [shapify] finds an occurrence of a bound variable [x] within
   the tuple -- that is, a [TName] node -- it iterates over all
   applications of set functions to [x], at the focus sort, within the
   focus atomic constraint. For each such set function, it creates a
   leaf at the current node. *)

let rec shapify mode tuple : shape list =
  match Annotation.content tuple with
  | TComponent _ ->
      []
  | TInner (sorts, tuple) when has_focus sorts ->
      assert (mode = MBinding);
      apply inner (shapify MNormal tuple)
  | TOuter (sorts, tuple) when has_focus sorts ->
      assert (mode = MBinding);
      apply outer (shapify MNormal tuple)
  | TAbstraction (sorts, tuple) when has_focus sorts ->
      assert (mode = MNormal);
      apply abstraction (shapify MBinding tuple)
  | TInner (_, tuple)
  | TOuter (_, tuple)
  | TAbstraction (_, tuple) ->
      shapify mode tuple
  | TTuple tuples ->
      Misc.flat_map (shapify mode) tuples
  | TName (x, tuple) ->
      let facts = Facts.tuple Focus.sort tuple in
      fold facts mode x (fun sf accu ->
	SLeaf sf :: accu
      ) Focus.atomic_constraint (shapify mode tuple)

(* ------------------------------------------------------------------------- *)

(* A shape that contains no conjunction node must have exactly one
   leaf. Such a shape certainly corresponds to an equivariant
   constraint. Indeed, the constraint refers to only one variable, so
   it must be preserved under renamings. *)

(* [prune s] returns [None] if the shape [s] has exactly one leaf, and
   otherwise discards the prefix that leads to the first conjunction
   node. *)

let rec prune s : shape option =
  match s with
  | SLeaf _ ->
      None
  | SInner s
  | SOuter s
  | SAbstraction s ->
      prune s
  | SConjunction _ ->
      Some s

(* ------------------------------------------------------------------------- *)

(* Now, we simplify shapes down to summaries. A shape is acceptable if
   and only if it simplifies down to a summary, without failure. A
   summary can be thought of as a conjunction of leaves: either a
   conjunction of [support] leaves, or a conjunction of [outer]
   leaves, or a mixed conjunction of [inner] and [bound] leaves.
   These shapes are acceptable because they correspond to constraints
   with multiple free variables that will be consistently renamed.
   Anything else is deemed unacceptable. *)

type summary =
  | SumSupport
  | SumOuter
  | SumInnerBound

let is_support =
   (=) SumSupport

let is_outer =
   (=) SumOuter

let is_innerbound =
  (=) SumInnerBound

(* ------------------------------------------------------------------------- *)

(* Simplification. *)

(* Note that we do not currently attempt to explain failure. It seems
   easier to explain (in the reference manual) which constraints are
   accepted, rather to explain (dynamically) why a constraint is
   rejected. *)

(* Note that we do need to explicitly keep track of modes here. The
   use of [project] above ensures that [SFSupport] leaves occur only
   in normal mode, while [SFInner], [SFOuter], and [SFBound] leaves
   occur only in binding mode. *)

exception Unacceptable

let rec simplify s : summary =
  match s with
  | SLeaf SFSupport ->
      SumSupport
  | SLeaf SFOuter ->
      SumOuter
  | SLeaf (SFInner | SFBound) ->
      SumInnerBound
  | SInner s ->
      let summary = simplify s in
      assert (is_support summary);
      (* What is known as [support] under an [inner] keyword
	 is known as [inner] to the outside. *)
      SumInnerBound
  | SOuter s ->
      let summary = simplify s in
      assert (is_support summary);
      (* What is known as [support] under an [outer] keyword
	 is known as [outer] to the outside. *)
      SumOuter
  | SAbstraction s ->
      let summary = simplify s in
      if is_outer summary then
	(* What is known as [outer] under an abstraction
	   is known as [support] to the outside. *)
	SumSupport
      else
	(* What is known as [inner], [bound], or [support]
	   inside an abstraction must not be referred to
	   at the outside. Note that it would be legal to
	   refer to [inner \ bound], but this is better
	   expressed as [free] at the outside anyway. *)
	raise Unacceptable
  | SConjunction ss ->
      let summaries = List.map simplify ss in
      if (List.for_all is_support summaries) then
	(* This must a normal type (with respect to the focus sort),
	   because uses of [support] at binding types have been
	   replaced with uses of [outer], [inner], and [bound].
	   Multiple uses of [support] at a normal type are allowed,
	   because their arguments will be consistently renamed
	   upon freshening. *)
	SumSupport
      else if (List.for_all is_outer summaries) then
	(* Multiple uses of [outer] at a binding type are allowed,
	   because their arguments will not be renamed upon freshening. *)
	SumOuter
      else if (List.for_all is_innerbound summaries) then
	(* Multiple, mixed uses of [inner] and [bound] at a binding
	   type are allowed, because their arguments will be partly
	   consistently renamed, partly not renamed. I know, this is
	   nonobvious. *)
	SumInnerBound
      else
        (* Anything else is forbidden. *)
	raise Unacceptable

(* ------------------------------------------------------------------------- *)

(* Put the above together. The initial mode is determined according to
   the type declaration's [binds] clause, which is given by the
   parameter [sorts]. *)

let check sorts tuple =
  let shape =
    match shapify (mode_of sorts) tuple with
    | [] ->
        (* If the shape has no leaves at all, then the constraint
	   has no free variables, so it is preserved under renamings. *)
	None
    | [ s ] ->
	Some s
    | ss ->
	Some (SConjunction ss)
  in
  Option.iter (fun shape ->
    (* Otherwise, prune the shape. It the shape only has one leaf,
       then the constraint is preserved under renamings. *)
    let shape =
      prune shape
    in
    Option.iter (fun shape ->
      (* If the constraint has more than one leaf, then attempt to
	 simplify it down to a summary. If this succeeds, then the
	 constraint is preserved under renamings. *)
      let (_ : summary) = simplify shape in
      ()
    ) shape
  ) shape

(* ------------------------------------------------------------------------- *)

(* Display an error message if required. *)

let check sorts tuple =
  try
    check sorts tuple
  with Unacceptable ->
    let loc, _, _ = Focus.atomic_constraint in
    Error.signal [ loc ]
      (sprintf "This constraint is unacceptable: I am unable to prove that it is preserved\n\
	        under renamings of the bound atoms at sort %s."
		    (SortTable.name Focus.sort))

end

(* ------------------------------------------------------------------------- *)

(* Iterate over all sorts and all atomic constraints. *)

let check_datacondef _ (t, params) =
  let sorts = (DatatypeTable.def t).datatype_sorts in
  let tuple, guard = open_guarded_tuple params in
  List.iter (fun ac ->
    SortTable.iter (fun sort () ->
      let module C = Check (struct
	let atomic_constraint = ac
	let sort = sort
      end) in
      C.check sorts tuple
    )
  ) (decompose guard)

(* ------------------------------------------------------------------------- *)

(* Iterate over all data constructor definitions. *)

let () =
  DataconTable.iter check_datacondef;
  Error.signaled()

