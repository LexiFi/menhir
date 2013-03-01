(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/yquem/cristal/fpottier/cvs/toy/compile.ml,v 1.19 2000/05/12 09:17:36 fpottier Exp $ *)

(* This module, boldly called a ``compiler'', simplifies the external expression language, so as to show only a
   simple, core language to our type-checker. This transformation could simply be called ``de-sugaring''.

   For simplicity, there are some cases where this transformation does not preserve the program's semantics.
   (These cases are documented in the code below.) This is not really a problem, as long as the program's type
   is preserved. *)

open ToySyntax
open InternalSyntax
open ToyParserUtil

(* The following exception is raised when a matching is heterogeneous, that is, when it involves two incompatible
   forms of structure, such as a pair and a record. *)

exception HeterogeneousMatching

(* The following exception is raised when a matching is non-rectangular, i.e. when two lines have a different number
   of columns. *)

exception NonRectangularMatching

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Finding free variables in a pattern}

   [pattern_vars] returns the set of variable names defined by a pattern. At the same time, it makes sure that the
   pattern is well-formed (i.e. [POr] patterns don't define any names) and linear (i.e. no variable occurs twice in
   the pattern). *)

exception IllegalPattern of string

let occur_check name accu =
  if StringSet.mem name accu then
    raise (IllegalPattern ("Variable " ^ name ^ " appears twice"));
  StringSet.add name accu

let rec add_pattern_vars accu = function
  | PWildcard
  | PConstant _ ->
      accu
  | PVar name ->
      occur_check name accu
  | PPair(pattern1, pattern2) ->
      add_pattern_vars (add_pattern_vars accu pattern1) pattern2
  | PRecord lpmap ->
      StringMap.fold (fun label pattern accu ->
	add_pattern_vars accu pattern
      ) lpmap accu
  | PConstruct(label, pattern) ->
      add_pattern_vars accu pattern
  | PAlias (pattern, name) ->
      add_pattern_vars (occur_check name accu) pattern
  | POr (pat1, pat2) ->
      if not (StringSet.is_empty (
        StringSet.union (pattern_vars pat1) (pattern_vars pat2))
      ) then
	raise (IllegalPattern "Disjunctive patterns mustn't perform bindings");
      accu
  | PRef pattern ->
      add_pattern_vars accu pattern

and pattern_vars pattern =
  add_pattern_vars StringSet.empty pattern

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Translating expressions} *)

(* This auxiliary function maps a constant to the name of its type. *)

let type_of_constant = function
  | ConstInt _ ->
      "int"
  | ConstUnit ->
      "unit"
  | ConstBool _ ->
      "bool"
  | ConstFloat _ ->
      "float"
  | ConstChar _ ->
      "char"
  | ConstString _ ->
      "string"

(* The following function translates expressions (as created by the parser) to simpler expressions (acceptable by
   the type inference engine). Indeed, many complex constructs may simply be viewed as syntactic sugar for
   (verbose) combinations of basic constructs. *)

let rec expression = function

  | EVar x ->
      VVar x
  | EFun [[PVar x], e] ->
      VLambda (KVar x, expression e)
  | EFun casel ->
      well_formed casel;
      matching casel
  | EApp (e1, e2) ->
      VApp (expression e1, expression e2)

  | ELet (_, [], e) ->

      (* Empty \texttt{let} constructs are eliminated. (This case shouldn't arise, but is added for completeness.) *)

      expression e

  | ELet (false, [PVar x, e1], e2) ->

      (* Simple \texttt{let} constructs are part of the internal language. *)

      VLet (KVar x, expression e1, expression e2)

  | ELet (false, [PWildcard, e1], e2) ->

      VLet (KWildcard, expression e1, expression e2)

  | ELet (false, [PConstant ConstUnit, e1], e2) ->

      VLet (KUnit, expression e1, expression e2)

  | ELet (false, [pat1, e1], e2) -> (
      let names = pattern_vars pat1 in
      match StringSet.cardinal names with

      | 0 ->

	  (* If a \texttt{let} construct binds no names, it can be replaced with a sequence. *)

	  expression (ESeq(EApp(EFun [[pat1], EVar "_unit"], e1), e2))

      | 1 ->

	  (* If a \texttt{let} construct binds exactly one name, it can be compiled to an elementary \texttt{let}
	     construct, by moving the pattern matching inside. *)

	  let x = StringSet.is_singleton names in

	  expression (ELet(false, [PVar x, EApp(EFun [[pat1], EVar x], e1)], e2))

      | n ->

	  (* A non-recursive, one-clause \texttt{let} definition which involves $n$ identifiers (where $n\geq 2$) is
	     compiled into $1+n$ elementary \texttt{let} definitions, where the first one evaluates the expression,
	     and the following $n$ are used to bind the pattern's components. *)

	  let obj = fresh() in

	  expression (ELet(false, [PVar obj, e1],
	    StringSet.fold (fun x body ->
	      ELet(false, [PVar x, EApp(EFun [[pat1], EVar x], EVar obj)], body)
	    ) names e2))

    )
  | ELet (true, [pat1, e1], e2) ->

      (* Recursive \texttt{let} constructs are turned into non-recursive ones. *)

      expression (ELet(false, [pat1, EApp(EVar "_rec", EFun [[pat1], e1])], e2))

  | ELet (recursive, binding :: bindings, e) ->

      (* \texttt{let} constructs involving several clauses are turned into single-clause ones by using a tuple. *)

      let tuple_binding = List.fold_left (fun (tuple_pat, tuple_e) (pat, e) ->
	PPair(pat, tuple_pat), EApp(EApp(EVar "_pair", e), tuple_e)
      ) binding bindings in

      expression (ELet(recursive, [tuple_binding], e))

  | ERecord lelist ->
      List.fold_right (fun (label, e) record ->
	VApp(VApp(VRecordUpdate label, record), expression e)
      ) lelist (VVar "{}")
  | ERecordAccess label ->
      VRecordAccess label
  | ERecordUpdate label ->
      VRecordUpdate label
  | ERecordRestrict label ->
      VRecordRestrict label
  | ERecordModify label ->
      VRecordModify label
  | ERecordTest label ->
      VRecordTest label

  (* A constant's value does not affect its type, so let's forget about it right here. *)

  | EConstant c ->
      VVar (Printf.sprintf "_%s" (type_of_constant c))

  | EConstruct label ->
      VConstruct label

  | ESeq (e1, e2) ->
      VApp(VLambda (KUnit, expression e2), expression e1)
  | EIf (e0, e1, e2) ->

      (* Our translation of \texttt{if} constructs does not preserve the program's semantics in a call-by-value
	 setting. However, its typing is preserved, which is what matters here. *)

      VApp(VApp(VApp(VVar "if", expression e0), expression e1), expression e2)

  | EVector elist ->
      List.fold_right (fun elem vector ->
	VApp(VApp(VVar "_vector_extend", expression elem), vector)
      ) elist (VVar "[||]")

  | ETry _ ->
      failwith "Exceptions currently unsupported." (* TEMPORARY *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Translating pattern matchings} *)

(* This function determines whether a matching is well-formed, i.e. contains only well-formed patterns and is
   rectangular. *)

and well_formed matching =
  
  let rec width = function
    | [] ->
	None
    | (patl, _) :: matching ->

	List.iter (fun pat ->
	  let _ = pattern_vars pat in
	  ()
	) patl;

	match List.length patl, width matching with
	| w, Some w' when w <> w' ->
	    raise NonRectangularMatching
	| w, _ ->
	    Some w in

  let _ = width matching in
  ()

(* This utility returns a matching's upper left pattern. The matching must be non-empty. *)

and upper_left = function
  | []
  | ([], _) :: _ ->
      assert false
  | (pat :: _, _) :: _ ->
      pat

(* This predicate tells whether a pattern is made up of a variable or a wildcard, possibly enclosed within
   aliases. *)

and is_wild_pat = function
  | PVar _
  | PWildcard ->
      true
  | PAlias (pat, _) ->
      is_wild_pat pat
  | _ ->
      false

(* This predicate tells whether a clause catches all possible combinations of values. *)

and is_catch_all (patl, _) =
  List.for_all is_wild_pat patl

(* This filter removes any clauses which appear after a catch-all clause. *)

and trim casel =
  match casel with
  | [] ->
      []
  | clause :: casel ->
      clause :: (if is_catch_all clause then [] else (trim casel))

(* This predicate tells whether a pattern is made up of a constructor pattern, possibly enclosed within
   aliases or within the left branch of a [POr] pattern. *)

and is_constructor_pat = function
  | PConstruct _ ->
      true
  | PAlias (pat, _) ->
      is_constructor_pat pat
  | POr (pat1, _) ->
      is_constructor_pat pat1
  | _ ->
      false

(* These functions help create definitions. *)

and edefine x y e =
  ELet(false, [PVar x, EVar y], e)

and vdefine x e1 e2 =
  VApp(VLambda(KVar x, e2), e1)

(* A (very crude) ``fresh'' name generator, used when auxiliary variables need to be introduced. *)

and fresh =
  let counter = ref 0
  in function () ->
    incr counter;
    "_x" ^ (string_of_int !counter)

(* [desugar_and_dispatch] accepts a matching, modifies it so that its first column contains no [PVar], [PAlias]
   or [POr] patterns (which we call ``desugaring'' here), and then passes it on to [dispatch] for further
   processing. The matching must contain at least one line and one column.

   As a special case, if the matching is a constructor matching, then some [PVar] and [PAlias] patterns are allowed
   to remain. This improves the typechecker's accuracy, e.g. when typing [function A z -> B z | x -> x].

   Before doing its duty, [desugar_and_dispatch] removes any clauses which appear after a catch-all clause. This
   isn't necessary for correctness (because unused clauses will be ignored anyway after we dispatch the matching
   to an appropriate compilation sub-routine), but it helps produce better output in common cases, because it
   allows the first two optimizations below to be applied more often. *)

and desugar_and_dispatch casel =
  match trim casel with

  | [ (PVar name) :: patl, action ] ->

      (* If there is only one clause, starting with a variable, then there is no need to generate a new name. *)

      VLambda(KVar name, matching [patl, action])

  | [ (PAlias(pat, x)) :: patl, action ] ->

      (* If there is only one clause, starting with an alias, then there is no need to generate a new name. *)

      VLambda(KVar x, VApp(desugar_and_dispatch [ pat :: patl, action ], VVar x))

  | casel ->

      (* Determine whether this is a constructor matching. If so, patterns which satisfy [is_wild_pat] are
	 allowed to remain, as a special case. *)

      let lenient = is_constructor_pat (upper_left casel) in

      (* Create a name which shall stand for the expression being first matched upon. To avoid unnecessary
	 $\eta$-expansion, create a flag which tells whether this name is actually used. *)

      let obj = fresh() in
      let obj_used = ref false in

      let rec desugar = function
	| ((pat :: patl, action) as clause) :: casel -> (
	    match pat with
	    | PVar name when not lenient ->
		obj_used := true;
		(PWildcard :: patl, edefine name obj action) :: (desugar casel)
	    | PAlias(pat, name) when not (lenient & (is_wild_pat pat)) ->
		obj_used := true;
		desugar ((pat :: patl, edefine name obj action) :: casel)
	    | POr(pat1, pat2) ->
		desugar ((pat1 :: patl, action) :: (pat2 :: patl, action) :: casel)
	    | PVar _
	    | PAlias _
	    | PWildcard
	    | PConstant _
	    | PPair _
	    | PRecord _
	    | PConstruct _
	    | PRef _ ->
		clause :: (desugar casel)
	  )
	| [] ->
	    []
	| ([], _) :: _ ->
	    assert false in

      let casel = desugar casel in

      (* If the name is actually used, then we need to bind it. *)

      if !obj_used then
	VLambda(KVar obj, VApp(dispatch casel, VVar obj))
      else
	dispatch casel

(* [dispatch_pair] compiles a matching which is known to contain a pair pattern in its upper left position. (Thus,
   it must have at least one line and one column.) The matching must have been previously desugared. *)

and dispatch_pair casel =

  (* Divide the pair into two columns. *)

  let casel = List.map (fun (patl, action) ->
    let patl = match patl with
    | PWildcard :: patl ->
	PWildcard :: PWildcard :: patl
    | (PPair(pat1, pat2)) :: patl ->
	pat1 :: pat2 :: patl
    | (PVar _ | PAlias _ | POr _) :: patl ->
	assert false
    | (PConstant _ | PRecord _ | PConstruct _ | PRef _) :: patl ->
	raise HeterogeneousMatching
    | [] ->
	assert false in
    (patl, action)
  ) casel in

  (* Add wrapper code to split the pair into its two components. This is, in fact, uncurrying, since we have
     compiled a function which expects a pair into one which expects two arguments. *)

  VApp(VVar "_uncurry", desugar_and_dispatch casel)

(* [dispatch_ref] compiles a matching which is known to contain a ref pattern in its upper left position. (Thus,
   it must have at least one line and one column.) The matching must have been previously desugared. *)

and dispatch_ref casel =

  (* Modify the matching's first column so as to expose the ref's content. *)

  let casel = List.map (fun (patl, action) ->
    let patl = match patl with
    | PWildcard :: _ ->
	patl
    | (PRef pat) :: patl ->
	pat :: patl
    | (PVar _ | PAlias _ | POr _) :: patl ->
	assert false
    | (PConstant _ | PRecord _ | PConstruct _ | PPair _) :: patl ->
	raise HeterogeneousMatching
    | [] ->
	assert false in
    (patl, action)
  ) casel in

  (* Add wrapper code to expose the ref's content. *)

  VApp(VVar "_deref", desugar_and_dispatch casel)

(* [dispatch_record] compiles a matching which is known to contain a record pattern in its upper left position. (Thus,
   it must have at least one line and one column.) The matching must have been previously desugared. *)

and dispatch_record casel =

  (* Find out which labels are mentioned in the record patterns. Any labels mentioned in some pattern, but not in
     some other pattern, are implicitly assumed to be present in the latter, together with a wildcard sub-pattern. *)

  let labels = List.fold_left (fun labels (patl, action) ->
    match patl with
    | (PRecord lpmap) :: _ ->
	StringSet.union (StringMap.domain lpmap) labels
    | PWildcard :: _ ->
	labels
    | (PVar _ | PAlias _ | POr _) :: patl ->
	assert false
    | (PConstant _ | PPair _ | PConstruct _ | PRef _) :: patl ->
	raise HeterogeneousMatching
    | [] ->
	assert false
  ) StringSet.empty casel in

  (* Divide the first column into as many columns as there are labels. *)

  let defaults =
    StringMap.lift (fun _ -> PWildcard) labels in

  let casel = List.map (fun (patl, action) ->
    let patl = match patl with
    | PWildcard :: patl ->
	StringSet.fold (fun _ patl -> PWildcard :: patl) labels patl
    | (PRecord lpmap) :: patl ->
	let lpmap = StringMap.fine_union (fun _ x -> x) defaults lpmap in
	StringMap.fold (fun _ pat patl -> pat :: patl) lpmap patl
    | _ ->
	assert false in
    (patl, action)
  ) casel in

  (* Add wrapper code to split the record into its two components. This is, in fact, a general form of uncurrying,
     since we have compiled a function which expects a record into one which expects a number of arguments. Here,
     we do not have predefined uncurrying functions for every possible combination of record fields, so we must
     code up an appropriate function. This requires binding a fresh variable.

     The case where all record patterns are empty must be treated specially. Otherwise, our compiled code would
     not use its argument at all, and thus would not force it to have a record type.

     Thus (slightly abusing notation):
     \begin{align*}
     \encode{\lambda\{\}.e} & = \lambda r.(\texttt{\_force\_record }r\texttt{; }\encode{e}) \\
     \encode{\lambda\{ \overline{l_i = x_i} \}.e} & = \lambda r.(\encode{e}\,\overline{r.x_i})
     \end{align*} *)

  let obj = fresh() in

  VLambda(KVar obj,

    if StringSet.is_empty labels then
      VApp(VLambda(KWildcard, matching casel),
	   VApp(VVar "_force_record", VVar obj))
    else
      StringSet.fold_rev (fun label body ->
	VApp(body, VApp(VRecordAccess label, VVar obj))
      ) labels (matching casel)

  )

(* [dispatch_wildcard] compiles a matching which is known to contain a wildcard pattern in its upper left
   position. (Thus, it must have at least one line and one column.) The matching must have been previously
   desugared. *)

and multiple_application e elist =
  List.fold_left (fun body arg ->
    EApp(body, arg)
  ) e elist

and (dispatch_wildcard : matching -> expression) = fun casel ->

  (* Here, the pattern matching is divided into two blocks, first the right part of all lines that have
     wildcards, then the remainder (everything after the first non-wildcard in the left column). *)

  let rec divide = function
    | (PWildcard :: patl, action) :: casel ->
	let first, remainder = divide casel in
	(patl, action) :: first, remainder
    | remainder ->
	[], remainder in

  (* Compile the first part, extended with an extra catch-all line at the end which executes the remainder part.
     Of course, if there is no remainder, then no extra line is required. *)

  let first, remainder = divide casel in
  if remainder = [] then

    VLambda(KWildcard, matching first)

  else begin

    let obj = fresh() in
    let z = fresh() in

    let rec extend = function
      | [] ->
	  assert false
      | [(patl, _) as last_clause] ->
	  let objl = List.map (fun _ -> fresh()) patl in
	  let pobjl = List.map (fun name -> PVar name) objl
	  and vobjl = List.map (fun name -> EVar name) (obj :: objl) in
	  [ last_clause ; (pobjl, multiple_application (EVar z) vobjl) ]
      | one :: more ->
	  one :: (extend more) in

    vdefine z (matching remainder) (VLambda(KVar obj, matching (extend first)))

  end

(* [dispatch_constructor] compiles a matching which is known to contain a data constructor pattern in its upper left
   position. (Thus, it must have at least one line and one column.) The matching must have been previously
   desugared.

   Here, we allow a slightly lenient definition of desugaring. Variable or wildcard patterns are allowed to remain,
   enclosed within an arbitrary number of aliases. Any [POr] patterns are forbidden, as well as any aliases bearing on
   other kinds of patterns. *)

and dispatch_constructor casel =

  (* This ``constructive'' predicate determines whether some line of the matching is compatible with a given data
     constructor. If it is, it returns a modified line, where the constructor pattern has been removed, so as to
     expose its sub-pattern. Otherwise, it returns nothing. *)

  let compatible label ((patl, action) as line) =
    match patl with
    | (PConstruct (label', pat)) :: patl when label = label' ->
	Some (pat :: patl, action)
    | (PWildcard | PVar _ | PAlias _) :: _ ->
	Some line
    | (PConstruct _) :: _ ->
	None
    | _ ->
	assert false in

  (* Gather the set of labels that appear before the first wildcard, and the remainder (the part of the matching
     below and including the first wildcard). *)

  let rec gather casel =
    match casel with
    | (patl, _) :: rest -> (
	match patl with
	| (PConstruct (label, _)) :: _ ->
	    let labels, remainder = gather rest in
	    StringSet.add label labels, remainder
	| (PWildcard | PVar _ | PAlias _) :: _ ->
	    StringSet.empty, casel
	| (POr _) :: _ ->
	    assert false
	| (PConstant _ | PPair _ | PRecord _ | PRef _) :: _ ->
	    raise HeterogeneousMatching
	| [] ->
	    assert false
      )
    | [] ->
	StringSet.empty, [] in

  let labels, remainder = gather casel in

  (* Remove any of these labels from the remainder part. *)

  let remainder = List.filter (fun (patl, _) ->
    match patl with
    | (PConstruct (label, _)) :: _ when StringSet.mem label labels ->
	false
    | (PConstruct _ | PWildcard | PVar _ | PAlias _) :: _ ->
	true
    | _ ->
	assert false
  ) remainder in

  (* Compile the matching. *)

  let body =
    match remainder with
    | [] ->
	VVar "_vreject"
    | _ ->
	desugar_and_dispatch remainder in

  StringSet.fold (fun label body ->
    let action = desugar_and_dispatch (Standard.filter_map (compatible label) casel) in
    VApp(VApp(VMatch label, action), body)
  ) labels body

(* [constant_matcher constant] returns (an abstract syntax tree for) a function which allows matching on the
   specified [constant]. It is the analogue of the [VMatch] abstract syntax tree used to match on a specific
   constructor.

   Because constant values are ignored, and there is only a finite number of constant types, we do not need
   to introduce a special [VConstantMatch] abstract syntax node. It suffices to define a number of primitive
   operations. *)

and constant_matcher constant =
  VVar (Printf.sprintf "_cmatch_%s" (type_of_constant constant))

(* [dispatch_constant] compiles a matching which is known to contain a constant pattern in its upper left
   position. (Thus, it must have at least one line and one column.) The matching must have been previously
   desugared.

   Constants are essentially a particular case of data constructors. The difference is that they don't have
   any arguments. *)

and dispatch_constant casel =

  (* This ``constructive'' predicate determines whether some line of the matching is compatible with a given
     constant. If it is, it returns a modified line, where the constant pattern has been removed. Otherwise, it
     returns nothing. *)

  let compatible constant ((patl, action) as line) =
    match patl with
    | (PConstant constant') :: patl when constant = constant' ->
	Some (patl, action)
    | PWildcard :: patl ->
	Some (patl, action)
    | (PConstant _) :: _ ->
	None
    | _ ->
	assert false in

  (* Gather the set of constants that appear before the first wildcard, and the remainder (the part of the matching
     below and including the first wildcard). *)

  let rec gather casel =
    match casel with
    | (patl, _) :: rest -> (
	match patl with
	| (PConstant constant) :: _ ->
	    let constants, remainder = gather rest in
	    ConstantSet.add constant constants, remainder
	| PWildcard :: _ ->
	    ConstantSet.empty, casel
	| (PVar _ | PAlias _ | POr _) :: _ ->
	    assert false
	| (PConstruct _ | PPair _ | PRecord _ | PRef _) :: _ ->
	    raise HeterogeneousMatching
	| [] ->
	    assert false
      )
    | [] ->
	ConstantSet.empty, [] in

  let constants, remainder = gather casel in

  (* Remove any of these constants from the remainder part. *)

  let remainder = List.filter (fun (patl, _) ->
    match patl with
    | (PConstant constant) :: _ when ConstantSet.mem constant constants ->
	false
    | (PConstant _ | PWildcard) :: _ ->
	true
    | _ ->
	assert false
  ) remainder in

  (* Compile the matching. In the special case where the explicit constant is [()], any remainder is ignored, and
     the (superfluous) call to [_cmatch_unit] is avoided. *)

  if ConstantSet.equal constants (ConstantSet.singleton ConstUnit) then

    VLambda (KUnit, matching (Standard.filter_map (compatible ConstUnit) casel))

  else begin

    let body =
      match remainder with
      | [] ->
	  VVar "_creject"
      | _ ->
	  desugar_and_dispatch remainder in

    ConstantSet.fold (fun constant body ->
      let action = matching (Standard.filter_map (compatible constant) casel) in
      VApp(VApp(constant_matcher constant, VLambda (KUnit, action)), body)
    ) constants body

  end

(* [dispatch] accepts a non-empty, desugared matching, and determines what kind of matching it is, so as to send
   it to an appropriate decomposition function. *)

and dispatch casel =
  match upper_left casel with
  | PWildcard ->
      dispatch_wildcard casel
  | PConstant _ ->
      dispatch_constant casel
  | PPair _ ->
      dispatch_pair casel
  | PRecord _ ->
      dispatch_record casel
  | PConstruct _ ->
      dispatch_constructor casel
  | PRef _ ->
      dispatch_ref casel
  | PVar _
  | PAlias _
  | POr _ ->
      assert false

(* [matching] compiles a matching, without making any assumptions about its shape or its contents. *)

and matching = function
  | [] ->

      (* An empty matching is compiled to a function which accepts no arguments. *)

      VVar "_reject"

  | ([], action) :: _ ->

      (* A matching with no columns is compiled to its action. Any unused clauses are ignored. *)

      expression action

  | casel ->

      desugar_and_dispatch casel

