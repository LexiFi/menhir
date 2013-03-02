(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/matching.ml,v 1.22.4.1 1998/08/31 17:21:23 fpottier Exp $ *)
(*

Compiling complex pattern matchings into simple ones. In complex matchings, basically anything is allowed; the only
rule is to not mix different kinds of structured patterns, like a pair and a record. Simple matchings can be either
a one-line VFun function, or a term built using the VMatcher/VConstantMatcher primitives. The first kind of simple
matchings does (safe) decomposition to gain access to structure components, while the second kind does a switch based
on the constructor tag.

Compilation into simple matchings is done in a typeless way, from source to source, so that the typechecker actually
sees simple matchings only.

There is no notion of "exhaustive matchings" here, because we have no user-defined variant types. Instead, the inferred
type closely reflects the capabilities of the code, and a matching which might be qualified of "not exhaustive" in
ML will receive a type such that it might not be applicable to any objects.

Unused clauses are not detected, because we might need to generate unused cases when compiling complex matchings
into simple ones, or when compiling Caml-Light code into code suitable for this compiler.

*)

open Errors
open Types
open Interpreter
open Aliasing

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The compiler raises this exception when encountering an ill-formed matching.

*)

exception MatchCompError of string

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Matchings are matrixes where each line contains a series of patterns and an action, to be executed if all patterns
are satisfied. Each pattern is matched against an object represented by a name; the list of objects forms the last
line.

*)

type pattern_matching =
  Matching of (pattern list * expression) list * string list

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Adding a new line at the top/bottom of a matching.

*)

let add_to_match_top (Matching(casel, objl)) new_case =
  Matching(new_case :: casel, objl)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A utility to create new names. Compiling complex matchings into simple ones requires binding new names to 
intermediate nodes. It is crucial to avoid any conflicts between these newly created names and the original names
of the program. A clean solution would be to use alpha-conversion where needed; the dirty, but quick, solution
adopted here is to generate sufficiently bizarre names.

*)

let new_name =
  let counter = ref 0
  in function () ->
    incr counter;
    "_x" ^ (string_of_int !counter)
;;

let is_internal_name name =
  try
    (String.get name 0 = '_') & (String.get name 1 = 'x')
  with Invalid_argument _ ->
    false
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Some empty sets.

*)

let empty_label_name_set =
  Set7.empty (fun (label, _) (label', _) -> Pervasives.compare label label')
;;

let empty_label_patternref_set =
  Set7.empty (fun (label, _) (label', _) -> Pervasives.compare label label')
;;

let empty_constant_set =
  Set7.empty Pervasives.compare
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Unaliasing is the process of removing PAlias and PVar patterns at toplevel. When such a pattern is found, a new let
binding (actually, coded using a beta-redex, for speed) is added in front of the corresponding action, and the pattern
can then be removed (PAlias) or changed into a wildcard (PVar). We need to know the name of the object to which the
matching is applied, in order to create the new bindings.

PVar patterns are turned into wildcards by binding the name to the object being matched inside the action expression.
The latter ("obj") will be typed in a fine way (see extensive comment in compile_constructor_matching below), so this
is fine.

This function also removes POr patterns at toplevel by distributing them.

*)

let unalias obj casel =

  let rec unalias = function
    (patl, action) :: rest -> (
      match patl with
	pat :: endpatl -> (
	  match pat with
	    PAlias(subpat, name) ->
	      let filtered_obj = apply_filter (create_pattern_filter subpat) (VVar obj) in
	      let action = VApp(VFun (PVar name, action), filtered_obj) in
	      unalias ((subpat :: endpatl, action) :: rest)
	  | PVar name ->
	      let action = VApp(VFun (PVar name, action), VVar obj) in
	      (PWildcard :: endpatl, action) :: (unalias rest)
	  | POr (pat1, pat2) ->
	      unalias ((pat1 :: endpatl, action) :: (pat2 :: endpatl, action) :: rest)
	  | _ ->
	      (patl, action) :: (unalias rest)
	)
      |	[] ->
	  raise (CantHappen "Matching.unalias expects at least one column.")
    )
  | [] ->
      [] in

  unalias casel
;;

let upper_left_pattern = function
    (pattern :: _, _) :: _ ->
      pattern
  | _ ->
      raise (CantHappen "Matching.upper_left_pattern expects at least one line and one column.")
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The bulk of the compiler's code. Each compile_ function takes a matching and returns its code, as an expression.
Generally speaking, which function is called depends on the upper left pattern; the function handles all lines that
have the same kind of pattern at the left. Wildcards are not treated in the same way by all functions; some functions
can filter them (e.g. products, refs) and some have to treat everything that follows as a block (e.g. constructors).

*)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Pairs.

*)

let rec compile_pair_matching casel obj objl =

  (* Generate names for each component of the pair. *)

  let left = new_name()
  and right = new_name() in

  (* Divide the pair into two columns. *)

  let rec divide = function
      ((PPair(pat1, pat2)) :: patl, action) :: rest ->
	add_to_match_top (divide rest) (pat1 :: pat2 :: patl, action)
    | (PWildcard :: patl, action) :: rest ->
	add_to_match_top (divide rest) (PWildcard :: PWildcard :: patl, action)
    | [] ->
	Matching([], left :: right :: objl)
    | _ ->
	raise (MatchCompError "Matching.compile_pair_matching expects pairs or wildcards only.") in

  (* Finish compiling. *)

  let code = compile_matching (divide casel) in

  (* Bind the components. *)

  VApp(VFun(PPair(PVar left, PVar right), code), VVar obj)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Constructors.

*)

and compile_constructor_matching casel obj objl =

  (* Given a specific constructor, keep only the lines that are relevant for it. *)

  let rec filter label = function
      ((PConstruct(label', pat)) :: patl, action) :: rest ->
	if label = label' then
	  (pat :: patl, action) :: (filter label rest)
	else
	  filter label rest
    | ((PWildcard :: _, _) as line) :: rest ->
	line :: (filter label rest)
    | [] ->
	[]
    | (_ :: _, _) :: _ ->
	raise (MatchCompError "Matching.compile_constructor_matching expects constructors or wildcards only.")
    | ([], _) :: _ ->
	raise (MatchCompError "No columns in Matching.compile_constructor_matching.") in

  (* Given a specific set of constructors, keep only the lines that are relevant for other constructors. *)

  let rec filter_out labels = function
      (((PConstruct(label, _)) :: _, _) as line) :: rest ->
	if Set7.mem label labels then
	  filter_out labels rest
	else
	  line :: (filter_out labels rest)
    | ((PWildcard :: _, _) as line) :: rest ->
	line :: (filter_out labels rest)
    | [] ->
	[]
    | (_ :: _, _) :: _ ->
	raise (MatchCompError "Matching.compile_constructor_matching expects constructors or wildcards only.")
    | ([], _) :: _ ->
	raise (MatchCompError "No columns in Matching.compile_constructor_matching.") in

  (* Gather the set of labels that appear before the first wildcard, and the remainder (the part of the matching
     below and including the first wildcard). *)

  let rec gather = function
      ((PConstruct(label, _)) :: _, _) :: rest ->
	let labels, remainder = gather rest in
	Set7.add label labels, remainder
    | ((PWildcard :: _, _) :: _) as remainder ->
	empty_label_set, remainder
    | [] ->
	empty_label_set, []
    | (_ :: _, _) :: _ ->
	raise (MatchCompError "Matching.compile_constructor_matching expects constructors or wildcards only.")
    | ([], _) :: _ ->
	raise (MatchCompError "No columns in Matching.compile_constructor_matching.") in

  let labels, remainder = gather casel in

  (* Remove the explicit labels from the remainder part. *)

  let remainder = filter_out labels remainder in

  (* Compile the remainder matching. *)

  let remainder_code =
    if remainder = [] then
      VVar "matchfail"
    else

      (* Here, we are writing the final handler function, which will be called when all explicit cases
	 have failed; this function is in charge of calling the remainder code. It would be correct to
	 write the line below with PWildcard as the function's pattern. This would mean that "obj" is
	 still bound to the first object being matched here. However, using PVar obj as below means
	 that we are binding "obj" as the argument of the handler function; in essence, this integrates
	 the knowledge that "obj" is rejected by all explicit cases. Thus, we get a better typing.

	 For instance, consider "function A -> B | x -> x". Without this kludge, x is bound to the
	 main function's argument and the typing is thus
	 -h -> +g | {
	   -h < +g, [ A: Pre unit; Pre 1 ] ;
	   [ B: Pre unit; Abs ], -h < +g
	 }
	 which is correct; however, with the kludge, we obtain
	 [ B: %e; A: Pre unit; %c ] -> [ B: %e; A: Abs; %c ] | {
	   Pre unit < %e
	 }
	 where it clearly appears that the function cannot return A. *)

      VFun (PVar obj, compile_matching (Matching(remainder, obj :: objl))) in

  (* Create a name to refer to the constructor's argument. The same name can be used for all constructors, since
     each one shall be compiled independently. *)

  let name = new_name() in

  (* We can now compile the matching. *)

  let matching_code = Set7.fold (fun label code ->

    (* For each explicit label, generate a sub-matching. *)

    let action = compile_matching (Matching(filter label casel, name :: objl)) in

    (* Generate the switch code, using the VMatcher primitives. *)

    VApp (VApp(VMatcher label, VFun(PVar name, action)), code)

  ) labels remainder_code in

  VApp(matching_code, VVar obj)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Refs. These are really a special case of constructors.

*)

and compile_ref_matching casel obj objl =

  (* Create a name to refer to the ref's argument. *)

  let name = new_name() in

  (* Generate a matching where the first column is now the ref's argument. *)

  let rec divide = function
      ((PRef pat) :: patl, action) :: rest ->
	add_to_match_top (divide rest) (pat :: patl, action)
    | (PWildcard :: patl, action) :: rest ->
	add_to_match_top (divide rest) (PWildcard :: patl, action)
    | [] ->
	Matching([], name :: objl)
    | _ ->
	raise (MatchCompError "Matching.compile_ref_matching expects refs or wildcards only.") in

  (* Compile the matching obtained above. *)

  let code = compile_matching (divide casel) in

  (* Generate the dereferencing code. *)

  VApp(VFun (PRef (PVar name), code), VVar obj)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Constants. This code is similar to the code that handles constructors, only slightly simpler since constants don't
have arguments.

*)

and compile_constant_matching casel obj objl =

  (* Given a specific constant, keep only the lines that are relevant for it. *)

  let rec filter constant = function
      ((PConstant constant') :: patl, action) :: rest ->
	if constant = constant' then
	  (patl, action) :: (filter constant rest)
	else
	  filter constant rest
    | (PWildcard :: patl, action) :: rest ->
	(patl, action) :: (filter constant rest)
    | [] ->
	[]
    | (_ :: _, _) :: _ ->
	raise (MatchCompError "Matching.compile_constant_matching expects constants or wildcards only.")
    | ([], _) :: _ ->
	raise (MatchCompError "No columns in Matching.compile_constant_matching.") in

  (* Given a specific set of constants, keep only the lines that are relevant for other constants. *)

  let rec filter_out constants = function
      (((PConstant constant) :: _, _) as line) :: rest ->
	if Set7.mem constant constants then
	  filter_out constants rest
	else
	  line :: (filter_out constants rest)
    | ((PWildcard :: _, _) as line) :: rest ->
	line :: (filter_out constants rest)
    | [] ->
	[]
    | (_ :: _, _) :: _ ->
	raise (MatchCompError "Matching.compile_constant_matching expects constants or wildcards only.")
    | ([], _) :: _ ->
	raise (MatchCompError "No columns in Matching.compile_constant_matching.") in

  (* Gather the set of constants that appear before the first wildcard, and the remainder (the part of the matching
     below and including the first wildcard). *)

  let rec gather = function
      ((PConstant constant) :: _, _) :: rest ->
	let constants, remainder = gather rest in
	Set7.add constant constants, remainder
    | ((PWildcard :: _, _) :: _) as remainder ->
	empty_constant_set, remainder
    | [] ->
	empty_constant_set, []
    | (_ :: _, _) :: _ ->
	raise (MatchCompError "Matching.compile_constant_matching expects constants or wildcards only.")
    | ([], _) :: _ ->
	raise (MatchCompError "No columns in Matching.compile_constant_matching.") in

  let constants, remainder = gather casel in

  (* Remove the explicit constants from the remainder part. *)

  let remainder = filter_out constants remainder in

  (* Optimize the case where all explicit constants are unit. In that case, if the remainder is non-empty, it
     is ignored. This might give a better typing or leave it unchanged; I'm not sure. *)

  let matching_code = if (Set7.mem ConstUnit constants) & (Set7.cardinality constants = 1) then

    VFun(PConstant ConstUnit, compile_matching (Matching(filter ConstUnit casel, objl)))

  else begin

    (* Compile the remainder matching. Constant types are "opaque", i.e. they are not expressive enough to
       allow correctly typechecking non-exhaustive matchings. Because of this, we have to use _matchaccept
       instead of matchfail as the final handler line. However, this is a hole in the type system.

       Note that using _matchaccept is necessary only if the pattern matching is not exhaustive. If it is,
       we can use a variant of matchfail (with the proper type). The difference is, the code will not appear
       to raise MatchFailure. However, determining whether a constant matching is exhaustive requires some
       knowledge about the builtin types which we don't want to build into the typechecker here. *)

    let remainder_code =
      if remainder = [] then
	VVar "_matchaccept"
      else
	VFun(PWildcard, compile_matching (Matching(remainder, obj :: objl))) in

    (* We can now compile the matching. *)

    Set7.fold (fun constant code ->

      (* For each explicit constant, generate a sub-matching. *)

      let action = compile_matching (Matching(filter constant casel, objl)) in

      (* Generate the switch code, using the VConstantMatcher primitives. *)

      VApp(VApp(VConstantMatcher constant, VFun(PConstant ConstUnit, action)), code)

    ) constants remainder_code

  end in

  VApp(matching_code, VVar obj)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Wildcards.

*)

and compile_wildcard_matching casel obj objl =

  (* Here, the pattern matching is divided into two blocks, first the right part of all lines that have
     wildcards, then the remainder (everything after the first non-wildcard in the left column). *)

  let rec divide = function
      (PWildcard :: patl, action) :: rest ->
	let first, remainder = divide rest in
	(patl, action) :: first, remainder
    | ((_ :: _, _) :: _) as remainder ->
	[], remainder
    | [] ->
	[], []
    | ([], _) :: _ ->
	raise (CantHappen "Matching.compile_wildcard_matching expects at least one column.") in

  (* Compile the first part, with an extra wildcard line at the end which executes the remainder part,
     if there is any. *)

  let first, remainder = divide casel in
  if remainder = [] then
    compile_matching (Matching(first, objl))
  else begin

    let rec add_failure_code = function
	[] ->
	  raise (CantHappen "Matching.compile_wildcard_matching expects at least one line.")
      | [(patl, action) as last_case] ->
	  let wildcards = List.map (fun _ -> PWildcard) patl in
	  [last_case; (wildcards, compile_matching (Matching(remainder, obj :: objl)))]
      | one :: more ->
	  one :: (add_failure_code more) in

    compile_matching (Matching(add_failure_code first, objl))

  end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Records.

*)

and gather_record_labels accu = function
    (patl, action) :: rest -> (
      match patl with
      	(PRecord lplist) :: _ ->
	  List.fold_right (fun (label, _) accu -> Set7.add label accu) lplist (gather_record_labels accu rest)
      | PWildcard :: _ ->
	  gather_record_labels accu rest
      | _ :: _ ->
	  raise (MatchCompError "Matching.gather_record_labels expects records or wildcards only.")
      | [] ->
	  raise (CantHappen "Matching.gather_record_labels expects a non-empty clause.")
    )
  | [] ->
      accu

and compile_record_matching casel obj objl =

  (* First, compute the set of record labels mentioned here. In each record pattern, missing labels will be
     considered as present, with a wildcard. *)

  let labels = gather_record_labels empty_label_set casel in

  (* Introduce a new name for each field. *)

  let names = Set7.fold (fun label names ->
    Set7.add (label, new_name()) names
  ) labels empty_label_name_set in

  (* Divide the first column into as many columns as there are labels. *)

  let rec divide = function

      ((PRecord lplist) :: patl, action) :: rest ->

	(* Start by associating a wildcard pattern to each label. *)

	let patterns = Set7.fold (fun label patterns ->
	  Set7.add (label, ref PWildcard) patterns
	) labels empty_label_patternref_set in

	(* Now, read the actual pattern associated to each present label. Note that we don't check against
	   duplicate labels. *)

	List.iter (function (label, pattern) ->
	  let _, patternref = Set7.memp (fun (label', _) -> Pervasives.compare label label') patterns in
	  patternref := pattern
	) lplist;

	(* Create the new pattern list. *)

	let patl = Set7.fold (fun (_, patternref) patl ->
	  !patternref :: patl
	) patterns patl in

	add_to_match_top (divide rest) (patl, action)

    | (PWildcard :: patl, action) :: rest ->

	(* Create the necessary number of wildcard patterns. *)

	let patl = Set7.fold (fun _ patl ->
	  PWildcard :: patl
	) labels patl in

	add_to_match_top (divide rest) (patl, action)

    | [] ->

	(* List the names in the same order as the patterns above. *)

	let objl = Set7.fold (fun (_, name) objl ->
	  name :: objl
	) names objl in

	Matching([], objl)

    | _ ->
	raise (MatchCompError "Matching.compile_record_matching expects records or wildcards only.") in

  (* Finish compiling. *)

  let code = compile_matching (divide casel) in

  (* Bind the components. *)

  let lplist = Set7.fold (fun (label, name) lplist ->
    (label, PVar name) :: lplist
  ) names [] in

  VApp(VFun(PRecord lplist, code), VVar obj)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Bringing everything together.

*)

and compile_matching matching = match matching with
    Matching([], _) ->

      (* Matchings with no lines are illegal. *)

      raise (CantHappen "At least one line is expected in Matching.compile_matching.")

  | Matching(([], action) :: _, _) ->

      (* A matching with no columns reduces to executing the action. We ignore any unused clauses. Note that this
	 has an impact on typing; for instance, the function entered by the user as (function _ -> 0 | _ -> true)
	 is compiled to (function _ -> 0) which has type 1 -> int. *)

      action

  | Matching(casel, obj :: objl) -> (

      (* Unalias the patterns. This removes all PAlias/POr/PVar patterns at toplevel. *)

      let casel = unalias obj casel in

      (* This matching has at least one line and one column. Determine what kind of matching it is by looking
	 for a structured pattern, then branch to the appropriate handler. *)

      match upper_left_pattern casel with
	PWildcard ->
	  compile_wildcard_matching casel obj objl
      |	PConstant _ ->
	  compile_constant_matching casel obj objl
      |	PPair _ ->
	  compile_pair_matching casel obj objl
      |	PRecord _ ->
	  compile_record_matching casel obj objl
      |	PConstruct _ ->
	  compile_constructor_matching casel obj objl
      |	PRef _ ->
	  compile_ref_matching casel obj objl
      | PVar _
      |	PAlias _
      |	POr _ ->
	  raise (CantHappen "Unexpected var/alias/or pattern in Matching.compile_matching.")

    )

  | Matching(casel, []) ->

      raise (CantHappen "At least one pattern, but no object in Matching.compile_matching.")

;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Making sure all lines of a matching have the same length.

*)

let rec matching_arity = function
    [] ->
      raise (CantHappen "Empty case list in Matching.matching_arity.")
  | [patl, _] ->
      List.length patl
  | (patl, _) :: rest ->
      let arity = matching_arity rest in
      if arity <> List.length patl then
	raise (MatchCompError "Not all cases have the same arity.");
      arity
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Detecting PAlias patterns in patterns.

*)

let rec is_simple_pat = function
    PWildcard
  | PVar _
  | PConstant _ ->
      true
  | PPair(pat1, pat2) ->
      (is_simple_pat pat1) & (is_simple_pat pat2)
  | PRecord lplist ->
      List.for_all (fun (_, pattern) -> is_simple_pat pattern) lplist
  | PConstruct (_, pattern) ->
      is_simple_pat pattern
  | PAlias _ ->
      false
  | POr (pat1, pat2) ->
      (is_simple_pat pat1) & (is_simple_pat pat2)
  | PRef pat ->
      is_simple_pat pat
;;

let is_simple_patl patl =
  List.for_all is_simple_pat patl
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Running the compiler over an entire abstract syntax tree. This eliminates all complex matchings (VGeneralFun) and
turns them into simple ones (VFun).

*)

let rec behavior = function
    VGeneralFun [(patl, action)] when is_simple_patl patl ->

      (* As an optimization, if the function has only one line and contains no PAlias patterns, then we can let
	 it go through unmodified; the typechecker knows how to handle it. We only have to curry it. *)

      List.fold_right (fun pat code -> VFun(pat, code)) patl (compile action)

  | VGeneralFun casel ->

      (* Compile the actions. *)

      let casel = compile_actions casel in

      (* Compute this matching's arity and make up as many names as necessary. *)

      let rec make_names = function
	  0 ->
	    []
	| n ->
	    (new_name()) :: (make_names (pred n)) in

      let objl = make_names (matching_arity casel) in

      (* Compile the code that applies the matching to these names. *)

      let code = compile_matching (Matching(casel, objl)) in

      (* Bind the names. This might introduce unnecessary eta expansions. *)

      let rec bind code = function
	  [] ->
	    code
	| name :: rest ->
	    VFun (PVar name, bind code rest) in

      bind code objl

  | VApp(VGeneralFun((([pat], _) :: _) as casel), VVar obj) ->

      (* As an optimization, we recognize this kind of beta redex and compile it directly, avoiding eta expansion. *)

      compile_matching (Matching(compile_actions casel, [obj]))

  | VGeneralTry(expr, bindings) ->

      (* Always add the re-raise clause at the end. *)

      let name = new_name() in
      let bindings = bindings @ [PVar name, VApp(VVar "raise", VVar name)] in

      (* Compile the expression and the actions. *)

      let expr = compile expr
      and bindings = compile_binding_actions bindings in

      (* Make up one name. *)
      
      let obj = new_name() in

      (* Compile the code that applies the matching to this name. *)

      let casel = List.map (function (pat, action) -> ([pat], action)) bindings in
      let code = compile_matching (Matching(casel, [obj])) in

      (* Bind the name. *)

      VTry (expr, VFun (PVar obj, code))

  | _ ->
      raise JustWalk

and compile term =
  walk behavior term

and compile_bindings pelist =
  List.fold_right (fun (pat, expr) pelist -> (pat, compile expr) :: pelist) pelist []

and compile_actions = function
    [] ->
      []
  | (patl, action) :: rest ->
      (patl, compile action) :: (compile_actions rest)

and compile_binding_actions = function (* Same as compile_actions, but recursive definitions aren't polymorphic *)
    [] ->
      []
  | (pat, action) :: rest ->
      (pat, compile action) :: (compile_binding_actions rest)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Eta-compressing the code to make it more readable while debugging. Note that eta-compression is allowed only when
the eta-redex was created by the pattern matching compiler; compressing arbitrary redexes would modify the program's
semantics.

It would be more elegant to avoid generating eta-redexes, rather than using an explicit pass to eliminate them
afterwards. However, the matching compiler's architecture doesn't make it easy.

*)

let rec eta expr =
  Interpreter.walk behavior expr

and eta_bindings bindings =
  Interpreter.walk_bindings behavior bindings

and behavior = function

  (* Detect generated eta-redexes. *)

  VFun(PVar name1, VApp(code, VVar name2))
    when (name1 = name2)
       & (is_internal_name name1)
       & (not (Set7.mem name1 (expression_vars code))) ->

    (* Eliminate them. *)
	 
    eta code

| _ ->
    raise JustWalk
;;

let compile_bindings bindings =
  eta_bindings (compile_bindings bindings)
;;
