(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/typechecking.ml,v 1.62.2.34 1999/06/21 12:21:01 francois Exp $ *)
(*

The type-checking engine. Its primary task is to generate constraints and to call the various simplification
functions.

*)

open Errors
open Rowsig
open Types
open Closure
open Interpreter
open Small
open Walk
open Env

exception BadSourceTerm of string

(* ----------------------------------------------------------------------------------------------------------------- *)

let empty_row_map = RowMap.empty()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Timing tests.

*)

let canonize_clock = Chrono.create();;
let reduction_clock = Chrono.create();;
let simplify_clock = Chrono.create();;
let normalize_clock = Chrono.create();;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Simplify groups all known simplifications. Everything must be attempted at let nodes, to avoid duplicating work.
On the other hand, at other nodes, it is allowable to perform fewer simplifications. Some rather inefficient
simplifications are applied only at toplevel.

Simplify returns a scheme which is garbage collected and carries correct signs.

To avoid performing needless simplifications, we keep track of what has been done. I would have thought this was
secondary, but the time difference is very measurable (25% of minimization time).

*)

type node_type =
    AnyNode
  | LetNode

let simplify bipolar node_type ((state, scheme) as argument) =

  Chrono.chrono simplify_clock (fun () ->
    match node_type with
      LetNode ->

	if state = AnyNode then begin

	  (* We are at a Let node, and the scheme isn't fully simplified. *)

	  (* Make sure no expanded variables remain. *)

	  Chrono.chrono normalize_clock (fun () -> Guards.normalize scheme);

	  (* If bipolar variables are allowed, we need to compute signs before doing canonization. Usually, we do it
	     only afterwards, since canonization opens new GC opportunities. *)

	  if bipolar then
	    Connexity.garbage scheme;

	  (* Canonization. *)

	  Chrono.chrono canonize_clock (fun () -> Canonize.canonize bipolar scheme);

	  (* Compute polarities and throw garbage away. *)

	  Connexity.garbage scheme;

	  (* Minimization. *)

	  let scheme = Chrono.chrono reduction_clock (fun () -> Equivalence.minimize scheme) in

	  (* Done. *)

	  (LetNode, scheme)

	end
	else
	  argument

    | AnyNode ->

        (* Compute polarities and throw garbage away. Normalization needn't be performed here. *)

	Connexity.garbage scheme;

        (AnyNode, scheme)

  )
;;

let stateless_simplify node_type scheme =
  let _, scheme = simplify false node_type (AnyNode, scheme) in
  scheme
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Creating explicit forks.

If these functions are called several times to compute a GLUB of n terms, we end up with n-1 forks instead of one
fork with n branches. However, one step of garbage collection is enough to cure this problem.

*)

let fork_GLB v1 v2 =
  let fork = fresh() in
  merge_link fork v1;
  merge_link fork v2;
  fork
;;

let fork_LUB v1 v2 =
  let fork = fresh() in
  merge_link v1 fork;
  merge_link v2 fork;
  fork
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This auxiliary function is used by build_domain and build_let_env to handle POr patterns; it computes an explicit
union of two closed variant types.

*)

let explicit_variant_LUB domain1 domain2 =
  match domain1, domain2 with
    TAbstract(stamp1, [leaf1]), TAbstract(stamp2, [leaf2])
    when (stamp1 = stamp2) & (stamp2 = Abstract.get_stamp "[]") -> (

      match (normalize_leaf leaf1, normalize_leaf leaf2) with
	TVar { link = Some { entries = entries1 }},
	TVar { link = Some { entries = entries2 }} ->

	  if not (RowMap.disjoint entries1 entries2) then
	    raise (InconsistentMatching "The same constructor cannot appear twice in a POr pattern.");
	  let entries = RowMap.union entries1 entries2 in
	  let remainder_span = Remainder (RowMap.domain entries) in
	  let remainder = sneg remainder_span TVAbsent in
	  make_tvariant entries remainder

      | _, _ ->
	  raise (InconsistentMatching "Arguments of a POr pattern in a VLet or VRec construct should be constructors.")
    )
  | _, _ ->
      raise (InconsistentMatching "Arguments of a POr pattern in a VLet or VRec construct should be constructors.")

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

build_domain creates the function's domain type after its body has been typechecked. It takes entries off the
inferred minimal context.

*)

let get_context_entry_by_name name context =

  (* This auxiliary function is somewhat tricky. Since contexts are indexed by identifiers, not names, a name
     should not be enough to conduct a context lookup. However, this function shall be called only when exiting
     lambda binders, so we know that the identifier we're looking for is the innermost identifier with this name;
     in other words, the greatest one. *)

  Set7.memp_greatest (fun ((name', _), _) -> Pervasives.compare name name') context
;;

let rec build_domain context body = function
  PWildcard ->
    context
| PVar name -> (
    try
      let (_, ty) as entry = get_context_entry_by_name name context in
      merge_link body ty;
      Set7.remove entry context
    with Not_found ->
      context
  )
| PConstant c ->
    merge_small_upper_bound body (atom_of_constant c);
    context
| PPair(pattern1, pattern2) ->
    let v1 = fresh()
    and v2 = fresh() in
    let context = build_domain context v1 pattern1 in
    let context = build_domain context v2 pattern2 in
    merge_small_upper_bound body (make_tproduct (TVar v1) (TVar v2));
    context
| PRecord lplist ->
    let entries, context = List.fold_left (fun (entries, context) (label, pattern) ->
      let v = fresh() in
      let context = build_domain context v pattern in
      let entries = RowMap.add label (neg (TRPresent (TVar v))) entries in
      entries, context
    ) (empty_row_map, context) lplist in
    let remainder = TVar (freshSK (Remainder (RowMap.domain entries)) KRecord) in
    merge_small_upper_bound body (make_trecord entries remainder);
    context
| PConstruct(label, pattern) ->
    let v = fresh() in
    let context = build_domain context v pattern in
    let entries = RowMap.add label (neg (TVPresent (TVar v))) empty_row_map in
    let remainder = sneg (Remainder (RowMap.domain entries)) TVAbsent in
    merge_small_upper_bound body (make_tvariant entries remainder);
    context
| PRef pattern ->
    let v = fresh() in
    let context = build_domain context v pattern in
    merge_small_upper_bound body (make_tref (TVar (fresh())) (TVar v));
    context

  (* Note: PAlias and POr patterns can never appear in a VFun construct because they are eliminated by the pattern
     matching compiler. However, they can appear in VRec constructs. It would be nice to cease supporting VRec
     constructs in favor of recursive VLets, but this isn't possible since Wright's rewriting algorithm turns some
     VLets back into VRecs. The other solution would be to replace VRec with a builtin primitive, but I again see
     two problems: the code for this primitive would be rather tricky to write and would duplicate the code that
     interprets recursive VLets; plus, VRecs would become applications of the primitive and would thus look expansive
     to Wright's algorithm unless we add special code. It does not seem to be worth the trouble.

     Finally, the pattern matching compiler does allow POr patterns to come through in one-line functions. *)

| PAlias (pattern, name) -> (
    let context = build_domain context body pattern in
    try
      let (_, domain2) as entry = get_context_entry_by_name name context in
      merge_link body domain2;
      Set7.remove entry context
    with Not_found ->
      context
  )
| POr (pat1, pat2) ->

    (* Make sure no variables are bound by this pattern. We need to pass two different fresh variables to the
       recursive calls, because we want to compute a disjunction, not a conjunction. It's a horrible hack. *)

    let original_context = context in
    let v1 = fresh()
    and v2 = fresh() in
    let context = build_domain context v1 pat1 in
    let context = build_domain context v2 pat2 in

    if Set7.compare original_context context <> 0 then
      raise (InconsistentMatching "POr patterns are not allowed to bind variables.");

    (* Compute an explicit LUB on the two domain types. The sub-patterns should be constructors (otherwise there
       is no reason to use a POr pattern), so the domain types should be variant types. *)

    merge_small_upper_bound body (explicit_variant_LUB v1.hi v2.hi);
    context
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function, a close cousin to build_domain, is used to handle complex let bindings. It builds a type for the
pattern exactly as build_domain; the difference is that instead of removing entries from an existing context, it
builds an environment by adding a fresh variable for each program variable.

Rather than returning an expected type, this function accepts a positive type variable, "body", and applies an upper
bound to it.

The second component is a list of (name, type variable) pairs, which tells which variable is associated with each
let-bound program variable.

*)

let empty_let_env =
  Set7.empty compare_labeled_pairs
;;

let rec build_let_env env body = function
    PVar name -> (
      let result = fresh() in
      try
	let _ = Set7.memp (compare_label_to_labeled_pair name) env in
	raise (InconsistentMatching ("Variable " ^ name ^ " appears twice in this pattern."))
      with Not_found ->
      	merge_link body result;
	Set7.add (name, result) env
    )
  | PAlias(pattern, name) ->
      let env = build_let_env env body pattern in
      build_let_env env body (PVar name)
  | PWildcard ->
      env
  | PConstant c ->
      merge_small_upper_bound body (atom_of_constant c);
      env
  | PPair(pattern1, pattern2) ->
      let v1 = fresh()
      and v2 = fresh() in
      let env = build_let_env env v1 pattern1 in
      let env = build_let_env env v2 pattern2 in
      merge_small_upper_bound body (make_tproduct (TVar v1) (TVar v2));
      env
  | PRecord lplist ->
      let entries, env = List.fold_left (fun (entries, env) (label, pattern) ->
	let v = fresh() in
	let env = build_let_env env v pattern in
	let entries = RowMap.add label (neg (TRPresent (TVar v))) entries in
	entries, env
      ) (empty_row_map, env) lplist in
      let remainder = TVar (freshSK (Remainder (RowMap.domain entries)) KRecord) in
      merge_small_upper_bound body (make_trecord entries remainder);
      env
  | PConstruct(label, pattern) ->
      let v = fresh() in
      let env = build_let_env env v pattern in
      let entries = RowMap.add label (neg (TVPresent (TVar v))) empty_row_map in
      let remainder = sneg (Remainder (RowMap.domain entries)) TVAbsent in
      merge_small_upper_bound body (make_tvariant entries remainder);
      env
  | PRef pattern ->
      let v = fresh() in
      let env = build_let_env env v pattern in
      merge_small_upper_bound body (make_tref (TVar (fresh())) (TVar v));
      env
  | POr(pat1, pat2) ->

      (* POr patterns are not allowed to bind variables, so they are never necessary, at least theoretically
	 speaking, in a let binding; there is always a way to perform the check in some other way, by using
	 a beta-redex. *)

      raise (InconsistentMatching "POr patterns are not allowed in let bindings.")

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Merging several branches of the inference tree requires that all inferred type schemes share the same
quantifiers, environment and constraint set. This is achieved by applying a subtyping rule to each of
those schemes.

One can show that it is sound and complete to take the union of the quantifiers, of the constraint sets,
and the intersection of the contexts (computed in a way similar to records).

We don't want to actually generate an intersection type; generating a fork is better because forks can be
eliminated more easily by the garbage collection and substitution algorithms. It is also made mandatory by our
requirement of generating small terms.

If this function is used to compute the intersection of n contexts, we will generate n-1 fresh variables, but
one step of garbage collection will be enough to make all of them but the last one disappear.

*)

let context_intersection context1 context2 =
  Set7.smart_union (fun (identifier, ty1) (_, ty2) ->
    (identifier, fork_GLB ty1 ty2)
  ) context1 context2
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Type schemes for the VConstantMatcher primitives are computed at startup to save time.

*)

let constant_matcher_type atom =

  let alphap = fresh() and alpham = fresh() in
  merge_link alpham alphap;
  let alphap = TVar alphap and alpham = TVar alpham in
  let effect1 = fresh()
  and effect2 = fresh()
  and effect3 = fresh() in
  merge_link effect1 effect3;
  merge_link effect2 effect3;
  let first = make_tarrow (pos atom_unit) (TVar effect1) alpham in
  let second = make_tarrow (pos atom) (TVar effect2) alpham in
  let third = make_tarrow (neg atom) (TVar effect3) alphap in

  let body = make_tarrow
      (neg first)
      (pos TBottom)
      (pos (make_tarrow (neg second) (pos TBottom) (pos third))) in
  let scheme = Scheme(empty_context, vpos body, vpos TBottom) in
  stateless_simplify LetNode scheme
;;

let int_matcher_type = constant_matcher_type atom_int
and unit_matcher_type = constant_matcher_type atom_unit
and bool_matcher_type = constant_matcher_type atom_bool
and float_matcher_type = constant_matcher_type atom_float
and char_matcher_type = constant_matcher_type atom_char
and string_matcher_type = constant_matcher_type atom_string

let constant_matcher_type constant =
  let scheme = match constant with
    ConstInt _ -> int_matcher_type
  | ConstUnit -> unit_matcher_type
  | ConstBool _ -> bool_matcher_type
  | ConstFloat _ -> float_matcher_type
  | ConstChar _ -> char_matcher_type
  | ConstString _ -> string_matcher_type
  in copy_scheme scheme
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The code that builds environment for let bindings is factored out because it is also used for toplevel lets.

The two following functions do the same job, in the non-recursive and recursive cases, respectively. They analyze a
list of bindings and return a new environment chunk, plus the contexts and the effects generated by the expressions.

The contexts and effects cannot be used until after the bindings have been removed from the environment, because they
share variables with the chunk.

*)

let rec create_nonrecursive_let_bindings node_type env names pelist =

  (* Analyze each clause. *)

  List.fold_right (fun (pattern, expr) (chunk, contexts, effects) ->

    (* If the pattern is a single variable, everything is simple. *)

    let chunk, contexts, effects = match pattern with
      PVar name ->

	(* Infer the expression's type, simplify. Drop the effect. Simplifying before dropping the effect means that
	   we might miss a tiny minimization opportunity, but it should be better, since the inferred scheme's state
	   allows skipping simplifications. Besides, if we didn't, handling the effect would be more difficult,
	   probably requiring a copy, as in the optimized recursive case. *)

	let _, Scheme(clause_context, clause_body, clause_effect) = simplify false node_type (infer env expr) in
	(Let(name, Scheme(clause_context, clause_body, fresh()))) :: chunk,
	clause_context :: contexts,
	clause_effect :: effects

    | _ ->
	
	(* In my tests, this code is used very rarely (e.g. 3 times on a 10000 lines program), so it's hard to
	   tell whether simplifying the clause's scheme helps. Let's just not do it. The reason why I used to do
	   it is that if the pattern has variables, the work is shared. *)

	let _, Scheme(clause_context, clause_body, clause_effect) = infer env expr in

        (* Analyze the pattern and associate a type variable to each bound program variable. *)

	let clause_let_env = build_let_env empty_let_env clause_body pattern in

        (* Create a type scheme for each variable. *)

	Set7.fold (fun (name, name_type) chunk ->

  	  (* An extra fresh variable has to be created to avoid creating variables signed both plus and minus.

	     Besides, copying the scheme prior to simplifying it is necessary, because it shares variables with
	     the other schemes defined by this clause, and simplification is destructive. *)

	  let result = fresh() in
	  merge_link name_type result;
	  let scheme = copy_scheme (Scheme(clause_context, result, fresh())) in
	  let scheme = stateless_simplify node_type scheme in
	  (Let(name, scheme)) :: chunk

      ) clause_let_env chunk,
      clause_context :: contexts,
      clause_effect :: effects in

    chunk, contexts, effects

  ) pelist ([], [], [])

and create_one_recursive_let_binding node_type env name expr =

  (* Since we have a recursive definition, we must typecheck the expression in an environment extended with a new
     lambda binder. *)

  let identifier = new_identifier name in
  let env' = (Lambda identifier) :: env in

  (* We can now typecheck the expression. Note that we do call simplify here, because it is faster, even though
     we have a single clause. The explanation is probably that tying the recursive knot causes a lot of closure
     computation, so simplifying first helps. *)

  let _, Scheme(context, body, effect) = simplify false node_type (infer env' expr) in

  (* Create the recursive fixpoint by removing the variable's context entry and adding the constraint that
     the result type is less than the required type. *)

  let context =
    try
      let (_, required_type) as entry = Set7.memp (compare_label_to_labeled_pair identifier) context in
      merge_link body required_type;
      Set7.remove entry context
    with Not_found ->
      context in

  (* Create the binding. Copying the scheme is necessary here, because we would otherwise damage 'context',
     which is linked to 'effect'. If there was no effect, we could avoid the copy and return the simplified
     context. *)
      
  let scheme = copy_scheme (Scheme(context, body, fresh())) in
  let scheme = stateless_simplify node_type scheme in

  [Let(name, scheme)], [context], [effect]

and create_recursive_let_bindings node_type env names pelist =

  (* Since we have a recursive definition, we must typecheck the clauses in an environment extended with new
     lambda binders. Compute it. *)

  let lambda_env = Set7.fold (fun name accu -> (Lambda (new_identifier name)) :: accu) names [] in

  (* We can now typecheck each of the clauses. Calling simplify is optional here, but is significantly faster,
     because this work shall be shared between the variables, and probably also because tying the recursive knot
     causes a lot of closure computation, which is reduced by simplifying beforehand. *)

  let env' = lambda_env @ env in
  let schemes = List.fold_right (fun (_, expr) schemes ->
    let _, scheme = simplify false LetNode (infer env' expr) in
    scheme :: schemes
  ) pelist [] in

  (* Merge their contexts and effects into one. *)

  let context, effect = List.fold_right (fun (Scheme(scheme_context, _, scheme_effect)) (context, effect) ->
    context_intersection scheme_context context, fork_LUB scheme_effect effect
  ) schemes (empty_context, fresh()) in

  (* For each clause, do the following: *)

  let context, let_env = Standard.list_fold_right2 (
    fun (pattern, _) (Scheme(_, actual_type, _)) (context, let_env) ->

    (* Compute the pattern's expected type and add the constraint that the actual type should be less than
       the expected type. Intuitively speaking, this gives a meaning to the type variables found in
       [clause_let_env]. *)

    let clause_let_env = build_let_env empty_let_env actual_type pattern in

    (* Create the recursive fixpoint by removing each variable's context entry and adding the constraint that
       the result type is less than the required type. *)

    let context = Set7.fold (fun (name, name_actual_type) context ->
      try
	let identifier = lookup_lambda_name name lambda_env in
	let (_, required_type) as entry = Set7.memp (compare_label_to_labeled_pair identifier) context in
	merge_link name_actual_type required_type;
	Set7.remove entry context
      with Not_found ->
	context
    ) clause_let_env context in

    (* Return the modified context, plus the global let environment. *)

    context, Set7.union clause_let_env let_env

  ) pelist schemes (context, empty_let_env) in

  (* We shall now simplify the constraint graph thus obtained. We want to call simplify once, not once per program
     variable defined by the let rec construct. It is faster, since the work is shared. (For instance, on the Format
     module, this yields a 25% gain on simplification time.)

     To do this, we have to create an imaginary record containing all of these variables. This might be slightly
     inefficient. In a production version, one might want to optimize this e.g. by adding positive contexts to type
     schemes. *)

  let entries = Set7.fold (fun (name, name_actual_type) entries ->

    let result = fresh() in
    merge_link name_actual_type result;

    RowMap.add name (pos (TRPresent (TVar result))) entries

  ) let_env empty_row_map in
  let signature = RowMap.domain entries in

  let remainder = spos (Remainder signature) TRMissing in
  let scheme = Scheme(
    context,
    vpos (make_trecord entries remainder),
    fresh()) in

  (* Copy the scheme prior to simplification, to avoid damaging the link between the context and the effect.
     Then, simplify it. *)

  let scheme = stateless_simplify node_type (copy_scheme scheme) in

  (* We can now build the let bindings. *)

  let chunk = RowSet.fold (fun name chunk ->

    (* Simulate a record selection to extract the proper field out of the above scheme. *)

    let Scheme(context, body, effect) = copy_scheme scheme in
    
    let assumption = fresh()
    and result = fresh() in
    merge_link assumption result;

    let expected_field_type = TRPresent (TVar assumption) in
    let entries = RowMap.add name (neg expected_field_type) empty_row_map in
    let remainder = TVar (freshSK (Remainder (RowMap.domain entries)) KRecord) in
    merge_small_upper_bound body (make_trecord entries remainder);

    let scheme = Scheme(context, result, effect) in

    (* No canonization or minimization should be necessary. Simply run garbage collection, to eliminate the extra
       constraints created by our encoding into records. *)

    Chrono.chrono simplify_clock (fun () ->
      Connexity.garbage scheme
    );

    (Let(name, scheme)) :: chunk

  ) signature [] in

  chunk, [context], [effect]

and bindings_names = function
    [] ->
      empty_label_set
  | (pattern, _) :: rest ->
      let names1 = pattern_vars pattern
      and names2 = bindings_names rest in
      try
	let name = Set7.choose (Set7.inter names1 names2) in
	raise (InconsistentMatching ("Variable " ^ name ^ " appears in two distinct clauses in this let definition."))
      with Not_found ->
	Set7.union names1 names2

and create_let_bindings node_type recursive env pelist =

  (* Gather names and make sure they are unique. *)

  let names = bindings_names pelist in

  (* Typecheck. *)

  match (recursive, pelist) with
    false, _ ->
      create_nonrecursive_let_bindings node_type env names pelist
  | true, [PVar name, expr] ->
      create_one_recursive_let_binding node_type env name expr
  | true, _ ->
      create_recursive_let_bindings node_type env names pelist

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Main type inference function.

The environment contains only let bindings; lambda bindings are kept implicit. Actually lambda bindings do
appear inside the environment because they are needed to disambiguate variable names, but only their position
within the environment matters and they have no associated types.

When encoutering unbound variables, it would be correct to add them to the context instead of declaring them
invalid. This would allow type-checking open terms. However, there is a problem with identifiers: if we
generate a new identifier each time we see an unbound name, then different occurrences of the same name will
end up as different identifiers, thus rendering unbound names unusable.

A note about variables signed both positively and negatively. First, note that we can theoretically avoid them
easily, because given any type scheme, we can rewrite it into another scheme with no such variables, with a size
loss of at most a factor of three (this is a simple application of canonization to the trivial case where unions
and intersections are allowed to have one argument). Reciprocally, we can go back from this expanded form to the
original form by replacing negative [resp. positive] variables with their unique upper [resp. lower] bound. So,
these variables are theoretically not a problem. Further, we note that the equivalence algorithm doesn't like
these variables, because each of them must be isolated in its own equivalence class, thus making reduction less
effective. So, we choose to avoid them in practice. Since we have proved that after a variable has been
introduced, its sign decreases as type inference goes on, it is enough to make sure that the type inference
rules never introduce any variables which are signed both positively and negatively. We introduce fresh
variables in two places, the (Var) and the (App) rules; each of these rules must be modified to introduce two
variables instead of one and link them with a single constraint. Several other rules, which are usually
theoretically described as special cases of the (App) rule, are also affected.

A note about when to call the GC. We want to call it after application nodes, because an application connects
a negative entry point to a positive one and can cause a large part of the constraint graph to become unreachable.
The same holds for record access and fixpoint constructs, which can be viewed as special cases of the application
rule. However, lambda abstraction and pair/record/datatype construction do not change the entry
points into the constraint sets, so they do not offer any significant GC opportunities.

*)

and infer env = function

    VBottom ->

      (* This constant, which is never generated by the parser, is used whenever we need an expression of type
	 internally. In particular, the typing of VRaise constructs uses it. *)

      let tv = fresh() in
      LetNode, Scheme(empty_context, tv, tv)

  | VConstant c ->

      LetNode, Scheme(empty_context, vpos (atom_of_constant c), fresh())

  | VVar name -> (

      try

      	match lookup_name name env with
	  Lambda identifier ->

	    (* If we want to avoid generating variables which are both positive and negative, then we must generate
	       two fresh variables instead of one here. *)

	    let assumption = fresh()
	    and result =     fresh()
	    and effect =     fresh() in
	    merge_link assumption result;
	    let context = Set7.add (identifier, assumption) empty_context in
	    LetNode, Scheme(context, result, effect)

	| Let (_, scheme) ->
	    LetNode, copy_scheme scheme

      with Not_found ->
      	raise (BadSourceTerm ("Variable " ^ name ^ " is unbound."))

    )
  | VFun (pattern, expr) ->

      let env = Set7.fold (fun name env ->  (Lambda(new_identifier name)) :: env) (pattern_vars pattern) env in
      let state, Scheme(context, range, effect) = infer env expr in
      let domain = fresh() in
      let context = build_domain context domain pattern in
      AnyNode, Scheme(context, vpos (make_tarrow (TVar domain) (TVar effect) (TVar range)), fresh())
	
  | VApp (expr1, expr2) -> (

      let _, Scheme(context1, ty1, effect1) = infer env expr1
      and _, Scheme(context2, ty2, effect2) = infer env expr2 in

      let effectp = fork_LUB effect1 effect2 in
      let context = context_intersection context1 context2 in

      let resultm = fresh() and resultp = fresh() in
      merge_link resultm resultp;
      let effectm = fresh() in
      merge_link effectm effectp;

      merge_small_upper_bound ty1 (make_tarrow (TVar ty2) (TVar effectm) (TVar resultm));

      simplify false AnyNode (AnyNode, Scheme(context, resultp, effectp))

  )
  | VLet (recursive, pelist, body) ->

      (* Analyze each clause and create the bindings. *)

      let chunk, contexts1, effects1 = create_let_bindings LetNode recursive env pelist in

      (* Typecheck the body in the augmented environment. *)

      let _, Scheme(context2, ty2, effect2) = infer (chunk @ env) body in

      (* Copy the context (and, implicitly, the constraints) required by the bound expressions. *)

      let context = List.fold_right context_intersection contexts1 context2
      and effect = List.fold_right fork_LUB effects1 effect2 in

      simplify false AnyNode (AnyNode, Scheme(context, ty2, effect))

  | VPair (expr1, expr2) ->

      let _, Scheme(context1, ty1, effect1) = infer env expr1
      and _, Scheme(context2, ty2, effect2) = infer env expr2 in
      let context = context_intersection context1 context2 in
      let effect = fork_LUB effect1 effect2 in
      simplify false AnyNode (AnyNode, Scheme(context, vpos (make_tproduct (TVar ty1) (TVar ty2)), effect))

  | VVector elist ->

      (* Create a type scheme for the (polymorphic) empty vector. *)

      let alpha = fresh()
      and beta = fresh()
      and gamma = fresh() in
      merge_link alpha beta;

      (* Add a type constraint for each element of the vector. *)

      let context = List.fold_right (fun expr context ->
	let _, Scheme(elem_context, elem_body, elem_effect) = infer env expr in
	merge_link elem_body beta;
	merge_link elem_effect gamma;
	context_intersection context elem_context
      ) elist empty_context in
      
      (AnyNode, Scheme(context, vpos (make_tvect (TVar alpha) (TVar beta)), gamma))

  | VRecord lxlist -> (

      try
	let culprit = find_duplicate_label lxlist in
	let message = "Field " ^ culprit ^ " appears twice in record!" in
	raise (BadSourceTerm message)
      with Not_found ->

	let effect = fresh() in
	
      	let context, entries =
	  List.fold_left (fun (context, entries) (label, expr) ->
          let _, Scheme(partial_context, ty, partial_effect) = infer env expr in
	  merge_link partial_effect effect;
	  let context = context_intersection partial_context context in
	  let entries = RowMap.add label (pos (TRPresent (TVar ty))) entries in
      	  context, entries
        ) (empty_context, empty_row_map) lxlist in

	let remainder = spos (Remainder (RowMap.domain entries)) TRMissing in
      	AnyNode, Scheme(context, vpos (make_trecord entries remainder), effect)

  )
  | VRecordAccess (expr, label) ->

      let _, Scheme(context, body, effect) = infer env expr in

      let assumption = fresh()
      and result = fresh() in
      merge_link assumption result;

      let entries = RowMap.add label (neg (TRPresent (TVar assumption))) empty_row_map in
      let remainder = TVar (freshSK (Remainder (RowMap.domain entries)) KRecord) in
      merge_small_upper_bound body (make_trecord entries remainder);

      simplify false AnyNode (AnyNode, Scheme(context, result, effect))

  | VRec (pattern, expr) ->

      (* This rule adds a constraint between the inferred type of the body, which was signed positively, and
         the context entry, which was signed negatively. Unless I'm mistaken, we can prove that this does not
	 cause any variable to be signed both positively and negatively. The proof should be similar to the
	 case of the (App) rule, but simpler. So, we have nothing special to do. *)

      let env = Set7.fold (fun name env ->  (Lambda(new_identifier name)) :: env) (pattern_vars pattern) env in
      let _, Scheme(context, body, effect) = infer env expr in
      let context = build_domain context body pattern in
      simplify false AnyNode (AnyNode, Scheme(context, body, effect))

  | VConstruct (label, expr) ->

      let _, Scheme(context, body, effect) = infer env expr in
      let entries = RowMap.add label (pos (TVPresent (TVar body))) empty_row_map in
      let remainder = TVar (freshSK (Remainder (RowMap.domain entries)) KVariant) in
      AnyNode, Scheme(context, vpos (make_tvariant entries remainder), effect)

  | VIf (expr0, expr1, expr2) ->

      let _, Scheme(context0, body0, effect0) = infer env expr0
      and _, Scheme(context1, body1, effect1) = infer env expr1
      and _, Scheme(context2, body2, effect2) = infer env expr2 in

      let context = context_intersection context0 context1 in
      let context = context_intersection context context2 in
      merge_small_upper_bound body0 atom_bool;

      let effect = fresh() in
      merge_link effect0 effect;
      merge_link effect1 effect;
      merge_link effect2 effect;

      simplify false AnyNode (AnyNode, Scheme(context, fork_LUB body1 body2, effect))

  | VCast (expr, scheme_expr) ->
      
      let _, scheme = simplify false LetNode (infer env expr) in

      (* Read the scheme typed in by the user. Note that there are lambda-bound variables in the environment
	 (either explicitly bound by the user, or generated by expansive let definitions) so the user is allowed
	 to type in a type scheme which has references to these identifiers. I don't know how useful this might
	 be in practice, but the functionality is there.

	 Actually, in practice, the user doesn't know how expansive lets are handled, so he doesn't know which
	 context entries should be added when making a cast. This makes casts much less useful than they could
	 be. *)

      let scheme' = Type_expr.scheme_expression_to_scheme env scheme_expr in

      (* Eliminate its bipolar variables and simplify it. *)

      let (_, scheme') as result = simplify true LetNode (AnyNode, scheme') in

      (* Make sure it is less general than the inferred scheme. *)

      Subtyping.subsumption scheme scheme';

      result

  | VUsage (expr, ctype_expr) ->

      let _, ((Scheme(_, body, effect)) as scheme) = infer env expr in

      (* Read the constrained type typed in by the user. This is a constrained type, not a type scheme; in
	 particular, it cannot carry a context. The effect of the VUsage construct is to simulate a use
	 of the given expression with the given constrained type. This is different from the VCast construct
	 above, which asserts that the given expression admits the given type scheme. The type of the VCast
	 construct, if successful, is the supplied scheme; the type of the VUsage construct is the original
	 type, plus the new constraints. *)

      let Ctype(body', effect') = Type_expr.ctype_expression_to_ctype env ctype_expr in

      (* Add the new constraint to the type scheme. *)

      merge_link body body';
      merge_link effect effect';

      (* Simplify the type scheme thus obtained. It might have bipolar variables, so we eliminate them. *)

      simplify true LetNode (AnyNode, scheme)

  (*

  Notes on typing exceptions.

  This is slightly tricky, because we need a way to connect the types of expressions that are raised to the
  types of exceptions that are caught. For instance, in the following code:

    try
      raise (F 1)
    with
      F x -> x

  we need a way to tell that the type of x (and hence, the type of the whole expression) is int, even though the
  type of (raise (F 1)) is bottom.

  There are several ways to solve this problem. The solution adopted by ML is to declare exception types
  explicitly. There are other solutions where no explicit declarations are required. Here, we
  perform an interesting analysis to determine the types of the exceptions potentially raised by each
  expression; this involves modifications to the type system, specifically annotating each arrow type with
  an exception type and each type scheme with an exception type. See for instance Guzman & Suarez, ACM SIGPLAN
  Workshop on ML and its applications, 1994.

  raise is encoded as a primitive function of type ('a -> 0 raises 'a). try can't be treated in such a way,
  unfortunately, but it is interesting to note that we can define

    let my_try action handler = try action() with x -> handler x;;

  and we obtain

    my_try: (unit -> 'a raises 'e) -> ('e -> 'a raises 'f) -> 'a raises 'f

  *)

  | VTry (expr, handler) ->

      (* Typecheck the body and the handler separately. *)

      let _, Scheme(context1, body1, effect1) = infer env expr
      and _, Scheme(context2, body2, effect2) = infer env handler in

      (* The semantics state that any exception that comes out of the body is sent to the handler. Thus, as far
	 as types are concerned, we apply the type of the handler to the body's effect. *)

      let effectm = fresh() and effectp = fresh() in
      merge_link effectm effectp;
      let resultm = fresh() and resultp = fresh() in
      merge_link resultm resultp;

      merge_small_upper_bound body2 (make_tarrow (TVar effect1) (TVar effectm) (TVar resultm));
      merge_link body1 resultp;
      merge_link effect2 effectp;

      let context = context_intersection context1 context2 in

      simplify false AnyNode (AnyNode, Scheme(context, resultp, effectp))

  (* The matcher primitives have well-known, fixed types. *)

  | VMatcher label ->

      (* Building a type scheme from scratch every time like this might be a slight waste of time. It would be nice
	 to build it once and then copy it using copy_scheme as needed, but this doesn't work because the label can
	 vary. Anyway, a quick timing test shows that the code below uses 1% of the total typechecking time, which
	 seems acceptable.

	 In the absence of guarded constraints, the type of (VMatcher "A") is

	    ('a -> 'c raises 'e)
         -> ([ A : Abs; 'r ] -> 'c raises 'e)
	 -> ([ A : Pre 'a; 'r ] -> 'c raises 'e)

	 only it is somewhat more complex in practice since bipolar variables are forbidden.

	 When guarded constraints are allowed, we are able to give it a finer type, as follows:
	    ('a -'f-> 'c)
         -> ([ A : Abs; 'r ] -'e-> 'd)
         -> ([ A : 'p; 'r ] -'e-> 'd)
         with the following constraints:
	    'p < Pre 'a             (as before)
            Pre < 'p ? 'c < 'd      (the output of the first function shall not be used unless A is present)
            Pre < 'p ? 'f < 'e      (the effect of the first function shell not appear unless A is present) *)

      let second_entries = RowMap.add label (pos TVAbsent) empty_row_map in

      let alphap = fresh() and alpham = fresh() in
      merge_link alpham alphap;
      let betap = fresh() and betam = fresh() in
      merge_link betam betap;
      let span = Remainder (RowMap.domain second_entries) in
      let gammap = freshSK span KVariant and gammam = freshSK span KVariant in
      merge_link gammam gammap;
      let effectm = fresh() and effectp = fresh() in
      merge_link effectm effectp;

      let phi = vneg (TVPresent (TVar alpham)) in

      let deltam, epsilonm =
	if !Flags.noguards then betam, effectm
	else begin
	  let deltam = fresh() and deltap = fresh() in
	  merge_link deltam deltap;
	  let epsilonm = fresh() and epsilonp = fresh() in
	  merge_link epsilonm epsilonp;
	  phi.guards <- Set7.add (HVPresent, epsilonp, effectm) (Set7.add (HVPresent, deltap, betam) empty_guard_set);
	  deltam, epsilonm
	end in
      
      let first = make_tarrow (TVar alphap) (TVar epsilonm) (TVar deltam) in
      let second = make_tarrow (pos (make_tvariant second_entries (TVar gammap))) (TVar effectm) (TVar betam) in
      let third_entries = RowMap.add label (TVar phi) empty_row_map in
      let third = make_tarrow (neg (make_tvariant third_entries (TVar gammam))) (TVar effectp) (TVar betap) in

      let vbottom = fresh() in
      let bottom = TVar vbottom in
      
      let body = make_tarrow (neg first) bottom (pos (make_tarrow (neg second) bottom (pos third))) in
      LetNode, Scheme(empty_context, vpos body, vbottom)

  | VConstantMatcher constant ->
      LetNode, constant_matcher_type constant

  | VRecordUpdate label ->

      (* This is the general record update primitive. VRecordUpdate "l" has type
	   { l : May 1; 'r } -> 'a -> { l : Pre 'a; 'r }
         Note that this allows the field "l" in the input record to be indifferently absent or present. *)

      let maybetop = neg (TRMaybe (TVar (fresh()))) in
      let first_entries = RowMap.add label maybetop empty_row_map in

      let alphap = fresh() and alpham = fresh() in
      merge_link alpham alphap;
      let span = Remainder (RowMap.domain first_entries) in
      let rhop = freshSK span KRecord and rhom = freshSK span KRecord in
      merge_link rhom rhop;
      let first = make_trecord first_entries (TVar rhom) in
      let third = make_trecord (RowMap.add label (pos (TRPresent (TVar alphap))) empty_row_map) (TVar rhop) in
      let vbottom = fresh() in
      let bottom = TVar vbottom in
      let body = make_tarrow (neg first) bottom (pos (make_tarrow (TVar alpham) bottom (pos third))) in
      LetNode, Scheme(empty_context, vpos body, vbottom)

  | VAsymRecordConcat ->

      (* This is the asymmetric record concatenation primitive. Its type is
	 
	   { rho1 } -> { rho2 } -> { rho3 }

	 with the following constraints:

	   rho2 < May 'a
	   Pre < rho2 ? Pre 'a < rho3  (If the field in the second record is present, it is used.)
	   Mis < rho2 ? rho1 < rho3    (If the field in the second record is missing, the first record is used.)

	 Taking the mono-polarity invariant into account, we must in fact write

	   { rho1m } -> { rho2m } -> { rho3p }

	 where

	   rho2m < May alpham
	   Pre alphap < rho2p
	   Mis < rho2m ? rho1p < rho3m
           Pre < rho2m ? rho2p < rho3m
           alpham < alphap
           rho1m < rho1p
	   rho3m < rho3p

	 Note that alpham and alphap are row variables of kind KRegular, a feature unused elsewhere.

	 *)

      (* TEMPORARY should move this type scheme to Builtin *)

      if !Flags.noguards then
	raise (CantHappen "Record concatenation (@) cannot be typed when -noguards in on.");

      let span = Remainder RowSet.empty in

      let alpham = freshSK span KRegular and alphap = freshSK span KRegular in merge_link alpham alphap;
      let rho1m = freshSK span KRecord and rho1p = freshSK span KRecord in merge_link rho1m rho1p;
      let rho2m = freshSK span KRecord and rho2p = freshSK span KRecord in
      let rho3m = freshSK span KRecord and rho3p = freshSK span KRecord in merge_link rho3m rho3p;

      merge_small_upper_bound rho2m (TRMaybe (TVar alpham));
      merge_small_lower_bound (TRPresent (TVar alphap)) rho2p;

      rho2m.guards <- Set7.add (HRMissing, rho1p, rho3m) (Set7.add (HRPresent, rho2p, rho3m) empty_guard_set);

      let r1 = make_trecord empty_row_map (TVar rho1m)
      and r2 = make_trecord empty_row_map (TVar rho2m)
      and r3 = make_trecord empty_row_map (TVar rho3p) in

      let vbottom = fresh() in
      let bottom = TVar vbottom in

      let body = make_tarrow (neg r1) bottom (pos (make_tarrow (neg r2) bottom (pos r3))) in

      LetNode, Scheme(empty_context, vpos body, vbottom)

  | VSymRecordConcat ->

      (* This is the symmetric record concatenation primitive. Its type is
	 
	   { rho1 } -> { rho2 } -> { rho3 }

	 with the following constraints:

	   Mis < rho1 ? rho2 < rho3    (If the field in the first record is missing, the second record is used.)
           Pre < rho1 ? rho2 < Mis     (If the field in the first record is present, the second one must be missing.)
           Mis < rho2 ? rho1 < rho3
           Pre < rho2 ? rho1 < Mis

      *)

      (* TEMPORARY should move this type scheme to Builtin *)

      if !Flags.noguards then
	raise (CantHappen "Record concatenation (@@) cannot be typed when -noguards in on.");

      let span = Remainder RowSet.empty in

      let rho1m = freshSK span KRecord and rho1p = freshSK span KRecord in merge_link rho1m rho1p;
      let rho2m = freshSK span KRecord and rho2p = freshSK span KRecord in merge_link rho2m rho2p;
      let rho3m = freshSK span KRecord and rho3p = freshSK span KRecord in merge_link rho3m rho3p;

      (* TEMPORARY Because of the small terms invariant, we need a variable, ``missing'', to stand for Mis in our
	 conditional constraints. Because the current implementation doesn't support \urow, this variable shall be
	 split whenever the conditional is split. This works, but wastes some computing time. In other words, we
	 must make ``missing'' a row variable, whereas a regular variable, prefixed by \urow, would be more
	 economical. This will have to be fixed in the next full rewrite (planned). *)

      let missing = freshSK span KRecord in
      merge_small_upper_bound missing TRMissing;

      rho1m.guards <- Set7.add (HRMissing, rho2p, rho3m) (Set7.add (HRPresent, rho2p, missing) empty_guard_set);
      rho2m.guards <- Set7.add (HRMissing, rho1p, rho3m) (Set7.add (HRPresent, rho1p, missing) empty_guard_set);

      let r1 = make_trecord empty_row_map (TVar rho1m)
      and r2 = make_trecord empty_row_map (TVar rho2m)
      and r3 = make_trecord empty_row_map (TVar rho3p) in

      let vbottom = fresh() in
      let bottom = TVar vbottom in

      let body = make_tarrow (neg r1) bottom (pos (make_tarrow (neg r2) bottom (pos r3))) in

      LetNode, Scheme(empty_context, vpos body, vbottom)

  | VGeneralFun _
  | VGeneralTry _ ->

      raise (CantHappen "Unexpected VGeneralFun/Try construct in Typechecking.infer.")

;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

States are internal, and need not be made visible outside.

*)

let infer env expr =
  let _, scheme = infer env expr in
  scheme
;;

let simplify bipolar node_type scheme =
  let _, scheme = simplify bipolar node_type (AnyNode, scheme) in
  scheme
;;
