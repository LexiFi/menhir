(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/infer.ml,v 1.14 2004/04/13 16:21:25 fpottier Exp $ *)

(** This module implements type inference. *)

open Sig

module Make
    (S : Solver)
    (B : BuiltinTypes with type 'a arterm = 'a S.A.arterm)
    (P : Primitives with type variable = S.variable
                     and type crterm = S.crterm)
= struct

  open Misc
  open S
  open B

  type primitive =
      P.primitive

  (** This exception is raised when an unbound type identifier is found. *)
  exception UnboundTypeIdentifier of tname

  (** [intern tenv typ] converts the type expression [typ] to a type.
      The environment [tenv] maps type identifiers to types. *)
  let rec intern tenv = function
  | TypVar name -> (
      try
	List.assoc name tenv
      with Not_found ->
	raise (UnboundTypeIdentifier name)
    )
  | TypArrow (typ1, typ2) ->
      arrow (intern tenv typ1) (intern tenv typ2)
  | TypTuple typs ->
      tuple (List.map (intern tenv) typs)

  (** [intern_scheme tenv name qs typ] produces a type scheme
      that binds [name] to [forall qs.typ]. *)
  let intern_scheme tenv name qs typ =
    let fqs, rtenv = variable_list qs in
    Scheme ([], fqs, CTrue, StringMap.singleton name (intern (rtenv @ tenv) typ))

  exception NonLinearPattern of name

  let add name accu =
    if StringSet.mem name accu then
      raise (NonLinearPattern name)
    else
      StringSet.add name accu

  let rec dvacc accu = function
    | PVar name ->
	add name accu
    | PWildcard ->
	accu
    | PAlias (name, p) ->
	dvacc (add name accu) p
    | PTypeConstraint (p, _) ->
	dvacc accu p
    | PTuple ps ->
	List.fold_left dvacc accu ps

  (** [dv p] returns the set of program variables defined (bound) by
      the pattern [p]. At the same time, it ensures that the pattern
      is well-formed. *)
  let dv p =
    dvacc StringSet.empty p

  (* TEMPORARY partager du code entre patterns et expressions? *)

  (** [infer_pat p t] generates a constraint that guarantees that [p]
      has type [t]. It implements the constraint generation rules for
      patterns. *)
  let rec infer_pat tenv p (t : crterm) =
    match p with

    | PVar name ->
	CInstance (name, t)

    | PWildcard ->
	CTrue

    | PAlias (name, p) ->
	CInstance (name, t) ^
	infer_pat tenv p t

    | PTypeConstraint (p, typ) ->
	(t =?= intern tenv typ) ^
	infer_pat tenv p t

    | PTuple ps ->
	exists_list ps (fun pts ->
	  CConjunction (List.map (fun (p, t) -> infer_pat tenv p t) pts) ^
          (t =?= tuple (List.map snd pts))
        )

  (** [extract_type] examines an expression and looks for a sufficiently explicit
      type annotation. If it finds one, it returns the type annotation, together
      with the expression (deprived of its annotation). Otherwise, it raises
      [Not_found]. *)
  let rec extract_type = function
    | ETypeConstraint (e, typ) ->
	typ, e
    | ELambda (PTypeConstraint (p, typ1), e2) ->
	let typ2, e2 = extract_type e2 in
	TypArrow (typ1, typ2), ELambda (p, e2)
    | _ ->
	raise Not_found

  type 'primitive recursive_value_definition_kind =
    | Implicit of name * 'primitive expression
    | Explicit of name * typ * 'primitive expression

  (** [explicit_or_implicit] examines a value definition and determines whether
      it carries an explicit type annotation. It also checks that the left-hand
      side is a variable. *)
  let rec explicit_or_implicit p e =
    match p with
    | PTypeConstraint (p, typ) ->
	explicit_or_implicit p (ETypeConstraint (e, typ))
    | PVar name -> (
	try
	  let typ, e = extract_type e in
	  Explicit (name, typ, e)
	with Not_found ->
	  Implicit (name, e)
      )
    | _ ->
	failwith "The left-hand side of a recursive definition must be a variable" (* TEMPORARY meilleur message *)

  (** Constraint contexts. *)
  type context =
      (crterm, variable) _constraint -> (crterm, variable) _constraint

  let rec infer_vdef tenv (qs, p, e) =
    let rqs, rtenv = variable_list qs in
    scheme rqs (dv p) (fun h ->
      exists (fun x ->
	let tenv' = rtenv @ tenv in
	infer_expr tenv' e x ^
	CLet (
	  [ monoscheme h ], 
	  infer_pat tenv' p x
        )
      )
    )

  and infer_binding tenv b =
(* TEMPORARY make sure that the bindings define distinct names *)
    match b with

    | BindValue vdefs ->

	let schemes = List.map (infer_vdef tenv) vdefs in
	(fun c -> CLet (schemes, c))

    | BindRecValue vdefs ->

      (* The constraint context generated for [let rec forall X1 . x1 : T1 = e1 and forall X2 . x2 = e2] is

	   let forall X1 (x1 : T1) in
	     let forall [X2] Z2 [
	       let x2 : Z2 in [ e2 : Z2 ]
	     ] ( x2 : Z2) in (
	       forall X1.[ e1 : T1 ] ^
	       [...]
	     )

	 In other words, we first assume that x1 has type scheme forall X1.T1.
	 Then, we typecheck the recursive definition x2 = e2, making sure that
	 the type variable X2 remains rigid, and generalize its type. This 
	 yields a type scheme for x2, which is then used to check that e1
	 actually has type scheme forall X1.T1.

	 In the above example, there are only one explicitly typed and one
	 implicitly typed value definitions.

	 In the general case, there are multiple explicitly and implicitly
	 typed definitions, but the principle remains the same. We generate
	 a context of the form

	   let schemes1 in
	     let forall [rqs2] fqs2 [
	       let h2 in c2
	     ] h2 in (
	       c1 ^
	       [...]
	     )

	 *)

      let schemes1, rqs2, fqs2, h2, c2, c1 =
	List.fold_left (fun (schemes1, rqs2, fqs2, h2, c2, c1) (qs, p, e) ->

	  (* Allocate variables for the quantifiers in the list [qs],
	     augment the type environment accordingly. *)

	  let rvs, rtenv = variable_list qs in
	  let tenv' = rtenv @ tenv in

	  (* Check whether this is an explicitly or implicitly typed
	     definition. *)

	  match explicit_or_implicit p e with
	  | Implicit (name, e) ->

	      let v = variable() in
	      let t = A.TVariable v in

	      schemes1,
	      rvs @ rqs2,
	      v :: fqs2,
	      StringMap.add name t h2,
	      infer_expr tenv' e t ^ c2,
	      c1

	  | Explicit (name, typ, e) ->

	      intern_scheme tenv name qs typ :: schemes1,
	      rqs2,
	      fqs2,
	      h2,
	      c2,
	      fl rvs (infer_expr tenv' e (intern tenv' typ)) ^ c1

	) ([], [], [], StringMap.empty, CTrue, CTrue) vdefs in

      fun c -> CLet (schemes1,
		 CLet ([ Scheme (rqs2, fqs2, CLet ([ monoscheme h2 ], c2), h2) ],
		       c1 ^ c
                 )
               )

  (** [infer_expr tenv e t] generates a constraint that guarantees that [e]
      has type [t]. It implements the constraint generation rules for
      expressions. *)
  and infer_expr tenv e (t : crterm) =
    match e with

    | EPrimApp (prim, el) ->
	P.generate t (fun tl ->
	  try
	    CConjunction (List.map2 (infer_expr tenv) el tl)
	  with Invalid_argument _ ->
	    (* If this fails, then the abstract syntax tree disagrees with
	       module [P] as to the arity of the primitive operation [prim]. *)
	    assert false
        ) prim

    | EVar name ->
	CInstance (name, t)

    | ELambda (p, e) ->
        exists (fun x1 ->                (* Allocate fresh type variables [x1] and [x2]. *)
          exists (fun x2 ->
     	    exists_set (dv p) (fun h ->  (* Allocate a fresh type variable for every variable defined by [p]. *)
              CLet (
	        [ monoscheme h ],        (* Bind the variables of [p] via a monomorphic [let] constraint. *)
                infer_pat tenv p x1 ^    (* Require [x1] to be a valid type for [p]. *)
                infer_expr tenv e x2     (* Require [x2] to be a valid type for [e]. *)
              )
            ) ^
            (t =?= arrow x1 x2)          (* Require the expected type [t] to be an arrow of [x1] to [x2]. *)
          )
        )

    | EApp (e1, e2) ->

	(* TEMPORARY [t] should be made monomorphic *)

	exists (fun x ->
	  infer_expr tenv e1 (arrow x t) ^
  	  infer_expr tenv e2 x
        )

    | ETuple es ->
	exists_list es (fun ets ->
	  CConjunction (List.map (fun (e, t) -> infer_expr tenv e t) ets) ^
          (t =?= tuple (List.map snd ets))
        )

    | EBinding (b, e) ->
	infer_binding tenv b (infer_expr tenv e t)

    | EExists (qs, e) ->
	exists_list qs (fun m ->
	  infer_expr (m @ tenv) e t
        )

    | ETypeConstraint (e, typ) ->
	(t =?= intern tenv typ) ^
	infer_expr tenv e t

  (** [infer e] determines whether the expression [e] is well-typed
      in the empty environment. *)
  let infer e =
    solve (exists (infer_expr [] e))

  (** [bind b] generates a constraint context that describes the top-level
      binding [b]. *)
  let bind b =
    infer_binding [] b

end
