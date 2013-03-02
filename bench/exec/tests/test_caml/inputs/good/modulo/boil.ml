(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/boil.ml,v 1.34 2002/05/30 15:29:18 fpottier Exp $ *)

(* This module provides a type inference algorithm for the language defined
   in [InternalSyntax]. *)

open Id
open KindUnification
open InternalSyntax
open Unification

(* Abstract syntax for inferred types. This syntax roughly corresponds to
   [InternalSyntax.typ], with differences described below.

   The productions for built-in types are modified: the built-in types are
   now constants, because that's how they are represented internally.

   One extra production has been added for meta-variables. Note that
   meta-variables and variables must not be confused, since the latter
   may actually bind free variables. *)

type typ =
  | IOTVar of id
  | IOTApp of typ * channel * typ
  | IOTAbs of channel * id * typ
  | IOTRowCons of id * typ * typ
  | IOTRowUniform of typ

  (* Built-in types. *)

  | IOTForall of kind
  | IOTPresent
  | IOTAbsent
  | IOTRecord
  | IOTArrow
  | IOTTuple of int
  | IOTInteger

  (* Additional productions. *)

  | IOTMetaVariable of id

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Environments} *)

type rank =
    int * int

module Env : sig

  (* The type of environments. *)

  type t

  (* An environment contains a kind environment, obtained by dropping
     information about term identifiers, type synonyms, record fields, etc. *)

  val kenv: t -> KEnv.kenv

  (* The initial environment. *)

  val initial: t

  (* Introducing a gap marker. This is done when entering the left-hand side
     of a [let] construct. *)

  val gap: t -> t

  (* Introducing a lambda marker. This is done when entering a
     $\lambda$-abstraction. *)

  val lambda: t -> t

  (* [mono env t] binds a fresh meta-variable immediately outside the nearest
     enclosing $\lambda$-abstraction, and equates it with the term [t]. This
     has the effect of making [t] ``monomorphic''. *)

  val mono: t -> term -> unit

  (* Introducing term identifiers. *)

  val bind_term: id -> scheme -> t -> t

  (* Access to term identifiers. *)

  exception UndefinedTermVariable of id
  val access_term: id -> t -> term

  (* Introducing type identifiers. Note that [bind_abstract_type None]
     introduces a fresh, anonymous abstract type. *)

  val bind_abstract_type: id option -> t -> t
  val bind_concrete_type: id -> term -> t -> t

  (* Access to type identifiers. *)

  exception UndefinedType of id
  val access_type: id -> t -> term

  (* Access to built-in types. *)

  val forall: t -> kind -> term -> term
  val present: t -> term -> term
  val absent: t -> term
  val record: t -> term -> term
  val arrow: t -> term -> term -> term
  val tuple: t -> term list -> term
  val int: t -> term

  val void: t -> term
  val unit: t -> term

  (* A mapping from indices back to abstract type syntax, provided here only
     for the built-in types. That is, [builtin index] interprets [index]
     within the initial environment and returns a description of the
     corresponding built-in type. *)

  val builtin: int -> typ

  (* Processing declarations. *)

  exception DuplicateRecordFieldDeclaration of id
  val declare: declaration -> t -> t

  (* Access to fields. *)

  exception UndefinedField of id
  val access_field: id -> t -> mutability

end = struct

  (* When a new type is introduced in the environment, it is either a new
     abstract type (represented by the variable 1), or a synonym for another
     type. *)

  type type_binding =
    | One
    | Synonym of term

  type t = {

      (* The current rank, i.e. the number of $\shift{}$ and $\gamma$
	 markers in the environment. *)

      rank: rank;

      (* The ``monomorphic'' rank, i.e. the rank that was current when
	 the nearest enclosing $\lambda$-abstraction was entered. *)

      mono: rank;

      (* A mapping from term identifiers to type schemes, together with
	 the rank that was current when the identifier was bound. *)

      terms: (rank * scheme) IdMap.t;

      (* A mapping from type identifiers to type bindings, together with the
	 rank that was current when the identifier was bound. *)

      types: (rank * type_binding) IdMap.t;

      (* A mapping from record field labels to mutability flags. *)

      fields: mutability IdMap.t;

      (* The underlying kind environment. *)

      kenv: KEnv.kenv

    } 

  let kenv env =
    env.kenv

  (* Computations on ranks. *) (* TEMPORARY redundant with Unification.Index *)

  let null =
    (0, 0)

  let increment (k, gamma) =
    (k + 1, gamma)

  let sub (k1, gamma1) (k2, gamma2) =
    (k1 - k2, gamma1 - gamma2)

  (* The initial environment. *)

  let initial = {
    rank = null;
    mono = null;
    terms = IdMap.empty;
    types = IdMap.empty;
    fields = IdMap.empty;
    kenv = KEnv.nil
  } 

  (* Introducing a gap marker. *)

  let gap env =
    let k, gamma = env.rank in
    { env with rank = (k, gamma + 1); kenv = KEnv.gap env.kenv }

  (* Introducing a lambda marker. This is done when entering a
     $\lambda$-abstraction. *)

  let lambda env =
    { env with mono = env.rank }

  (* [mono env t] binds a fresh meta-variable immediately outside the nearest
     enclosing $\lambda$-abstraction, and equates it with the term [t]. This
     has the effect of making [t] ``monomorphic''. *)

  let mono env t =
    let (k, gamma) as offset = sub env.rank env.mono in
    let okenv = env.kenv in
    let ikenv = KEnv.cut k gamma okenv in
    schedule (closure (metavariable ikenv) (shift offset) okenv, t)

  (* Introducing new term identifiers. *)

  let bind_term x scheme env =
    { env with terms = IdMap.add x (env.rank, scheme) env.terms }

  (* Access to term identifiers. Looking up the term mapping yields a type
     scheme, whose gap variable we instantiate with the difference between the
     current rank and the then-current rank, so as to take an instance of the
     type scheme that is meaningful within the current context. *)

  exception UndefinedTermVariable of id

  let access_term x env =
    try
      let rank, scheme = IdMap.find x env.terms in
      instance env.kenv (sub env.rank rank) scheme
    with Not_found ->
      raise (UndefinedTermVariable x)

  (* Introducing a new, abstract type identifier. *)

  let bind_abstract_type x env =
    let rank = increment env.rank in
    { env with
      rank = rank;
      kenv = KEnv.cons x env.kenv;
      types = match x with
              | None -> env.types
	      |	Some x -> IdMap.add x (rank, One) env.types }

  (* Introducing a new, concrete type identifier. *)

  let bind_concrete_type x t env =
    { env with types = IdMap.add x (env.rank, Synonym t) env.types }

  (* Access to type identifiers. Looking up the type mapping yields a type
     term, which we shift appropriately, so as to obtain a term that is
     meaningful within the current context. *)

  exception UndefinedType of id

  let access_type x env =
    try
      let rank, tb = IdMap.find x env.types in
      let delta = sub env.rank rank in
      match tb with
      |	One ->
	  variable (increment delta) env.kenv
      |	Synonym t ->
	  closure t (shift delta) env.kenv
    with Not_found ->
      raise (UndefinedType x)

  (* [absolute env k] returns a term which represents the (built-in) abstract
     type whose index was [k] in the initial environment. *)

  let absolute env k =
    let k', gamma' = env.rank in
    variable (k + k', gamma') env.kenv

  (* Access to built-in types. *)
  (* TEMPORARY in the presence of an infinite number of kinds, hash-cons them *)

  let forall env kind t =
    match kind with
    | _ -> (* TEMPORARY *)
	app (absolute env 1) t

  let present env t =
    app (absolute env 2) t

  let absent env =
    absolute env 3

  let record env t =
    app (absolute env 4) t

  let arrow env t1 t2 =
    app (app (absolute env 5) t1) t2

  let tuple env ts =
    let k = List.length ts in
    List.fold_left app (absolute env (7+k)) ts

  let int env =
    absolute env 6

  (* The empty record type. *)

  let void env =
    record env (row_uniform (absent env))

  (* The [unit] type is the empty tuple type. *)

  let unit env =
    tuple env []

  (* A mapping from indices back to names, provided here only for the built-in
     types. That is, [builtin index] interprets [index] within the initial
     environment and returns the name of the corresponding built-in type. *)

  let builtin = function
    | 1 -> IOTForall star
    | 2 -> IOTPresent
    | 3 -> IOTAbsent
    | 4 -> IOTRecord
    | 5 -> IOTArrow
    | 6 -> IOTInteger
    | n -> assert (n >= 7); IOTTuple (n - 7)

  (* [declare env d] augments the environment [env] to reflect the effect of
     the declaration [d]. *)

  exception DuplicateRecordFieldDeclaration of id

  let declare d env =
    match d with
    | IDeclareField (id, mut) ->
	if IdMap.mem id env.fields then
	  raise (DuplicateRecordFieldDeclaration id);
	{ env with fields = IdMap.add id mut env.fields }

  exception UndefinedField of id

  let access_field x env =
    try
      IdMap.find x env.fields
    with Not_found ->
      raise (UndefinedField x)

end

type environment =
    Env.t

let metavariable env =
  metavariable (Env.kenv env)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Interpreting the type syntax} *)

(* [convert_type env texp] converts the type expression [texp] into
   a term of type [Unification.term], given the environment [env]. *)

let rec interpret env = function
  | ITVar x ->
      Env.access_type x env
  | ITApp (t1, p, t2) ->
      application (interpret env t1) p (interpret env t2)
  | ITAbs (p, x, t) ->
      abstraction p (interpret (Env.bind_abstract_type (Some x) env) t)
  | ITRowCons (label, t1, t2) ->
      row_cons label (interpret env t1) (interpret env t2)
  | ITRowUniform t ->
      row_uniform (interpret env t)
  | ITForall (kind, t) ->
      Env.forall env kind (interpret env t)
  | ITPresent t ->
      Env.present env (interpret env t)
  | ITAbsent ->
      Env.absent env
  | ITRecord t ->
      Env.record env (interpret env t)
  | ITArrow (t1, t2) ->
      Env.arrow env (interpret env t1) (interpret env t2)
  | ITTuple tl ->
      Env.tuple env (List.map (interpret env) tl)
  | ITInteger ->
      Env.int env

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Sets and maps over identifiers. *)

exception DuplicateIdentifierOrRecordLabel of id

let add x s =
  if IdSet.mem x s then
    raise (DuplicateIdentifierOrRecordLabel x)
  else
    IdSet.add x s

let empty =
  IdSet.empty

let disjoint s1 s2 =
  try
    raise (DuplicateIdentifierOrRecordLabel (IdSet.choose (IdSet.inter s1 s2)))
  with Not_found ->
    ()

let augment m env =
  IdMap.fold Env.bind_term m env

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Patterns. *)

exception IllegalPattern

(* [fdvpat] returns the sets of free and defined variables of its pattern
   argument. The free variables are those which appear within [IPConstant]
   patterns.

   The function also checks that no variable is defined twice. *)

let rec fdvpat fv dv = function
  | IDisplay e ->
      fdvpat fv dv e
  | IEVar id ->
      fv, add id dv
  | IEApp (pat1, pat2) ->
      let fv, dv = fdvpat fv dv pat1 in
      fdvpat fv dv pat2
  | IERecordEmpty
  | IPWildcard
  | IEInteger _ ->
      fv, dv
  | IERecordExtend (elements, pat) ->

      List.fold_left (fun (fv, dv) element ->
	match element with
	| IRElement (id, pat) ->
	    fdvpat fv dv pat
	| IRBinding _ ->
	    assert false
      ) (fdvpat fv dv pat) elements

  | IETuple patl
  | IEChoice patl ->

      List.fold_left (fun (fv, dv) pat ->
	fdvpat fv dv pat
      ) (fv, dv) patl

  | IPTypAbs (pat, _) ->
      fdvpat fv dv pat
  | IPConstant e ->
      fvexpr fv e, dv
  | IETypeConstraint (e, _) ->
      fdvpat fv dv e
  | IEAbs _
  | IETypAbs _
  | IETypApp _
  | IEBinding _
  | IERecordUpdate _
  | IEExists _
  | IEForall _ ->
      assert false

(* For now, ``constants'' must be variables or polymorphic applications
   thereof. *) (* TEMPORARY si IPConstant est toujours employ'e avec IEVar
   uniquement, on peut simplifier? *)

and fvexpr fv = function
  | IEVar id ->
      IdSet.add id fv
  | IETypApp (e, _) ->
      fvexpr fv e
  | _ ->
      assert false

(* This external version of [fdvpat] checks that no free variable is also
     defined -- that would lead to variable capture while inferring the
     pattern's type. *)

let fdvpat pat =
  let (fv, dv) as fdv = fdvpat empty empty pat in
  disjoint fv dv;
  fdv

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Preparing [let] bindings. *)

(* [trim e] makes sure that [e] complies with our expectations concerning
   [let] bindings; that is, at least one the type schemes created by a call to
   [generalize] must be instantiated. This is achieved by making some [let]
   bindings monomorphic and by rejecting some recursive [let] bindings --
   details below. *)

exception IllegalLetRecBinding of id list
  (* None of these identifiers appears outside of the clauses that define
     them. *)

let rec trim e =
  let accu = ref empty in

  (* [trime e] adds [e]'s free term variables to the accumulator [accu]. *)

  let rec trime = function
    | IEVar id ->
	accu := IdSet.add id !accu
    | IETuple es
    | IEChoice es ->
	List.iter trime es
    | IERecordEmpty
    | IEInteger _ ->
	()
    | IERecordExtend (contents, e) ->
	accu := IdSet.union !accu (trimc contents);
	trime e
    | IETypeConstraint (e, _)
    | IDisplay e
    | IETypAbs (_, _, e)
    | IETypApp (e, _)
    | IEExists (_, e)
    | IEForall (_, e) ->
	trime e
    | IEApp (e1, e2)
    | IERecordUpdate (e1, _, e2) ->
	trime e1;
	trime e2
    | IEAbs (id, _, e) ->
	accu := IdSet.union !accu (IdSet.remove id (trim e))
    | IEBinding (b, e) ->
	accu := IdSet.union !accu (trimb (trim e) b);
    | IPWildcard
    | IPTypAbs _
    | IPConstant _ ->
	assert false

  in
  trime e;
  !accu

(* [trimc] deals with record contents. *)

and trimc = function
  | [] ->
      empty
  | (IRElement (_, e)) :: contents ->
      IdSet.union (trim e) (trimc contents)
  | (IRBinding b) :: contents ->
      trimb (trimc contents) b

(* [trimb fv b] examines the binding [b], assuming that the free term
   variables of the expression which lies within its scope are given by
   [fv], and returns the set of term variables that remain free outside of
   the binding.

   As a side effect, it may modify the binding, turning polymorphic a [let]
   into a monomorphic one. It may also reject some recursive [let] bindings. 
   (Details below.) *)

and trimb fv = function
  | IBLetGen (g, pat, e) ->

      (* Determine which variables within [fv] remain free outside of the
	 binding. *)

      let fvp, dv = fdvpat pat in
      let fv' = IdSet.diff fv dv in

      (* If a [let] binding does not in fact bind any term variables, because
	 they are anonymous or because they are unused, then it need not be
	 polymorphic. In fact, it \emph{must} not be polymorphic, because our
	 typing rules for polymorphic [let] assume that at least one of the
	 bound term variables is referenced within the scope of the
	 binding. *)

      if IdSet.equal fv fv' then
	g := Monomorphic;

      (* Return the set of all free term variables. The variables free within
	 [pat] or [e] are free within the whole expression. *)

      IdSet.union (IdSet.union fv' fvp) (trim e)

  | IBLetRec (explicit, implicit) ->

      (* Add the free variables found within the explicitly annotated
	 clauses. *)

      let fv = List.fold_left (fun fv (_, _, e) ->
	IdSet.union fv (trim e)
      ) fv explicit in

      (* Check whether at least one of the variables bound by the
	 un-annotated clauses is referenced within the scope of the binding
	 or within one of the explicitly annotated clauses. If not, then we
	 reject the program, again because our typing rules rely on this
	 assumption. *)

      let fv' = List.fold_left (fun fv' (id, _) ->
	IdSet.remove id fv'
      ) fv implicit in

      if (implicit <> []) & (IdSet.equal fv fv') then
	raise (IllegalLetRecBinding (List.map fst implicit));

      (* To conclude, add the free variables not yet accounted for, and
	 remove all bound variables. *)

      let fv' = List.fold_left (fun fv' (_, e) ->
	IdSet.union fv' (trim e)
      ) fv' implicit in

      let fv' = List.fold_left (fun fv' (id, _, _) ->
	IdSet.remove id fv'
      ) fv' explicit in

      let fv' = List.fold_left (fun fv' (id, _) ->
	IdSet.remove id fv'
      ) fv' implicit in

      fv'

  | IBLetType _ ->
      fv

let trim e =
  let _ = trim e in
  e

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Bindings.

   The parameter [env] represents the current environment. [infer_binding]
   returns an updated environment, which reflects the binding's effect. *)

exception UpdateOfImmutableField of id

(* A hack to allow exporting some ``interesting'' terms for display. *)

let display =
  ref []

let rec infer_mono_binding env pat e =

  (* Check that the pattern is well-formed, and create a mapping from every
     identifier defined by the pattern to a fresh meta-variable. *)

  let _, dv = fdvpat pat in
  let m = IdSet.fold (fun id m ->
    IdMap.add id (metavariable env) m
  ) dv IdMap.empty in

  (* Treating the pattern as if it were an expression, infer its type within
     an environment augmented with [m]. This yields the pattern's shape;
     require [e] to have that shape. *)

  let shape = metavariable env in
  infer (augment (IdMap.map inject m) env) shape pat;
  infer env shape e;

  (* Return [m], causing it to be (possibly) generalized and appended to the
     original environment. *)

  m

and infer_binding env = function
  | IBLetGen (g, pat, e) ->

      if !g = Monomorphic then

	augment (IdMap.map inject (infer_mono_binding env pat e)) env

      else

	augment (generalize (Env.kenv env) (fun () ->

	  (* Introduce a new gap variable $\gamma$ at the end of the
	     environment, so that every term fetched from the environment will
	     be shifted by $\shift{\gamma}$. Proceed with the binding as if it
	     were monomorphic, yielding a mapping from [pat]'s defined variables
	     to types. Lastly, perform generalization, which turns each of these
	     types into a type scheme, and augment the environment. *)

	  infer_mono_binding (Env.gap env) pat e

	) IdMap.map) env

  | IBLetRec (explicit, implicit) ->

      (* Following Mark Jones (see \emph{Typing Haskell in Haskell}), we first
	 augment the environment with all of the user-specified type schemes,
	 then infer types for the non-annotated bindings, then check that the
	 annotated bindings were indeed correct. *)

      (* Augment the environment with bindings for the identifiers whose type
	 scheme was given. *)

      let env = List.fold_left (fun env (id, (quantifiers, texp), _) ->
	Env.bind_term id (generic env quantifiers texp) env
      ) env explicit in

      (* Infer types for the un-annotated identifiers, generalize them, and
	 augment the environment. *)

      let env = augment (generalize (Env.kenv env) (fun () ->

	(* Associate a meta-variable with every un-annotated identifier.

	   The environment in which these expressions are typechecked begins
	   with a new gap variable, since we are entering a generic [let]
	   binding, and binds every identifier to the meta-variable we chose
	   for it. *)

	let env' = Env.gap env in

	let implicit = List.map (fun (id, e) ->
	  id, metavariable env', e
	) implicit in

	let env' = List.fold_left (fun env' (id, x, _) ->
	  Env.bind_term id (inject x) env'
        ) env' implicit in

	(* Within this environment, we infer each expression's type and equate
	   it with the chosen meta-variable. *)

	List.iter (fun (_, x, e) ->
	  infer env' x e
        ) implicit;

	(* Return a mapping from identifiers to types, with which the
	   environment is to be augmented. *)

	List.fold_left (fun m (id, x, _) ->
	  IdMap.add id x m
        ) IdMap.empty implicit

      ) IdMap.map) env in

      (* We can now check that the declared type schemes were correct. To do
         so, we typecheck each expression, under suitable rigid type
         introductions and under a type constraint, within the full
         environment. *)

      List.iter (fun (_, (quantifiers, texp), e) ->
	infer env (metavariable env) (IEForall (quantifiers, IETypeConstraint (e, texp)))
      ) explicit;

      (* We are done, and return the new environment. *)

      env

  | IBLetType idtlist ->

      (* Parse the type in the current environment--type abbreviations are
	 non-recursive. Then, extend the environment with a definition for
	 [id]. As a result, every use of [id] will be replaced with its
	 definition during constraint generation. Not a particularly efficient
	 or subtle approach, but a simple one. *)

      List.fold_left (fun accu (id, texp) ->
	Env.bind_concrete_type id (interpret env texp) accu
      ) env idtlist

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Creating generic type schemes.

   Sometimes, when given an explicit type scheme by the user, it is useful to
   turn it into a generic type scheme. Indeed, giving a variable a generic
   type scheme allows using it without performing explicit type applications.

   [generic env quantifiers texp] returns a generic version of the type scheme
   described by [quantifiers] and [texp]. The current environment is used to
   interpret type identifiers in [texp]. *)

and generic env quantifiers texp =
  generalize (Env.kenv env) (fun () ->
    let env' = Env.gap env in
    let x = metavariable env' in
    infer env' x (IEForall (quantifiers, IETypeConstraint (IPWildcard, texp)));
    x
  ) (fun f x -> f x)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Expressions.

   [infer env expected e] checks that [e] has type [expected] within the
   environment [env]. *)

and infer env expected = function
  | IDisplay e ->

      display := expected :: !display;
      infer env expected e

  | IEVar id ->

      (* Look up [id] in the environment, and require the result to match our
	 expectation. *)

      schedule (expected, Env.access_term id env)

  | IEAbs (id, texpopt, e) ->

      (* Introduce two meta-variables to represent the function's domain and
	 range. If the domain is given, use that instead. *)

      let x =
	match texpopt with
	| None ->
	    metavariable env
	| Some texp ->
	    interpret env texp
      and y =
	metavariable env in

      (* Unify the expected type with $X\rightarrow Y, then require the body
	 [e] to have type $Y$ under an environment augmented with the
	 assumption $id:X$. The position of the $\lambda$-abstraction is also
	 recorded in the environment, so new ``monomorphic'' meta-variables
	 created while examining [e] will appear to have been allocated
	 here. *)

      schedule (expected, Env.arrow env x y);
      infer (Env.bind_term id (inject x) (Env.lambda env)) y e

  | IEApp (e1, e2) ->

      (* Introduce a meta-variable $X$ to stand for [e1]'s domain. *)

      let x = metavariable env in

      (* Type-check [e1] and [e2] with appropriate expected
	 types. Applications are regarded as potentially expansive, so the
	 return type is made monomorphic. *)

      Env.mono env expected;
      infer env (Env.arrow env x expected) e1;
      infer env x e2

  | IETypAbs (idopt, kind, e) ->

      (* Require [e] to have type $X$, where $X$ is a fresh meta-variable,
	 under an environment augmented with a new rigid variable. Then, the
	 whole expression's type is $\forall_\kappa\,(\Lambda X)$. Within [e],
	 the type identifier [id], if provided, is bound to the (rigid)
	 variable $1$. *)

      let env' = Env.bind_abstract_type idopt env in 
      let x = metavariable env' in
      schedule (expected, Env.forall env kind (abs x));
      infer env' x e

  | IETypApp (e, kind) ->

      (* Require [e] to have type $\forall_\kappa X$, for some $X$. Then, the
	 expected type must be of the form $X\,Y$, for some $Y$. *)

      let x = metavariable env
      and y = metavariable env in

      schedule (expected, app x y);
      infer env (Env.forall env kind x) e

  | IEBinding (b, e) ->

      (* Typecheck [e] within an environment augmented according to the
	 meaning of the binding [b]. *)

      infer (infer_binding env b) expected e

  | IERecordEmpty ->

      schedule (expected, Env.void env)

  | IEInteger _ ->
      
      schedule (expected, Env.int env)

  | IETuple elist ->

      (* Infer each expression's type and build a tuple type. *)

      schedule (expected, Env.tuple env (List.map (fun e ->
	let x = metavariable env in
	infer env x e;
	x
      ) elist))

  | IERecordExtend (contents, e) ->

      (* Introduce fresh variables to stand for the contents of the records we
	 are reading and creating. *)

      let src = metavariable env
      and dst = metavariable env in

      (* Unify our expected type against the destination record type. *)

      schedule (expected, Env.record env dst);

      (* Analyze the contents with which we are extending the source
	 record. *)

      infer_contents (env, empty, src, dst) contents;

      (* Infer [e]'s type and unify it against the source record type. *)

      infer env (Env.record env src) e

  | IERecordUpdate (e1, id, e2) ->

      (* Ensure that the field [id] is mutable. *)

      if Env.access_field id env = Immutable then
	raise (UpdateOfImmutableField id);

      (* Ensure that the field already exists within the record, and that its
	 new value has the same type. The expression evaluates to unit. *)

      let head = metavariable env
      and tail = metavariable env in

      schedule (expected, Env.unit env);
      infer env (Env.record env (row_cons id (Env.present env head) tail)) e1;
      infer env head e2

  | IEExists (ids, e) ->

      (* Introduce these type identifiers in the environment, associated to
	 fresh meta-variables, then typecheck [e]. *)

      let env = List.fold_left (fun env id ->
	Env.bind_concrete_type id (metavariable env) env
      ) env ids in

      infer env expected e

  | IEForall (ids, e) ->

      (* Introduce these type identifiers in the environment, associated to
	 (rigid) variables, then typecheck [e]. This ensures that [e] is
	 polymorphic with respect to these types. Then, return a generic
	 instance of [e]'s type, where the rigid variables have been replaced
	 with fresh meta-variables. *)

      let env, expected = List.fold_left (fun (env, expected) id ->
	let env' = Env.bind_abstract_type (Some id) env in
	let expected' = metavariable env' in
	schedule (expected, closure expected' (cons (metavariable env) Unification.id) (Env.kenv env));
	env', expected'
      ) (env, expected) ids in

      infer env expected e

  | IETypeConstraint (e, texp) ->

      schedule (expected, interpret env texp);
      infer env expected e

  | IPWildcard ->

      ()

  | IEChoice elist ->

      (* Require all expressions' types to match. *)
	  
      List.iter (infer env expected) elist

  | IPTypAbs (pat, kind) ->

      (* This pattern matches values that were built using explicit
	 polymorphic abstraction. Upon matching, we expect such a value to
	 have polymorphic type, and eliminate a universal quantifier, so that
	 the sub-pattern [pat] will be matched against an instance of the
	 value.

	 Interestingly enough, this typing rule is almost identical to that of
	 polymorphic application (see [IETypApp]), except for one thing: here,
	 the whole expression is polymorphic and its sub-expression is an
	 instance of it, while it was the other way round in the case of
	 [IETypApp]. *)

      let x = metavariable env
      and y = metavariable env in

      schedule (expected, Env.forall env kind x);
      infer env (app x y) pat

  | IPConstant e ->

      infer env expected e

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Record contents.

   [infer_content content (env, pf, src, dst)] determines the effect of the
   record line [content] within the type environment [env]. [pf] is the set of
   record fields that have been previously defined in the same record; it is
   used to prohibit duplicate field specifications. [src] and [dst] are rows
   which describe the types of the records being read and written,
   respectively. The function returns a new environment (this feature allows
   record contents to bind variables) and new values of [pf], [src] and
   [dst]. *)

and infer_content (env, pf, src, dst) = function
  | IRBinding b ->

      infer_binding env b, pf, src, dst

  | IRElement (id, e) ->

      (* Create meta-variables to stand for the tails of the source and
	 destination records. *)

      let src' = metavariable env
      and dst' = metavariable env in

      (* Require the field to be absent in the source record. *)

      schedule (src, row_cons id (Env.absent env) src');

      (* Require the destination record to carry this field, with [e]'s
	 type. If the field is mutable, require its type to be monomorphic. *)

      let x =
	metavariable env in

      if Env.access_field id env = Mutable then
	Env.mono env x;

      infer env x e;

      schedule (dst, row_cons id (Env.present env x) dst');

      (* Typecheck further declarations within the same environment. *)

      env, add id pf, src', dst'

and infer_contents ((env, pf, src, dst) as info) = function
  | [] ->

      (* Upon reaching the end of the record, require the source and
	 destination types to match. *)

      schedule (src, dst)

  | content :: contents ->

      infer_contents (infer_content info content) contents

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* The entry point.

   [check] checks that a whole program is well-typed within the initial
   environment. *)

let check (IProgram (d, e)) =
  let env = List.fold_right Env.declare d Env.initial in
  infer env (metavariable env) (trim e)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Converting types back to abstract type syntax. *)

(* Here comes the main conversion function. A term is converted by examining
   its structure, keeping the environment up-to-date as we go.

   Whenever a closure is encountered, it is rewritten into a series of
   $\beta$-redexes. For instance, $\clo{a}{b.s}$ is rewritten to $(\Lambda
   (\clo{a}{\sprotect{s}})) \,b$, and similarly in the $n$-ary case. These
   terms are equivalent, since the latter reduces to the former. *)

let rec convert = function
  | TMetaVariable x ->
      IOTMetaVariable x
  | TVariable (_, _, RefName id) ->
      IOTVar id
  | TVariable (_, _, RefBuiltin k) ->
      Env.builtin k
  | TAbstraction (id, p, t) ->
      IOTAbs (id, p, convert t)
  | TApplication (t1, p, t2) ->
      IOTApp (convert t1, p, convert t2)
  | TClosure (t, s) ->
      arrange t [] s
  | TRowCons (l, t1, t2) ->
      IOTRowCons (l, convert t1, convert t2)
  | TRowUniform t ->
      IOTRowUniform (convert t)
  | TShift _
  | TCons _ ->
      assert false

and arrange a ids = function
  | TCons (id, b, s) ->
      IOTApp (arrange a (id :: ids) s, "", convert b)
  | TShift _ ->
      List.fold_right (fun id t -> IOTAbs (id, "", t)) ids (convert a)
  | _ ->
      assert false

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* A pretty-printer for our output syntax. *)

module P = Tree.Make (struct

  type tree = typ

  type label =
    | LAbstraction
    | LApplicationL
    | LApplicationR
    | LRowCompL
    | LRowCompR
    | LRowUniform

  open Tree

  let rec describe = function
    | IOTMetaVariable x
    | IOTVar x ->
	[ Token x ]
    | IOTAbs (x, p, t) ->
	[ Token ("\\" ^ p ^ (if p = "" then "" else ":") ^ x ^ "."); Son (LAbstraction, t) ]
    | IOTApp (t1, p, t2) ->
	[ Son (LApplicationL, t1); Token (" " ^ p ^ (if p = "" then "" else ":")); Son (LApplicationR, t2) ]
    | IOTRowCons (label, t1, t2) ->
	[ Token label; Token ": "; Son (LRowCompL, t1); Token "; "; Son (LRowCompR, t2) ]
    | IOTRowUniform t ->
	[ Token "!"; Son (LRowUniform, t) ]
    | IOTForall _ -> (* TEMPORARY *)
	[ Token "Forall" ]
    | IOTPresent ->
	[ Token "Present" ]
    | IOTAbsent ->
	[ Token "Absent" ]
    | IOTRecord ->
	[ Token "{}" ]
    | IOTArrow ->
	[ Token "->" ]
    | IOTTuple k ->
	[ Token ("(" ^ (string_of_int k) ^ ")") ]
    | IOTInteger ->
	[ Token "int" ]

  let parenthesize label tree =
    match label, tree with
    | (LAbstraction | LApplicationR), (IOTRowCons _ | IOTRowUniform _) ->
	angle_brackets
    | LRowCompR, (IOTRowCons _ | IOTRowUniform _) ->
	nothing
    | (LApplicationL | LRowCompL | LRowUniform), (IOTRowCons _ | IOTRowUniform _) ->
	assert false

    | _, IOTMetaVariable _
    | _, IOTVar _
    | _, IOTForall _ (* TEMPORARY *)
    | _, IOTPresent
    | _, IOTAbsent
    | _, IOTRecord
    | _, IOTArrow
    | _, IOTTuple _
    | _, IOTInteger ->
	nothing

    | LRowUniform, _ ->
	parentheses
    | LRowCompL, _
    | LRowCompR, _ ->
	nothing

    | LAbstraction, _ ->
	nothing
    | _, IOTAbs _ ->
	parentheses

    | LApplicationL, IOTApp _ ->
	nothing
    | LApplicationR, IOTApp _ ->
	parentheses

end)

let print =
  P.print

