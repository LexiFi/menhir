open AbstractSyntax

(* ------------------------------------------------------------------------- *)

(* The constraint generator is parameterized over a data constructor
   table and a main expression. It produces a unification problem with
   a distinguished variable (which represents the type of the main
   expression). *)

module Run (G : sig

  val dcenv: Wf.data_constructor_table
  val main: expression

end) : sig

  val problem: Unification.problem
  val root: Unification.variable

end = struct

open G

(* ------------------------------------------------------------------------- *)

(* The constraint generator builds a unification problem. *)

module B =
  Unification.Build (struct end)

open B

(* ------------------------------------------------------------------------- *)

(* The constraint generator maintains a mapping [vvenv] of value
   variables to internal types (that is, unification terms) and a
   mapping [tvenv] of external type variables to internal type
   variables (that is, unification variables). *)

type vvenv =
   Unification.term StringMap.t

type tvenv =
   Unification.variable StringMap.t

(*<corrige>*)
(* ------------------------------------------------------------------------- *)

(* The environment [tvenv] is extended by mapping external type
   variables to fresh unification variables. *)

let introduce_type_var (tvenv : tvenv) (x : type_variable) : tvenv =
  StringMap.add x (fresh()) tvenv

let introduce_type_vars tvenv xs =
  List.fold_left introduce_type_var tvenv xs

(* ------------------------------------------------------------------------- *)

(* External types are translated into internal types by looking up [tvenv]. *)

let translate_type_var tvenv (x : type_variable) : Unification.term =
  TVar (StringMap.find x tvenv)

let translate_type tvenv t =
  Type.lift (translate_type_var tvenv) t

(* ------------------------------------------------------------------------- *)

(* Type schemes are translated and instantiated at the same time: that
   is, their quantifiers are translated to fresh unification
   variables. As a result, applying [instantiate_scheme] twice to the
   same type scheme yields two distinct (that is, fresh) instances. *)

let instantiate_scheme (Wf.Scheme (xs, domain, codomain)) =
  let tvenv = introduce_type_vars StringMap.empty xs in
  List.map (translate_type tvenv) domain,
  translate_type tvenv codomain

(* This looks up and instantiates the type scheme associated with the
   data constructor [dc]. *)

let instantiate_dc dc =
  instantiate_scheme (StringMap.find dc dcenv)

(* ------------------------------------------------------------------------- *)

(* The functions that follow typecheck a pattern against an expected
   type. This has the effect of augmenting the (implicit) current
   unification problem as well as the (explicit) accumulator
   [vvenv]. *)

let type_pat_var (vvenv : vvenv) (x : value_variable) expected : vvenv =

  (* Record the fact that [x] has type [expected] in the environment. *)

  StringMap.add x expected vvenv

let type_pat vvenv (PConApp (dc, xs)) expected =

  (* Create a fresh instance of the type scheme associated with [dc].
     The list [domain] dictates the types of the constructor's
     parameters, while the type [codomain] is the constructor's result
     type. *)

  let domain, codomain = instantiate_dc dc in

  (* Require the constructor's result type to coincide with the
     pattern's expected type. *)

  expected =?= codomain;

  (* Require every parameter to satisfy its expected type. *)

  List.fold_left2 type_pat_var vvenv xs domain

(* ------------------------------------------------------------------------- *)

(* This equates the type [t] with an arrow type [alpha1 -> alpha2],
   where [alpha1] and [alpha2] are fresh variables, and returns
   [alpha1] and [alpha2]. *)

let decompose_arrow t =
  let domain = TVar (fresh())
  and codomain = TVar (fresh()) in
  TArrow (domain, codomain) =?= t;
  domain, codomain

(*</corrige>*)
(* ------------------------------------------------------------------------- *)
(* All exception values manipulated by the program must have the
   same type.  However, this can be any type.  We introduce a
   global type variable which will be unified with the types of
   exception values at the [raise] and [try...with] constructs. *)

let type_of_exn = TVar (fresh())

(* The constraint generator accepts two environments [vvenv] and
   [tvenv], an expression [e], and an expected type [expected]. It
   (implicitly) emits a constraint and returns no explicit result. *)

let rec type_expr
    (vvenv : vvenv) (tvenv : tvenv)
    (e : expression) (expected : Unification.term) =
(*<sujet>
  () (* INCOMPLETE! of course something should be done here... *)
</sujet>*)
(*<corrige>*)
  match e with

    (* Value variables are looked up in the environment. The type
       found there is equated with the expected type. *)

  | EVar x ->
      StringMap.find x vvenv =?= expected

    (* Functions and recursive functions are dealt with by
       [type_fun]. *)

  | EFun (x, e) ->
      type_fun vvenv tvenv None x e expected
  | ERecFun (f, x, e) ->
      type_fun vvenv tvenv (Some f) x e expected

    (* In an application, a fresh variable is created to stand
       for the unknown type of the argument. Generation is then
       straightforward. *)

  | EApp (e1, e2) ->
      let domain = TVar (fresh()) in
      type_expr vvenv tvenv e1 (TArrow (domain, expected));
      type_expr vvenv tvenv e2 domain

    (* Data constructor application requires taking an instance of the
       data constructor's type scheme, checking that its codomain
       matches the expected type, and checking that every argument
       satisfies its expected type. This is pretty much like
       [type_pat] above. *)

  | EConApp (dc, es) ->
      let domain, codomain = instantiate_dc dc in
      expected =?= codomain;
      List.iter2 (type_expr vvenv tvenv) es domain

    (* At a [let] construct, we create a fresh variable to stand
       for the type of [e1], extend the value environment with a
       binding of [x] to that variable, and continue with [e2].
       Note that this is monomorphic [let] -- for simplicity, no
       generalization is performed. *)

  | ELet (x, e1, e2) ->
      let tx = TVar (fresh()) in
      type_expr vvenv tvenv e1 tx;
      let vvenv = StringMap.add x tx vvenv in
      type_expr vvenv tvenv e2 expected

    (* At a [match] construct, we we create a fresh variable [tx] to
       stand for the type of [e], and check that every branch defines
       a function from type [tx] to type [expected]. *)

  | EMatch (e, bs) ->
      let tx = TVar (fresh()) in
      type_expr vvenv tvenv e tx;
      List.iter (fun b -> type_branch vvenv tvenv b tx expected) bs

  | ERaise e ->
      type_expr vvenv tvenv e type_of_exn

  | ETryWith(e1, x, e2) ->
      type_expr vvenv tvenv e1 expected;
      type_expr (StringMap.add x type_of_exn vvenv) tvenv e2 expected

    (* At an [exists] construct, new type variables become in scope.
       To reflect this, we extend the type environment [tvenv]. *)

  | EExists (xs, e) ->
      let tvenv = introduce_type_vars tvenv xs in
      type_expr vvenv tvenv e expected

    (* At a type annotation, we check that the type annotation matches
       the expected type. *)

  | ETypeAnnotation (e, t) ->
      translate_type tvenv t =?= expected;
      type_expr vvenv tvenv e expected

(* At a function definition, we decompose the expected type into an
   arrow type of the form [domain -> codomain]. If this is a recursive
   function named [f], we extend the value environment with a binding
   of [f] to its expected type. In all cases, we extend the value
   environment with a binding of the formal argument [x] to the type
   [domain]. Last, we check that the function's body has type
   [codomain]. *)

and type_fun vvenv tvenv f x e expected =
  let domain, codomain = decompose_arrow expected in
  let vvenv = Option.fold (fun f vvenv -> StringMap.add f expected vvenv) f vvenv in
  let vvenv = StringMap.add x domain vvenv in
  type_expr vvenv tvenv e codomain

(* To check that a branch has type [domain] to [codomain], we first
   check that the pattern [p] has type [domain], which at the same
   time extends the value environment with bindings for the pattern's
   variables; then, we check that the expression [e] has type
   [codomain] under this extended environment. *)

and type_branch vvenv tvenv (Branch (p, e)) domain codomain =
  let vvenv = type_pat vvenv p domain in
  type_expr vvenv tvenv e codomain
(*</corrige>*)

(* ------------------------------------------------------------------------- *)

(* To conclude, apply the generator to the main expression and extract the
   resulting unification problem. *)

let root =
  fresh()

let problem =
  type_expr StringMap.empty StringMap.empty main (TVar root);
  finished()

end
