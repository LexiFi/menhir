open IL

(* Building a toplevel function definition. *)

let mkfun name params body =
  StructValDef (
    name,
    EFun (params, body)
  )

let app = function
  | [] ->
      assert false
  | f :: xs ->
      List.fold_left (fun f x ->
	EApp (f, x)
      ) f xs

(* Building a simple variable value. *)

let vvar x =
  EVar (PathVar x)

let vvar1 x =
  vvar (x ^ "1")

let vvar2 x =
  vvar (x ^ "2")

let primed = function
  | EVar (PathVar x) ->
      EVar (PathVar (x ^ "'"))
  | _ ->
      assert false

let pair x y =
  ETuple [ x; y ]

let tuple xs =
  ETuple xs

let dcon d es =
  EData (PathVar d, es)

let vpair x y =
  pair (vvar x) (vvar y)

let mvar x =
  MEPath (PathVar x)

let mdot m x =
  MEPath (PathDot (m, x))

let mtvar x =
  MTEPath (PathVar x)

(* Creating types. *)

let tvar v =
  TypVar v

let tapp t ts =
  TypApp (PathVar t, ts)

let arrow t1 t2 =
  TypArrow (t1, t2)

let tcon t =
  tapp t []

(* Creating type schemes. *)

let poly qs t =
  { quantifiers = qs; body = t }

let mono t =
  poly [] t

(* Expressions. *)

let assertfalse =
  EApp (vvar "assert", vvar "false")

(* Methods. *)

let sharp e m =
  EMethodCall (e, m)

let concrete_method m e =
  CIMethod (
    Public,
    NonVirtual,
    m,
    None,
    Some e
  )

let concrete_typed_method m t e =
  CIMethod (
    Public,
    NonVirtual,
    m,
    Some t,
    Some e
  )

(* Booleans. *)

let conjunction = function
  | [] ->
      vvar "true"
  | [ b ] ->
      b
  | b :: bs ->
      List.fold_left (fun accu b ->
	EInfixApp (accu, "&&", b)
      ) b bs

