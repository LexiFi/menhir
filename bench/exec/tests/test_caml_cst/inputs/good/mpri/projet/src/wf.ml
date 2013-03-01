open AbstractSyntax

(* ------------------------------------------------------------------------- *)

(* Type schemes. *)

type scheme =
  | Scheme of type_variable list * typ list * typ

type data_constructor_table =
    scheme StringMap.t

(* ------------------------------------------------------------------------- *)

(* Basic checks and error messages. *)

(* No attempt is made at providing accurate locations. *)

let unbound category x =
  Error.error [] (Printf.sprintf "Unbound %s: %s." category x)

let duplicate category x =
  Error.error [] (Printf.sprintf "Duplicate %s: %s." category x)

let arity_check category c (expected : int) (actuals : 'a list) =
  let got = List.length actuals in
  if expected <> got then
    Error.error [] (
      Printf.sprintf
	"The %s %s expects %d parameters,\n\
	 but is here applied to %d arguments."
	category c expected got
    )

let add category x d m =
  try
    StringMap.strict_add x d m
  with StringMap.StrictAdd _ ->
    duplicate category x

let lookup category x m =
  try
    StringMap.find x m
  with Not_found ->
    unbound category x

(* ------------------------------------------------------------------------- *)

(* Check that a type is well-formed under a map [tcenv] of type
   constructors to integer arities and a set [tvenv] of type
   variables. *)

let rec check_typ tcenv tvenv = function
  | TVar x ->
      lookup "type variable" x tvenv
  | TArrow (t1, t2) ->
      check_typs tcenv tvenv [ t1; t2 ]
  | TConApp (tc, ts) ->
      let expected = lookup "type constructor" tc tcenv in
      arity_check "type constructor" tc expected ts;
      check_typs tcenv tvenv ts

and check_typs tcenv tvenv ts =
  List.iter (check_typ tcenv tvenv) ts

(* ------------------------------------------------------------------------- *)

(* Check that a data constructor definition is well-formed. Construct a map
   [dcenv] of data constructors to their argument types. This is where we
   build the type scheme associated with each data constructor. *)

let tvar x =
  TVar x

let check_datacon_definition tcenv tvenv xs tc dcenv (DefCon (tag, ts)) =
  check_typs tcenv tvenv ts;
  let scheme = Scheme (xs, ts, TConApp (tc, List.map tvar xs)) in
  add "data constructor" tag scheme dcenv

(* ------------------------------------------------------------------------- *)

(* Check that (the right-hand side of) a data type definition is
   well-formed. Construct [dcenv]. *)

let tvars xs =
  List.fold_right (fun x tvenv ->
    add "type variable" x () tvenv
  ) xs StringMap.empty

let check_datatype_definition tcenv dcenv (DefDataType (xs, tc, dcdefs)) =
  List.fold_left (check_datacon_definition tcenv (tvars xs) xs tc) dcenv dcdefs

(* ------------------------------------------------------------------------- *)

(* Check that the data type definitions are well-formed. Construct
   [tcenv] and [dcenv]. *)

let check_datatype_definitions defs =
  let tcenv =
    List.fold_right (fun (DefDataType (xs, tc, _)) tcenv ->
      add "type constructor" tc (List.length xs) tcenv
    ) defs StringMap.empty
  in
  let dcenv =
    List.fold_left (check_datatype_definition tcenv) StringMap.empty defs
  in
  tcenv, dcenv

(* ------------------------------------------------------------------------- *)

(* Check that a data constructor application is well-formed with
   respect to [dcenv] -- that is, the data constructor exists and is
   applied to an appropriate number of arguments. *)

let check_conapp dcenv dc actuals =
  let Scheme (_, ts, _) = lookup "data constructor" dc dcenv in
  arity_check "data constructor" dc (List.length ts) actuals

(* ------------------------------------------------------------------------- *)

(* Check that a pattern is well-formed with respect to [dcenv]. Construct
   the pattern's domain. *)

let check_pat_var dcenv domain x =
  add "value variable" x () domain

let check_pat dcenv (PConApp (dc, xs)) =
  check_conapp dcenv dc xs;
  List.fold_left (check_pat_var dcenv) StringMap.empty xs

(* ------------------------------------------------------------------------- *)

(* Check expressions and branches. *)

let introduce x vvenv =
  StringMap.add x () vvenv

let rec check_branch tcenv dcenv tvenv vvenv (Branch (p, e)) =
  let vvenv = StringMap.union vvenv (check_pat dcenv p) in
  check_exp tcenv dcenv tvenv vvenv e

and check_exp tcenv dcenv tvenv vvenv = function
  | EVar x ->
      lookup "value variable" x vvenv
  | EFun (x, e) ->
      let vvenv = introduce x vvenv in
      check_exp tcenv dcenv tvenv vvenv e
  | ERecFun (f, x, e) ->
      let vvenv = introduce f vvenv in
      let vvenv = introduce x vvenv in
      check_exp tcenv dcenv tvenv vvenv e
  | EApp (e1, e2) ->
      check_exps tcenv dcenv tvenv vvenv [ e1; e2 ]
  | EConApp (dc, es) ->
      check_conapp dcenv dc es;
      check_exps tcenv dcenv tvenv vvenv es
  | ELet (x, e1, e2) ->
      check_exp tcenv dcenv tvenv vvenv e1;
      let vvenv = introduce x vvenv in
      check_exp tcenv dcenv tvenv vvenv e2
  | EMatch (e, bs) ->
      check_exp tcenv dcenv tvenv vvenv e;
      List.iter (check_branch tcenv dcenv tvenv vvenv) bs
  | ERaise e ->
      check_exp tcenv dcenv tvenv vvenv e
  | ETryWith(e1, x, e2) ->
      check_exp tcenv dcenv tvenv vvenv e1;
      let vvenv = introduce x vvenv in
      check_exp tcenv dcenv tvenv vvenv e2
  | EExists (xs, e) ->
      let tvenv = StringMap.union tvenv (tvars xs) in
      check_exp tcenv dcenv tvenv vvenv e
  | ETypeAnnotation (e, t) ->
      check_exp tcenv dcenv tvenv vvenv e;
      check_typ tcenv tvenv t

and check_exps tcenv dcenv tvenv vvenv es =
  List.iter (check_exp tcenv dcenv tvenv vvenv) es

(* ------------------------------------------------------------------------- *)

(* Check that a program is well-formed and return the data constructor
   table. *)

let check_program (Program (defs, e)) =
  let tcenv, dcenv = check_datatype_definitions defs in
  check_exp tcenv dcenv StringMap.empty StringMap.empty e;
  dcenv

