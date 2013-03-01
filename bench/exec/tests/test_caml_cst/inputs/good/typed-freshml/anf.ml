open Source
open Typed

(* ------------------------------------------------------------------------- *)

(* Expressions and values are distinguished. *)

type raw_expression =
  | EValue of value list
  | EFresh of var list * expression
  | ELetCall of var list * callee * value list * expression
  | ELetWhere of var list * contrainte * expression * expression
  | EAssert of assertion * contrainte * expression
  | ECase of value list * (pattern list * expression) list
  | EAbsurd
  | EFail
  | ENextCase

and expression =
    raw_expression Location.t

(* ------------------------------------------------------------------------- *)

(* Function definitions. *)

type fundef =
    var list        (* arguments *)
  * contrainte      (* precondition, dependent on arguments *)
  * var list        (* results *)
  * contrainte      (* postcondition, dependent on arguments and results *)
  * expression      (* function body *)

(* ------------------------------------------------------------------------- *)

(* Printers. *) (* TEMPORARY add indentation *)

open Printf
open Print

let print_variable env buffer x =
  bprintf buffer "%s" (Annotation.content (Var.AtomIdMap.find x env))

let print_variables env buffer xs =
  seplist comma (print_variable env) buffer xs

let rec print_value env buffer = function
  | VVar x ->
      print_variable env buffer x
  | VBool b ->
      bprintf buffer "%b" b
  | VTagTuple (tag, tuple, _) ->
      bprintf buffer "%s(%a)" (SymbolTable.tagname tag) (print_tuple env) tuple

and print_tuple env buffer tuple =
  let vs = Layout.fold (fun v vs -> v :: vs) tuple [] in
  print_value_list env buffer vs

and print_value_list env buffer vs =
  seplist comma (print_value env) buffer vs

let kwhere buffer =
  bprintf buffer " where "

let kassert buffer =
  bprintf buffer " assert "

let print_guard _ _ buffer _ =
  bprintf buffer "where <constraints>" (* TEMPORARY print the constraints! *)

let extend x env =
  Var.AtomIdMap.add x env

let extends xs env =
  List.fold_right extend xs env

let rec extendp p env =
  match p with
  | VVar x ->
      extend x env
  | VBool _ ->
      env
  | VTagTuple (_, tuple, _) ->
      Layout.fold extendp tuple env

let extendps ps env =
  List.fold_right extendp ps env

let rec print_expression env buffer e =
  match Annotation.content e with
  | EValue vs ->
      print_value_list env buffer vs
  | EFresh (xs, e) ->
      let env = extends xs env in
      bprintf buffer "fresh %a in %a" (print_variables env) xs (print_expression env) e
  | ELetCall (xs, callee, vs, e) ->
      let env' = extends xs env in
      bprintf buffer "let %a = %s(%a) in %a"
	(print_variables env') xs
	(Primitive.print_callee callee)
	(print_value_list env) vs
	(print_expression env') e
  | ELetWhere (xs, guard, e1, e2) ->
      let env' = extends xs env in
      bprintf buffer "let %a%a = %a in %a"
	(print_variables env') xs
	(print_guard kwhere env') guard
	(print_expression env) e1
	(print_expression env') e2
  | EAssert (assertion, claim, e) ->
      bprintf buffer "%s %a in %a"
	(match assertion with StaticAssertion -> "assert" | DynamicAssertion -> "check")
	(print_guard kassert env) claim
	(print_expression env) e
  | ECase (vs, branches) ->
      bprintf buffer "case %a of%a\nend"
	(print_value_list env) vs
	(preclist nl (print_branch env)) branches
  | EAbsurd ->
      bprintf buffer "absurd"
  | EFail ->
      bprintf buffer "fail"
  | ENextCase ->
      bprintf buffer "next case"

and print_branch env buffer (ps, e) =
  let env = extendps ps env in
  bprintf buffer "\n| %a -> %a"
    (print_value_list env) ps
    (print_expression env) e

let print_fundef buffer f (xs, pre, ys, post, body) =
  let env = extends xs Var.AtomIdMap.empty in
  let env' = extends ys env in
  bprintf buffer "fun %s accepts %a%a produces %a%a =\n%a\n"
    (SymbolTable.funname f)
    (print_variables env) xs
    (print_guard kwhere env) pre
    (print_variables env') ys
    (print_guard kwhere env') post
    (print_expression env) body

let print_fundefs buffer fundefs =
  Valfun.AtomMap.iter (print_fundef buffer) fundefs

(* ------------------------------------------------------------------------- *)

(* Contexts. *)

type context =
  | CEmpty
  | CCompose of context * context
  | CLet of binding Location.t

and binding =
  | BValue of value list * var list * contrainte
  | BExpr of expression * var list * contrainte
  | BCall of callee * value list * var list * contrainte

(* Building contexts. *)

let compose c1 c2 =
  CCompose (c1, c2)

(* Applying a context to an expression. *)

let vvar x =
  VVar x

let vvars xs =
  List.map vvar xs

let rec fill (c : context) (e : expression) : expression =
  match c with
  | CEmpty ->
      e
  | CCompose (c1, c2) ->
      fill c1 (fill c2 e)
  | CLet binding ->
      let pretend claim e =
	match claim with
	| FTrue ->
	    e
	| _ ->
	    Annotation.transfer binding (EAssert (StaticAssertion, claim, e))
      in
      Annotation.map (function
	| BValue (vs, xs, claim) ->
	    ECase (vs, [ vvars xs, pretend claim e ])
	| BExpr (e0, xs, claim) ->
	    ELetWhere (xs, claim, e0, e)
	| BCall (f, vs, xs, claim) ->
	    ELetCall (xs, f, vs, pretend claim e)
      ) binding

(* ------------------------------------------------------------------------- *)

(* [suggestion typ] maps types to base name suggestions. *)

let suggestion = function
  | TAtom ->
      "atom"
  | TAtomSet ->
      "atom_set"
  | TBool ->
      "bool"
  | TData t ->
      SymbolTable.typename t

(* [name e] produces a vector of fresh names whose length matches the
   result arity of [e]. Whenever a new name is produced, its type is
   recorded, so that we can still answer [typeof] queries for the
   transformed program. *)

let table : Source.typ Var.AtomMap.t ref =
  ref Var.AtomMap.empty

let name arity =
  List.map (fun typ ->
    let typ = Lazy.force typ in
    let base = "?" ^ suggestion typ in
    let x = Source.Var.Atom.freshb (Annotation.make Location.dummy base) in
    table := Var.AtomMap.add x typ !table;
    x
  ) arity

let typeof x =
  try
    Var.AtomMap.find x !table
  with Not_found ->
    SymbolTable.typeof x

let typeofv = function
  | VVar x ->
      typeof x
  | VBool _ ->
      TBool
  | VTagTuple (tag, _, _) ->
      SymbolTable.tagtype tag
	(* TEMPORARY this simple-minded approach will fail with parameterized datatypes *)

(* ------------------------------------------------------------------------- *)

(* [valueify] converts a single expression into a pair of a
   context and a vector of values, whose length is the expression's
   result arity. *)

let rec valueify e : context * value list =
  let loc x = Annotation.make e.loc x in
  let clet b = CLet (loc b) in
  let arity = e.arity in
  match e.content with

  | Typed.EVar x ->

      (* Variable. Trivial translation. *)

      CEmpty, [ VVar x ]

  | Typed.EBool b ->

      CEmpty, [ VBool b ]

  | Typed.ETagTuple (tag, estruct, guard) ->

      (* Data constructor application. Translate the arguments to
	 values, extracting a computational context if necessary, and
	 produce a structured value. *)

      let context, vstruct = valueify_struct estruct in
      context, [ VTagTuple (tag, vstruct, loc guard) ]

  | Typed.EMulti es ->

      (* Tuple. Translate the components to values, extracting a
	 computational context if necessary, and produce a list of
	 values. *)

      List.fold_right (fun e (contexts, vs) ->
	let context, v = valueify e in
	assert (List.length v = 1);
	compose context contexts, v @ vs
      ) es (CEmpty, [])

  | Typed.ECall (f, e) ->

      (* Function call. Translate the arguments to values. Since the
	 target language only has [ELetCall], generate fresh names for
	 the results. *)

      let context, vs = valueify e in
      let xs = name arity in 
      compose
        context
        (clet (BCall (f, vs, xs, FTrue))),
      vvars xs

  | Typed.EFresh _
  | Typed.ECase _
  | Typed.EAbsurd
  | Typed.EFail
  | Typed.ENextCase
  | Typed.EAssert _ ->

      (* Arbitrary computational form. Move it out of the way by
	 introducing [let]-bound fresh names. Note that this generated
	 [let] construct carries no claim. *)

      let xs = name arity in
      clet (BExpr (convert e, xs, FTrue)),
      vvars xs

  | Typed.ELetMulti (e1, xs, claim, e2) ->

      (* Source-level [let] construct. See below. *)

      let context2, vs2 = valueify e2 in
      compose (translate_left_hand_side e1 xs claim) context2, vs2

(* The source language only has one [let] construct, possibly carrying a
   [where] claim, whereas the target language has three forms, [ELetWhere],
   [ELetCall], and [ECase] -- where the latter is used to bind variables to
   values and record the corresponding equations between values.

   It is sometimes not easy to decide how a source-level [let] construct
   should be translated. The rule that we adopt is this. If the left-hand side
   is a function call, we use [ELetCall], regardless of whether a claim was
   provided, since claims do not help in that case. (The claim is not lost,
   though. It is still checked.) Otherwise, if there is a claim, then we
   translate to [ELetWhere], even if the left-hand side has some value
   structure; this means that only the claim will be used and the value
   structure will not be recorded. If there is no claim, then we translate to
   a combination of [ELetWhere], for the computational part, and [ECase], for
   the value part. *)

and translate_left_hand_side e1 xs claim =
  let loc x = Annotation.make e1.loc x in
  let clet b = CLet (loc b) in
  match e1.content, claim with

  | Typed.ECall (f, e1), _ ->

      (* The left-hand side is a function call. Translate to [ELetCall],
	 without dropping the claim, if there is one. *)

      let context1, vs1 = valueify e1 in
      compose context1 (clet (BCall (f, vs1, xs, claim)))

  | _, FTrue ->

      (* There is no claim. Decompose the left-hand side into a computational
	 part and a value part, and use [ECase] to bind the latter. *)

      let context1, vs1 = valueify e1 in
      compose context1 (clet (BValue (vs1, xs, claim)))

  | _, _ ->

      (* There is a claim. Regardless of the structure of the left-hand side,
	 translate to [ELetWhere]. Note that, if the left-hand side begins
	 with a [let] or [fresh] construct, we do not attempt to lift that
	 construct out -- so this is not quite A-normal form. *)

      clet (BExpr (convert e1, xs, claim))   


(* ------------------------------------------------------------------------- *)

(* [valueify_struct] converts a structured tuple of expressions into a
   pair of a context and a vector of values. Each expression is
   assumed to construct a single value, that is, to have arity 1. *)

and valueify_struct estruct : context * value Typed.structured_tuple =
  Layout.farm CEmpty compose valueify1 estruct

and valueify1 e =
  match valueify e with
  | context, [ v ] ->
      context, v
  | _ ->
      assert false (* [e] has arity 1 *)

(* ------------------------------------------------------------------------- *)

(* Converting expressions. *)

and convert (e : Typed.expression) : expression =
  let loc content = Annotation.make e.loc content in
  match e.content with
  | Typed.EVar _
  | Typed.EBool _
  | Typed.ETagTuple _

      (* The expression exhibits toplevel value structure. Convert it to a
	 value, lifting the computational context to the outside. *)

  | Typed.EMulti _

      (* Tuple. Proceed as above -- indeed, the target language only offers
	 tuples of values, not tuples of computational expressions. (A direct
	 treatment of these would be problematic, as it would require
	 splitting the postcondition.) *)

  | Typed.ECall _ ->

      (* Function call. Translate to [ELetCall], because the target language
	 requires it. *)

      let context, vs = valueify e in
      fill context (loc (EValue vs))

  | Typed.ELetMulti (e1, xs, claim, e2) ->

      (* Source-level [let] construct. Proceed as above. *)

      fill (translate_left_hand_side e1 xs claim) (convert e2)

  (* The computational forms below have straightforward translations.
     Note that we do not introduce [let]-bound fresh names, as in
     [valueify]. Doing so would be harmful, as it would prevent the
     postcondition from being properly propagated into the expression
     that appears in tail position. *)

  | Typed.EFresh (xs, e) ->
      loc (EFresh (xs, convert e))
  | Typed.ECase (e, branches) ->
      let context, vs = valueify e in
      fill context (loc (ECase (vs, List.map convert_branch branches)))
  | Typed.EAssert (assertion, cs, e) ->
      loc (EAssert (assertion, cs, convert e))
  | Typed.EAbsurd ->
      loc EAbsurd 
  | Typed.EFail ->
      loc EFail
  | Typed.ENextCase ->
      loc ENextCase

and convert_branch (ps, e) =
  ps, convert e

(* ------------------------------------------------------------------------- *)

(* Converting function definitions. *)

let fundefs =
  Valfun.AtomMap.map (fun (xs, pre, ys, post, body) ->
    xs, pre, ys, post, convert body
  ) SymbolTable.fundefs

