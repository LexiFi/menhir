(* This file implements the monadic translation for exceptions. *)

open AbstractSyntax

(* The translation uses the following algebraic data type:

     type ('a, 'b) %mon = | %V of 'a | %E of 'b

Results of the form %V(v) denote normal termination with value v.
Results of the form %E(v) denote abrupt termination on an uncaught exception
whose value is v.

% signs are used in the names of the type constructor and data
constructors so that they cannot conflict with names already present
in the source program.
*)

let montype : type_constructor = "%mon"
let vconstr : data_constructor = "%V"
let econstr : data_constructor = "%E"

let mondecl : data_type_definition =
  DefDataType(["a"; "b"], montype,
              [DefCon(vconstr, [TVar "a"]);
               DefCon(econstr, [TVar "b"])])

(* A generator for fresh variable names.  Again, we use a % so that
   these names cannot appear in the source code. *)

let gensym_counter = ref 0

let gensym() =
  incr gensym_counter; Printf.sprintf "%%%d" !gensym_counter

(* We provide optimizing implementations of some of the basic monadic
   operations.   Notice how the implementation of [bind] avoids
   generating administrative redexes. *)

let return (v : expression) = EConApp(vconstr, [v])

(* "bind a f" is equivalent to
      match a with
      | %V(x) -> f x
      | %E(x) -> %E(x)
   where "x" is a fresh variable.
   It avoids generating the "match" when the shape of "a" allows so. *)

let bind (a : expression) (f : expression -> expression) =
  match a with
  | EConApp(cstr, [v]) when cstr = vconstr ->
     f v
  | EConApp(cstr, [v]) when cstr = econstr ->
     EConApp(econstr, [v])
  | _ ->
     let x = gensym() in
     EMatch(a, [Branch(PConApp(vconstr, [x]), f (EVar x));
                Branch(PConApp(econstr, [x]), EConApp(econstr, [EVar x]))])

(* "bindlist al f" is similar, for a list al = [a1; ...; aN] of expressions:
      match a1 with
      | %E(x1) -> %E(x1)
      | %V(x1) ->
          ...
          match aN with
          | %E(xN) -> %E(xN)
          | %V(xN) -> f [x1, ..., xN]
*)

let rec bindlist (al : expression list) (f : expression list -> expression) =
  match al with
  | [] -> f []
  | hd :: tl ->
      bind hd (fun hd' -> bindlist tl (fun tl' -> f (hd' :: tl')))

(* <corrige> *)

(* "bindlet a x b" is equivalent to
      match a with
      | %E(x) -> %E(x)
      | %V(x) -> b
   Unlike in "bind", the name of the variable "x" is provided, a fresh
   name is not generated. *)

let bindlet (a : expression) (x : value_variable) (b : expression) =
  match a with
  | EConApp(cstr, [v]) when cstr = vconstr ->
     ELet(x, v, b)
  | EConApp(cstr, [v]) when cstr = econstr ->
     EConApp(econstr, [v])
  | _ ->
     EMatch(a, [Branch(PConApp(vconstr, [x]), b);
                Branch(PConApp(econstr, [x]), EConApp(econstr, [EVar x]))])

(* "trywith a x b" is equivalent to
      match a with
      | %E(x) -> b
      | %V(x) -> %V(x)
*)

let trywith (a : expression) (x : value_variable) (b : expression) =
  match a with
  | EConApp(cstr, [v]) when cstr = vconstr ->
     return v
  | EConApp(cstr, [v]) when cstr = econstr ->
     ELet(x, v, b)
  | _ ->
     EMatch(a, [Branch(PConApp(vconstr, [x]), EConApp(vconstr, [EVar x]));
                Branch(PConApp(econstr, [x]), b)])

(* The translation of a type involves a number of fresh type variables,
   which stand for (unknown) effects, or types of exceptions. *)

let transl_type (ty : typ) : type_variable list * typ =

  let new_vars = ref [] in

  let make_mon_type ty =
    let v = gensym() in
    new_vars := v :: !new_vars;
    TConApp(montype, [ty; TVar v]) in

  let rec transl_value_type ty =
    match ty with
    | TVar v -> TVar v
    | TArrow(t1, t2) ->
	(* The translation of an arrow type involves inserting a monad in
	   the codomain: [[t1 -> t2]] is [[t1] -> %mon ([t2], 'b)], where ['b]
	   is a fresh type variable, standing for the (unknown) effect of
	   this arrow. *)
        TArrow(transl_value_type t1, make_mon_type (transl_value_type t2))
    | TConApp(cstr, tl) ->
        TConApp(cstr, List.map transl_value_type tl) in

  let ty' = make_mon_type (transl_value_type ty) in
  (!new_vars, ty')

let rec transl_expr (e : expression) : expression =
  match e with
  | EVar v -> return (EVar v)
  | EFun(x, a) -> return (EFun(x, transl_expr a))
  | ERecFun(f, x, a) -> return (ERecFun(f, x, transl_expr a))
  | EApp(a, b) ->
      bind (transl_expr a) (fun a' ->
      bind (transl_expr b) (fun b' -> EApp(a', b')))
  | EConApp(cstr, al) ->
      bindlist (List.map transl_expr al)
               (fun al' -> return (EConApp(cstr, al')))
  | ELet(x, a, b) ->
      bindlet (transl_expr a) x (transl_expr b)
  | EMatch(a, branches) ->
      bind (transl_expr a) (fun a' ->
      EMatch(a', List.map transl_branch branches))
  | ERaise a ->
      bind (transl_expr a) (fun a' -> EConApp(econstr, [a']))
  | ETryWith(a, x, b) ->
      trywith (transl_expr a) x (transl_expr b)
  | EExists(vars, a) ->
      EExists(vars, transl_expr a)
  | ETypeAnnotation(a, ty) ->
      (* The fresh type variables [vars] that appear in the translated type
	 annotation [ty'] are existentially bound, so that the type inference
	 engine will infer appropriate values. *)
      let (vars, ty') = transl_type ty in
      EExists(vars, ETypeAnnotation(transl_expr a, ty'))

and transl_branch (Branch(pat, expr)) =
  Branch(pat, transl_expr expr)
(*</corrige>*)

(* [transl_program p] takes a program [p] written in full mini-ML,
   including the [raise] and [try...with] constructs, and returns
   a program [p'] in pure mini-ML, without the [raise] and [try...with]
   constructs. *)

(*<sujet>
let transl_program (Program(datatypes, body)) =
  Program(mondecl :: datatypes, body) (* translation of [body] to be implemented *)
</sujet>*)

(*<corrige>*)
let transl_program (Program(datatypes, body)) =
  Program(mondecl :: datatypes, transl_expr body)
(*</corrige>*)

