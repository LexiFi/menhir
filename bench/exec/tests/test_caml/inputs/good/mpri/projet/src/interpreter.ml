(* This file defines an interpreter for our small programming language *)

open Printf
open AbstractSyntax

(* This exception is raised when the interpreter encounters a fatal error *)
exception EvaluationError of string

(* Finite maps from value_variables to some data type *)
module Env =
  Map.Make(struct type t = value_variable let compare = compare end)

(* The representation of values (results of evaluation) *)
type value =
  | Con of data_constructor * value list
(*<sujet>
  (* | ??? *)
</sujet>
<corrige>*)
  | Closure of value_variable * expression * environment
  | RecClosure of value_variable * value_variable * expression * environment

(* [Closure(x, a, e)] is the closure of the non-recursive function
   [EFun(x,a)] in the environment [e].
   [RecClosure(f, x, a, e)] is the closure of the recursive function 
   [ERecFun(f, x, a)] in the environment [e].
*)

(*</corrige>*)

(* Environments are finite maps from value_variables to values *)
and environment = value Env.t

(* Return the value associated with [var] in [env]. *)
let find_var var env =
  try Env.find var env
  with Not_found -> raise (EvaluationError ("unbound variable " ^ var))

(* Return an environment that extends [env] with a binding from 
   [var] to [value] *)
let bind_var var value env =
  Env.add var value env

(* Print a value on standard output *)
let rec print_value v =
  match v with
  | Con(cstr, []) ->
      printf "%s" cstr
  | Con(cstr, v1 :: vl) ->
      printf "%s(" cstr;
      print_value v1;
      List.iter (fun v -> printf ", "; print_value v) vl;
      printf ")"
(*<sujet>
  (* | ??? *)
</sujet>
<corrige>*)
  | Closure(_, _, _) ->
      printf "<fun>"
  | RecClosure(_, _, _, _) ->
      printf "<fun>"
(*</corrige>*)

(* Exception raised by [eval env expr] if the evaluation of [expr]
   raises an exception and does not catch it.  The argument is the
   value of the exception. *)

exception UncaughtException of value

(* [eval env expr] returns the value of expression [expr] in environment [env] *)
let rec eval env expr =
(*<sujet>
  raise (EvaluationError "to be implemented")
</sujet>
<corrige>*)
  (* This follows the structure of the ``canonical efficient interpreter''
     in lecture 1. *)
  match expr with
  | EVar v ->
      find_var v env
  | EFun(x, a) ->
      Closure(x, a, env)
  | ERecFun(f, x, a) ->
      RecClosure(f, x, a, env)
  | EApp(a, b) ->
      (* [a] must evaluate to a closure, but it can either be non-recursive
         or recursive. *)
      let va = eval env a in
      let vb = eval env b in
      begin match va with
      | Closure(param, body, env') ->
          (* The non-recursive case is as in lecture 1: we bind the
             formal parameter [param] to the value [eval env b]
             of the actual argument [b], then evaluate the function body. *)
          eval (bind_var param vb env') body
      | RecClosure(fun_id, param, body, env') as clos ->
          (* For the recursive case, we must also bind the function name
             [fun_id] to its value, which is the recursive closure [clos]
             itself. *)
          eval (bind_var param vb (bind_var fun_id clos env')) body
      | _ ->
          (* This should never happen if the program is well-typed. *)
          raise (EvaluationError "application of a non-function")
      end
  | EConApp(cstr, args) ->
      Con(cstr, eval_list env args)
  | ELet(x, a, b) ->
      let va = eval env a in
      eval (bind_var x va env) b
  | EMatch(a, branches) ->
      begin match eval env a with
      | Con(cstr, vl) ->
          eval_match env cstr vl branches
      | _ ->
          (* This should never happen if the program is well-typed. *)
          raise (EvaluationError "argument of `match' is not a data type")
      end
  | ERaise a ->
      raise (UncaughtException (eval env a))
  | ETryWith(a, x, b) ->
      begin try
        eval env a
      with UncaughtException vexn ->
        eval (bind_var x vexn env) b
      end
  (* EExists and ETypeAnnotation are just annotations for the type inference
     engine.  They play no role in evaluation. *)
  | EExists(ty_vars, a) ->
      eval env a
  | ETypeAnnotation(a, ty) ->
      eval env a

and eval_list env exprlist =
  (* Evaluate a list of expressions and return the list of their values. *)
  match exprlist with
  | [] -> []
  | e1 :: el -> 
      (* Force left-to-right evaluation *)
      let v1 = eval env e1 in
      let vl = eval_list env el in
      v1 :: vl
(*</corrige>*)

(* [eval_match env cstr args branches] evaluates a pattern-matching.
   The value being matched is [Con(cstr, args)].  *)

and eval_match env cstr args branches =
(*<sujet>
  raise (EvaluationError "to be implemented")
</sujet>
<corrige>*)
  match branches with
  | [] ->
      raise (EvaluationError ("no matching branch for " ^ cstr))
  | Branch(PConApp(cstr', vars), action) :: remainder ->
      if cstr = cstr' then
        if List.length vars = List.length args then
          (* If [vars = [x1;...;xN]] and [args = [v1;...vN]],
             [List.fold_right2 bind_var vars args env] is
             [bind_var x1 v1 (... (bind_var xN vN env))]. *)
          eval (List.fold_right2 bind_var vars args env) action
        else
          (* This should never happen if the program is well-typed. *)
          raise (EvaluationError("arity mismatch on constructor " ^ cstr))
      else
        eval_match env cstr args remainder
(*</corrige>*)

(* Evaluate the given program and print its value. *)
let eval_program (Program(datatypes, body)) =
  try
    print_value (eval Env.empty body); print_newline()
  with
  | UncaughtException vexn ->
      print_string "Uncaught exception "; print_value vexn; print_newline()
  | EvaluationError msg ->
      printf "Error during evaluation: %s\n" msg
