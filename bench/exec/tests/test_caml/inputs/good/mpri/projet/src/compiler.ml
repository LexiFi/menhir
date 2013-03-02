(* This file defines a compiler from our programming language
   to our abstract machine. *)

open AbstractSyntax
open Machine

(* Exception used to report compilation errors *)
exception CompilationError of string

(*<corrige>*)
(* The source language uses names to identify variables, while the abstract
   machine uses de Bruijn indices to access the environment:
   the value of variable [x] is the [n]-th slot of the environment,
   where [n] is the de Bruijn index of [x] (with the convention
   that indices start at 0).  

   One possibility is to translate the source language to de Bruijn
   indices first, resulting in an abstract syntax tree where [EVar]
   carries an index instead of a variable name, then translate this
   modified source term to machine code as shown in lecture 2.

   However, it is not difficult (and globally simpler) to translate
   from names to indices on the fly during compilation.  To this end,
   the compilation functions are parameterized by a compilation
   environment, which is a list of variable names [x0; ...; xN] in the
   order in which they are bound: [x0] is the name of the most
   recently bound variable, [x1] the name bound just before, etc.
   Therefore, the variable named [xi] has index [i] and the machine can
   recover its value using the [IAcc i] instruction. *)

type compilation_environment = string list

(* [index_of_var v cenv] returns the index of the variable named [v]
   according to the compilation environment [cenv].  *)

let rec index_of_var v cenv =
  match cenv with
  | [] -> raise (CompilationError("unbound variable " ^ v))
  | v0 :: cenv' -> if v = v0 then 0 else 1 + index_of_var v cenv'

(* Auxiliary function that generates the code to bind the arguments of
   a constructor to the variables [vars], then continues with the code [k].
   We assume that the constructed value is initially at the top of the stack.
   In other terms, if the machine state before executing the code
   [bind_match_vars vars k] is 
       stack:  Cstr(v0, ..., vN) :: s      env:  e
   the machine state before executing the code [k] is
       stack:  s                           env:  vN :: ... :: v0 :: e
   Note that the order in which we add the fields to the environment
   does not matter, as long as the compilation environment is modified
   accordingly.  With the order chosen here, the compilation environment
   to compile the right-hand-side of the branch must be
   [xN :: ... :: x0 :: cenv], or in other terms [List.rev_append vars env].
*)

let bind_match_vars vars k =
  let rec bind pos vars =
    match vars with
    | [] -> IDrop :: k
    | [v] -> IField pos :: ILet :: k
    | v1 :: vl -> IDup :: IField pos :: ILet :: bind (pos + 1) vl
  in bind 0 vars

(* This is a "smart constructor" for the [IEndlet] instruction.
   It returns code equivalent to [IEndlet n :: k], but if [k]
   itself starts with an [IEndlet n'] instruction, the two [IEndlet]
   are merged into a single [IEndlet (n+n')].  Also, if [n = 0],
   we do not insert any instruction. *)

let iendlet n k =
  if n = 0 then k else
    match k with
    | IEndlet n' :: k' -> IEndlet (n + n') :: k'
    | _ -> IEndlet n :: k

(* The compilation scheme is similar to that given in lecture 2 for
   the Modern SECD, including tail-call elimination.
   [compile_expr] corresponds to the [C] compilation scheme in the slides,
   [compile_tail_expr] to the [T] compilation scheme. *)

(* [compile_expr cenv expr k] produces the instructions that compute [expr],
   leave the machine value of [expr] at the top of the stack,
   and continue with the code [k].  [cenv] is the compilation environment
   appropriate for translating variable names in [expr] to their indices. *)

let rec compile_expr cenv expr k =
  match expr with
  | EVar v ->
      IAccess (index_of_var v cenv) :: k
  | EFun(x, a) ->
      IClosure (compile_tail_expr (x :: cenv) a) :: k
  | ERecFun(f, x, a) ->
      IRecClosure (compile_tail_expr (x :: f :: cenv) a) :: k
  | EApp(a, b) ->
      compile_expr cenv a (compile_expr cenv b (IApply :: k))
  | EConApp(cstr, args) ->
      compile_expr_list cenv args (IConstr(cstr, List.length args) :: k)
  | ELet(x, a, b) ->
      compile_expr cenv a (ILet :: compile_expr (x :: cenv) b (iendlet 1 k))
  | EMatch(a, branches) ->
      compile_expr cenv a (compile_branches cenv branches @ k)
  | EExists(ty_vars, a) ->
      compile_expr cenv a k
  | ETypeAnnotation(a, ty) ->
      compile_expr cenv a k
  | ETryWith _
  | ERaise _ ->
      assert false (* these constructs are eliminated by the monadic translation *)

and compile_expr_list cenv exprs k =
  match exprs with
  | [] -> k
  | e1 :: el -> compile_expr cenv e1 (compile_expr_list cenv el k)
  
(* [compile_branches] compiles the branches for a pattern-matching.
   The generated code assumes that the constructed value being 
   scrutinized is initially at the top of the stack. *)

and compile_branches cenv branches =
  match branches with
  | [] ->
      (* No branch matches, so we just halt the machine on error. *)
      [IHalt]
  | Branch(PConApp(cstr, vars), action) :: remainder ->
      (* We must test the constructor of the scrutinee, using an
         [IIfconstr(cstr)] instruction.  However, this instruction
         consumes its argument at the top of the stack, while we
         need to keep it, to test other branches if the test fails
         or to bind the arguments of the constructed value to 
         pattern variables if the test succeeds.  We therefore
         duplicate the top of the stack before testing. *)
      [IDup;
       IIfconstr(cstr,
                 bind_match_vars vars
                   (compile_expr (List.rev_append vars cenv) action
                      (iendlet (List.length vars) [])),
                 compile_branches cenv remainder)]
       (* If the test succeeds, execution proceeds with the code that
          1- binds the arguments of the constructed value to the
             pattern variables [vars], using [bind_match_vars];
          2- evaluates the right-hand-side expression [action],
             leaving its value at the top of the stack;
          3- restores the machine environment to what it was at
             the beginning of the match; to this end, we need to
             remove the bindings introduced by [bind_match_vars]
             with an [IEndlet (List.length vars)] instruction.

          If the test fails, we proceed with the code that tests
          the other alternatives of the pattern-matching ([remainder]). *)

(* [compile_tail_expr cenv expr] produces the instructions that 
   compute [expr] and return its value to the calling function.
   It should be applied only to expressions [expr] that are in
   tail-call position in the source program. *)

and compile_tail_expr cenv expr =
  match expr with
  | EApp(a, b) ->
      (* Applications in tail-call position becomes [ITailapply]
         instructions. *)
      compile_expr cenv a (compile_expr cenv b [ITailapply])
  | ELet(x, a, b) ->
      (* [b] is in tail-call position, but not [a]. *)
      compile_expr cenv a (ILet :: compile_tail_expr (x :: cenv) b)
  | EMatch(a, branches) ->
      (* The right-hand-sides of [branches] are in tail-call position,
         but not [a]. *)
      compile_expr cenv a (compile_tail_branches cenv branches)
  | EExists(ty_vars, a) ->
      compile_tail_expr cenv a
  | ETypeAnnotation(a, ty) ->
      compile_tail_expr cenv a
  | _ ->
      (* Default case: just compile [expr] normally and stick a
         [IReturn] instruction after. *)
      compile_expr cenv expr [IReturn]

(* [compile_tail_branches] is similar to [compile_branches] but
   applies to matches in tail-call position.  The differences with
   [compile_branches] are:
   1- the right-hand-sides are compiled with [compile_tail_expr]
      instead of [compile_expr];
   2- there is no need to add [IEndlet] instructions at the end of
      right-hand-sides, because they return immediately, discarding
      the current environment. *)

and compile_tail_branches cenv branches =
  match branches with
  | [] ->
      [IHalt]
  | Branch(PConApp(cstr, vars), action) :: remainder ->
      [IDup;
       IIfconstr(cstr,
                 bind_match_vars vars
                   (compile_tail_expr (List.rev_append vars cenv) action),
                 compile_tail_branches cenv remainder)]
(*</corrige>*)

(* Return the machine code (of type [Machine.code]) corresponding to
   the given source program. *)

let compile_program (Program(datatypes, body)) =
(*<sujet>
  raise (CompilationError "to be implemented")
</sujet>*)
(*<corrige>*)
  compile_expr [] body []
(*</corrige>*)
