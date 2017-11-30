open Syntax
open SortUnification

(* -------------------------------------------------------------------------- *)

(* An environment maps (terminal and nonterminal) symbols to unification
   variables. *)

type symbol =
  string

module Env =
  StringMap

type env =
  variable Env.t

let find x env : variable =
  try
    Env.find x env
  with Not_found ->
    assert false (* unbound terminal or nonterminal symbol *)

let extend env (xvs : (symbol * variable) list) =
  List.fold_left (fun env (x, v) ->
    Env.add x v env
  ) env xvs

(* -------------------------------------------------------------------------- *)

(* [allocate xs] allocates a fresh unification variable [v] for every element
   [x] of the list [xs]. It returns the lists [xvs] and [vs]. *)

let allocate (xs : 'a list) : ('a * variable) list * variable list =
  let xvs = List.map (fun x -> x, fresh()) xs in
  let  vs = List.map snd xvs in
  xvs, vs

(* -------------------------------------------------------------------------- *)

(* [check_parameter env param expected] checks that the parameter [param] has
   sort [expected]. A parameter is either a symbol or an application of a
   symbol to a number of parameters. Every application is total -- the
   language does not have partial applications. The sort of every application
   is [star], but the sort of a variable is unrestricted. *)

let rec check_parameter env (param : parameter) (expected : variable) =
  match param with
  | ParameterVar x ->
      let x = Positions.value x in
      unify (find x env) expected
  | ParameterApp (x, actuals) ->
      let x = Positions.value x in
      (* This application has sort [star]. *)
      unify star expected;
      (* Retrieve the expected sort of each parameter. The call to
         [domain] cannot fail because every nonterminal symbol has
         an arrow sort. *)
      let expected = domain (find x env) in
      (* TEMPORARY check arity *)
      (* Check the sort of each actual parameter. *)
      List.iter2 (check_parameter env) actuals expected
  | ParameterAnonymous _ ->
      (* Anonymous rules have been eliminated already. *)
      assert false

(* -------------------------------------------------------------------------- *)

(* The following functions respectively check that a producer, a branch,
   a rule, and a grammar are well-sorted under an environment [env]. *)

let check_producer env (producer : producer) =
  let (_, param, _) = producer in
  (* A producer must have sort [star]. *)
  check_parameter env param star

let check_branch env (branch : parameterized_branch) =
  List.iter (check_producer env) branch.pr_producers

let enter_rule env (nt : symbol) (rule : parameterized_rule) : env =

  (* For each formal parameter, allocate a fresh variable. *)
  (* TEMPORARY should we check that the formals have distinct names? *)
  let formals, domain = allocate rule.pr_parameters in

  (* Connect these variables with the sort of the symbol [nt]. *)
  (* Because it is performed first, this particular unification
     cannot fail. *)
  begin try
    unify (find nt env) (arrow domain)
  with Unify _ | Occurs _ ->
    assert false
  end;

  (* Extend the environment. *)
  extend env formals

let check_rule env (nt : symbol) (rule : parameterized_rule) =

  (* Extend the environment within this rule. *)
  let env = enter_rule env nt rule in

  (* Check each branch in this extended environment. *)
  List.iter (check_branch env) rule.pr_branches

let check_grammar env g =

  (* Each rule must be well-sorted. *)
  StringMap.iter (check_rule env) g.p_rules;

  (* The start symbols must have sort [star]. *)
  StringMap.iter (fun nt _ ->
    unify (find nt env) star
  ) g.p_start_symbols

(* -------------------------------------------------------------------------- *)

(* This wrapper catches unification errors and displays error messages. *)

let check_grammar env (g : grammar) : unit =
  try
    check_grammar env g
  with
  | Unify (v1, v2) ->
      assert false (* TEMPORARY *)
  | Occurs (v1, v2) ->
      assert false

(* -------------------------------------------------------------------------- *)

let infer_grammar (g : grammar) : sort Env.t =

  (* For each (terminal or nonterminal) symbol, allocate a unification
     variable. The terminal symbols have sort [star], so we can use
     this particular variable. *)

  let env =
    StringMap.fold (fun tok _ env ->
      Env.add tok star env
    ) g.p_tokens Env.empty
  in

  let env =
    StringMap.fold (fun nt rule env ->
      let env = Env.add nt (fresh()) env in
      (* The following line unifies the sort of [nt] with an arrow of
         appropriate arity. It cannot fail. This strategy should lead
         to slightly better unification error messages. *)
      let _ : env = enter_rule env nt rule in
      env
    ) g.p_rules env
  in

  (* Impose sort equality constraints. *)

  check_grammar env g;

  (* TEMPORARY ground any unassigned sort variables? *)

  (* Decode the environment, so our user doesn't have to deal with
     unification variables. *)

  Env.map decode env
