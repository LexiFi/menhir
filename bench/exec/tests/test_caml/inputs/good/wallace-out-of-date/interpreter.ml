(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/interpreter.ml,v 1.24.4.7 1999/06/21 12:21:01 francois Exp $ *)
(*

Interpreting language expressions.

*)

open Type_expr
open Errors

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The expression language.

*)

type constant =
    ConstInt of int
  | ConstUnit
  | ConstBool of bool
  | ConstFloat of float
  | ConstChar of char
  | ConstString of string

and expression =
    VBottom
  | VConstant of constant
  | VVar of string
  | VApp of expression * expression
  | VLet of bool * bindings * expression
  | VPair of expression * expression
  | VRecord of (string * expression) list
  | VRecordAccess of expression * string
  | VRec of pattern * expression
  | VConstruct of string * expression
  | VIf of expression * expression * expression
  | VCast of expression * type_scheme_expression
  | VUsage of expression * constrained_type_expression
  | VVector of expression list

  (* Simple, internal constructs... *)

  | VFun of pattern * expression
  | VTry of expression * expression
  | VMatcher of string
  | VConstantMatcher of constant
  | VRecordUpdate of string
  | VAsymRecordConcat
  | VSymRecordConcat

  (* ...and their complex, external counterparts. *)

  | VGeneralFun of (pattern list * expression) list
  | VGeneralTry of expression * bindings

and pattern =
    PWildcard
  | PVar of string
  | PConstant of constant
  | PPair of pattern * pattern
  | PRecord of (string * pattern) list
  | PConstruct of string * pattern
  | PAlias of pattern * string
  | POr of pattern * pattern
  | PRef of pattern

and bindings =
  (pattern * expression) list

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Values

*)

module MyMap = Genmap.Make(Setl)

type value =
    ValConstant of constant
  | ValPrimitive of (value -> value)
  | ValClosure of environment * pattern * expression
  | ValPair of value * value
  | ValRecord of (string, value) MyMap.t
  | ValConstruct of string * value
  | ValRef of value ref
  | ValUninitialized
  | ValVector of value array

and environment = (string * value ref) list

let empty_record = (MyMap.empty Pervasives.compare : (string, value) MyMap.t)

exception UserException of value

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Toplevel phrases

*)

type phrase =
    PhraseDirective of string
  | PhraseLet of bool * bindings
  | PhraseValue of string * type_scheme_expression
  | PhraseInclude of string
  | PhraseAbstractType of string * (bool list)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Runtime errors.
First, the bad ones (which would cause a core dump in a compiled program), then the normal ones.

*)

exception ExecutionFailure of string					(* General runtime error *)
exception WrongMatchStructure						(* Pattern and value have different shapes *)

exception MatchFailure						        (* Pattern matching has failed *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Typing constants.
This is really part of the typechecker, but it is used by the interpreter too, for convenience.

*)

let atom_of_constant = function
    ConstInt _ ->
      Types.atom_int
  | ConstUnit ->
      Types.atom_unit
  | ConstBool _ ->
      Types.atom_bool
  | ConstChar _ ->
      Types.atom_char
  | ConstFloat _ ->
      Types.atom_float
  | ConstString _ ->
      Types.atom_string
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handling runtime errors

*)

let failure msg = raise (ExecutionFailure msg);;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Utilities to find duplicates in lists. They are quadratic but will be used only on small lists (the user typed
them in!).

*)

let rec find_duplicate_label = function
    [] ->
      raise Not_found
  | (label, _) :: rest ->
      try
	List.assoc label rest;
	label
      with Not_found ->
        find_duplicate_label rest
;;

let rec has_duplicate = function
    [] ->
      false
  | elem :: rest ->
      (has_duplicate rest) or (List.mem elem rest)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

pattern_vars returns the set of variable names defined by a pattern. At the same time, it makes sure that the
pattern is well-formed (i.e. record patterns don't have duplicate labels, POr patterns don't define any names)
and linear (i.e. a variable does not occur twice in the pattern).


*)

exception InconsistentMatching of string

let empty_name_set =
  Set7.empty Pervasives.compare
;;

let rec add_pattern_vars accu = function
    PWildcard
  | PConstant _ ->
      accu
  | PVar name ->
      if Set7.mem name accu then
	raise (InconsistentMatching ("Variable " ^ name ^ " appears twice in pattern."));
      Set7.add name accu
  | PPair(pattern1, pattern2) ->
      add_pattern_vars (add_pattern_vars accu pattern1) pattern2
  | PRecord lplist -> (
      try
      	let culprit = find_duplicate_label lplist in
      	let message = "Field " ^ culprit ^ " appears twice in record pattern!" in
      	raise (InconsistentMatching message)
      with Not_found ->
      	List.fold_left (fun accu (_, pattern) -> add_pattern_vars accu pattern) accu lplist
    )
  | PConstruct(label, pattern) ->
      add_pattern_vars accu pattern
  | PAlias (pattern, name) ->
      if Set7.mem name accu then
	raise (InconsistentMatching ("Variable " ^ name ^ " appears twice in pattern."));
      add_pattern_vars (Set7.add name accu) pattern
  | POr (pat1, pat2) ->
      if not (Set7.is_empty (add_pattern_vars (add_pattern_vars empty_name_set pat1) pat2)) then
      	raise (InconsistentMatching "POr patterns are not allowed to bind names.");
      accu
  | PRef pattern ->
      add_pattern_vars accu pattern
;;

let pattern_vars pattern =
  add_pattern_vars empty_name_set pattern
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

More utilies to deal with free program variables.

*)

let rec add_bindings_vars accu pelist =
  List.fold_right (fun (pattern, expr) (pat_vars, expr_vars) ->
    add_pattern_vars pat_vars pattern, add_expression_vars expr_vars expr
  ) pelist accu

and bindings_vars bindings =
  add_bindings_vars (empty_name_set, empty_name_set) bindings

and add_expression_vars accu = function
    VBottom
  | VConstant _ ->
      accu
  | VVar name ->
      Set7.add name accu
  | VApp(expr1, expr2) ->
      add_expression_vars (add_expression_vars accu expr1) expr2
  | VLet(recursive, bindings, body) ->
      let pat_vars, expr_vars = bindings_vars bindings in
      if recursive then begin
	let expr_vars = add_expression_vars expr_vars body in
	Set7.union accu (Set7.diff expr_vars pat_vars)
      end
      else begin
	let body_vars = expression_vars body in
  	Set7.union accu (Set7.union expr_vars (Set7.diff body_vars pat_vars))
      end
  | VPair(expr1, expr2) ->
      add_expression_vars (add_expression_vars accu expr1) expr2
  | VRecord lxlist ->
      List.fold_right (fun (_, expr) accu -> add_expression_vars accu expr) lxlist accu
  | VRecordAccess (expr, _) ->
      add_expression_vars accu expr
  | VRec (pat, expr) ->
      let pat_vars = pattern_vars pat
      and expr_vars = expression_vars expr in
      Set7.union accu (Set7.diff expr_vars pat_vars)
  | VConstruct(_, expr) ->
      add_expression_vars accu expr
  | VIf(expr0, expr1, expr2) ->
      add_expression_vars (add_expression_vars (add_expression_vars accu expr0) expr1) expr2
  | VCast(expr, _) ->
      add_expression_vars accu expr
  | VUsage(expr, _) ->
      add_expression_vars accu expr
  | VVector xlist ->
      List.fold_left add_expression_vars accu xlist
  | VFun(pat, expr) ->
      let pat_vars = pattern_vars pat
      and expr_vars = expression_vars expr in
      Set7.union accu (Set7.diff expr_vars pat_vars)
  | VTry(expr1, expr2) ->
      add_expression_vars (add_expression_vars accu expr1) expr2
  | VMatcher _
  | VConstantMatcher _
  | VRecordUpdate _
  | VSymRecordConcat 
  | VAsymRecordConcat ->
      accu
  | VGeneralFun _
  | VGeneralTry _ ->
      raise (CantHappen "Unexpected general construct in Interpreter.add_expression_vars.")

and expression_vars expr =
  add_expression_vars empty_name_set expr
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The code used to interpret let constructs is factored out because it is also used for toplevel lets.
The function returns the chunk of execution environment introduced by the let binding.

*)

let rec create_let_exec_environment recursive env pelist =

  if not recursive then begin

    let original_env = env in
    List.fold_right (fun (pattern, expr) env ->
      let value = interpret original_env expr in
      (bind_pattern pattern value) @ env
    ) pelist []

  end
  else begin

    (* Gather the names. *)

    let names = List.fold_right (fun (pattern, _) names ->
      Set7.union names (pattern_vars pattern)
    ) pelist empty_name_set in

    (* Create an environment containing place-holders. *)

    let env_chunk = Set7.fold (fun name env_chunk ->
      (name, ref ValUninitialized) :: env_chunk
    ) names [] in

    (* For each clause: *)

    List.iter (fun (pattern, expr) ->

      (* Interpret it and bind the results to the variables. *)

      let bindings = bind_pattern pattern (interpret (env_chunk @ env) expr) in

      (* Modify the environment, in place. *)

      List.iter (fun (name, result) ->
	let cell = List.assoc name env_chunk in
	cell := !result
      ) bindings

    ) pelist;

    (* Return the new environment chunk. *)

    env_chunk

  end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Interpreting.
interpret takes an environment, an expression and returns the corresponding value.
filter takes an environment, a value, a pattern-matching and returns the corresponding value.
bind_pattern takes a pattern and a value and returns the corresponding environment or raises MatchFailure.

*)

and interpret env expr = match expr with
  VBottom ->
    interpret env VBottom
| VConstant c ->							(* Constants are values *)
    ValConstant c
| VVar name -> (
    try
      match !(List.assoc name env) with					(* Variables are taken from the environment *)
        ValUninitialized ->
	  failure ("Variable " ^ name ^ " is used before being initialized.")
      | value ->
      	  value
    with Not_found ->
      failure ("Variable " ^ name ^ " is missing from the execution environment.")
  )
| VFun (pattern, expr) ->
    ValClosure (env, pattern, expr)					(* Build a closure *)
| VMatcher label ->
    ValPrimitive (function yes_code ->
      ValPrimitive (function no_code ->
	ValPrimitive (function
	    ValConstruct (label', value) when label = label' ->
	      interpret_application yes_code value
	  | ValConstruct _ as value ->
	      interpret_application no_code value
	  | _ ->
	      failure "Non-variant argument passed to a matcher primitive."
	)
      )
    )  
| VConstantMatcher constant ->
    ValPrimitive (function yes_code ->
      ValPrimitive (function no_code ->
	ValPrimitive (function
	    ValConstant constant' when constant = constant' ->
	      interpret_application yes_code (ValConstant ConstUnit)
	  | (ValConstant constant') as value when atom_of_constant constant = atom_of_constant constant' ->
	      interpret_application no_code value
	  | _ ->
	      failure "Ill-typed argument passed to a constant matcher primitive."
        )
      )
    )
| VRecordUpdate label ->
    ValPrimitive (function
	ValRecord lvmap ->
	  ValPrimitive (function value ->
	    ValRecord (MyMap.override label value lvmap)
          )
      |	_ ->
	  failure "First argument of record updater primitive isn't a record."
    )
| VSymRecordConcat
| VAsymRecordConcat ->

    (* Symmetric record concatenation is actually exactly the same primitive as asymmetric concatenation. The only
       difference is in the typing: the former has a more restricted domain. *)

    ValPrimitive (function
	ValRecord lvlist1 ->
	  ValPrimitive (function
	      ValRecord lvlist2 ->
		ValRecord (MyMap.concat lvlist1 lvlist2)
	    | _ ->
		failure "Second argument of record concatenation primitive isn't a record."
          )
      |	_ ->
	  failure "First argument of record concatenation primitive isn't a record."
    )
| VApp (expr1, expr2) ->
    let value2 = interpret env expr2					(* Evaluate the argument *)
    and value1 = interpret env expr1 in					(* Evaluate the function *)
    interpret_application value1 value2
| VLet (recursive, pelist, body) ->
    let env_chunk = create_let_exec_environment recursive env pelist in
    interpret (env_chunk @ env) body
| VPair (expr1, expr2) ->
    ValPair(interpret env expr1, interpret env expr2)
| VRecord lelist ->
    ValRecord (List.fold_right (fun (label, expr) accu ->
      MyMap.add label (interpret env expr) accu
    ) lelist empty_record)
| VRecordAccess (expr, label) -> (
    let value = interpret env expr in
    match value with
      ValRecord lvmap -> (
        try
	  MyMap.find label lvmap
	with Not_found ->
	  failure ("Missing record field " ^ label)
      )
    | _ ->
      failure "Attempted record access on non-record."
  )
| VRec (PVar name, VFun (pattern, expr)) ->
    let rec closure = ValClosure((name, ref closure) :: env, pattern, expr)	(* Build a recursive closure *)
    in closure
| VRec (PVar name, expr) ->
    let ref_cell = ref ValUninitialized in				(* Use a placeholder *)
    let value = interpret ((name, ref_cell) :: env) expr in		(* Create a recursive environment *)
    ref_cell := value;							(* Update the environment afterwards *)
    value
| VRec (pattern, expr) ->
    (* Be lazy and use a let rec construct *)
    interpret env (VLet(true, [pattern, expr], expr))
| VConstruct (name, expr) ->
    ValConstruct (name, interpret env expr)
| VIf (expr0, expr1, expr2) -> (
    match interpret env expr0 with
      ValConstant (ConstBool true) -> interpret env expr1
    | ValConstant (ConstBool false) -> interpret env expr2
    | _ -> failure "Argument of if isn't a boolean value."
  )
| VTry (expr, handler) -> (
    try
      interpret env expr
    with UserException value ->

      (* We do not try to catch match failures here and re-propagate the user exception. We assume that the code
	 has been rewritten to include a catch-all clause if necessary, so a match failure here is really an error. *)

      interpret_application (interpret env handler) value

  )
| VCast (expr, _) ->
    interpret env expr
| VUsage (expr, _) ->
    interpret env expr
| VVector elist ->
    ValVector (Array.of_list (List.map (interpret env) elist))
| VGeneralFun _ 
| VGeneralTry _ ->
    failure "Unexpected VGeneralFun/Try construct in Interpreter.interpret."

and interpret_application value1 value2 =
  match value1 with
    ValClosure(env', pattern, expr) -> 					(* Beta redex *)
      let env'' = bind_pattern pattern value2 in
      interpret (env'' @ env') expr
  | ValPrimitive primitive ->						(* Primitive application *)
      primitive value2
  | _ ->
      failure "Call of non-function."

and bind_pattern pat value = match pat with
  PWildcard ->
    []
| PVar name ->
    [name, ref value]
| PConstant constant -> (
    match value with
      ValConstant constant' when constant = constant' -> []
    | ValConstant constant' when atom_of_constant constant = atom_of_constant constant' -> raise MatchFailure
    | _ -> raise WrongMatchStructure
  )
| PPair (pat1, pat2) -> (
    match value with
      ValPair (value1, value2) -> (bind_pattern pat1 value1) @ (bind_pattern pat2 value2)
    | _ -> raise WrongMatchStructure
  )
| PRecord lplist -> (
    match value with
      ValRecord lelist -> bind_record_pattern lelist lplist
    | _ -> raise WrongMatchStructure
  )
| PConstruct (name, pat') -> (
    match value with 
      ValConstruct (name', value') when name = name' -> bind_pattern pat' value'
    | ValConstruct _ -> raise MatchFailure
    | _ -> raise WrongMatchStructure
  )
| PAlias (pat, name) ->
    (name, ref value) :: (bind_pattern pat value)
| POr (pat1, pat2) -> (
    try
      bind_pattern pat1 value
    with MatchFailure ->
      bind_pattern pat2 value
  )
| PRef pat -> (
    match value with
      ValRef valref ->
	bind_pattern pat !valref
    | _ ->
	raise WrongMatchStructure
  )

and bind_record_pattern lvmap = function
  [] ->
    []
| (label, pat) :: rest -> (
    let value = try
      MyMap.find label lvmap
    with Not_found ->
      raise WrongMatchStructure
    in (bind_pattern pat value) @ (bind_record_pattern lvmap rest)
  )
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Printing values.

*)

open Printing

let print_constant = function
  ConstInt n -> print_int n
| ConstUnit -> print_string "()"
| ConstBool b -> if b then print_string "true" else print_string "false"
| ConstChar c -> print_char '\''; print_char c; print_char '\''
| ConstFloat f -> print_float f
| ConstString s -> print_char '"'; print_string (String.escaped s); print_char '"'
;;

let rec print_value = function
  ValConstant c ->
    print_constant c
| ValClosure _ ->
    print_string "<fun>"
| ValPrimitive _ ->
    print_string "<prim>"
| ValPair (val1, val2) ->
    print_string "(";
    print_value val1;
    print_string ", ";
    print_value val2;
    print_string ")"
| ValRecord lvlist ->
    print_string "{ ";
    print_label_value_list lvlist;
    print_string " }"
| ValConstruct (name, value) ->
    print_string name;
    print_string " ";
    print_value value
| ValRef r ->
    print_string "ref (";
    print_value !r;
    print_string ")"
| ValUninitialized ->
    print_string "<uninitialized>"
| ValVector varray ->
    print_string "[| ";
    print_value_semi_list (Array.to_list varray);
    print_string " |]"

and print_label_value_list lvmap =
  MyMap.fold (fun label value first ->
    if not first then
      print_string "; ";
    print_string label;
    print_string " = ";
    print_value value;
    false
  ) lvmap true

and print_value_semi_list = function
    [] ->
      ()
  | [value] ->
      print_value value
  | value :: rest ->
      print_value value;
      print_string "; ";
      print_value_semi_list rest
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A generic function to walk an expression and produce a modified, but mostly unchanged, expression. This function has
two advantages: hiding the repetitive code and enhancing sharing with the old expression as much as possible. The 
idea is the same as in Walk.walk.

*)

exception JustWalk

let rec walk behavior expr =
  try
    behavior expr
  with JustWalk -> match expr with
    VBottom
  | VConstant _
  | VVar _ ->
      expr
  | VApp (expr1, expr2) ->
      let expr1' = walk behavior expr1
      and expr2' = walk behavior expr2 in
      if (expr1 == expr1') & (expr2 == expr2') then expr else VApp(expr1', expr2')
  | VLet (recursive, bindings, body) ->
      let bindings' = walk_bindings behavior bindings in
      let body' = walk behavior body in
      if (bindings == bindings') & (body == body') then expr else VLet(recursive, bindings', body')
  | VPair (expr1, expr2) ->
      let expr1' = walk behavior expr1
      and expr2' = walk behavior expr2 in
      if (expr1 == expr1') & (expr2 == expr2') then expr else VPair(expr1', expr2')
  | VRecord lxlist ->
      let unchanged, lxlist' = List.fold_right (fun ((label, expr) as elem) (unchanged, lxlist') ->
	let expr' = walk behavior expr in
	if expr == expr' then (unchanged, elem :: lxlist')
	else (false, (label, expr') :: lxlist')
      ) lxlist (true, []) in
      if unchanged then expr else VRecord lxlist'
  | VRecordAccess (body, label) ->
      let body' = walk behavior body in
      if body == body' then expr else VRecordAccess(body', label)
  | VRec (pattern, body) ->
      let body' = walk behavior body in
      if body == body' then expr else VRec(pattern, body')
  | VConstruct (label, body) ->
      let body' = walk behavior body in
      if body == body' then expr else VConstruct(label, body')
  | VIf (expr0, expr1, expr2) ->
      let expr0' = walk behavior expr0
      and expr1' = walk behavior expr1
      and expr2' = walk behavior expr2 in
      if (expr0 == expr0') & (expr1 == expr1') & (expr2 == expr2') then expr else VIf(expr0', expr1', expr2')
  | VCast (body, scheme) ->
      let body' = walk behavior body in
      if body == body' then expr else VCast(body', scheme)
  | VUsage (body, ctype) ->
      let body' = walk behavior body in
      if body == body' then expr else VUsage(body', ctype)
  | VVector xlist ->
      let unchanged, xlist' = List.fold_right (fun expr (unchanged, xlist') ->
	let expr' = walk behavior expr in
	(unchanged & (expr == expr'), expr' :: xlist')
      ) xlist (true, []) in
      if unchanged then expr else VVector xlist'
  | VFun (pattern, body) ->
      let body' = walk behavior body in
      if body == body' then expr else VFun (pattern, body')
  | VTry (body, handler) ->
      let body' = walk behavior body
      and handler' = walk behavior handler in
      if (body == body') & (handler == handler') then expr else VTry(body', handler')
  | VMatcher _
  | VConstantMatcher _
  | VRecordUpdate _
  | VSymRecordConcat 
  | VAsymRecordConcat ->
      expr
  | VGeneralFun plelist ->
      let unchanged, plelist' = List.fold_right (fun ((patl, expr) as elem) (unchanged, plelist') ->
	let expr' = walk behavior expr in
	if expr == expr' then (unchanged, elem :: plelist')
	else (false, (patl, expr') :: plelist')
      ) plelist (true, []) in
      if unchanged then expr else VGeneralFun plelist'
  | VGeneralTry (body, bindings) ->
      let body' = walk behavior body
      and bindings' = walk_bindings behavior bindings in
      if (body == body') & (bindings' == bindings) then expr else VGeneralTry(body', bindings')

and walk_bindings behavior bindings =
  let unchanged, bindings' = List.fold_right (fun ((pattern, expr) as elem) (unchanged, bindings') ->
    let expr' = walk behavior expr in
    if expr == expr' then (unchanged, elem :: bindings')
    else (false, (pattern, expr') :: bindings')
  ) bindings (true, []) in
  if unchanged then bindings else bindings'
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A code pretty-printer. It is used to debug the internal code rewriting phases. Currently, its treatment of VCast
and of VGeneralFun is non-existent; besides, it prints code that cannot be parsed for internal constructs such as
VFun/VTry/VMatcher/VConstantMatcher/VRecordUpdate/VSymRecordConcat/VAsymRecordConcat.

*)

open Format

let () =
  set_max_boxes max_int
;;

let shield do_it content =
  if do_it then
    print_string "(";
  content();
  if do_it then
    print_string ")"
;;

let pattern_priority = function
    PWildcard | PVar _ | PConstant _ | PPair _ | PRecord _ ->
      0
  | PConstruct _ | PRef _ ->
      1
  | POr _ ->
      2
  | PAlias _ ->
      3
;;

let expr_priority = function
    VBottom | VConstant _ | VVar _ | VPair _ | VRecord _ | VVector _
  | VMatcher _ | VConstantMatcher _ | VRecordUpdate _ | VSymRecordConcat | VAsymRecordConcat | VCast _ | VUsage _ ->
      0
  | VRecordAccess _ ->
      1
  | VApp _ | VConstruct _ ->
      2
  | VLet _ | VRec _ | VIf _ | VFun _ | VTry _ | VGeneralFun _ | VGeneralTry _ ->
      3
;;

let rec print_something_list printer separator = function
    [] ->
      ()
  | [one] ->
      printer one
  | one :: more ->
      printer one;
      separator();
      print_something_list printer separator more
;;

let rec print_expr current_shield_level expr =
  open_box 0;

  let shield_level = expr_priority expr in
  let must_shield = shield_level >= current_shield_level in
  let next_level = succ shield_level in

  shield must_shield (fun () ->
    match expr with
      VBottom ->
	print_string "~0"
    | VConstant constant ->
	print_constant constant
    | VVar name ->
	print_string name
    | VApp(expr1, expr2) ->
	print_expr next_level expr1; print_space(); print_expr shield_level expr2
    | VLet(recursive, bindings, expr) ->
	print_string (if recursive then "let rec" else "let"); print_space();
	print_let_bindings bindings;
	print_space(); print_string "in"; print_space();
	print_expr max_int expr
    | VPair(expr1, expr2) ->
	print_string "(";
	print_expr max_int expr1;
	print_string ",";
	print_space();
	print_expr max_int expr2;
	print_string ")"
    | VRecord lxlist ->
	print_string "{ ";
	print_something_list print_label_expression semicolon_separator lxlist;
	print_string " }"
    | VRecordAccess(expr, label) ->
	print_expr next_level expr;
	print_string ".";
	print_string label
    | VRec(pattern, expr) ->
	print_string "rec"; print_space();
	print_pattern max_int pattern;
	print_space(); print_string "in"; print_space();
	print_expr max_int expr
    | VConstruct(label, expr) ->
	print_string label;
	print_space();
	print_expr shield_level expr
    | VIf(expr0, expr1, expr2) ->
	print_string "if"; print_space();
	print_expr max_int expr0;
	print_space(); print_string "then"; print_space();
	print_expr max_int expr1;
	print_space(); print_string "else"; print_space();
	print_expr max_int expr2
    | VCast(expr, scheme_expr) ->
	print_string "(";
	print_expr max_int expr; (* TEMPORARY Need to implement a pretty-printer for type expressions. *)
	print_string ")"
    | VUsage(expr, ctype_expr) ->
	print_string "[";
	print_expr max_int expr; (* TEMPORARY Need to implement a pretty-printer for type expressions. *)
	print_string "]"
    | VVector xlist ->
	print_string "[| ";
	print_something_list (print_expr max_int) semicolon_separator xlist;
	print_string " |]"
    | VFun (pattern, expr) ->
	print_string "_fun"; print_space();
	print_pattern max_int pattern;
	print_space(); print_string "->"; print_space();
	print_expr max_int expr
    | VTry (expr1, expr2) ->
      	print_string "_try"; print_space();
      	print_expr max_int expr1;
      	print_space(); print_string "with"; print_space();
      	print_expr max_int expr2
    | VMatcher label ->
	print_string ("_match_" ^ label)
    | VConstantMatcher constant ->
	print_string "_match_";
	print_constant constant
    | VRecordUpdate label ->
	print_string ("_override_" ^ label)
    | VSymRecordConcat ->
	print_string "_sym_concat_"
    | VAsymRecordConcat ->
	print_string "_asym_concat_"
    | VGeneralFun plelist ->
	raise (CantHappen "Interpreter.print_expr doesn't handle VGeneralFun.")
    | VGeneralTry (expr, bindings) ->
	print_string "try"; print_space();
	print_expr max_int expr;
	print_space(); print_string "with"; print_space();
	print_try_bindings bindings
  );
  close_box()

and print_constant = function
    ConstInt x ->
      print_int x
  | ConstUnit ->
      print_string "()"
  | ConstBool x ->
      print_string (if x then "true" else "false")
  | ConstFloat x ->
      print_float x
  | ConstChar x ->
      print_string ("'" ^ (Char.escaped x) ^ "'")
  | ConstString x ->
      print_string ("\"" ^ (String.escaped x) ^ "\"")

and semicolon_separator() =
  print_string ";"; print_space()

and print_label_expression (label, expr) =
  print_string label;
  print_string " ="; print_space();
  print_expr max_int expr

and print_bindings separator binder bindings =
  print_something_list (print_binding binder) separator bindings

and print_binding binder (pattern, expr) =
  print_pattern max_int pattern;
  binder();
  print_expr max_int expr

and print_let_bindings bindings =
  print_bindings
    (function () -> print_space(); print_string "and"; print_space())
    (function () -> print_space(); print_string "="; print_space())
    bindings

and print_try_bindings bindings =
  print_bindings
    (function () -> print_space(); print_string "|"; print_space())
    (function () -> print_space(); print_string "->"; print_space())
    bindings

and print_pattern current_shield_level pattern =
  open_box 0;

  let shield_level = pattern_priority pattern in
  let must_shield = shield_level >= current_shield_level in
  let next_level = succ shield_level in

  shield must_shield (fun () ->
    match pattern with
      PWildcard ->
	print_string "_"
    | PVar name ->
	print_string name
    | PConstant constant ->
	print_constant constant
    | PPair(pat1, pat2) ->
	print_string "(";
	print_pattern max_int pat1;
	print_string ",";
	print_space();
	print_pattern max_int pat2;
	print_string ")"
    | PRecord lplist ->
	print_string "{ ";
	print_something_list print_label_pattern semicolon_separator lplist;
	print_string " }"
    | PConstruct (label, pat) ->
	print_string label;
	print_space();
	print_pattern next_level pat
    | PAlias (pat, name) ->
	print_pattern next_level pat;
	print_space(); print_string "as"; print_space();
	print_string name
    | POr(pat1, pat2) ->
	print_pattern next_level pat1;
	print_space(); print_string "|"; print_space();
	print_pattern next_level pat2
    | PRef pat ->
	print_string "ref"; print_space();
	print_pattern next_level pat
  );
  close_box()

and print_label_pattern (label, pat) =
  print_string label;
  print_string " ="; print_space();
  print_pattern max_int pat
;;

let print_toplevel_let_bindings recursive bindings =
  print_string (if recursive then "let rec" else "let"); print_space();
  print_let_bindings bindings;
  print_string ";;";
  print_newline()
;;
