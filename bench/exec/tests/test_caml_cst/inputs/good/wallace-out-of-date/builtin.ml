(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/builtin.ml,v 1.31.4.14 1999/04/08 12:43:45 francois Exp $ *)
(*

The default type & execution environments.

It would be possible to give type 'a -> 'a -> bool to all comparison functions, but this would mean that any two
values can be compared, because 'a -> 'a -> bool is equivalent to Top -> Top -> bool. Such generic comparison
primitives can easily be implemented in the interpreter; however, comparing values of different shapes is
usually a user programming error, so we restrict the comparison primitives to type int -> int -> bool.

*)

open Rowsig
open Types
open Closure
open Interpreter
open Small
open Typechecking

(* ----------------------------------------------------------------------------------------------------------------- *)

let empty_row_map = RowMap.empty()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Some utilities to create simple type schemes.

*)

let ground_type body =
  Scheme(empty_context, vpos body, fresh())
;;

let single_type absty =
  let t = TVar (fresh()) in
  Scheme(empty_context, vpos (absty t), fresh())
;;

let variant_type label =
  let entries = RowMap.add label (pos (TVPresent (pos atom_unit))) empty_row_map in
  let remainder = spos (Remainder (RowMap.domain entries)) TVAbsent in
  make_tvariant entries remainder

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Some basic type schemes.

*)

let intcomptype = ground_type (
  make_tarrow (neg atom_int) (pos TBottom) (pos (make_tarrow (neg atom_int) (pos TBottom) (pos atom_bool)))
)
let intinttype = ground_type (make_tarrow (neg atom_int) (pos TBottom) (pos atom_int))
let genericcomptype = ground_type (
  make_tarrow (neg TTop) (pos TBottom) (pos (make_tarrow (neg TTop) (pos TBottom) (pos atom_bool)))
)
let intoptype = ground_type (
  make_tarrow (neg atom_int) (pos TBottom) (pos (make_tarrow (neg atom_int) (pos TBottom) (pos atom_int)))
)
let booloptype = ground_type (
  make_tarrow (neg atom_bool) (pos TBottom) (pos (make_tarrow (neg atom_bool) (pos TBottom) (pos atom_bool)))
)

let matchfailtype = ground_type (
  make_tarrow
    (neg (make_tvariant empty_row_map (sneg (Remainder RowSet.empty) TVAbsent)))
    (pos TBottom)
    (pos TBottom)
)

(* _matchaccept is used to typecheck pattern matchings involving constants, without a catch-all clause. It raises
   ConstantMatchFailure (a user exception) whenever it is called, so its type reflects this. *)

let matchaccepttype = ground_type (
  make_tarrow (neg TTop) (pos (variant_type "ConstantMatchFailure")) (pos TBottom)
)

let raisetype = single_type (fun x ->
  make_tarrow x x (pos TBottom)
)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Creating the builtin type environment.

*)

let pre_type_environment = [
  "plus", intoptype;
  "minus", intoptype;
  "times", intoptype;
  "int__minus", intinttype;
  "not", ground_type (make_tarrow (neg atom_bool) (pos TBottom) (pos atom_bool));
  "leq", genericcomptype;
  "geq", genericcomptype;
  "lth", genericcomptype;
  "gth", genericcomptype;
  "equal", genericcomptype;
  "succ", ground_type (make_tarrow (neg atom_int) (pos TBottom) (pos atom_int));
  "pred", ground_type (make_tarrow (neg atom_int) (pos TBottom) (pos atom_int));
  "or", booloptype;
  "and", booloptype;
  "quit", ground_type (make_tarrow (neg atom_unit) (pos TBottom) (pos TBottom));
  "ref", single_type (fun x -> make_tarrow x (pos TBottom) (pos (make_tref x x)));
  "!", single_type (fun x -> make_tarrow (neg (make_tref (pos TBottom) x)) (pos TBottom) x);
  ":=", single_type (fun x ->
    make_tarrow (neg (make_tref x (neg TTop))) (pos TBottom) (pos (make_tarrow x (pos TBottom) (pos atom_unit)))
  );
  "vect_length", ground_type (make_tarrow (neg (make_tvect (pos TBottom) (neg TTop))) (pos TBottom) (pos atom_int));
  "vect_item", single_type (fun x ->
    make_tarrow
      (neg (make_tvect (pos TBottom) x))
      (pos TBottom)
      (pos (make_tarrow (neg atom_int) (pos (variant_type "ArrayBound")) x))
  );
  "vect_assign", single_type (fun x ->
    make_tarrow
      (neg (make_tvect x (neg TTop)))
      (pos TBottom)
      (pos (make_tarrow (neg atom_int)
	                (pos (variant_type "ArrayBound"))
	                (pos (make_tarrow x (pos TBottom) (pos atom_unit)))))
  );
  "matchfail", matchfailtype;
  "reject", matchfailtype;
  "_matchaccept", matchaccepttype;
  "raise", raisetype
];;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Defining the primitive operations.

*)

let raise_label label =
  raise (UserException (ValConstruct (label, ValConstant ConstUnit)))
;;

let prim_plus = function 
  ValConstant (ConstInt x) ->
    ValPrimitive (function
      ValConstant (ConstInt y) -> ValConstant (ConstInt (x + y))
    | _ -> raise (ExecutionFailure "Second argument of primitive plus is not an integer.")
  )
| _ -> raise (ExecutionFailure "First argument of primitive plus is not an integer.")
;;

let prim_minus = function 
  ValConstant (ConstInt x) ->
    ValPrimitive (function
      ValConstant (ConstInt y) -> ValConstant (ConstInt (x - y))
    | _ -> raise (ExecutionFailure "Second argument of primitive minus is not an integer.")
  )
| _ -> raise (ExecutionFailure "First argument of primitive minus is not an integer.")
;;

let prim_times = function 
  ValConstant (ConstInt x) ->
    ValPrimitive (function
      ValConstant (ConstInt y) -> ValConstant (ConstInt (x * y))
    | _ -> raise (ExecutionFailure "Second argument of primitive times is not an integer.")
  )
| _ -> raise (ExecutionFailure "First argument of primitive times is not an integer.")
;;

let prim_unary_minus = function
    ValConstant (ConstInt x) ->
      ValConstant (ConstInt (-x))
  | _ ->
      raise (ExecutionFailure "Argument of primitive unary minus is not an integer.")
;;

let prim_succ = function
  ValConstant (ConstInt x) ->
    ValConstant(ConstInt (succ x))
| _ -> raise (ExecutionFailure "Argument of primitive succ is not an integer.")
;;

let prim_pred = function
  ValConstant (ConstInt x) ->
    ValConstant(ConstInt (pred x))
| _ -> raise (ExecutionFailure "Argument of primitive pred is not an integer.")
;;

let prim_not = function
  ValConstant (ConstBool b) -> ValConstant (ConstBool (not b))
| _ -> raise (ExecutionFailure "Argument of primitive not is not a boolean.")
;;

let prim_or = function
  ValConstant (ConstBool b1) -> 
    ValPrimitive (function
      ValConstant (ConstBool b2) -> ValConstant (ConstBool (b1 or b2))
    | _ -> raise (ExecutionFailure "Second argument of primitive or is not a boolean."))
| _ -> raise (ExecutionFailure "First argument of primitive or is not a boolean.")
;;

let prim_and = function
  ValConstant (ConstBool b1) -> 
    ValPrimitive (function
      ValConstant (ConstBool b2) -> ValConstant (ConstBool (b1 & b2))
    | _ -> raise (ExecutionFailure "Second argument of primitive and is not a boolean."))
| _ -> raise (ExecutionFailure "First argument of primitive and is not a boolean.")
;;

let prim_leq = function x -> ValPrimitive (function y -> ValConstant (ConstBool (x <= y)));;
let prim_geq = function x -> ValPrimitive (function y -> ValConstant (ConstBool (x >= y)));;
let prim_lth = function x -> ValPrimitive (function y -> ValConstant (ConstBool (x < y)));;
let prim_gth = function x -> ValPrimitive (function y -> ValConstant (ConstBool (x > y)));;
let prim_equal = function x -> ValPrimitive (function y -> ValConstant (ConstBool (x = y)));;

let prim_quit = function
  ValConstant ConstUnit -> print_endline "Good bye"; flush stdout; exit(0)
| _ -> raise (ExecutionFailure "Argument of primitive quit is not unit.")
;;

let prim_ref = function value -> ValRef (ref value)
;;

let prim_refaccess = function
  ValRef r ->
    !r
| _ ->
    raise (ExecutionFailure "Argument of primitive prim_refaccess is not a ref cell.")
;;

let prim_refassign = function
  ValRef r ->
    ValPrimitive (function value -> r := value; ValConstant ConstUnit)
| _ ->
    raise (ExecutionFailure "Argument of primitive prim_refassign is not a ref cell.")
;;

let prim_vect_length = function
    ValVector varray ->
      ValConstant (ConstInt (Array.length varray))
  | _ ->
      raise (ExecutionFailure "Argument of primitive prim_vect_length is not a vector.")
;;

let prim_vect_item = function
    ValVector varray ->
      ValPrimitive (function
	  ValConstant (ConstInt index) -> (

	    try
	      Array.get varray index
	    with Invalid_argument _ ->
	      raise_label "ArrayBound"

	)
	| _ ->
	    raise (ExecutionFailure "Second argument of primitive prim_vect_item is not an integer.")
      )
  | _ ->
      raise (ExecutionFailure "First argument of primitive prim_vect_item is not a vector.")
;;

let prim_vect_assign = function
    ValVector varray ->
      ValPrimitive (function
	  ValConstant (ConstInt index) ->

	    (* Let's be cool and check whether the index is valid without waiting for the value. Our type reflects
	       this behavior. *)

	    let _ = try
	      Array.get varray index
	    with Invalid_argument _ ->
	      raise_label "ArrayBound" in

	    ValPrimitive (function value -> Array.set varray index value; ValConstant ConstUnit)

	| _ ->
	    raise (ExecutionFailure "Second argument of primitive prim_vect_assign is not an integer.")
      )
  | _ ->
      raise (ExecutionFailure "First argument of primitive prim_vect_assign is not a vector.")
;;

let prim_matchfail _ =
  raise Interpreter.MatchFailure
;;

let prim_matchaccept _ =
  raise_label "ConstantMatchFailure"
;;

let prim_raise value =
  raise (UserException value)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Creating the builtin execution environment.

*)

let exec_environment = List.map (function (name, value) -> (name, ref value)) [
  "plus", ValPrimitive prim_plus;
  "minus", ValPrimitive prim_minus;
  "times", ValPrimitive prim_times;
  "int__minus", ValPrimitive prim_unary_minus;
  "not", ValPrimitive prim_not;
  "leq", ValPrimitive prim_leq;
  "geq", ValPrimitive prim_geq;
  "lth", ValPrimitive prim_lth;
  "gth", ValPrimitive prim_gth;
  "equal", ValPrimitive prim_equal;
  "succ", ValPrimitive prim_succ;
  "pred", ValPrimitive prim_pred;
  "or", ValPrimitive prim_or;
  "and", ValPrimitive prim_and;
  "quit", ValPrimitive prim_quit;
  "ref", ValPrimitive prim_ref;
  "!", ValPrimitive prim_refaccess;
  ":=", ValPrimitive prim_refassign;
  "vect_length", ValPrimitive prim_vect_length;
  "vect_item", ValPrimitive prim_vect_item;
  "vect_assign", ValPrimitive prim_vect_assign;
  "matchfail", ValPrimitive prim_matchfail;
  "_matchaccept", ValPrimitive prim_matchaccept;
  "raise", ValPrimitive prim_raise
];;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Defining synonyms for easier use.

*)

let define_synonym (new_name, old_name) (pre_type_env, exec_env) =
  let old_type = List.assoc old_name pre_type_env
  and old_value = List.assoc old_name exec_env in
  ((new_name, old_type) :: pre_type_env), ((new_name, old_value) :: exec_env)
;;

let (pre_type_environment, exec_environment) =
  List.fold_right define_synonym [
    "int__+", "plus";
    "int__-", "minus";
    "int__*", "times";
    "eq__<=", "leq";
    "eq__>=", "geq";
    "eq__<", "lth";
    "eq__>", "gth";
    "eq__=", "equal";
    "eq__==", "equal";
    "bool__||", "or";
    "bool__&", "and";
    "bool__&&", "and";
    "int__succ", "succ";
    "int__pred", "pred"
  ] (pre_type_environment, exec_environment)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Simplifying the type environment.

*)

let new_primitive_binding (name, scheme) =
  Env.Let(name, simplify true LetNode scheme)

let type_environment =
  List.map new_primitive_binding pre_type_environment

