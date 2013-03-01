(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/wright.ml,v 1.9.4.4 1999/06/21 12:21:02 francois Exp $ *)
(*

Implementing Andrew Wright's restriction of let-polymorphism to syntactic values. This is done in a very
straightforward way by turning dubious let constructs into plain function applications.

*)

open Errors
open Interpreter

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function checks whether an expression is non-expansive, that is, the result of its evaluation cannot contain
newly created mutable objects. It is only called on external syntax trees, i.e. it runs before VGeneralFun and 
VGeneralTry have been rewritten to VFun/VApp/VMatcher/VTry. This is a good thing because it can give better results
this way.

*)

let rec is_nonexpansive = function
    VBottom
  | VConstant _
  | VVar _
  | VGeneralFun _
  | VVector [] ->
      true
  | VApp(VVar "raise", expr) ->
      is_nonexpansive expr
  | VApp _ 
  | VVector _ ->
      false
  | VLet (_, bindings, body) ->
      List.for_all (function (_, expr) -> is_nonexpansive expr) bindings && is_nonexpansive body
  | VPair (expr1, expr2) ->
      is_nonexpansive expr1 && is_nonexpansive expr2
  | VRecord lelist ->
      List.for_all (function (_, expr) -> is_nonexpansive expr) lelist
  | VRecordAccess (expr, _) ->
      is_nonexpansive expr
  | VRec (_, expr) ->
      is_nonexpansive expr
  | VConstruct (_, expr) ->
      is_nonexpansive expr
  | VIf (_, expr1, expr2) ->
      is_nonexpansive expr1 && is_nonexpansive expr2
  | VCast (expr, _) ->
      is_nonexpansive expr
  | VUsage (expr, _) ->
      is_nonexpansive expr
  | VGeneralTry (expr, bindings) ->
      is_nonexpansive expr && List.for_all (function (_, expr) -> is_nonexpansive expr) bindings

    (* The following constructs are internal and should not appear here. However, we still handle them, since this
       can be useful when this function is invoked from the debugger and passed an internal term. *)

  | VTry _ ->
      false
  | VFun _
  | VMatcher _
  | VConstantMatcher _
  | VRecordUpdate _
  | VSymRecordConcat
  | VAsymRecordConcat ->
      true
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function does the same job for a list of bindings. If at least one of the bindings is expansive, then the
whole definition is regarded as expansive and all bindings will be turned into lambda abstractions. Although it
would be possible to do a finer job, it does not seem to be worth the trouble; the user can use 'let in' instead
of 'let and' if she is bothered by this behavior.

*)

let is_nonexpansive_bindings bindings =
  List.for_all (function (_, expr) -> is_nonexpansive expr) bindings
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This utility takes a list of bindings [p_i, e_i] and turns it into two tuples (p_1 ... p_n) and (e_1 ... e_n).
Actually, we don't have tuples, so we have to encode them into pairs.

*)

let rec separate_bindings = function
    [] ->
      raise (CantHappen "Empty bindings in Wright.separate_bindings.")
  | [binding] ->
      binding
  | (pattern, expr) :: rest ->
      let ptuple, etuple = separate_bindings rest in
      PPair(pattern, ptuple), VPair(expr, etuple)
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This filter takes an expression and recodes all let constructs whose bindings are expansive into functions.

*)

let rec behavior = function
    VLet(recursive, bindings, body) when not (is_nonexpansive_bindings bindings) ->
      let body = walk behavior body in
      let bindings = walk_bindings behavior bindings in
      let ptuple, etuple = separate_bindings bindings in
      VApp(VGeneralFun [[ptuple], body], if recursive then VRec(ptuple, etuple) else etuple)
  | _ ->
      raise JustWalk
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The external interface combines everything into one function.

*)

let filter_bindings bindings =
  not (is_nonexpansive_bindings bindings), walk_bindings behavior bindings
;;
