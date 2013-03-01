(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/Attic/env.ml,v 1.1.2.1 1999/04/05 16:45:38 francois Exp $ *)

open Errors
open Types

type environment_element =
    Lambda of identifier
  | Let of string * type_scheme

 and environment = environment_element list

let new_identifier = 
  let counter = ref 0
  in function name ->
    incr counter;
    (name, !counter)

let rec lookup_name name = function
    [] ->
      raise Not_found
  | (Let(name',_) as elem) :: _ when name = name' ->
      elem
  | (Lambda(name', _) as elem) :: _ when name = name' ->
      elem
  | _ :: rest ->
      lookup_name name rest

let rec lookup_lambda_name name = function
    [] ->
      raise Not_found
  | (Lambda((name', _) as identifier)) :: rest ->
      if name = name' then identifier
      else lookup_lambda_name name rest
  | _ ->
      raise (CantHappen "Env.lookup_lambda_name must be called on a list of lambda bindings and be successful.")

let do_let_env action env =
  List.iter (function
      Lambda _ ->
      	raise (CantHappen "Unexpected lambda binder in Env.do_let_env.")
    | Let (name, scheme) ->
      	action name scheme
  ) env

