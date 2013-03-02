(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/small.ml,v 1.24.2.8 1999/04/01 17:08:59 francois Exp $ *)

open Types

let vspos span term =
  let v = freshSK span (kind_of_term term) in
  v.lo <- term;
  v

let vsneg span term =
  let v = freshSK span (kind_of_term term) in
  v.hi <- term;
  v

let spos span term =
  TVar (vspos span term)

let sneg span term =
  TVar (vsneg span term)

let vpos term =
  vspos Field term

let vneg term =
  vsneg Field term

let pos term =
  spos Field term

let neg term =
  sneg Field term

