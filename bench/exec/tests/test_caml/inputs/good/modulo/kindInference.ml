(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/kindInference.ml,v 1.4 2002/05/30 15:29:18 fpottier Exp $ *)

(* TEMPORARY comment *)
(* TEMPORARY interface *)

open Id
open KindUnification
open Syntax

(* Environments. *)

exception UnboundTypeIdentifier of id
(* TEMPORARY suppress this error elsewhere *)

let rec lookup id = function
  | [] ->
      raise (UnboundTypeIdentifier id)
  | (id', k) :: env ->
      if id = id' then k
      else lookup id env

let bind id k env =
  (id, k) :: env

(* Kind inference. *)

let rec infer env = function
  | TypVar id ->
      lookup id env
  | TypApp (t1, p, t2) ->
      let k = variable() in
      check env t1 (arrow p (infer env t2) k);
      k
  | TypAbs (p, id, k, t) ->
      arrow p k (infer (bind id k env) t)
  | TypArrow (t1, t2) ->
      check env t1 star;
      check env t2 star;
      star
  | TypRecord t ->
      check env t (row (RowDomain.empty));
      star
  | TypRowCons ((label, actuals), t) ->
      begin
	match actuals with
	| None ->
	    ()
	| Some actuals ->
	    let tf = List.fold_left (fun t1 (p, t2) -> TypApp (t1, p, t2)) (TypVar label) actuals in
	    check env tf star
      end;
      let domain = RowDomain.variable() in
      check env t (row (RowDomain.sum (IdSet.singleton label) domain));
      row domain
  | TypRowEmpty ->
      row (RowDomain.variable())
  | TypTuple ts ->
      List.iter (function t -> check env t star) ts;
      star
  | TypInteger ->
      star

and check env t kind =
  unify (infer env t) kind

