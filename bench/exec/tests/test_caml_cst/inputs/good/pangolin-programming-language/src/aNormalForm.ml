(* $Id: aNormalForm.ml 23 2007-09-27 13:38:59Z yann.regisgianas $ *)

(* Pangolin, a functional programming language for correct programs.
   Copyright (C) 2007, Yann Régis-Gianas, François Pottier.
   Contact: yann.regisgianas@gmail.com

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

(* This module implements a translation of terms in the implicit language
    into a special form, called A-normal form. In this form, each computation
    result is named. *)

open CoreSyntax
open CoreSyntaxExt

(* {1 Context handling} *)

(* Compose two contexts. *)

let app c1 c2 = 
  fun x -> c1 (c2 x)

(* Close a context with a particular term. *)

let close (c, u) = 
  c u

(* The empty context is the identity function. *)

let empty_context = 
  fun x -> x

(* From a context C[], build C[let x where F = t in []]. *)

let letcontext pos x f t c =
  app c (fun u -> mk_lterm pos (ELet (create_let_abs ([x], f, t, u))))
    
(* Generate a fresh logic binding that is composed of a fresh name, an
    unknown type, an implicit assertion. *)

let fresh_logic_binding pos = 
  let x = Var.Atom.freshb (PIdentifier.fresh_value_id' (pos, "_res")) in
    (x, ImplicitFormula pos)

(* Values are defined by the predicate [is_value]. *)

let rec is_value = function
  | EId x -> 
      true

  | EKApp (k, vs) -> 
      List.for_all is_value (List.map (fun v -> v.tvalue) vs)

  | EProd vs -> 
      List.for_all is_value (List.map (fun v -> v.tvalue) vs)

  | EPrimitive _ -> 
      true

  | EForallTys abs -> 
      let (vs, v) = open_eforalltys_abs abs in is_value v.tvalue

  | EExistsTys abs -> 
      let (vs, v) = open_eexiststys_abs abs in is_value v.tvalue

  | EDeferred -> 
      true

  | _ -> 
      false

let is_extended_value = function
  | ELam _ -> true
  | v -> is_value v

(* Given a term [t] an a context [C[]], augment the context and transform the
    term in a term [u] such that the composition [C[u]] is equivalent to [t]
    under the current context except that [C[u]] is in A-Normal form. *)
 
let rec term context t = 
  let pos = t.tpos in
  let context, t = 
    match t.tvalue with

    | ELam abs -> 
	let (largs, ins, outs, body) = open_fun_abs abs in
	let ubody = close (term empty_context body) in
	  (context, ELam (create_fun_abs (largs, ins, outs, ubody)))

    | ELet abs -> 
	let (ids, f, t1, t2) = open_let_abs abs in
	let u1 = close (term empty_context t1) in
	let u2 = close (term empty_context t2) in
	  (context, ELet (create_let_abs (ids, f, u1, u2)))

    | ELetRec abs -> 
	let (ids, f, t1, t2) = open_letrec_abs abs in
	let u1 = close (term empty_context t1) in
	let u2 = close (term empty_context t2) in
	  (context, ELetRec (create_letrec_abs (ids, f, u1, u2)))

    | EApp (f, largs, ts) ->
	let context, f = term_as_value context f in
	let context, us = Misc.list_fold_map term_as_value context ts in
          (context, EApp (f, largs, us))

    | EKApp (k, ts) ->
	let context, us = Misc.list_fold_map term_as_value context ts in
	  (context, EKApp (k, us))

    | EProd [ t ] -> 
	let (context, t) = term context t in
	  (context, t.tvalue)

    | EProd ts ->
	let context, us = Misc.list_fold_map term_as_value context ts in
	  (context, EProd us)

    | EIf (c, t, f) ->
	let context, u = term_as_value context c in
	let t = close (term empty_context t) in
	let f = close (term empty_context f) in
	  (context, EIf (u, t, f))

    | EExistsTys abs -> 
	let (vs, t) = open_eexiststys_abs abs in
	let u = close (term empty_context t) in
	  (context, EExistsTys (create_eexiststys_abs (vs, u)))

    | EForallTys abs -> 
	let (vs, t) = open_eforalltys_abs abs in
	let u = close (term empty_context t) in
	  (context, EForallTys (create_eforalltys_abs (vs, u)))
	  
    | EAnnot (t, ty) ->
	let (context, u) = term context t in
	  (context, EAnnot (u, ty))

    | ECase (s, cs) ->
	let context, s = term_as_value context s in
	let cs = List.map clause cs in
	  (context, ECase (s, cs))

    | EAbsurd ->
	(context, EAbsurd)

    | EAssert (f, t) ->
	let u = close (term empty_context t) in
	  (context, EAssert (f, u))

    | ELetLogic abs ->
	let (bs, f, t) = open_let_logic_abs abs in
	let u = close (term empty_context t) in
	  (context, ELetLogic (create_let_logic_abs (bs, f, u)))

    | v when is_extended_value v -> 
	(context, v)

    | _ -> 
	assert false

  in
    (context, mk_lterm pos t)
	    
and clause abs =
  let (k, t) = open_clause_abs abs in
    create_clause_abs (k, close (term empty_context t))
	
and term_as_value context t = 
    if is_value t.tvalue then
      (context, t)
    else 
      let context, u = term context t in
      let x, f = fresh_logic_binding t.tpos in
	(letcontext t.tpos x f u context, mk_lterm t.tpos (EId x))

let term t = 
  close (term empty_context t)

let component = function
  | CValue (ids, f, def) ->
      CValue (ids, f, term def)

  | CRecValue (ids, f, def) ->
      CRecValue (ids, f, term def)

  | c -> c

let rec program = function
  | PEmpty pos -> 
      PEmpty pos

  | PConsComponent abs ->
      let (pos, c, p) = open_pcons_abs abs in
	PConsComponent (create_pcons_abs (pos, component c, program p))
  
let process = program
