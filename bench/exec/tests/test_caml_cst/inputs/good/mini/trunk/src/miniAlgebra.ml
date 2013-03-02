(**************************************************************************)
(*  Mini, a type inference engine based on constraint solving.            *)
(*  Copyright (C) 2006. François Pottier, Yann Régis-Gianas               *)
(*  and Didier Rémy.                                                      *)
(*                                                                        *)
(*  This program is free software; you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation; version 2 of the License.               *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  General Public License for more details.                              *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program; if not, write to the Free Software           *)
(*  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA         *)
(*  02110-1301 USA                                                        *)
(*                                                                        *)
(**************************************************************************)

(* $Id: miniAlgebra.ml 421 2006-12-22 09:27:42Z regisgia $ *)

open MultiEquation
open CoreAlgebra
open Positions
open MiniAst

module Ast = MiniAst

type symbol = int

type associativity = 
  | AssocLeft 
  | NonAssoc 
  | AssocRight 
  | EnclosedBy of string * string
    
type builtin_dataconstructor = dname * tname list * MiniAst.typ

let builtin_env = 
  let arrow_type pos t1 t2 =
    TypApp (pos, TypVar (pos, TName "->"), [ t1; t2 ])
  in
  let tuple_type2 pos t1 t2 = 
    TypApp (pos, TypVar (pos, TName "*"), [ t1; t2 ])
  in
  let gen_tvar v = 
    TypVar (undefined_position, TName v) 
  in
  [|
    TName "pre", (false, NonAssoc, -1, KArrow (KStar, KStar), []);
    TName "abs", (false, NonAssoc, -1, KStar, []);
    TName "pi" , (false, EnclosedBy ("{", "}"), -1, KArrow (KEmptyRow, KStar), []);
    TName "->" , (true, AssocRight, 0, KArrow (KStar, KArrow (KStar, KStar)), []);
    TName "*"  , (true, NonAssoc, 1, KArrow (KStar, KArrow (KStar, KStar)), 
	  [ (DName "_Tuple", [ TName "a"; TName "b" ], 
	     arrow_type undefined_position (gen_tvar "a") 
	       (arrow_type undefined_position (gen_tvar "b")
		  (tuple_type2 undefined_position 
		     (gen_tvar "a") (gen_tvar "b"))))
	  ]);
    TName "int" , (false, NonAssoc, 2, KStar, []);
    TName "char", (false, NonAssoc, 2, KStar, []);
    TName "unit", (false, NonAssoc, 3, KStar, 
	     [ (DName "_Unit", [], 
		TypVar (undefined_position, TName "unit"))])
  |] 

let get_infix (i, _, _,_,_) = 
  i

let get_assoc (_, a, _,_,_) = 
  a

let get_priority (_, _, p,_,_) = 
  p

let as_symbol name = 
  Misc.just_try (fun () -> Misc.array_associ name builtin_env)

let init_builtin_env variable =   
  Array.fold_left 
    (fun acu (o, (_,_,_,arity, ds)) -> 
       (o, (arity, 
	    TVariable (variable ?name:(Some o) ()),
	    ds
	   )
       ) :: acu) 
    [] builtin_env
    
let is_builtin op = 
  true
	  
let infix op = 
  try
    get_infix (snd builtin_env.(op))
  with Not_found -> false
    
let priority op = 
  try
    get_priority (snd builtin_env.(op))
  with Not_found -> max_int

let associativity op = 
  try
    get_assoc (snd builtin_env.(op))
  with Not_found -> NonAssoc

type 'a environment = tname -> 'a arterm

let symbol tenv (i : tname) = 
  tenv i

let type_of_primitive tenv = function
  | PIntegerConstant _ -> symbol tenv (TName "int")
  | PUnit -> symbol tenv (TName "unit")
  | PCharConstant _ -> symbol tenv (TName "char")

let mkref tenv x = 
  let v = symbol tenv (TName "ref") in
    TTerm (App (v, x))

let mkunit tenv = 
  symbol tenv (TName "unit")

let pre tenv x = 
  let v = symbol tenv (TName "pre") in
    TTerm (App (v, x))

let record_constructor tenv x =
  let v = symbol tenv (TName "pi") in
    TTerm (App (v, x))

let abs tenv = 
  symbol tenv (TName "abs")

let arrow tenv t u = 
  let v = symbol tenv (TName "->") in
    TTerm (App (TTerm (App (v, t)), u))

let n_arrows tenv ts u = 
  List.fold_left (fun acu x -> arrow tenv acu x) u ts

let tuple tenv ps = 
  let n = if ps = [] then "unit" else "*" in
  let v = symbol tenv (TName n) in
    List.fold_left (fun acu x -> TTerm (App (acu, x))) v ps

let result_type tenv t = 
  let a = symbol tenv (TName "->") in
  let rec chop n t = 
    if n = 0 then t 
    else 
      match t with
	| TTerm (App (TTerm (App (v, t)), u)) when v = a -> chop (n-1) u
	| u -> assert (n <= 0); u
  in
    chop (-1) t

let arg_types tenv t = 
  let a = symbol tenv (TName "->") in
  let rec chop acu = function
    | TTerm (App (TTerm (App (v, t)), u)) when v = a -> chop (t :: acu) u
    | x -> acu
  in
    List.rev (chop [] t) 

let tycon_args t =
  let rec chop acu = function
    | TTerm (App (t, u)) -> chop (u :: acu) t
    | _ -> acu
  in
    chop [] t
      
let rec tycon_name = function
  | TTerm (App (u, _)) -> tycon_name u
  | TVariable v as t -> t
  | _ -> assert false
      
