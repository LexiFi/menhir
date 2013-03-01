(* $Id$ *)

include CoreAlgebra
open Sig
open Positions
open MiniAst

module Ast = MiniAst

type associativity = 
  | AssocLeft 
  | NonAssoc 
  | AssocRight 
  | EnclosedBy of string * string
    
type builtin_dataconstructor = string * string list * MiniAst.typ

let builtin_env = 
  let arrow_type pos t1 t2 =
    TypApp (pos, TypVar (pos, "->"), [ t1; t2 ])
  in
  let tuple_type2 pos t1 t2 = 
    TypApp (pos, TypVar (pos, "*"), [ t1; t2 ])
  in
  let gen_tvar v = TypVar (undefined_position, v) 
  in
  [|
    "pre", (false, NonAssoc, -1, KArrow (KStar, KStar), [], []);
    "abs", (false, NonAssoc, -1, KStar, [], []);
    "pi", (false, EnclosedBy ("{", "}"), -1, KArrow (KEmptyRow, KStar), [],[]);
    "->", (true, AssocRight, 0, KArrow (KStar, KArrow (KStar, KStar)), [], []);
    "*", (true, NonAssoc, 1, KArrow (KStar, KArrow (KStar, KStar)), 
	  [ "a"; "b" ], 
	  [ ("_Tuple", [], 
	     arrow_type undefined_position (gen_tvar "a") 
	       (arrow_type undefined_position (gen_tvar "b")
		  (tuple_type2 undefined_position 
		     (gen_tvar "a") (gen_tvar "b"))))
	  ]);
    "int", (false, NonAssoc, 2, KStar, [], []);
    "char", (false, NonAssoc, 2, KStar, [], []);
    "unit", (false, NonAssoc, 3, KStar, 
	     [],
	     [ ("_Unit", [], 
		TypVar (undefined_position, "unit"))])
  |] 

let get_infix (i, _, _,_,_,_) = i

let get_assoc (_, a, _,_,_,_) = a

let get_priority (_, _, p,_,_,_) = p

let as_symbol name = 
  Misc.just_try (fun () -> Misc.array_associ name builtin_env)

let init_builtin_env variable =   
  Array.fold_left (fun acu (o, (_,_,_,k, vars, ds)) -> 
		     (o, (k, 
			  TVariable (variable ?name:(Some o) ()),
			  vars, 
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

type 'a environment = string -> 'a arterm

let symbol tenv i = 
  tenv i

let type_of_primitive tenv = function
  | PIntegerConstant _ -> symbol tenv "int"
  | PUnit -> symbol tenv "unit"
  | PCharConstant _ -> symbol tenv "char"

let mkref tenv x = 
  let v = symbol tenv "ref" in
    TTerm (App (v, x))

let mkunit tenv = 
  symbol tenv "unit" 

let pre tenv x = 
  let v = symbol tenv "pre" in
    TTerm (App (v, x))

let record_constructor tenv x =
  let v = symbol tenv "pi" in
    TTerm (App (v, x))

let abs tenv = 
  symbol tenv "abs"

let uniform v = 
  TTerm (RowUniform v)

let rowcons label x y = 
  let intern_label = RowLabel.import label in
    TTerm (RowCons (intern_label, x, y))

let n_rowcons typed_labels y = 
  List.fold_left (fun acu (l, t) -> rowcons l t acu) y typed_labels

let arrow tenv t u = 
  let v = symbol tenv "->" in
    TTerm (App (TTerm (App (v, t)), u))

let n_arrows tenv ts u = 
  let v = symbol tenv "->" in
    List.fold_left (fun acu x -> arrow tenv acu x) u ts

let tuple tenv ps = 
  let n = if ps = [] then "unit" else "*" in
  let v = symbol tenv n in
    List.fold_left (fun acu x -> TTerm (App (acu, x))) v ps

let result_type tenv t = 
  let a = symbol tenv "->" in
  let rec chop n t = 
    if n = 0 then t 
    else 
      match t with
	| TTerm (App (TTerm (App (v, t)), u)) when v = a -> chop (n-1) u
	| u -> assert (n <= 0); u
  in
    chop (-1) t

let arg_types tenv t = 
  let a = symbol tenv "->" in
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

exception InvalidTypeConstructorUse of Positions.position * string *
  int * int

