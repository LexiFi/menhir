(* $Id: xCorePrinter.ml 55 2007-10-01 14:50:33Z yann.regisgianas $ *)

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

open XCoreSyntax.Raw
module B = Buffer
open Pprint
open PprintExt

let with_fannot = ref false

let atom (x, _) = 
  text x

let kw k = 
  text k

let sym k = 
  text k

let id x = 
  let s = PIdentifier.as_string x in
    text s

let type_id x = 
  id x

let ids xs =
  tupled (List.map id xs)

let quantified q quantifiers ts body t = 
  if ts = [] then body t else 
    group (kw q ++ quantifiers ts ^^ sym "." $$ group (body t))

let indent_code t = 
  hang 2 (group t)

let primitive_type = function
  | TInt  -> text "int"
  | TBool -> text "bool"
  | TUnit -> text "unit"

let rec term_type ?(paren = false) t = 
  let d, paren' = 
    match t with
      | TPrimitive p -> 
	  primitive_type p, false
	    
      | TVar x -> 
	  type_id x, false
	    
      | TArrow (ty1, ty2) -> 
	  (term_type ty1 ++ sym "->" ++ term_type ty2), true

      | TProd [] ->
	  lparen ^^ rparen, false
	    
      | TProd ts -> 
	  lparen 
	  ^^ paren_sep (sym " * ") (List.map (term_type ~paren:true) ts) 
	  ^^ rparen, false
	    
      | TApp (tycon, args) -> 
	  type_id tycon ++ tupled (List.map term_type args), false
  in
    if paren && paren' then lparen ^^ d ^^ rparen else d

let rec kind = function
  | KStar ->
      sym "*"

  | KArrow [ KStar ] ->
      sym "*" ++ sym "->" ++ sym "*"

  | KArrow ks ->
      tupled (List.map kind ks) ++ sym "->" ++ sym "*"

let binding (x, ty) = 
  id x ++ sym ":" ++ term_type ty

let bindings bs =
  tupled (List.map binding bs)

let rec logic_bindings (bs, f) = 
  bindings bs ^^ optional_formula f

and logic_arguments largs = 
  if largs = [] then empty
  else bracket (fbindings' largs)

and primitive = function
  | PInt i -> 
      text (string_of_int i)

  | PTrue ->
      kw "true"

  | PFalse -> 
      kw "false"

and formula = function

  | FDeferred ->
      sym "?"

  | FTrue -> 
      kw "True"

  | FFalse ->
      kw "False"

  | FVar x -> 
      id x 

  | FLam (bs, f) ->
      kw "fun" ++ fbindings bs ++ sym "->" $ iformula f

  | FForallTys (ts, f) ->
      quantified "forall_types" type_parameters ts iformula f

  | FForall (bs, _, f) ->
      quantified "forall" fbindings bs iformula f

  | FExists (bs, f) ->
      quantified "exists" fbindings bs iformula f
      
  | FEq (_, f1, f2) ->
      group (lparen ^^ lformula f1 $ sym "=" ++ lformula f2 ^^ rparen)

  | FApp (f, [ t1; t2 ]) ->
      binary_operator f t1 t2

  | FApp (f, args) ->
      (match f.fvalue with
	 | FVar f -> 
	     id f ++ tupled (List.map lformula args)
	 | f -> 
	     paren (formula f) ++ tupled (List.map lformula args))
	
  | FKApp (f, args) ->
      id f ++ tupled (List.map lformula args)

  | FPrimitive p ->
      formula_primitive p

  | FProd [ f ] ->
      lformula f

  | FProd fs -> 
      tupled (List.map lformula fs)

  | FAnnot (f, ty) ->
      if !with_fannot then
	parens (lformula f ++ sym ":" ++ formula_type ty)  
      else 
	lformula f

and binary_operator f lhs rhs = 
  match f.fvalue with
    | FPrimitive ((PAnd | POr | PEquiv | PImply 
		  | PAdd | PSub | PDiv | PMult) as p) ->
	paren (align (lformula lhs $ formula_primitive p ++ lformula rhs))

    | FVar x ->
	(match (PIdentifier.as_string x).[0] with
	   | '<' | '=' | '>' | '*' | '%' | '-' | '+' | '/' | '\\' | '@' | '&'
	   | '^' 
	       -> 
	       paren (align (lformula lhs $ id x ++ lformula rhs))
	   | _ ->
	       lformula f ++ tupled' lformula [lhs; rhs])

    | FAnnot (f, _) ->
	binary_operator f lhs rhs

    | x ->
	formula x ++ tupled' lformula [lhs; rhs]

and iformula f = 
  indent_code (lformula f)

and lformula f =
  formula f.fvalue

and formula_primitive = function
  | Pre -> 
      kw "pre" 

  | Post -> 
      kw "post"

  | PNot ->
      kw "not"

  | PAnd ->
      kw "and"

  | POr ->
      kw "or"

  | PEquiv ->
      kw "<=>"

  | PImply ->
      kw "=>"

  | PGreaterEqualThan ->
      kw ">="

  | PGreaterThan ->
      kw ">"

  | PLessEqualThan ->
      kw "<="

  | PLessThan ->
      kw "<"

  | PAdd ->
      kw "+"

  | PSub ->
      kw "-"

  | PMult ->
      kw "*"

  | PDiv ->
      kw "/"

  | PNeg ->
      kw "-"

  | PEPrimitive x ->
      primitive x

and fbinding (x, ty) = 
  id x ++ sym ":" ++ formula_type ty

and formula_type = function
  | FTProp ->
      kw "prop"

  | FTVar x ->
      id x

  | FTArrow (ty1, ty2) ->
      formula_type ty1 ++ sym "~>" ++ formula_type ty2

  | FTCArrow (ty1, ty2) ->
      formula_type ty1 ++ sym "->" ++ formula_type ty2

  | FTPrimitive pt ->
      primitive_type pt

  | FTProd [] ->
      lparen ^^ rparen
	
  | FTProd ts -> 
      lparen 
      ^^ paren_sep (sym " * ") (List.map formula_type ts) 
      ^^ rparen
	
  | FTApp (tycon, args) ->
      id tycon ++ tupled (List.map formula_type args)

and primitive_type = function
  | TInt ->
      kw "int"

  | TBool ->
      kw "bool" 

  | TUnit ->
      kw "unit" 

and fbindings bs = 
  tupled (List.map fbinding bs)

and fbindings' bs = 
  comma_sep (List.map fbinding bs)

and optional_formula = function
  | ImplicitFormula _ -> 
      empty

  | ExplicitFormula f ->
      line ^^ kw "where" ++ iformula f

and term t = 
  match t.tvalue with
  | EId x ->
      id x

  | EKApp (k, args) ->
      id k ++ tupled (List.map term args)

  | EApp (f, [], args, _) ->
      term f ++ tupled (List.map term args)

  | EApp (f, largs, args, _) ->
      term f 
      ++ bracket (comma_sep' lformula largs)
      ++ tupled (List.map term args)

  | ELam (largs, ins, outs, body) ->
      group 
	(kw "fun" 
	 ++ logic_arguments largs
	 ++ logic_bindings ins $ kw "returns" ++ logic_bindings outs
	++ sym "->")
	$ indent_code (term body)

  | ELet (xs, f, body, sequel) ->
      kw "let" ++ let_bindings xs ^^ optional_formula f ++ sym "=" 
	$ indent_code (term body)
	$ kw "in" 
	$ indent_code (term sequel)

  | ELetRec (xs, f, body, sequel) ->
      kw "let rec" ++ let_bindings xs ^^ optional_formula f ++ sym "=" 
	$ indent_code (term body)
	$ kw "in" 
	$ indent_code (term sequel)

  | ECase (scrutinee, ty, cs) ->
      kw "match" 
      ++ paren (indent_code (term scrutinee) ++ sym ":" ++ term_type ty)
      ++ kw "with" $ clauses cs

  | EPrimitive p ->
      primitive p

  | EForallTys (ts, t) ->
      quantified "forall" type_parameters ts iterm t 

  | EDeferred ->
      kw "?" 

  | EProd ts ->
      tupled (List.map term ts)

  | EIf (c, t, f) ->
      kw "if" ++ term c ++ kw "then" $ iterm t $ kw "else" $ iterm f

  | EAbsurd ->
      kw "absurd"

  | EAssert (f, t) ->
      kw "assert" ++ lformula f ++ kw "in" ++ iterm t

  | ELetLogic (bs, f, t) ->
      kw "let logic" ++ fbindings bs ++ kw "where" ++ lformula f 
      ++ kw "in" ++ iterm t

and iterm t = 
  indent_code (term t)

and let_bindings bs =
  tupled (List.map let_binding bs)

and let_binding (x, s) =
  lparen ++ id x ++ sym ":" ++ type_scheme s ++ rparen

and type_scheme (TScheme (ts, ty)) = 
  quantified "forall" type_parameters ts term_type ty

and formula_type_scheme (FTScheme (ts, ty)) = 
  quantified "forall" type_parameters ts formula_type ty

and clauses cs = 
  vcat (List.map clause cs)

and clause (p, t) =
  sym "|" ++ pattern p ++ sym "->" $ iterm t

and pattern = function
  | PVar (x, ty) -> 
      id x ++ sym ":" ++ term_type ty

  | PApp (k, ps) ->
      id k ++ tupled (List.map pattern ps)

and type_parameters ts = 
  group (space_sep (List.map type_parameter ts))

and type_parameter = 
  type_id

let rec component = function
  | CValue (xs, f, t) ->
      nest 2 
	(kw "val" ++ let_bindings xs ^^ optional_formula f ++ sym "="
	   $ indent_code (term t))

  | CRecValue (xs, f, t) ->
      nest 2 
	(kw "val rec" ++ let_bindings xs ^^ optional_formula f ++ sym "=" 
	   $ indent_code (term t))
      
  | CTypeDef (tid, k, DAlgebraic (ts, ds)) ->
      nest 2 
	(kw "type" ++ type_id tid ++ type_parameters ts ++ sym "="
	   $ vcat (List.map (fun (k, ty) -> 
			      sym "|" ++ id k ++ sym ":" ++ term_type ty) ds))

  | CTypeDef (tid, k, DeferredType) ->
      nest 2
	(kw "deferred type" ++ type_id tid ++ sym ":" ++ kind k)

  | CFact (Lemma n, p) ->
      nest 2
	(kw "lemma" ++ id (UniqueIdentifier.as_id n) ++ sym ":" ++ iformula p)

  | CFact (Axiom n, p) ->
      nest 2
	(kw "axiom" ++ id (UniqueIdentifier.as_id n) ++ sym ":" ++ iformula p)

  | CPredicate (pid, s, PDAbbrev f) ->
      nest 2 
	(kw "predicate" ++ id pid 
	 ++ sym ":" ++ formula_type_scheme s 
	 ++ sym "=" ++ lformula f)

  | CPredicate (pid, s, PDInductive fs) ->
      kw "inductive predicate" ++ id pid 
      ++ sym ":" ++ formula_type_scheme s 
      ++ sym "=" 
	$ vcat (List.map (fun f -> sym "|" ++ lformula f) fs)

  | CLogicFunction (pid, s, fdef) ->
      kw "logic function" ++ paren (id pid ++ sym ":" ++ formula_type_scheme s)
      ++ logic_function_definition fdef
	
and logic_function_definition = function
  | LFInductive (ts, xs, cs) ->
      tupled (List.map id xs)
      ++ sym "=" 
      ++ quantified "forall" type_parameters ts 
	Misc.id 
	(indent 2 (vcat (List.map (fun c -> sym "|" ++ logic_clause c) cs)))

  | LFAbbrev f ->
      sym "=" ++ lformula f

and logic_clause (p, f) =
  pattern p ++ sym "->" ++ lformula f

let rec program = function
  | PEmpty -> 
      empty

  | PConsComponent (c, sequel) ->
      (component c) $$ (program sequel)

let program p = as_string (program p)

let formula_as_string p = as_string (group (formula p))

let formula_type_as_string p = as_string (formula_type p)

let formula_type_scheme_as_string p = as_string (formula_type_scheme p)

let term_type_as_string p = as_string (term_type p)

let type_scheme_as_string p = as_string (type_scheme p)

