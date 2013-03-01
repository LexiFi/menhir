(* $Id: xCoreInConstraint.ml 52 2007-10-01 14:46:30Z yann.regisgianas $ *)

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

open Constraint

type name = Constraint.sname
type names = sname list

let mk_lterm pos t = 
  { tpos = pos; tvalue = t }

let mk_lformula pos t = 
  { fpos = pos; fvalue = t }

type  xsyntax = 
  | XProgram of program
  | XComponent of component
  | XTerm of lterm
  | XLetBinding of (id * type_scheme * formula_type)
  | XLogicBindings of (let_bindings * optional_lformula)
  | XBinding of (id * term_type * formula_type) 
  | XBindings of (id * term_type * formula_type) list
  | XFBinding of fbinding
  | XFBindings of fbindings
  | XFormula of lformula
  | XOptFormula of optional_lformula
  | XFormulaTypeScheme of formula_type_scheme
  | XFormulaType of formula_type
  | XClause of clause
  | XPattern of pattern
  | XInductiveCases of lformula list
  | XPredicateDef of predicate_definition
  | XLogicClause of logic_clause
  | XLogicFunctionDefinition of logic_function_definition 
  | XAnnot of  xsyntax * term_type
  | XTermType of term_type

type  tc = xsyntax Constraint.tconstraint


let as_xcore_letbinding = function XLetBinding x -> x | _ -> assert false
let as_xcore_binding = function XBinding x -> x | _ -> assert false
let as_xcore_fbinding = function XFBinding x -> x | _ -> assert false
let as_xcore_term = function XTerm t -> t | _ -> assert false
let as_xcore_formula = function XFormula t -> t | _ -> assert false
let as_xcore_clause = function XClause c -> c | _ -> assert false
let as_xcore_pattern = function XPattern c -> c | _ -> assert false
let as_xcore_logic_clause = function XLogicClause c -> c | _ -> assert false

let mk_xcore_fbindings pos ids = 
  let cids = 
    List.map (fun x -> CGraft 
	      (PIdentifier.position x, x,
	       fun t s k -> XFBinding
		 (x, CoreInferenceInternals.formula_type k s)))
      ids
  in
    CTerm (pos, cids, fun bindings ->
	     XFBindings (List.map as_xcore_fbinding bindings)) 

let mk_xcore_primitive pos p = 
  CTerm (pos, [], fun _ -> XTerm (mk_lterm pos (EPrimitive p)))

let mk_xcore_deferred_term pos =
  CTerm (pos, [], fun _ -> XTerm (mk_lterm pos EDeferred))

let mk_xcore_eabsurd pos =
  CTerm (pos, [], fun _ -> XTerm (mk_lterm pos EAbsurd))

let mk_xcore_assert pos c t =
  CTerm (pos, [c;t], function [ XFormula f; XTerm t ] -> 
	   XTerm (mk_lterm pos (EAssert (f, t)))
	   | _ -> assert false)

let mk_xcore_var x = 
  let pos = PIdentifier.position x in
    CTerm (PIdentifier.position x, [], 
	   fun _ -> XTerm (mk_lterm pos (EId x)))

let mk_xcore_forall_tys pos vs t = 
  CTerm (pos, [ t ], function [ XTerm t ] ->
	   XTerm (mk_lterm pos (EForallTys (vs, t)))
	   | _ -> assert false)

let mk_xcore_term pos t = 
  CGraft (pos, t, fun t s k -> t)

let mk_xcore_app pos ret_type f largs args =
  let c = 
    CGraft (pos, ret_type, 
	    fun t s k -> XTermType (CoreInferenceInternals.term_type k s))
  in
    CTerm (pos, c :: f :: largs @ args, 
	   function XTermType ty :: XTerm f :: args ->
	     let (largs, args) = Misc.extract (List.length largs) args in
	       XTerm (mk_lterm pos (EApp (f, 
					  List.map as_xcore_formula largs, 
					  List.map as_xcore_term args, ty)))
	     | _ -> assert false)

let mk_xcore_kapp pos f args =
  CTerm (pos, f :: args, function (XTerm f) :: args ->
	   (match f.tvalue with
	      | EId k -> 
		  XTerm (mk_lterm pos (EKApp (k, List.map as_xcore_term args)))
	      | _ -> assert false)
	   | _ -> assert false)

let mk_xcore_bindings pos ids = 
  let cids = 
    List.map (fun x -> CGraft 
	      (PIdentifier.position x, x,
	       fun t s k -> XBinding
		 (x, 
		  CoreInferenceInternals.term_type k s,
		  CoreInferenceInternals.formula_type k s)))
      ids
  in
    CTerm (pos, cids, fun bindings ->
	     XBindings (List.map as_xcore_binding bindings)) 

let implicit_formula_abstracted_by bs = function
  | ExplicitFormula f ->
      ExplicitFormula (mk_lformula f.fpos (FLam (bs, f)))

  | f -> f

let mk_xcore_lam pos largs ins pre outs post body = 
  CTerm (pos, [ 
	   mk_xcore_fbindings pos largs;
	   mk_xcore_bindings pos ins; pre; 
	   mk_xcore_bindings pos outs; post ; body ], 
	 function [ 
	   XFBindings largs;
	   XBindings ins; XOptFormula pre; 
	   XBindings outs; XOptFormula post; XTerm body ] ->
	   let ins = List.map (fun (x, ty, _) -> (x, ty)) ins in
	   let outs = List.map (fun (x, ty, _) -> (x, ty)) outs in
	     XTerm (mk_lterm pos (ELam (largs, (ins, pre), (outs, post), body)))
	   | _ -> assert false
	)

let mk_xcore_if pos c t f = 
  CTerm (pos, [ c; t; f ], 
	 function [ XTerm c; XTerm t; XTerm f ] ->
	   XTerm (mk_lterm pos (EIf (c, t, f)))
	   | _ -> assert false)

let extract_quantified_tys (FTScheme (ts, _)) = ts

let extract_rigid_variables bs = 
  List.fold_left (fun vs (_, TScheme (ts, _)) -> vs @ ts) [] bs

let mk_xcore_logic_bindings pos ids f = 
  let cids = 
    List.map (fun x -> CGraft 
	      (PIdentifier.position x, x,
	       fun t s k -> 
		 let fs = CoreInferenceInternals.type_scheme k s in
		 let ft = CoreInferenceInternals.formula_type k s in
		   XLetBinding (x, fs, ft)))
      ids
  in
  let f = CGraft (pos, f, fun t s k -> t) in
    CTerm (pos, cids @ [ f ] , fun cs ->
	     let (bindings, f) = Misc.extract (List.length cids) cs in
	     let f = match f with [ XOptFormula f ] -> f | _ -> assert false in
	     let bs = 
	       List.map (function XLetBinding (x, s, _) -> (x, s)
			 | _ -> assert false) bindings 
	     in
	     let vs = extract_rigid_variables bs in

	     let f = 
	       match f with 
		 | ImplicitFormula pos -> 
		     ImplicitFormula pos
		 | ExplicitFormula f -> 
		     ExplicitFormula (mk_lformula pos (FForallTys (vs, f)))
	     in
	       XLogicBindings (bs, f))


let mk_xcore_let pos lt def t = 
  CTerm (pos, [ lt; def; t ],
	 function [ XLogicBindings (bs, f); XTerm def; XTerm t ] ->
	   let gs = extract_rigid_variables bs in
	   let def = mk_lterm pos (EForallTys (gs, def)) in
	     XTerm (mk_lterm pos (ELet (bs, f, def, t)))
	   | _ -> assert false)

let mk_xcore_letrec pos lt def t = 
  CTerm (pos, [ lt; def; t ],
	 function [ XLogicBindings (bs, f); XTerm def; XTerm t ] ->
	   let gs = extract_rigid_variables bs in
	   let def = mk_lterm pos (EForallTys (gs, def)) in
	     XTerm (mk_lterm pos (ELetRec (bs, f, def, t)))
	   | _ -> assert false)

let mk_xcore_prod pos ts =
  CTerm (pos, ts, function ts ->
	   XTerm (mk_lterm pos (EProd (List.map as_xcore_term ts))))

let mk_xcore_pvar pos x = 
  CGraft (pos, x, 
	  fun t s k -> XPattern 
	    (PVar (x, CoreInferenceInternals.term_type k s)))

let mk_xcore_pkapp pos k ps =
  CTerm (pos, ps, function ps ->
	   XPattern (PApp (k, List.map as_xcore_pattern ps)))
    
let mk_xcore_case pos s cs = 
  let c = CGraft (pos, s, fun t s k -> 
		     XAnnot (t, CoreInferenceInternals.term_type k s))
  in
  CTerm (pos, c :: cs, 
	 function (XAnnot (XTerm s, ty)) :: cs ->
	   let cs = List.map as_xcore_clause cs in
	     XTerm (mk_lterm pos (ECase (s, ty, cs)))
	   | _ -> assert false)

let mk_xcore_clause pos pname t =
  let p = CGraft (pos, pname, fun t s k -> t) in
    CTerm (pos, [ p; t ], 
	   function [ XPattern p; XTerm t ] ->
	     XClause (p, t)
	     | _ -> assert false)


let mk_xcore_fvar x = 
  let pos = PIdentifier.position x in
    CTerm (PIdentifier.position x, [], 
	   fun _ -> XFormula (mk_lformula pos (FVar x)))

let mk_xcore_fprod pos ts =
  CTerm (pos, ts, function ts ->
	   XFormula (mk_lformula pos (FProd (List.map as_xcore_formula ts))))

let mk_xcore_logic_clause pos pname t =
  let p = CGraft (pos, pname, fun t s k -> t) in
    CTerm (pos, [ p; t ], 
	   function [ XPattern p; XFormula t ] ->
	     XLogicClause (p, t)
	     | _ -> assert false)

let mk_xcore_logic_cases_on pos xs cs =
  CTerm (pos, cs, 
	 fun cs ->
	   let cs = List.map as_xcore_logic_clause cs in
	     XLogicFunctionDefinition (LFInductive ([], xs, cs)))

let mk_xcore_let_logic pos bs f t = 
  CTerm (pos, [ mk_xcore_fbindings pos bs; f; t ], 
	 function [ XFBindings bs; XFormula f; XTerm t ] ->
	   XTerm (mk_lterm pos (ELetLogic (bs, f, t)))
	   | _ -> assert false
	)

let mk_xcore_flam pos ins body = 
  CTerm (pos, [ mk_xcore_fbindings pos ins; body ], 
	 function [ XFBindings ins; XFormula body ] ->
	   XFormula (mk_lformula pos (FLam (ins, body)))
	   | _ -> assert false
	)

let mk_xcore_fannot pos n = 
  CGraft (pos, n, fun t s k -> 
	    let f = as_xcore_formula t in
	      XFormula (mk_lformula pos 
			  (FAnnot (f, CoreInferenceInternals.formula_type k s))))

let mk_xcore_fapp pos f args =
  CTerm (pos, f :: args, function (XFormula f) :: args ->
	   XFormula (mk_lformula pos (FApp (f, 
					    List.map as_xcore_formula args)))
	   | _ -> assert false)

let remove_fannot = function
  | FAnnot (f, _) -> f.fvalue
  | f -> f

let mk_xcore_fkapp pos f args =
  CTerm (pos, f :: args, function (XFormula f) :: args ->
	   let k = match remove_fannot f.fvalue with FVar k -> k 
	     | _ -> assert false in
	     XFormula (mk_lformula pos 
			 (FKApp (k, List.map as_xcore_formula args)))
	   | _ -> assert false)

let mk_xcore_fforall_tys pos qid f = 
  let qc = 
    CGraft (pos, qid, 
	    fun t s k ->
	      XFormulaTypeScheme 
		(CoreInferenceInternals.formula_type_scheme k s))
  in
  let qf = 
    CGraft (pos, f, fun t s k -> t)
  in
    CTerm (pos, [ qc; qf ], 
	   function [ XFormulaTypeScheme s; XFormula f ] ->
	     let FTScheme (vs, _) = s in 
	       if vs = [] then
		 XFormula f
	       else
		 XFormula (mk_lformula pos (FForallTys (vs, f)))
	     | _ -> assert false)

let mk_xcore_forall pos ins tgs body = 
  CTerm (pos, [ mk_xcore_fbindings pos ins; body ], 
	 function [ XFBindings ins; XFormula body ] ->
	   XFormula (mk_lformula pos (FForall (ins, tgs, body)))
	   | _ -> assert false
	)

let mk_xcore_exists pos ins body = 
  CTerm (pos, [ mk_xcore_fbindings pos ins; body ], 
	 function [ XFBindings ins; XFormula body ] ->
	   XFormula (mk_lformula pos (FExists (ins, body)))
	   | _ -> assert false
	)

let mk_xcore_feq pos ty f1 f2 = 
  let cty = CGraft (pos, ty, 
		    fun t s k -> 
		      XFormulaType (CoreInferenceInternals.formula_type k s))
  in
  CTerm (pos, [ cty; f1; f2 ], 
	 function [ XFormulaType ty; XFormula f1; XFormula f2 ] ->
	   XFormula (mk_lformula pos (FEq (ty, f1, f2)))
	   | _ -> assert false
	)

let mk_xcore_fdeferred pos = 
  CTerm (pos, [], fun _ -> XFormula (mk_lformula pos FDeferred))

let convert_primitive = function 
  | CoreSyntax.PInt x -> PInt x
  | CoreSyntax.PTrue -> PTrue
  | CoreSyntax.PFalse -> PFalse

let convert_formula_primitive = function
  | CoreSyntax.Pre -> Pre
  | CoreSyntax.Post -> Post
  | CoreSyntax.PAnd -> PAnd
  | CoreSyntax.POr -> POr
  | CoreSyntax.PEquiv -> PEquiv
  | CoreSyntax.PImply -> PImply
  | CoreSyntax.PNot -> PNot
  | CoreSyntax.PLessThan -> PLessThan
  | CoreSyntax.PGreaterThan -> PGreaterThan
  | CoreSyntax.PLessEqualThan -> PLessEqualThan
  | CoreSyntax.PGreaterEqualThan -> PGreaterEqualThan
  | CoreSyntax.PAdd -> PAdd
  | CoreSyntax.PSub -> PSub
  | CoreSyntax.PMult -> PMult
  | CoreSyntax.PDiv -> PDiv
  | CoreSyntax.PNeg -> PNeg
  | CoreSyntax.PEPrimitive x -> PEPrimitive (convert_primitive x)
      

let mk_xcore_formula_primitive pos p = 
  CTerm (pos, [], 
	 fun _ -> XFormula (mk_lformula pos 
			      (FPrimitive (convert_formula_primitive p))))

let mk_xcore_formula_peprimitive pos p = 
  CTerm (pos, [p], 
	 function [ XTerm t ] -> 
	   let p = match t.tvalue with EPrimitive p -> p | _ -> assert false in
	     XFormula (mk_lformula pos (FPrimitive (PEPrimitive p)))
	   | _ -> assert false)

let mk_xcore_formula pos f = 
  CTerm (pos, [], fun _ -> XFormula (mk_lformula pos f))

let mk_xcore_implicit_formula pos = 
  CTerm (pos, [], fun f -> XOptFormula (ImplicitFormula pos))

let mk_xcore_explicit_formula pos f = 
  CTerm (pos, [f], function [ XFormula f ] -> 
	   XOptFormula (ExplicitFormula f)
	   | _ -> assert false)

let mk_xcore_optional_formula pos t = 
  CGraft (pos, t, fun t s k -> t)

let mk_xcore_inductive_cases pos cs = 
  CTerm (pos, cs, fun cs -> XInductiveCases (List.map as_xcore_formula cs))

let mk_xcore_pdinductive pos cs = 
  CTerm (pos, cs,
	 function cs ->
	   let cs = List.map as_xcore_formula cs in
	     XPredicateDef (PDInductive (cs)))

let mk_xcore_pdabbrev pos f = 
  CTerm (pos, [ CGraft (pos, f, fun t s k -> t) ], 
	 function [ XFormula f ] -> XPredicateDef (PDAbbrev f)
	   | _ -> assert false)

let mk_xcore_lfabbrev pos f = 
  CTerm (pos, [ f ],
	 function [ XFormula f ] -> XLogicFunctionDefinition (LFAbbrev f)
	   | _ -> assert false)

let mk_xcore_xformula pos f = 
  CGraft (pos, f, fun t s k -> t) 

let mk_xcore_cvalue pos bs t = 
  CTerm (pos, [ bs; t ], 
	 function [ XLogicBindings (bs, f); XTerm t ] ->
	   let gs = extract_rigid_variables bs in
	     XComponent (CValue (bs, f, mk_lterm pos (EForallTys (gs, t))))
	   | _ -> assert false)

let mk_xcore_crecvalue pos bs t = 
  CTerm (pos, [ bs; t ], 
	 function [ XLogicBindings (bs, f); XTerm t ] ->
	   let gs = extract_rigid_variables bs in
	     XComponent (CRecValue (bs, f, mk_lterm pos (EForallTys (gs, t))))
	   | _ -> assert false)

let mk_xcore_type_definition pos t k tdef = 
  CTerm (pos, [], fun _ -> XComponent (CTypeDef (t, k, tdef)))

let mk_xcore_logic_function pos x p =
  CTerm (pos, [ 
	   CGraft (pos, x, fun t s k -> 
		     XFormulaTypeScheme 
		       (CoreInferenceInternals.formula_type_scheme k s));
	   CGraft (pos, p, fun t s k -> t)
	 ],
	 function [ XFormulaTypeScheme ((FTScheme (vs', ty)) as s); 
		    XLogicFunctionDefinition fdef ] -> 
	   let fdef = 
	     match fdef with
	       | LFInductive (vs, xs, cs) ->
		   LFInductive (vs @ vs', xs, cs)
		     
	       | fdef -> fdef
	   in
	     XComponent (CLogicFunction (x, s, fdef))
	   | _ -> assert false)

let mk_xcore_predicate_definition pos p pdef = 
  CTerm (pos, 
	 [ CGraft 
	     (pos, p, 
	      fun t s k -> 
		XFormulaTypeScheme 
		  (CoreInferenceInternals.formula_type_scheme k s));
	   pdef
	 ], 
	 function [ XFormulaTypeScheme s; XPredicateDef pdef ] -> 
	   let pdef = 
	     match pdef with
	       | PDInductive _ -> pdef
	       | PDAbbrev t -> 
		   let gs = extract_quantified_tys s in 
		     PDAbbrev (mk_lformula pos (FForallTys (gs, t)))
	   in
	     XComponent (CPredicate (p, s, pdef))
	 | _ -> assert false)

let mk_xcore_fact pos s f = 
  let s = match s with 
    | CoreSyntax.Lemma n -> Lemma n
    | CoreSyntax.Axiom n -> Axiom n
  in
    CTerm (pos, [f], 
	   function [ XFormula f ] -> XComponent (CFact (s, f))
	   | _ -> assert false)

let mk_xcore_pempty pos = 
  CTerm (pos, [], fun _ -> XProgram PEmpty)

let mk_xcore_pcons pos c p = 
  CTerm (pos, [ c; p ], 
	 function 
	   | [ XComponent c ; XProgram p ] -> XProgram (PConsComponent (c, p))
	   | _ -> assert false)

let as_xcore_program = function
  | XProgram x -> x 
  | _ -> assert false

   
     

