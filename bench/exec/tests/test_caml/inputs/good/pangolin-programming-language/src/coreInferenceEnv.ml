(* $Id: coreInferenceEnv.ml 54 2007-10-01 14:50:09Z yann.regisgianas $ *)

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

type t =
    {
      ids   : PIdentifier.t CoreSyntax.Var.AtomMap.t;
      types : CoreInferenceInternals.env;
      tvars : PIdentifier.t CoreSyntax.Var.AtomMap.t;
    }

let empty =
  {
    ids   = CoreSyntax.Var.AtomMap.empty;
    types = CoreInferenceInternals.empty_env;
    tvars = CoreSyntax.Var.AtomMap.empty
  }

let explicit_id x = 
  let s = CoreSyntax.Var.Atom.basename x in 
    s

let introduce_identifier env x = 
  let id = explicit_id x in 
  let env = 
    { 
      ids   = CoreSyntax.Var.AtomMap.add x id env.ids;
      types = env.types;
      tvars = env.tvars
    }
  in
    (env, id)

let introduce_identifiers env xs = 
  Misc.list_fold_map introduce_identifier env xs

let explicit_identifier_for y env = 
  try 
    CoreSyntax.Var.AtomMap.find y env.ids
  with Not_found -> assert false

let types_env env = 
  env.types

let introduce_type_variable_from_atom k v env = 
  let name = PIdentifier.fresh (CoreSyntax.Var.Atom.basename v) in
    CoreInferenceInternals.introduce_type_variable k v name env

let introduce_type_constant_variable env v n = 
  CoreInferenceInternals.introduce_type_variable MultiEquation.Constant v n env

let introduce_rigid_type_variable env v = 
  introduce_type_variable_from_atom MultiEquation.Rigid v env

let introduce_flexible_type_variable env v = 
  introduce_type_variable_from_atom MultiEquation.Flexible v env
    
let introduce_type_identifier env x = 
  let id = explicit_id x in
    CoreSyntax.Var.AtomMap.add x id env

let introduce_rigid_type_variables vs env = 
  { env with 
      types = List.fold_left introduce_rigid_type_variable env.types vs;
      tvars = List.fold_left introduce_type_identifier env.tvars vs
  }

let introduce_type_constant x env = 
  let id = explicit_id x in
    { env with 
	types = introduce_type_constant_variable env.types x id;
	tvars = CoreSyntax.Var.AtomMap.add x id env.tvars
    }, id

let lookup_rigid_type_variable env v = 
  CoreInferenceInternals.lookup_type_variable v env.types

let lookup_type_constant env v = 
  CoreInferenceInternals.lookup_type_variable v env.types

let introduce_flexible_type_variables vs env = 
  { env with 
      types = List.fold_left introduce_flexible_type_variable env.types vs;
      tvars = List.fold_left introduce_type_identifier env.tvars vs
  }

let lookup_flexible_type_variable env v = 
  CoreInferenceInternals.lookup_type_variable v env.types

let explicit_type_identifier_for v env = 
  try
    CoreSyntax.Var.AtomMap.find v env.tvars
  with Not_found -> assert false

open CoreSyntax

let freshtvar a =
  Var.Atom.freshb (PIdentifier.fresh_type_id (Position.dummy, a)) 

let formula_type_scheme_of_pre = 
  let a = freshtvar "'a" in
  let b = freshtvar "'b" in
  let c = freshtvar "'c" in
    
    FTScheme (create_formula_scheme_abs 
		([ a; b; c ], 
		 FTArrow (FTCArrow (FTVar a, FTVar b),
			  FTArrow (FTVar c, 
				   FTArrow (FTVar a, FTProp)))))
			

let formula_type_scheme_of_post = 
  let a = freshtvar "'a" in
  let b = freshtvar "'b" in
  let c = freshtvar "'c" in
    
    FTScheme (create_formula_scheme_abs 
		([ a; b; c ], 
		 FTArrow (FTCArrow (FTVar a, FTVar b),
			  FTArrow (FTVar c,
				   FTArrow (FTVar a,
					    FTArrow (FTVar b,
						     FTProp))))))
			
let formula_type_of_logical_connective =
  FTArrow (FTProd [ FTProp; FTProp ], FTProp)

let formula_type_of_not =
  FTArrow (FTProp, FTProp)

let formula_type_of_integer_relation =
  FTArrow (FTProd [ FTPrimitive TInt; FTPrimitive TInt ], FTProp)

let formula_type_of_integer_function =
  FTArrow (FTProd [ FTPrimitive TInt; FTPrimitive TInt ], FTPrimitive TInt)
