(* $Id: pangolin.ml 58 2007-10-12 14:30:55Z yann.regisgianas $ *)

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

let core_inference_options = CoreInference.options ()
let arguments = Options.process "Usage:\n" [
  core_inference_options
] 

let filename = 
  try 
    List.hd arguments 
  with _ -> 
    Error.global_error "during initialization"
      (Printf.sprintf "   No input files.")

let raw_ast          = SyntacticAnalysis.process filename
let internal_ast     = NameAnalysis.process raw_ast
let internal_anf_ast = ANormalForm.process internal_ast
let full_ast         = CorePostConditionsInference.process internal_anf_ast
let xast             = CoreInference.process full_ast 
let unit_and_proofs  = VerificationGeneration.process xast
