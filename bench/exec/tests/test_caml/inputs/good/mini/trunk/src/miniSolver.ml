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

(* $Id: miniSolver.ml 421 2006-12-22 09:27:42Z regisgia $ *)

(** This module instantiate the {!Solver} module for the Mini language. 
    It also manages some tracing of the solving process. *)

open Misc
open MiniAlgebra
open CoreAlgebra
open MultiEquation
include Solver (* TEMPORARY probablement inutile *)

let solve_constraint_task = "solve-constraint"
  
let solve_constraint ?tracer c =
  solve ?tracer:tracer c

let print_env_task = "print-env"

let rec print_env ?use_user_def print env = 
  let print_entry acu (name, t) =
    if name.[0] <> '_' then
      acu
      ^ "val " ^ name ^ ": " ^ (print t) ^ "\n"
    else 
      acu
  in
    Printf.printf "%s\n" 
      (List.fold_left print_entry "" (environment_as_list env))

let register_tasks term_printer =
  Processing.register
    solve_constraint_task 
    ([], ignore)
    [ [ MiniInfer.generate_constraint_task; 
	MiniSyntacticAnalysis.parse_constraint_task ] ]
    (fun t -> solve_constraint (List.hd t))
    (const true);
    
  Processing.register
    print_env_task ([], ignore)
    [ [ solve_constraint_task ] ]
    (fun t -> print_env term_printer (List.hd t)) 
    (const true);
    
