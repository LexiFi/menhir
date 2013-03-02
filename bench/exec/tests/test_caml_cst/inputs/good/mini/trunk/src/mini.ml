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

(* $Id $*)

(** Task registration. *)
let _ = 
  MiniSyntacticAnalysis.register_tasks ();
  MiniPrettyPrinter.register_tasks MiniSyntacticAnalysis.parse_program_task;
  MiniInfer.register_tasks MiniSyntacticAnalysis.parse_program_task;
  MiniConstraintPrinter.register_tasks ();
  MiniSolver.register_tasks (Print.print_variable true) 
  
(** Program execution. *)
let _ = 
  Errors.handle 
    (fun () ->
       Processing.execute 
	 ~default_start: MiniSyntacticAnalysis.parse_program_task
	 ~default_end: MiniSolver.print_env_task
	 ~usage:("usage: "^Sys.executable_name^" [options] filename\n\
                  List of possible tasks: " ^
		   (String.concat ", " (Processing.get_registered_tasks ()))))

