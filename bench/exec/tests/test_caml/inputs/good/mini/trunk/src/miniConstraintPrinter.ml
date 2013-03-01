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

(** This module implements a pretty printer for the constraint of the 
    Mini language. *)
include ConstraintPrettyPrinter (* TEMPORARY fusionner ces deux modules *)
open PrettyPrinter
open Constraint

let print_constraint_task = "print-constraint"
  
let print_constraint args = 
  let c = List.hd args in
  (* Remove the context since it is not interesting. *)
  let c = match c with
      CLet ([ Scheme (_, _, _, c, _) ], _) -> c
    | c -> c
  in
  printf_constraint (Txt (Channel stdout)) c

let register_tasks () =
  Processing.register
    print_constraint_task ([], ignore)
    [ [ MiniInfer.generate_constraint_task; 
	MiniSyntacticAnalysis.parse_constraint_task
      ] ]
    print_constraint
    (Misc.const true)


