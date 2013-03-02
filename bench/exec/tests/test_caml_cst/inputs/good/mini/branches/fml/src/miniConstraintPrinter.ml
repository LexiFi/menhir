(* $Id $*)

(** This module implements a pretty printer for the constraint of the 
    Mini language. *)
include ConstraintPrettyPrinter.Make (MiniConstraint) (MiniTermPrinter)
open PrettyPrinter
open Sig

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
    [ [ MiniInfer.generate_constraint_task ] ]
    print_constraint
    (Misc.const true)


