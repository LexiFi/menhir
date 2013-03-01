(* $Id $*)

(** Task registration. *)
let _ = 
  MiniSyntacticAnalysis.register_tasks ();
  MiniCoercionInsertion.register_tasks MiniSyntacticAnalysis.parse_program_task;
  MiniPrettyPrinter.register_tasks 
    MiniSyntacticAnalysis.parse_program_task
    MiniCoercionInsertion.generate_coercions_task;
  MiniInfer.register_tasks ();
  MiniConstraintPrinter.register_tasks ();
  MiniSolver.register_tasks (MiniTermPrinter.print_variable true) 
  
(** Program execution. *)
let _ = 
  Errors.handle 
    (fun () ->
       Processing.execute 
	 ~default_start: MiniSyntacticAnalysis.parse_program_task
	 ~default_end: MiniSolver.print_env_task
	 ~usage:("usage: "^Sys.executable_name^" [options] filename"))

