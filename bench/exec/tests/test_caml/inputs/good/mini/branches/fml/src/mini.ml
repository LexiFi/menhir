(* $Id $*)

(** Task registration. *)


let _ = 
  MiniSyntacticAnalysis.register_tasks ();
  MiniPrettyPrinter.register_tasks     MiniSyntacticAnalysis.parse_program_task;   
  MiniInternalize.register_tasks       MiniSyntacticAnalysis.parse_program_task;
  MiniElaboration.register_tasks       MiniInternalize.internalize_task;
  MiniAstPrettyPrinter.register_tasks  MiniElaboration.elaborate_task;
  MiniInfer.register_tasks             MiniElaboration.elaborate_task;
  MiniConstraintPrinter.register_tasks ();
  MiniSolver.register_tasks 
    (MiniTermPrinter.print_variable true) 
    (MiniTermPrinter.print_crterm) 
  
(** Program execution. *)
let _ = 
  Errors.handle 
    (fun () ->
       Processing.execute 
	 ~default_start: MiniSyntacticAnalysis.parse_program_task
	 ~default_end: MiniSolver.print_env_task
	 ~usage:("usage: "^Sys.executable_name^" [options] filename")
        [ 
          "--elaborate", Arg.Int MiniElaboration.set, 
          "set elaboration level"
        ])

