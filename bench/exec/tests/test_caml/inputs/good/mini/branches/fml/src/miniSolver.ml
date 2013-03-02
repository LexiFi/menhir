(* $Id$ *)

(** This module instantiate the {!Solver} module for the Mini language. 
    It also manages some tracing of the solving process (as a mutable 
    graph and as text dumping). 
*)

open Misc
open Sig
open MiniAlgebra
open MiniMultiEquation
open IntRank

include (Solver.Make (MiniConstraint) (MiniUnifier) :
           Sig.Solver with module Unifier = MiniUnifier 
                      and module Unifier.MultiEquation = MiniMultiEquation)

let solve_constraint_task = "solve-constraint"
  
let solve_constraint ?tracer c =
  solve ?tracer:tracer c

let print_env_task = "print-env"

let rec print_env ?use_user_def print print_more env = 
  let print_entry acu (name, tv) =
    match tv with 
    | TVariable t ->
      if name.[0] <> '_' then
        acu
        ^^ "val " ^^ name ^^ ": " ^^ (print t) ^^ "\n"
      else 
        acu
    | _ -> 
      if name.[0] <> '_' then
        acu
        ^^ "val " ^^ name ^^ ": " ^^ (print_more tv) ^^ "\n"
      else 
        acu  
  in
    Printf.printf "%s\n" 
      (List.fold_left print_entry "" (environment_as_list env))

let no_filename = "____NoFileName____"
let trace_filename = ref no_filename
let counter = ref 0
let next_counter () = incr counter; !counter
open MiniGraph
let is_internal = function
  | CLet _ | CConjunction _ | CDisjunction _ | CInstance _ -> true
  | _ -> false

let refresh_graph solving_step c graph =
  InfiniteArray.iteri 
    (fun n nd -> 
       if nd.data <> None then
         let data = unSome nd.data in
         match data.extra_info with

           | ConstraintNode c' when c == c' && data.solved <> Yes ->
               if solving_step then (
                 data.solved <- Yes;)
               else data.solved <- Being

           | _ -> ())
    graph.nodes

let unify_in_graph t1 t2 graph =
  let is_term t d = 
    match t, d.extra_info with 

      | TTerm _, TermNode t' when t = t' -> true 

      | TVariable v, VarNode v' when v == v' -> true

      | _ -> false
  in
  let n1 = search_node graph (is_term t1) 
  and n2 = search_node graph (is_term t2) 
  in
    connect graph n1 n2;
    connect graph n2 n1

let is_variable v d = 
  match d.extra_info with 
    | VarNode v' when UnionFind.equivalent v v' -> true
    | _ -> false

let unify_vars_in_graph v1 v2 graph =
  try 
    let n1 = search_node graph (is_variable v1) 
    and n2 = search_node graph (is_variable v2) in
      connect graph n1 n2;
      connect graph n2 n1
  with Not_found -> ()
    
let highlight_vars vars gvars graph = 
    List.iter (fun v -> 
                 let ns = search_nodes graph (is_variable v) in
                   List.iter (fun n -> 
                                let nd = data_of_node graph n in
                                  if List.memq v gvars then
                                    nd.solved <- Yes
                                  else
                                    nd.solved <- Being) ns)
      vars


let write_dot ?c ?filename graph = 
  let filename, has_filename = 
    match filename with 
        None -> 
          if !trace_filename <> no_filename then 
            !trace_filename^string_of_int (next_counter ())^".dot", true
          else no_filename, false
      | Some filename -> filename, true
  in
  if has_filename then
    let out = open_out filename in
      MiniGraph.dump_as_dot 
        (fun n nd ->
           let current_constraint = ref false in
           let color = 
             match nd.solved with
                 No    -> "black"
               | Yes   -> "green"
               | Being -> "blue" in
           let shape = 
             match nd.extra_info with
                 VarNode var -> 
                   let kind = variable_kind var in 
                     if kind = Rigid then
                       "shape = \"diamond\", fontcolor=\"red\","
                     else if kind = Constant then 
                       "fontcolor=\"blue\","
                     else ""
                 | _ -> ""
           in
           let label = 
           "label =\""^(* ^(string_of_int n)^" "^ *)
             (match nd.extra_info with
                  ConstraintNode c' -> 
                    if c <> None && unSome c == c' then 
                      current_constraint := true; 
                    nd.label
                | TermNode vt -> nd.label
                | VarNode var -> 
                    nd.solved <- No; 
                    MiniTermPrinter.print_variable false var
                    ^"["^(string_of_int (variable_rank var))
                      ^"]"
                | IdNode id -> id
                | NoInfo -> nd.label)^"\","
           in
             label
             ^shape
             ^(if !current_constraint then "style=\"bold\"," else "")
             ^"color="^(if !current_constraint then "red" else color))
        out
        graph;
      close_out out
        
let do_graph f = 
  if !trace_filename <> no_filename then f ()
    
  
    
      
let print_header header title =
    Printf.printf "%s HEADER: " title;
    print_string "(";
    let f = ref true in
    let sep () = if !f then (f := false; "") else "; " in
    StringMap.iter (fun name t -> Printf.printf "%s%s : %s"
                    (sep ()) name (MiniTermPrinter.print_term false t))
    header;
    print_string ")\n"

let tracer graph = function 
    Solve c -> 
      Printf.printf "SOLVE:";
      do_graph (fun () -> refresh_graph false c (unSome !graph));
      do_graph (fun () -> write_dot ~c (unSome !graph));
      MiniConstraintPrinter.printf_constraint
        (PrettyPrinter.Txt (PrettyPrinter.Channel stdout)) c
  | Solved c -> 
      Printf.printf "SOLVED:";
      do_graph (fun () -> refresh_graph true c (unSome !graph));
      do_graph (fun () -> write_dot ~c (unSome !graph));
      MiniConstraintPrinter.printf_constraint
        (PrettyPrinter.Txt (PrettyPrinter.Channel stdout)) c
  | Init c -> 
      do_graph (fun () -> graph := Some (constraint_as_graph c))
  | UnifyTerms (t1, t2) -> 
      do_graph (fun () -> unify_in_graph t1 t2 (unSome !graph));
      do_graph (fun () -> write_dot (unSome !graph));
      Printf.printf "UNIFY:";
      Printf.printf "%s =?= %s\n" 
        (MiniTermPrinter.print_term false t1)
        (MiniTermPrinter.print_term false t2)
  | UnifyVars (v1, v2) -> 
      do_graph (fun () -> unify_vars_in_graph v1 v2 (unSome !graph));
      do_graph (fun () -> write_dot (unSome !graph));
      Printf.printf "UNIFY_VARS:";
      Printf.printf "%s =?= %s\n" 
        (MiniTermPrinter.print_variable false v1)
        (MiniTermPrinter.print_variable false v2)
  | Generalize (rank, vars) ->
(*  TODO_DISPLAY
    let generalized_vars = 
        List.filter (fun v -> (UnionFind.find v).rank = Rank.none) vars
      in
      do_graph (fun () -> 
                  highlight_vars vars generalized_vars (unSome !graph)); *)
      Printf.printf "DECIDE GEN FOR: %d %s\n" rank
        (print_separated_list " " (MiniTermPrinter.print_variable false) vars)
  | ChoppedHeader header ->
      print_header header "CHOPPED"
  | Generalized vars -> 
      Printf.printf "GENERALIZED: ";
      List.iter (fun var -> 
          Printf.printf "%s; " 
            (MiniTermPrinter.print_variable false var) 
        )
        vars;
      print_string "\n" 
  | ReturnHeader header ->
      Printf.printf "------------------------------------------------------------------------------------\n";
      print_header header "RETURNED"
  | ImpredOccurence (var, sigma) ->
      Printf.printf "ImpredOccurence: %s => %s \n"
        (MiniTermPrinter.print_variable false var)
        (MiniTermPrinter.print_term false sigma)
  | ImpredSolving alphas ->   
      Printf.printf "StartImpred: (";
      List.iter (fun alpha -> 
          Printf.printf "%s;" 
            (MiniTermPrinter.print_variable false alpha) 
        )
        alphas;
      print_string ")\n" 
  | ImpredSubstitution substitution -> 
      Printf.printf "ImpredResult: (";
      List.iter (fun (var, sigma) -> 
          Printf.printf "%s : %s;" 
            (MiniTermPrinter.print_variable false var) 
            (MiniTermPrinter.print_term false sigma)
        )
        substitution;
      print_string ")\n"

       
let optional_tracer () =
  if Processing.is_task_traced solve_constraint_task then
    let graph = ref None in
      fun c -> (tracer graph c)
  else ignore

let register_tasks term_printer term_printer_more =
  Processing.register
    solve_constraint_task 
    ([
       "--trace-dot"    , Arg.Set_string trace_filename  , 
       "filename Write filename.dot to trace the solver.";
     ], ignore)
    [ [ MiniInfer.generate_constraint_task; MiniSyntacticAnalysis.parse_constraint_task ] ]
    (fun t -> solve_constraint ~tracer:(optional_tracer ()) (List.hd t)) 
    (const true);
    
  Processing.register
    print_env_task ([], ignore)
    [ [ solve_constraint_task ] ]
    (fun t -> print_env term_printer term_printer_more (List.hd t)) 
    (const true);
    
