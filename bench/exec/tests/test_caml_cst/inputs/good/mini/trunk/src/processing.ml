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

(* $Id: processing.ml 422 2006-12-22 09:34:25Z regisgia $ *)

(** A simple task manager.
    It enables the registration of the options and the control
    of what has to be done by the program. 
*)

open Misc

(** {2 Tasks} *)

(* FIXME: try to type this. *)
type task_name = string
type process_type = task_name 
type process_types = process_type list list
type process_data  = Obj.t
type process_datas = Obj.t list

type process = 
    { 
      input_type : process_type list list;
      output_type: process_type;
      code       : process_types * process_datas -> process_type * process_data
    }

type options = (Arg.key * Arg.spec * Arg.doc) list * Arg.anon_fun

type task = 
    {
      name       : task_name;
      options    : options;
      depends_on : task_name list list;
      process	 : process;
      is_filled  : unit -> bool
    }

let registered_tasks = Hashtbl.create 13 

let is_registered tname =
  Hashtbl.mem registered_tasks tname

let get_registered_tasks () =
   Hashtbl.fold (fun t _ acu -> t :: acu) registered_tasks []

let task tname = 
  try
    Hashtbl.find registered_tasks tname
  with Not_found -> failwith ("Unknown task: "^ tname)

let as_process_code tname deps (f : 'a -> 'b) = 
  fun (pts, pds) -> 
    if deps = pts then 
      (tname, (Obj.magic f : Obj.t list -> Obj.t) pds)
    else failwith ("Task typing problem during "^tname^" application.")

(* Caution: the [af] function must only grab the argument, no real action
   can be done until the task is executed by the task manager. *)
let register tname ((opts, af) as options) deps process is_filled =

  (* Check that tname is not already used. *)
  if is_registered tname then
    failwith ("Task registration problem: "^tname
	      ^" is already registered")

  (* Check that dependencies are met. *)
  else if (List.exists (List.exists (notf is_registered)) deps) then
    failwith ("Task registration problem: the dependencies cannot be met for "
	      ^ tname ^".")

  (* This registration is consistent. *)
  else 
      Hashtbl.add registered_tasks tname
	{
	  name       = tname;
	  options    = options;
	  depends_on = deps;
	  process    = 
	    {
	      input_type  = deps;
	      output_type = tname;
	      code        = as_process_code tname deps process
	    };
	  is_filled  = is_filled
	}

(** {2 Processing} *)

let deps tname =
  (task tname).depends_on

let rec needed_tasks_until stopper tasks tname =
  let rec try_tasks tasks = function 
    | [] -> raise Not_found
    | t :: ts -> 
	try 
	  needed_tasks_until stopper (t :: tasks) t
	with Not_found -> try_tasks tasks ts
  in
  if tname = stopper then tasks
  else 
    let deps = deps tname in 
      (* If a task has no deps, it should be an initial task. *)
      if deps = [] then
	raise Not_found
      else 
	let ts = List.fold_left try_tasks tasks deps in
	  ts

let debug_flag = ref false

let traced_tasks = ref StringSet.empty

let trace_task tname () =
  traced_tasks := StringSet.add tname (!traced_tasks)

let current_task = ref "__no_task"

let is_task_traced task = 
  StringSet.mem task (!traced_tasks)

let is_current_task_traced () =
  is_task_traced (!current_task) 

let debug s = 
  if !debug_flag || is_current_task_traced () then 
    (output_string stderr (s ^ "\n"); flush stderr)
    
let todo = ref []

let do_task tname = 
  if not (List.mem tname (!todo)) then
    todo := tname :: (!todo)

let add_wanted_tasks tasks = 
 let insert tasks t =
    let deps = (task t).depends_on in
    let rec task_remove t = function
      | [] -> []
      | dts :: ts when List.mem t dts -> ts
      | dts :: ts -> dts :: ts
    in
    let rec insert_task seen tasks deps =
      if deps = [] then
        (List.rev seen) @ t :: tasks
      else match tasks with
        | []       -> failwith ("Impossible to do "^t)
        | t' :: ts -> insert_task (t' :: seen) ts (task_remove t' deps)
    in
      insert_task [] tasks deps
  in
    List.fold_left insert tasks (!todo)

let processing_options tstart tend = 
  [
    "--start"    , Arg.Set_string tstart  , "taskname Task to begin with";
    "--end"      , Arg.Set_string tend    , "taskname Task to end with";
    "--trace-all", Arg.Set debug_flag     , " Trace";
    "--do"       , Arg.String do_task     , "taskname Do a task"
  ]

let trace_option tname =
  ("--trace-"^tname), Arg.Unit (trace_task tname), (" Trace "^tname)

let options tstart tend = 
  let options, afs = 
    Hashtbl.fold (fun tname t (opts, afs) ->  
		    (trace_option tname :: fst t.options @ opts, 
		     snd t.options :: afs))
      registered_tasks
      ([], [])
  in
    Arg.align (processing_options tstart tend @ options), 
    fun s -> List.iter (fun f -> f s) afs

let is_valid_initial_task tname =
  is_registered tname &&
    (* A registered task is a valid initial task if it has
       been given its inputs as execution arguments. *)
    (task tname).is_filled ()

let options_analysis default_tstart default_tend usage = 
  let tstart = ref default_tstart and tend = ref default_tend in 
  let options, anon_funs = options tstart tend in
    Arg.parse options anon_funs usage;
    options, !tstart, !tend
      
let execute_tasks options usage tstart tend =
    if not (is_registered tend) then 
      begin
	Arg.usage options usage;
	failwith (tend ^ " has not been registered.")
      end
    else if not (is_valid_initial_task tstart) then
      begin
	Arg.usage options usage;
	if not (is_registered tstart) then
	  failwith (tstart ^ " is not a valid initial task.")
	else (
	  Printf.printf "%s is waiting for arguments.\n" tstart;
	  exit 0
	) 
      end
    else 
      let ntasks = needed_tasks_until tstart [] tend @ [ tend ] in
      let ntasks_with_todo = add_wanted_tasks ntasks in
      let rec find_first r = function
	| [] -> assert false
	| t :: ts -> 
	    try StringMap.find t r with Not_found -> find_first r ts
      in
	List.fold_left 
	  (fun r t -> 
	     current_task := t;
	     debug ("Processing: "^t);
	     let input_type = (task t).process.input_type in
	     let args = List.map (find_first r) input_type in
	       StringMap.add t 
		 (snd ((task t).process.code (input_type, args))) r)
	  StringMap.empty
	  ntasks_with_todo

let execute ~default_start ~default_end ~usage = 
  let options, tstart, tend = options_analysis default_start default_end usage 
  in 
    ignore (execute_tasks options usage tstart tend)
      
