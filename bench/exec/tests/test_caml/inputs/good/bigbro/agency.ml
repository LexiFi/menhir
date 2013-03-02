(* $Header: /net/pauillac/caml/repository/bigbro/agency.ml,v 1.1.1.1 2001/02/13 15:39:35 fpottier Exp $ *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The type declaration. It would be nice to move it to some other module and make this one totally independent of the
type of jobs by turning it into a functor. However, we would have circular dependency problems. We use a ref-based
solution, which means that the type of jobs must be known (otherwise a polymorphic reference would be needed).

*)

type job =
    JobInitial of string
  | JobNetworkCheck of Url.url * Url.url * bool
  | JobRecurse of string * Url.url * Link_info.document_source

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The initialization function.

*)

let action = ref (fun _ -> assert false);;
let termination = ref (fun _ -> assert false);;

let initialize user_action user_termination =
  action := user_action;
  termination := user_termination
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Initialize the global variables and the lock. By convention, in the rest of this file, functions whose name begins
with internal_ assume that the lock has been acquired.

Here is a neat trick. (Rather delicate to understand maybe, but neat because it solves a problem in one line of code.)
The Agency reports that all jobs have been completed when the thread count drops to 0. At the beginning of the program,
the main thread queues several jobs. If one of them completes before the main thread is done queuing, completion will
be incorrectly reported. The solution is to artificially have the thread count start out at 1, and have the main
thread call killed() when done queuing. This way, completion cannot be reported until after the queuing is done.

*)

let queue = Queue.create()
and thread_count = ref 1
and lock = Mutex.create()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function spawns a thread to handle a job. The thread count must be less than the maximum allowed.

*)

let rec internal_spawn job =

  (* Increment the thread count. *)

  incr thread_count;
  Settings.DebugOutput.print (fun () -> Printf.sprintf "Thread count goes up to %d.\n" !thread_count);

  (* Spawn a thread for this job. Wrap it with some code to decrease the thread counter and start a new job
     when this one is done. *)

  let _ = Thread.create (fun () ->
    try
      !action job;
      killed()
    with error ->
      killed();
      raise error
  ) () in

  ()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function is called when a thread just died or is about to die. If any jobs are in the queue, it spawns a new
thread.

*)

and killed() =

  let finished = Thread_utils.protect lock (fun () ->

    (* Decrease the thread count to account for the dying thread. *)

    decr thread_count;
    Settings.DebugOutput.print (fun () -> Printf.sprintf "Thread count goes down to %d.\n" !thread_count);

    (* Check whether there any jobs in the queue, and if so, spawn one. *)

    try
      let job = Queue.take queue in
      internal_spawn job;
      false
    with Queue.Empty ->
      !thread_count = 0

  ) () in

  (* If the last job was just processed, signal the user. *)

  if finished then
    !termination()
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Accepting a new job. If the thread count is less than the maximum, we shall spawn a thread immediately. Otherwise,
we shall queue the request.

*)

let add job =
  Thread_utils.protect lock (fun () ->

    if !thread_count < Settings.max_threads then
      internal_spawn job
    else begin
      Settings.DebugOutput.print (fun () -> "Queuing a job.\n");
      Queue.add job queue
    end

  ) ()
;;
