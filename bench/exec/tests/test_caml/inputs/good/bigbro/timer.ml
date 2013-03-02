(* $Header: /net/pauillac/caml/repository/bigbro/timer.ml,v 1.2 2001/08/01 11:43:54 fpottier Exp $ *)

open Unix

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Type declarations.

*)

type task = Task of (unit -> unit)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

If the settings specify no timeout value, then all operations in this module become no-ops. This test is done only
once.

*)

let schedule, cancel, beat = match Settings.timeout with
  None ->
    let nop x = () in
    nop, nop, nop

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

If the settings do specify a timeout value, then the module's implementation is non-trivial.

This queue contains all outstanding requests. It is accompanied by a lock. Since the execution delay is constant,
execution order corresponds to scheduling order, which allows us to use a FIFO queue.

*)

| Some delay ->

    let queue = Queue.create()
    and lock = Mutex.create() in

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Scheduling and canceling tasks.

*)

    let schedule task =
      Thread_utils.protect lock (fun () ->

	(* Determine when this task should be executed and prepend a new request to the queue. *)

	Settings.DebugOutput.print (fun () -> "Scheduling a task.\n");
	Queue.add (time() +. delay, ref task) queue

      ) () in

    let trivial_task =
      Task (fun () -> ()) in

    let cancel task =
      Thread_utils.protect lock (fun () ->

	(* Unfortunately, the module Queue does not allow removing elements at a random position in the queue.
	   We use Queue.iter to walk the queue. When we find the desired element, we can't remove it, so we replace its
	   task with a trivial one. The request will still be executed, but it will have no effect. *)

	let found = ref false in

	Queue.iter (fun (_, taskref) ->
	  if !taskref == task then begin
	    Settings.DebugOutput.print (fun () -> "Cancelling a task.\n");
	    taskref := trivial_task;
	    found := true
	  end
	) queue;

	if not !found then
	  raise Not_found

      ) () in

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This function provides the heartbeat of the timer; it must be called as often as possible.

*)

    let beat () =

      Thread_utils.protect lock (fun () ->

	(* Get the current time. *)

	let time = time() in

	(* This loop executes all requests whose delay has expired. *)

	let rec loop () =

	  try

	    (* Have a look at the first element of the queue. *)

	    let stamp, { contents = Task task } = Queue.peek queue in

	    (* Check whether the first element of the queue should be executed. *)

	    if stamp <= time then begin

	      (* If so, go ahead. Note that we are still within the section protected by the mutex, which forbids the
		 user code from calling our module. *)

	      task();

	      (* Dequeue this request, and keep going. *)

	      let _ = Queue.take queue in
	      loop()

	    end

	  with Queue.Empty ->
	    () in

	loop()

      ) () in

    schedule, cancel, beat
;;
