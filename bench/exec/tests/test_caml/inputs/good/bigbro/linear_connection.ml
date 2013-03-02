(* $Header: /net/pauillac/caml/repository/bigbro/linear_connection.ml,v 1.3 2001/08/01 11:43:54 fpottier Exp $ *)

open Unix

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Exceptions.

*)

type connection_action =
    CouldNotCreateSocket
  | CouldNotResolve
  | CouldNotBind
  | CouldNotConnect
  | CouldNotSend
  | CouldNotReceive
  | CouldNotClose

exception ConnectionError of connection_action * error

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Wrapping action into a handler for Unix errors.

*)

let wrap action_type action =
  try
    action()
  with Unix_error(error, _, _) ->
    raise (ConnectionError (action_type, error))
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

This is a wrapper around Unix.gethostbyname; it adds caching. Since it has state, a lock is needed when working
in a multi-threaded environment.

*)

let dns_table = Hashtbl.create 307
and dns_lock = Mutex.create()

let gethostbyname name =
  try
    Hashtbl.find dns_table name
  with Not_found ->
    let result = gethostbyname name in
    Hashtbl.add dns_table name result;
    result
;;

let gethostbyname name =
  Thread_utils.protect dns_lock gethostbyname name
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Code to resolve machine names.

*)

let resolve name =
  try
    inet_addr_of_string name
  with Failure _ ->
    try
      (gethostbyname name).h_addr_list.(0)
    with Not_found ->
      raise (ConnectionError (CouldNotResolve, ENETUNREACH))
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A utility to close a socket without allowing any exceptions to be raised.

*)

let silent_close socket =
  try
    close socket
  with _ ->
    ()
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Thread.kill is not implemented under Windows, so we can't use the "scheduled suicide" approach. All we can do is
a timed read.

*)

let has_kill =
  match Sys.os_type with
    "Unix" -> true
  | "Win32" -> false
  | _ -> assert false

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The main function.

Timeouts are handled right here at the core of the networking code, because when a timeout occurs, it is necessary
not only to kill the thread but also to close the socket. The socket is not easily made visible to outside code,
hence the decision to handle timeouts here.

*)

let request request host port timeout_handler =

  (* Create a socket. 

     The socket can be closed in two different ways: either by this thread when it is done (under normal or exceptional
     conditions), or by the timeout code. However, a mutex is not needed, because this thread attempts to close the
     socket only after the suicide has been cancelled. So, there is no race condition. It is impossible for the socket
     to be closed twice.

     The reader might wonder why there is no race condition. It is possible to view things slightly differently by
     saying that there *is* a race condition, but it's taken care of by the lock found in Timer. *)

  let socket = wrap CouldNotCreateSocket (fun () -> socket PF_INET SOCK_STREAM 0) in

  (* Queue a request to kill this thread after a certain amount of time has elapsed. *)

  let self = Thread.self() in
  let suicide = Timer.Task (fun () -> 

    (* This code is executed by the main thread if the timeout task fires.

       As explained above, closing the socket here is OK, because this code is executed while Timer's lock is
       locked. So, if the connection thread continues running after we close the socket and before we kill it,
       it cannot reach the point where it attempts to close its own socket, because it will block on the call
       to Timer.cancel. *)

    silent_close socket;
    Thread.kill self;
    timeout_handler();
    Agency.killed()

  ) in

  let limit_time = if has_kill then begin
    Timer.schedule suicide;
    None
  end

  (* If we don't have Thread.kill, we have to use timed reads instead. Record the time. *)

  else match Settings.timeout with
    None ->
      None
  | Some timeout ->
      Some (time() +. timeout) in

  (* Prepare to obtain an answer. *)

  let answer = try

    (* Set up a client address and bind the socket to it. *)

    wrap CouldNotBind (fun () -> bind socket (ADDR_INET(inet_addr_any, 0)));

    (* Resolve the host name and connect to the host. *)

    wrap CouldNotConnect (fun () -> connect socket (ADDR_INET(resolve host, port)));

    (* Send our data. *)

    let _ = wrap CouldNotSend (fun () -> send socket request 0 (String.length request) []) in

    (* As long as the connection remains up, read from it. *)

    let buffer_length = 1024 in
    let buffer = String.create buffer_length in

    let rec receive answer =

      (* If we don't have Thread.kill, use a timed read operation to detect timeouts during reading.
	 Note that timeouts on DNS and connect operations are detected only after they complete. *)

      begin
	match limit_time with
	  Some limit ->
	    if not (Thread.wait_timed_read socket (limit -. time())) then begin

	      (* If the read operation timed out, commit suicide. This code is identical to the suicide task,
		 except that we can die in peace using Thread.exit instead of Thread.kill, which is unavailable
		 under Windows. Since Thread.kill is implemented by raising an exception, there is no need to
	         close the socket here; it shall be done by our handler. Also, for the same reason, we *must not*
                 call Agency.killed! Otherwise the thread count would be decreased twice. *)

	      timeout_handler();
	      Thread.exit()

	    end
	| None ->
	    ()
      end;	  

      (* Although this is not very clearly documented, it seems that a return value of 0 indicates that there
	 is no more data to be read; the call would block if any data was still expected. *)

      let amount = wrap CouldNotReceive (fun () -> recv socket buffer 0 buffer_length []) in
      if amount = 0 then answer else receive (answer ^ (String.sub buffer 0 amount)) in

    receive ""

  with error ->

    (* If we are still alive, the timeout didn't fire, cancel it.
       This call cannot raise Not_found, because if it does, the task has been run already, and we're dead. *)
    
    if has_kill then
      Timer.cancel suicide;

    (* Always attempt to close the socket before exiting, but do not allow close to throw an exception which
       would hide the original one. *)

    silent_close socket;
    raise error

  in

  (* If we are still alive, the timeout didn't fire, cancel it.
     This call cannot raise Not_found, because if it does, the task has been run already, and we're dead. *)

  if has_kill then
    Timer.cancel suicide;

  (* Close the socket and return the answer. *)

  wrap CouldNotClose (fun () -> close socket);
  answer
;;
