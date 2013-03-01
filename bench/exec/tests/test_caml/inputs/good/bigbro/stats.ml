(* $Header: /net/pauillac/caml/repository/bigbro/stats.ml,v 1.1.1.1 2001/02/13 15:39:36 fpottier Exp $ *)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A single record holds our statistics.

*)

type stats = {

  (* Checks. *)

  mutable total: int;
  mutable successes: int;
  mutable failures: int;

  (* Tasks. *)

  mutable file_head: int;
  mutable file_get: int;
  mutable file_get_bytes: int;
  mutable http_head: int;
  mutable http_get: int;
  mutable http_get_bytes: int

}

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Global data is, as usual, protected by a lock.

*)

let lock = Mutex.create()
and stats = {
  total = 0;
  successes = 0;
  failures = 0;

  file_head = 0;
  file_get = 0;
  file_get_bytes = 0;
  http_head = 0;
  http_get = 0;
  http_get_bytes = 0
}

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The entry points, used to update the stats.

*)

let got_check_outcome successful =
  Thread_utils.protect lock (fun () ->
    stats.total <- succ stats.total;
    if successful then
      stats.successes <- succ stats.successes
    else
      stats.failures <- succ stats.failures
  ) ()
;;

let did_file_head () =
  Thread_utils.protect lock (fun () ->
    stats.file_head <- succ stats.file_head
  ) ()
;;

let did_file_get () =
  Thread_utils.protect lock (fun () ->
    stats.file_get <- succ stats.file_get
  ) ()
;;

let file_size_was bytes =
  Thread_utils.protect lock (fun () ->
    stats.file_get_bytes <- stats.file_get_bytes + bytes
  ) ()
;;

let did_http_head () =
  Thread_utils.protect lock (fun () ->
    stats.http_head <- succ stats.http_head
  ) ()
;;

let did_http_get () =
  Thread_utils.protect lock (fun () ->
    stats.http_get <- succ stats.http_get
  ) ()
;;

let http_size_was bytes =
  Thread_utils.protect lock (fun () ->
    stats.http_get_bytes <- stats.http_get_bytes + bytes
  ) ()
;;
