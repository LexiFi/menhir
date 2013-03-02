(* $Header: /net/pauillac/caml/repository/bigbro/main.ml,v 1.2 2001/08/01 11:43:54 fpottier Exp $ *)

open Agency

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The main job processing function, and the program termination function, both parameters to the job agency.

*)

let () = Agency.initialize (function
    JobInitial url_text ->
      Check_master.check_initial_link url_text
  | JobNetworkCheck (url, mapped_url, should_recurse) ->
      Check_http.check url mapped_url should_recurse
  | JobRecurse (data, url, source) ->
      Check_master.check_html data url source
) (fun () ->
     Link_info.raw_postamble();
     Settings.RawOutput.close();
     Link_info.html_postamble();
     Settings.HtmlOutput.close();
     Settings.DebugOutput.close();
     exit(0)
)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Handle anonymous arguments. Each of them is assumed to be a URL which needs to be checked.

In addition, if -stdin is set, then URLs are read from standard input. This allows extending the program by making it
a component of a pipe line (using cat, find, etc.).

*)

let () = List.iter (
  function other -> Agency.add (JobInitial other)
) Settings.anonymous

let () = if Settings.stdin then begin

  (* Read from standard input. *)

  try
    while true do
      Agency.add (JobInitial (input_line stdin))
    done
  with End_of_file ->
    ()

end

(* This lets the Agency know that we are done queuing requests. See its code for more explanations. *)

let () = Agency.killed();;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Now, the core of the program flow control is in Agency, which takes care of spawning a thread for each job. It
shall automatically terminate the program when done.

The main thread does not have anything special to do. It must not call exit(), though; that would terminate the
program prematurely. We have a use for it; it will call Timer every second to handle timeouts. This can be done
efficiently by calling Thread.sleep rather than busy-waiting.

If no timeouts have been requested, then we don't have anything to do at all, so we just terminate the main thread.
This should save some time and memory. Unfortunately, in O'Caml 1.07, Thread.exit terminates the whole program when
using the native threads implementation. I can't do anything about it; so, in that case, I don't terminate the main
thread.

*)

if (Settings.timeout = None) & (Sys.os_type = "Unix") then
  Thread.exit()
else
  while true do
    Unix.sleep 1;
    Timer.beat()
  done

