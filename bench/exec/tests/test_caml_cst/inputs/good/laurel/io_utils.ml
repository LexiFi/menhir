(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/io_utils.ml,v 1.3 2000/02/28 16:21:42 fpottier Exp $ *)

(* This function reads a whole file at once. It raises [Sys_error] if an error occurs. *)

let read_whole_file filename =
  let channel = open_in_bin filename in
  try
    let length = in_channel_length channel in
    let data = String.create length in
    really_input channel data 0 length;
    close_in channel;
    data
  with error ->
    close_in channel;
    raise error

(* [execute expected command] executes the specified [command] (using \verb+/bin/sh+), waits for its completion, and
   returns its output as a string. If the command does not terminate normally, its exit status is checked. If it is
   part of the user-specified [expected] list, then [Exited] is raised, together with the exit status. Otherwise,
   [Failure] is raised, together with a human-readable error message. *)

exception Exited of int

let execute expected command =
  let channel = Unix.open_process_in command in
  let buffer = Buffer.create 16384 in
  let chunk = String.create 16384 in
  let rec read () =
    let length = input channel chunk 0 16384 in
    if length = 0 then
      ()
    else begin
      Buffer.add_substring buffer chunk 0 length;
      read()
    end in
  read();
  match Unix.close_process_in channel with
  | Unix.WEXITED 0 ->
      Buffer.contents buffer
  | Unix.WEXITED code when List.mem code expected ->
      raise (Exited code)
  | _ ->
      failwith (Printf.sprintf "Command `%s` failed to execute properly." command)

