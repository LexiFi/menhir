(* Input-output utilities. *)

(* ------------------------------------------------------------------------- *)
(* [exhaust channel] reads all of the data that's available on [channel]. *)

let chunk_size =
  2048

let exhaust channel =
  let buffer = Buffer.create chunk_size in
  let chunk = String.create chunk_size in
  let rec loop () =
    let length = input channel chunk 0 chunk_size in
    if length = 0 then
      Buffer.contents buffer
    else begin
      Buffer.add_substring buffer chunk 0 length;
      loop()
    end
  in
  loop()

(* ------------------------------------------------------------------------- *)
(* [invoke command] invokes an external command (which expects no
   input) and returns its output, if the command succeeds. It returns
   [None] if the command fails. *)

let invoke command =
  let ic = Unix.open_process_in command in
  let result = exhaust ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 ->
      Some result
  | _ ->
      None

