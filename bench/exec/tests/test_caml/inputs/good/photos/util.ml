(* $Header: /home/yquem/cristal/fpottier/cvs/photos/util.ml,v 1.8 2004/06/12 08:47:09 fpottier Exp $ *)

open Printf
open Log

(* [finally f x h] evaluates the application [f x], yielding an
   outcome [y] -- either a value or an exception. It then invokes [h],
   and (if [h] terminates normally) propagates the outcome [y]. *)

let finally (f : 'a -> 'b) (x : 'a) (h : unit -> unit) : 'b =
  let y =
    try
      f x
    with e ->
      h();
      raise e
  in
  h();
  y

(** Eliminating duplicates in a list. *)
let rec eliminate_duplicates o list =
  match list with
  | []
  | [ _ ] ->
      list
  | x1 :: ((x2 :: _) as rest1) ->
      if o x1 x2 = 0 then eliminate_duplicates o rest1
      else x1 :: (eliminate_duplicates o rest1)

let eliminate_duplicates list =
  eliminate_duplicates compare (List.sort compare list)

(** Reading all of an input channel. *)
let rec build buffer offset = function
  | [] ->
      assert (offset = 0);
      buffer
  | chunk :: chunks ->
      let size = String.length chunk in
      let offset' = offset - size in
      String.blit chunk 0 buffer offset' size;
      build buffer offset' chunks

let chunk_size =
  2048

let rec exhaust length chunks channel =
  let chunk = String.create chunk_size in
  let read = input channel chunk 0 chunk_size in
  if read = chunk_size then
    exhaust (length + read) (chunk :: chunks) channel
  else 
    let buffer = String.create (length + read) in
    String.blit chunk 0 buffer length read;
    build buffer length chunks

let exhaust channel =
  exhaust 0 [] channel

(* ----------------------------------------------------------------------------------------------------------------- *)

(** External commands. *)
let command c =
  let code = Sys.command c in
  if code <> 0 then begin
    log (sprintf "External command failed.\nCommand: %s\nExit code: %d." c code);
    exit(1) (* TEMPORARY il faudrait envoyer le message ailleurs et/ou lancer une exception *)
  end

(* Applying a command to a single argument. *)
let command1 cmd arg =
  command (sprintf "%s %s" cmd (Filename.quote arg))

(* Applying a command to two arguments. *)
let command2 cmd arg1 arg2 =
  command (sprintf "%s %s %s" cmd (Filename.quote arg1) (Filename.quote arg2))

(** Quoting and space-delimiting a list of arguments. *)
let mapply cmd args =
  List.fold_left (fun cmd arg -> cmd ^ " " ^ (Filename.quote arg)) cmd args

(** Invoking an external program that expects no input. *)
let invoke read commande =
  let ic = Unix.open_process_in commande in
  let result = read ic in
  match Unix.close_process_in ic with
  | Unix.WEXITED 0 ->
      Some result
  | _ ->
      None

let oneliner =
  invoke input_line

let multiliner =
  invoke exhaust

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Working on a temporary copy of a file. *)

let tempcopy filename action =

  (* Choose a temporary filename. If the file already exists, it will be wiped
     out, but that should be OK since this is inside /tmp. *)

  let tempname = "/tmp/" ^ (Filename.basename filename) in

  let qfile = Filename.quote filename
  and qtemp = Filename.quote tempname in

  (* Copy the file. *)

  command (sprintf "/bin/cp -f %s %s" qfile qtemp);

  (* Trigger the action, then remove the temporary copy. *)

  finally action tempname (fun () -> command (sprintf "/bin/rm -f %s" qtemp))

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Creating a temporary directory. *)

let _ =
  Random.self_init()

let maketempdir prefix =
  let rec loop counter =
    if counter >= 10 then begin
      log "Unable to create a temporary directory inside /tmp.";
      exit 1
    end;
    let name = sprintf "/tmp/%s%06x" prefix ((Random.bits()) land 0xFFFFFF) in
    if Sys.command (sprintf "mkdir %s" (Filename.quote name)) = 0 then
      name
    else
      loop (counter + 1)
  in loop 0

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Getting a file's last modification date. *)

let moddate filename =
  (Unix.stat filename).Unix.st_mtime

let dateprotect action srcname dstname =
  if Sys.file_exists dstname then begin
    let srcdate = moddate srcname
    and dstdate = moddate dstname in
    if srcdate > dstdate then
      action srcname dstname
  end
  else
    action srcname dstname

(* ----------------------------------------------------------------------------------------------------------------- *)
(** Pretty-printing a date. *)

let humandate date =
  let udate = Unix.localtime date in
  sprintf "%02d/%02d/%4d %02d:%02d:%02d"
    udate.Unix.tm_mday
    (udate.Unix.tm_mon + 1)
    (udate.Unix.tm_year + 1900)
    udate.Unix.tm_hour
    udate.Unix.tm_min
    udate.Unix.tm_sec

(* ----------------------------------------------------------------------------------------------------------------- *)

(** Creating a directory if it does not exist. *)
let mkdirp dirname =
  if not (Sys.file_exists dirname) then
    command1 "mkdir -p" dirname;
  dirname

(** Looping over all files in a directory. *)
let folddir action dirname accu =
  let handle = Unix.opendir dirname in
  let rec loop accu =
    match (try Some (Unix.readdir handle) with End_of_file -> None) with
    | Some ("." | "..") ->
	loop accu
    | Some filename ->
	loop (action filename accu)
    | None ->
	accu
  in
  finally loop accu (fun () -> Unix.closedir handle)

let iterdir action dirname =
  folddir (fun filename () -> action filename) dirname ()

(** Finding all photos (.jpg files) in a directory. *)
let all_photos dirname =
  folddir (fun filename accu ->
    if Filename.check_suffix filename ".jpg" then
      filename :: accu
    else
      accu
  ) dirname []

(** Looping over all photos (.jpg files) in a directory. *)
let for_each_photo dirname action =
  iterdir (fun filename ->
    if Filename.check_suffix filename ".jpg" then
      action filename
  ) dirname

