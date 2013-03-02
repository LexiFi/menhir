(* Prefix functions using Settings.ml preferences *)
let prefix_results str =
  Filename.concat Settings.results_dir str
;;

let prefix_data str =
  Filename.concat Settings.data_dir str
;;

let prefix_inputs str =
  Filename.concat Settings.inputs_dir str
;;

let base2ml str =
  str ^ ".ml"
;;

let mll2ml file = 
  (Filename.chop_extension file) ^ ".ml"

let execsuffix opt str =
  str ^ (if opt then ".opt" else ".byte")

let base2cmo str =
  str ^ ".cmo"
;;

let ml2cmo str =
  (Filename.chop_extension str) ^ ".cmo"
;;

let ml2o str =
  (Filename.chop_extension str) ^ ".o"
;;

let ml2mli str =
  (Filename.chop_extension str) ^ ".mli"
;;

let lib2obj opt str =
  if String.length str > 0 then
  (
    if opt then
      str ^ ".cmx"
    else
      str ^ ".cmo"
  )
  else
    ""
;;

exception NotASourceFile of string;;

let src2obj opt file =
  if Filename.check_suffix file ".ml" then
    (Filename.chop_extension file) ^ (if opt then ".cmx" else ".cmo")
  else
  if Filename.check_suffix file ".mli" then
    (Filename.chop_extension file) ^ (".cmi")
  else
    raise (NotASourceFile file)
;;

let is_directory s =
  match (Unix.stat s).Unix.st_kind with
  | Unix.S_DIR ->
      true
  | _ ->
      false

(* Scan a directory for files ending with a given extension *)
(* if filter if the null string, then, all files in the directory are returned*)
(* let scan_files directory filter =
if (Sys.file_exists directory) && (is_directory directory) then
(
let lst = Array.to_list (Sys.readdir directory) in
if (String.length filter) > 0 then
List.filter (fun e -> Filename.check_suffix e filter) lst
else
lst
)
else []
*)

let rec scan_files suffix directory =
  if Sys.file_exists directory then
    if (is_directory directory) &&
    (not (List.mem (Filename.basename directory)
            Settings.ignored )) then
      List.flatten (List.map (fun x ->
                  scan_files suffix (Filename.concat directory x)
            ) (Array.to_list (Sys.readdir directory)))
    else
    if Filename.check_suffix directory suffix then
      (if (List.mem (Filename.basename directory) Settings.ignored) then
          []
        else
          [ directory ])
    else
      []
  else
    []

(* Scan a directory for subdirectories *)
let scan_subdirs directory =
  if (Sys.file_exists directory) && (is_directory directory) then
    (
      let lst = Array.to_list (Sys.readdir directory) in
      List.filter (fun e -> is_directory e) lst
    )
  else []

(* get the last modification time of a file *)
let last_mod file =
  (Unix.stat file).Unix.st_mtime
;;

(* compare the dates of the 2 files *)
let newer src dest =
  if (Sys.file_exists dest) then
    (last_mod src) > (last_mod dest)
  else
    true
;;

(* see if an element of the list is newer *)
let has_newer srclist dest =
  if (Sys.file_exists dest) then
    (
      let doit = ref false in
      List.iter (fun src -> doit := newer src dest) srclist;
      !doit
    ) else
    true
;;

(* calculate timed elapsed between 2 ticks*)
let old_tick =
  let t0 = Unix.times () in
  ref (t0.Unix.tms_utime +. t0.Unix.tms_stime +. t0.Unix.tms_cutime +. t0.Unix.tms_cstime)

let tick () =
  let t1 = Unix.times () in
  let newtick = (t1.Unix.tms_utime +. t1.Unix.tms_stime +.
      t1.Unix.tms_cutime +. t1.Unix.tms_cstime) in
  let delta = newtick -. !old_tick in
  old_tick := newtick;
  delta

(* create a directory if it doesnt exist *)
let check_create_dir dir =
  Log.log_up();
  Log.logi (Printf.sprintf "checking existence of directory '%s'\n" dir);
  if Sys.file_exists dir then
    (if (not (is_directory dir)) then
        failwith (Printf.sprintf "%s must be a directory. Aborting" dir)
    )
  else
  if (Sys.file_exists (Filename.dirname dir)) then
    Unix.mkdir dir 0o750
  else
    (
      Log.loge (Printf.sprintf
            "creating directory '%s'. The directory '%s' should exists\n"
            dir (Filename.dirname dir));
      exit 1;
    )

;;

let dir_stack = ref []

let file_size file = 
  let fstat = Unix.stat file in
  fstat.Unix.st_size

let popd () =
  let (dir, stack) = match !dir_stack with
    | hd:: tl -> hd, tl
    | [] -> Printf.printf "cdpop warning : no directory to pop\n"; Sys.getcwd(), [] in
  dir_stack := stack;
  Log.logdir (Printf.sprintf "cd %s\n" dir);
  Sys.chdir dir
;;

let pushd (dir: string) =
  dir_stack := (Sys.getcwd ():: !dir_stack);
  Log.logdir (Printf.sprintf "cd %s\n" dir);
  Sys.chdir dir
;;

let file_size file =
  let stat = Unix.stat file in
  stat.Unix.st_size
;;
