let log0 str =
  print_string str;
  flush stdout
;;

let log1 str =
  if Settings.verbosity > 0 then
    (
      print_string str;
      flush stdout;
    )
;;

let log2 str =
  if Settings.verbosity > 1 then
    (
      print_string str;
      flush stdout;
    )
;;

let log3 str =
  if Settings.verbosity > 2 then
    (
      print_string str;
      flush stdout;
    )
;;

let f = open_out Settings.log_file
let log_counter = ref 0

let log_up () = 
  log_counter :=  1 + !log_counter

let logc str =
  Printf.fprintf f "%-6d %-8s %s" !log_counter "COMMAND:"  str
;;

let logdir str =
  Printf.fprintf f "%-6d %-8s %s" !log_counter "CHDIR:"  str
;;

let logi str =
  Printf.fprintf f "%-6d %-8s %s" !log_counter "INFO:" str
;;

let loge str =
  Printf.fprintf stderr "%s %s" "FATAL ERROR:" str;
  Printf.fprintf f "%s %s" "FATAL ERROR:" str
;;

(* Deprecated *)
let logf str =
  output_string f str
;;

let logclose () =
  close_out f
;;
