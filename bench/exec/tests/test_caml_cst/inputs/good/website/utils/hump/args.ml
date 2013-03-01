let _ = Config.dbg "Entering module args.ml"


module M = Messages

let db_name = ref Config.db_name
let db_user = ref Config.db_user
let db_passwd = ref Config.db_passwd
let db_host = ref Config.db_host

let verbose_mode = ref false

(* Please keep options in alphabetical order. *)
let options  = ref [
  "-d", Arg.Set_string db_name, M.option_d ;
  "-h", Arg.Set_string db_host, M.option_h ;
  "-p", Arg.Set_string db_passwd, M.option_p ;
  "-u", Arg.Set_string db_user, M.option_u ;
  "-v", Arg.Set verbose_mode, M.option_v ;
]

let add_option o = options := !options @ [o]

let ref_db = ref None

let db () =
  match !ref_db with
    None -> failwith M.err_not_connected
  | Some connection -> connection

let parse () =
  Arg.parse !options
      (fun _ -> ())
      (M.usage ^ "\n" ^ M.options_are);
  ref_db := Some 
      (Db.connect ~host: !db_host ~password: !db_passwd !db_user !db_name)

let verbose s = 
  if !verbose_mode then (print_string s ; print_newline ())

let err_verbose s = 
  if !verbose_mode then prerr_endline s
