let _ = Config.dbg "Entering module agents.ml"


open Types
open Db.Users

let get id =
  match select (Args.db()) ~id () with
    [] -> raise Not_found
  | s :: _ -> s

let get_by_login login =
  match select (Args.db()) ~login () with
    [] -> raise Not_found
  | s :: _ -> s

let get_or_fail id =
  try get id
  with Not_found -> failwith (Messages.err_unknown_iduser id)

let get_by_login_or_fail login =
  try get_by_login login
  with Not_found -> failwith (Messages.err_unknown_user login)

let create
  ~login ~mail ~passwd
  () =
  let db = Args.db () in
  let login = Misc.no_blanks login in

  (* controls: not the same login already in the table, and login not empty. *)
  (
    match login with
      "" -> failwith Messages.err_invalid_login
    | _ ->
	try 
	  ignore (get_by_login login);
	  failwith (Messages.err_login_already_exists login)
	with
	  Not_found -> ()
  );

  Db.Users.insert db 
    ~login ~mail ~passwd
    ();

  get_by_login login

let update ?mail ?passwd id =
  let db = Args.db () in
  Db.Users.update db ~key_id: id
    ?mail ?passwd
    ();
  get id

let list () = Db.Users.select (Args.db()) ()

(*
let string_name u =
  match u.name, u.firstname with
    ("", _) | (_, "") -> u.login
  | (name, fname) -> u.firstname ^ " " ^ u.name
*)


