let _ = Config.dbg "Entering module sessions.ml"

module S = Db.Sessions

type sid = string

let get sid =
  match S.select (Args.db()) ~id: sid () with
    [] -> raise Not_found
  | s :: _ -> s

let get_user host sid =
  try
    let s = get sid in
    if s.S.host = host then
      let t = Unix.time () in
      if s.S.time +. Config.session_timeout >= t then
	(
	 S.update (Args.db()) ~key_id: sid ~time: t ();
	 let u = Users.get s.S.iduser in

	 (* initialiser la fonction utilisée pour stocker
	    qui fait des modifs dans les tables *)
	 Db.log_who := (fun () -> u.Db.Users.id) ;
	 Some u
	)
      else
	(
	 S.delete (Args.db()) ~id: sid ();
	 None
	)
    else
      None
  with _ -> None

let create host user =
  let hash = Cryptokit.Hash.md5 () in
  hash#add_string (host^user.Db.Users.login) ;
  let sid = 
    Printf.sprintf "%s%s"
      (Misc.random_string ())
      hash#result
  in
  Db.Sessions.insert (Args.db())
    ~id: sid
    ~iduser: user.Db.Users.id
    ~time: (Unix.time ())
    ~host
    ();
  sid

let delete sid user =
  try
    let s = get sid in
    (* return true to indicate the session has been deleted *)
    if s.Db.Sessions.iduser = user.Db.Users.id then
      (
       Db.Sessions.delete (Args.db()) ~id: sid ();
       true
      )
    else
      false
  with
    Not_found ->
      false


