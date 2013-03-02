let _ = Config.dbg "Entering module link_kind.ml"

let get id = 
  match Db.Link_kinds.select (Args.db()) ~id () with
    [] -> raise Not_found
  | t :: _ -> t

let get_or_fail id = 
  try get id 
  with Not_found -> 
    let mes = Printf.sprintf
       "no link_kind with key %s"
       (Mysql.ml2int id)
    in
    failwith mes

let create ~icon ~name () =
  let db__ = Args.db() in
  let random_icon = Misc.random_string () in
  Db.Link_kinds.insert db__ ~icon: random_icon ();
  match Db.Link_kinds.select db__ ~icon: random_icon () with
    [] -> failwith "insertion of link_kind failed"
  | t :: _ ->
     Db.Link_kinds.update db__ ~icon ~name ~key_id: t.Db.Link_kinds.id ();
     get_or_fail t.Db.Link_kinds.id

let update ?icon ?name id =
  let db__ = Args.db() in
  Db.Link_kinds.update db__ ?icon ?name ~key_id: id ();
  get_or_fail id

let delete t =
  let id = t.Db.Link_kinds.id in
  let db = Args.db () in
  Db.Contrib_link.delete db ~idlink_kind: id ();
  Db.Link_kinds.delete db ~id ()

let list () =
  Db.Link_kinds.select (Args.db()) ()

let sort =
  let comp t1 t2 =
    compare
      (String.lowercase t1.Db.Link_kinds.name)
      (String.lowercase t2.Db.Link_kinds.name)
  in
  List.sort comp
    

