let _ = Config.dbg "Entering module contribs.ml"

let get id = 
  match Db.Contribs.select (Args.db()) ~id () with
    [] -> raise Not_found
  | t :: _ -> t

let get_or_fail id = 
  try get id 
  with Not_found -> 
    let mes = Printf.sprintf
       "no contribs with key %s"
       (Mysql.ml2int id)
    in
    failwith mes

let create ~description ~name ?status ~url ?date ~homepage_indexed ~version () =
  let db = Args.db() in
  let date = match date with None -> Unix.time () | Some d -> d in
  let random_desc = Misc.random_string () in
  Db.Contribs.insert db ~description: random_desc ();
  match Db.Contribs.select db ~description: random_desc () with
    [] -> failwith "insertion of contribs failed"
  | t :: _ ->
     Db.Contribs.update db ~date ~description ~name ~status
	~url ~homepage_indexed ~version ~key_id: t.Db.Contribs.id ();
     get_or_fail t.Db.Contribs.id

let update ?date ?description ?name ?status ?url ?homepage_indexed ?version id =
  let db = Args.db() in
  Db.Contribs.update db
    ?date ?description ?name ?status 
    ?url ?homepage_indexed ?version ~key_id: id ();
  get_or_fail id

let delete c =
  let id = c.Db.Contribs.id in
  let db = Args.db () in
  Db.Contrib_author.delete db ~idcontrib: id ();
  Db.Contrib_link.delete db ~idcontrib: id ();
  Db.Contrib_prop.delete db ~idcontrib: id ();
  Db.Contribs.delete db ~id ()

let list () =
  Db.Contribs.select (Args.db()) ()

let sort =
  let comp t1 t2 =
    compare
      (String.lowercase t1.Db.Contribs.name)
      (String.lowercase t2.Db.Contribs.name)
  in
  List.sort comp

let sort_by_date =
  let comp c1 c2 = compare c2.Db.Contribs.date c1.Db.Contribs.date in
  List.sort comp

let props ?kind c =
  let l = 
    List.fold_left
      (fun acc t -> 
	try (Props.get_or_fail t.Db.Contrib_prop.idprop)::acc
	with _ -> acc
      )
      []
      (Db.Contrib_prop.select (Args.db()) ~idcontrib: c.Db.Contribs.id ())
  in
  match kind with
    None -> l
  | Some k -> List.filter (fun p -> p.Db.Props.kind = k) l

let add_prop c p =
  let idcontrib = c.Db.Contribs.id in
  let idprop = p.Db.Props.id in
  let db = Args.db () in
  match Db.Contrib_prop.select db ~idcontrib ~idprop () with
    [] -> Db.Contrib_prop.insert db ~idcontrib ~idprop ()
  | _ -> ()

let remove_prop c p =
  let idcontrib = c.Db.Contribs.id in
  let idprop = p.Db.Props.id in
  let db = Args.db () in
  Db.Contrib_prop.delete db ~idcontrib ~idprop () 

let authors c =
  List.fold_left
    (fun acc t -> 
      try (Authors.get_or_fail t.Db.Contrib_author.idauthor)::acc
      with _ -> acc
    )
    []
    (Db.Contrib_author.select
       (Args.db()) 
       ~idcontrib: c.Db.Contribs.id ())

let add_author c a =
  let idcontrib = c.Db.Contribs.id in
  let idauthor = a.Db.Authors.id in
  let db = Args.db () in
  match Db.Contrib_author.select db ~idcontrib ~idauthor () with
    [] -> Db.Contrib_author.insert db ~idcontrib ~idauthor ()
  | _ -> ()

let remove_author c a =
  let idcontrib = c.Db.Contribs.id in
  let idauthor = a.Db.Authors.id in
  let db = Args.db () in
  Db.Contrib_author.delete db ~idcontrib ~idauthor () 

let detail_url c =
  Printf.sprintf "%s?%s=%d" 
    Config.loc_cgi_hump 
    Constant.var_contrib
    c.Db.Contribs.id

let raw_detail_url c =
  Printf.sprintf "%s?%s=%d" 
    Config.raw_loc_cgi_hump 
    Constant.var_contrib
    c.Db.Contribs.id

