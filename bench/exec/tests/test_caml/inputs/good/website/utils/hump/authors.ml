let _ = Config.dbg "Entering module authors.ml"

let get id = 
  match Db.Authors.select (Args.db()) ~id () with
    [] -> raise Not_found
  | t :: _ -> t

let get_or_fail id = 
  try get id 
  with Not_found -> 
    let mes = Printf.sprintf
       "no authors with key %s"
       (Mysql.ml2int id)
    in
    failwith mes

let get_by_name ~name ~firstname =
  match Db.Authors.select (Args.db()) ~name ~firstname () with
    [] -> None
  | a :: _ -> Some a

let create ~firstname ~mail ~name ~url () =
  let db__ = Args.db() in
  match get_by_name ~name ~firstname with
    None ->
      (
       let random_firstname = Misc.random_string () in
       Db.Authors.insert db__ ~firstname: random_firstname ();
       match Db.Authors.select db__ ~firstname: random_firstname () with
	 [] -> failwith "insertion of authors failed"
       | t :: _ ->
	   Db.Authors.update db__ ~firstname ~mail ~name ~url ~key_id: t.Db.Authors.id ();
	   get_or_fail t.Db.Authors.id
      )
  | Some _ ->
      failwith Messages.err_author_already_exists

let update ?firstname ?mail ?name ?url id =
  let db = Args.db() in
  let current = get_or_fail id in
  let name = 
    match name with 
      None -> current.Db.Authors.name
    | Some s -> s
  in
  let firstname =
    match firstname with 
      None -> current.Db.Authors.firstname
    | Some s -> s
  in
  match Db.Authors.select db ~name ~firstname () with
    l when List.exists (fun t -> t.Db.Authors.id <> id) l ->
      failwith Messages.err_author_already_exists
  | _ ->
      Db.Authors.update db ~firstname ?mail ~name ?url ~key_id: id ();
      get_or_fail id

let delete a =
  let id = a.Db.Authors.id in
  let db = Args.db () in
  Db.Contrib_author.delete db ~idauthor: id ();
  Db.Authors.delete db ~id ()

let list () =
  Db.Authors.select (Args.db()) ()

let sort =
  let lower s =
    let len = String.length s in
    let b = Buffer.create len in
    for i = 0 to len - 1 do
      let c =
	match s.[i] with
	| 'à' | 'â' | 'ä' -> 'a'
	| 'é' | 'è' | 'ê' | 'ë' -> 'e'
	| 'ï' | 'î' -> 'i'
	| 'ô' | 'ö' -> 'o'
	| 'ù' | 'û' | 'ü' -> 'u'
	| c -> Char.lowercase c
      in
      Buffer.add_char b c
    done;
    Buffer.contents b
  in
  let comp a1 a2 =
    match compare
	(lower a1.Db.Authors.name)
	(lower a2.Db.Authors.name)
    with
      0 ->
	compare 
	  (lower a1.Db.Authors.firstname)
	  (lower a2.Db.Authors.firstname)
    | n -> n
  in
  List.sort comp
  
let string_name a =
  match a.Db.Authors.name, a.Db.Authors.firstname with
    ("","") -> "<no name>"
  | (name, fname) -> fname ^ " " ^ name

let string_name_alt a =
  match a.Db.Authors.name, a.Db.Authors.firstname with
    ("","") -> "<no name>"
  | (name, fname) -> name ^ " " ^ fname

let choices () =
  List.map
    (fun a -> (string_of_int a.Db.Authors.id,
	       string_name_alt a)
    )
    (sort (list ()))
