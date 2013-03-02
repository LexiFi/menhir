let _ = Config.dbg "Entering module props.ml"

let get ?kind id = 
  match Db.Props.select (Args.db()) ?kind ~id () with
    [] -> raise Not_found
  | t :: _ -> t

let get_or_fail ?kind id = 
  try get ?kind id 
  with Not_found -> 
    let mes = Printf.sprintf
       "no props with key %s"
       (Mysql.ml2int id)
    in
    failwith mes

let create ?icon ~kind ~name ?url ?father () =
  let db = Args.db() in
  let random_name = Misc.random_string () in
  Db.Props.insert db ~name: random_name ();
  match Db.Props.select db ~name: random_name () with
    [] -> failwith "insertion of props failed"
  | t :: _ ->
      Db.Props.update db ~icon ~kind ~name ~url ~key_id: t.Db.Props.id ();
      (
       match father with
	 None -> ()
       | Some f ->
	   Db.Assoc_prop.insert db 
	     ~idfather: f.Db.Props.id
	     ~idchild: t.Db.Props.id
	     ()
      );
      get_or_fail t.Db.Props.id

let list () =
  Db.Props.select (Args.db()) ()

let roots () =
  let l = Db.Assoc_prop.select (Args.db()) () in
  let pred t = 
    not (List.exists (fun u -> u.Db.Assoc_prop.idchild = t.Db.Props.id) l)
  in
  List.filter pred (list())

let kind_root kind =
  match List.filter (fun p -> p.Db.Props.kind = kind) (roots ()) with
    [] -> failwith ("no root for kind "^(Messages.of_prop_kind kind))
  | h :: _ -> h

let sort =
  let comp t1 t2 =
    compare
      (String.lowercase t1.Db.Props.name)
      (String.lowercase t2.Db.Props.name)
  in
  List.sort comp

let children t =
  sort
    (List.fold_left
       (fun acc t ->
	 try (get t.Db.Assoc_prop.idchild)::acc
	 with Not_found -> acc
       )
       []
       (Db.Assoc_prop.select (Args.db()) ~idfather: t.Db.Props.id ())
    )

let rec delete p =
  let db = Args.db () in
  let id = p.Db.Props.id in
  let children = children p in
  List.iter delete children;
  Db.Assoc_prop.delete db ~idfather: id ();
  Db.Assoc_prop.delete db ~idchild: id ();
  Db.Contrib_prop.delete db ~idprop: id ();
  Db.Props.delete db ~id ()


let father p =
  match Db.Assoc_prop.select (Args.db()) ~idchild: p.Db.Props.id () with
    [] -> None
  | t :: _ -> Some (get_or_fail t.Db.Assoc_prop.idfather)

let rec ancestors p =
  match father p with
    None -> []
  | Some f -> f :: (ancestors f)

let is_root p = (father p) = None

let update ?icon ?kind ?name ?url ?father id =
  let db = Args.db() in
  Db.Props.update db ?icon ?kind ?name ?url ~key_id: id ();
  (
   match father with
     None -> ()
   | Some f ->
       let p = get_or_fail id in
       if List.mem p (ancestors f) then
	 ()
       else
	 (
	  Db.Assoc_prop.delete db ~idchild: id ();
	  Db.Assoc_prop.insert db ~idchild: id ~idfather: f.Db.Props.id ()
	 )
  );
  get_or_fail id



let choices ?kind () =
  let roots = roots () in
  let rec iter tab acc = function
      [] -> acc
    | t :: q ->
        let s = Printf.sprintf "%s%s" tab t.Db.Props.name in
        let new_acc = iter (tab^"--") 
            (
	     (
	      (if List.mem t roots then
		"" 
	      else 
		string_of_int t.Db.Props.id
	      ), 
	      s
	     ) :: acc)
	    (sort (children t))
        in
        iter tab new_acc q
  in
  List.rev 
    (iter "" [] 
       (sort 
	  (List.filter 
	     (fun t -> 
	       match kind with
		 None -> true
	       | Some k -> t.Db.Props.kind = k) 
	     (match kind with
	       None -> roots
	     | Some k -> 
		 (List.flatten (List.map children roots))
	     )
	  )
       )
    )

let ancestors p =
  let rec iter acc p =
    match father p with
      None -> acc
    | Some p2 ->
	iter (p :: acc) p2
  in
  iter [] p

let fullname p = 
  String.concat " :: " 
    (List.map (fun p -> p.Db.Props.name) (ancestors p))
  
