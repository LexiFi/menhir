let _ = Config.dbg "Entering module main.ml"


(** Main module of the main cgi *)

module A = Ocgi.Args

let one_day = 24. *. 3600.
let age_old_contribs = 365. *. one_day

(** {2 Cgi Arguments} *)

let topic = ref None
let kind = ref None
let author = ref None
let status = ref None
let browse = ref None
let contrib = ref None

type sort = Sname | Sdate | Sstatus | Sprop of Db.Props.t
let sort = ref Sname

let sort_of_int = function
    0 -> Sname
  | -1 -> Sdate
  | -2 -> Sstatus
  | n -> 
      try Sprop (Props.get n)
      with _ -> Sname

let int_of_sort = function
    Sname -> 0
  | Sdate -> -1
  | Sstatus -> -2
  | Sprop p -> p.Db.Props.id

let set_sort n = sort := sort_of_int n

let f_set r f_get n = 
  try r := Some (f_get n)
  with _ -> r := None

let set_topic = f_set topic (Props.get ~kind: Types.Topic)
let set_kind = f_set kind (Props.get ~kind: Types.Kind)
let set_author = f_set author Authors.get
let set_status = f_set status Conv.status_of_int
let set_browse = f_set browse Props.get
let set_contrib = f_set contrib Contribs.get_or_fail

let options = [
  "browse", A.Int set_browse, [] ;
  "topic", A.Int set_topic, [];
  "kind", A.Int set_kind, [] ;
  "author", A.Int set_author, [] ;
  "status", A.Int set_status, [] ;
  "sort", A.Int set_sort, [] ;
  Constant.var_contrib, A.Int set_contrib, [] ;
] 

(** {2 Selecting contribs} *)

let select () =
  let contribs = Contribs.list () in
  match !browse, !topic, !kind, !author with
    None, None, None, None ->
      (
       (* get the latest contribs modified *)
       let l = Contribs.sort_by_date contribs in
       fst (Misc.get_n_first_ele Constant.latest_updates_number l)
      )
  | _ ->
      let f_true _ = true in
      let pred_prop r =
	match !r with
	  None -> f_true
	| Some p ->
	    fun c -> List.mem p (Contribs.props c)
      in
      let pred_browse = pred_prop browse in
      let pred_topic = pred_prop topic in
      let pred_kind = pred_prop kind in
      let pred_author =
	match !author with
	  None -> f_true
	| Some author -> 
	    fun c -> List.mem author (Contribs.authors c)
      in
      Contribs.sort 
	(List.filter
	   (fun c -> pred_browse c && pred_topic c && pred_kind c && pred_author c)
	   contribs
	)

(** {2 Sorting and grouping} *)

let sub_partition_items f_name pred contribs vals =
  let rec iter acc = function
    | ([], _ ) -> (acc, [])
    | (contribs,[]) -> 
	(acc, contribs)
    | (contribs, v :: q) ->
	let (l1, l2) = List.partition (pred v) contribs in
	let new_acc =
	  match l1 with
	    [] -> acc
	  | _ -> (f_name v, l1) :: acc
	in
	iter new_acc (l2, q)
  in
  iter [] (contribs, vals)

let partition_items f_name f_children pred contribs vals =
  let rec iter acc = function
      ([], _) -> acc
    | (contribs,[]) ->
	acc @ [Some Messages.others, contribs, []]
    | (contribs, v :: q) ->
	let (l1, l2) = List.partition (pred v) contribs in
	let (children, l2) = 
	  let l = f_children v in
	  sub_partition_items f_name pred l2 l
	in
	let new_acc =
	  match (l1,children) with
	    ([], []) -> acc
	  | _ ->
	      (Some (f_name v), l1, children) :: acc
	in
	iter new_acc (l2, q)
  in
  iter [] (contribs, vals)	    

(** Groups and sort contribs according to {!Main.sort}.
   Build a list of tuples
   (optional section title, sorted contribs in this section,
    list of subsections (title, sorted contribs)).
*)
let rec sort_and_group contribs =
  match !sort with
    Sname ->
      [None, Contribs.sort contribs, []]
  | Sdate ->
      let today = Unix.time () in
      let (recent, old) = List.partition
	  (fun c -> c.Db.Contribs.date +. age_old_contribs > today)
	  contribs
      in
      (
       match old with
	 [] -> [None, Contribs.sort_by_date recent, []]
       | l -> 
	   [ Some Messages.recent_contribs, Contribs.sort_by_date recent, [] ;
	     Some Messages.old_contribs, Contribs.sort_by_date old, [] ;
	   ]
      )

  | Sstatus ->
      partition_items
	Messages.of_status_opt
	(fun _ -> [])
	(fun st c -> c.Db.Contribs.status = st)
	contribs
	(None :: (List.map (fun s -> Some s) Types.status))

  | Sprop p ->
      partition_items
	(fun k ->  k.Db.Props.name)
	Props.children
	(fun k c -> List.mem k (Contribs.props c))
	contribs
	(List.rev (Props.sort (Props.children (Props.kind_root p.Db.Props.kind))))

(** {2 Displaying contribs} *)

let p = Printf.bprintf

let string_of_prop_kind = function
    Types.Kind -> "kind"
  | Types.License -> "license"
  | Types.Topic -> "topic"
  | Types.Attribute -> "attribute"

let url ?browse ?topic ?author ?kind sort =
  Printf.sprintf "%s?sort=%d%s%s%s%s"
    Config.loc_cgi_hump
    (int_of_sort sort)
    (
     match browse with 
       None -> "" 
     | Some t -> Printf.sprintf "&amp;browse=%d" t.Db.Props.id
    )
    (
     match topic with 
       None -> "" 
     | Some t -> Printf.sprintf "&amp;topic=%d" t.Db.Props.id
    )
    (
     match author with 
       None -> "" 
     | Some a -> Printf.sprintf "&amp;author=%d" a.Db.Authors.id
    )
    (
     match kind with 
       None -> "" 
     | Some k -> Printf.sprintf "&amp;kind=%d" k.Db.Props.id
    )

let html_prop_ancestors p =
  String.concat
    " :: "
    (
     List.map
       (fun p -> Printf.sprintf "<a href=\"%s\">%s</a>"
	   (url ~browse: p !sort)
	   (Ocgi.Misc.escape_entities p.Db.Props.name)
       )
       (Props.ancestors p)
    )

let print_author b a =
   p b "<author name=\"%s\" %s %s/>\n"
    (Authors.string_name a)
    (match a.Db.Authors.url with
      None -> ""
    | Some s -> Printf.sprintf "url=\"%s\"" s)
    (match a.Db.Authors.mail with
      None -> ""
    | Some s -> Printf.sprintf "mail=\"%s\"" s)

let print_attribute b a =
  p b "<attribute title=\"%s\" icon=\"%s\"/>\n"
    (Ocgi.Misc.escape_entities a.Db.Props.name)
    (Ocgi.Misc.string_of_opt a.Db.Props.icon)

let print_contrib ?(detail=false) b c =
  p b "<hump-item\n";
  if not detail then 
    (
     match Contribs.props ~kind: Types.Kind c with
       [] -> ()
     | x :: _ -> 
	 p b "  kind=\"%s\"\n" (Ocgi.Misc.escape_entities x.Db.Props.name);
	 match x.Db.Props.icon with
           None -> ()
	 | Some f ->
	     p b "  icon=\"%s\"\n" f
    )
  else
    p b "  detail=\"%s\"\n" (Contribs.detail_url c);

  p b "  title=\"%s\"\n" (Ocgi.Misc.escape_entities c.Db.Contribs.name);
  p b "  date=\"%s\"\n"
    (Messages.string_of_date ~hours: false c.Db.Contribs.date);
  (
   match c.Db.Contribs.status with
     None -> ()
   | Some st -> p b "  status=\"%s\"\n" (Messages.of_status st)
  );
  (
   match c.Db.Contribs.version with
     None -> ()
   | Some s -> p b "  version=\"%s\"\n" s
  );
  p b ">\n";
  p b "<file type=\"details\" href=\"%s?contrib=%i\"/>\n" 
    Config.loc_cgi_hump c.Db.Contribs.id ;
  p b "<file type=\"website\" href=\"%s\"/>\n" (Ocgi.Misc.escape_entities c.Db.Contribs.url) ;
  List.iter (print_attribute b) (Contribs.props ~kind: Types.Attribute c);
  List.iter (print_author b) (Contribs.authors c);
  p b "<desc>%s</desc>\n" 
    (Ocgi.Misc.escape_entities c.Db.Contribs.description);
  if detail then
    (
     let kinds = Contribs.props ~kind: Types.Kind c in
     let topics = Contribs.props ~kind: Types.Topic c in
     let licenses = Contribs.props ~kind: Types.License c in
     let print prop =
       p b "<property type=\"%s\">%s</property>"
	 (string_of_prop_kind prop.Db.Props.kind)
	 (html_prop_ancestors prop)
     in
     List.iter (List.iter print) [ kinds; topics; licenses; ]
    );
  p b "</hump-item>\n"

let print_contrib_list b l =
  p b "<hump-list>";
  List.iter (print_contrib b) l;
  p b "</hump-list>"

let print_contribs b l =
  let groups = sort_and_group l in
  let print_sub (s, l) =
    p b "<h3>%s</h3>" s;
    print_contrib_list b l
  in
  List.iter
    (fun (title_opt, l, children) ->
      (
       match title_opt with
	 None -> ()
       | Some t -> p b "<h2>%s</h2>" t
      );
      print_contrib_list b l;
      List.iter print_sub children ;
    )
    groups

let title_of_kind k = 
  let p = Props.kind_root k in
  p.Db.Props.name

let print_panel_select_view b =
  let usable_prop_kinds = 
    List.filter ((<>) Types.Attribute) Types.prop_kinds
  in
  let other_prop_kinds =
    match !browse with
      None -> usable_prop_kinds
    | Some p ->
	List.filter
	  (fun k -> k <> p.Db.Props.kind) 
	  usable_prop_kinds
  in
  let kind_props = List.map Props.kind_root other_prop_kinds in

  let f_browse =
    let k = match !browse with None -> None | Some p -> Some p.Db.Props.kind in
    fun prop ->
      let label = 
	Messages.by_ 
	  (String.uncapitalize (Props.kind_root prop.Db.Props.kind).Db.Props.name)
      in
      if Some prop.Db.Props.kind = k then
	p b "<li><span class=\"active\">%s</span></li>" label
      else
	let first_subprop =
	  match Props.sort (Props.children prop) with
	    [] -> prop
	  | p :: _ -> p	    
	in
	p b "<a href=\"%s\">%s</a>"
          (url ~browse: first_subprop !sort)
          label
  in

  let prop_kinds_pairs = 
    List.map (fun p -> (Sprop p, String.uncapitalize p.Db.Props.name)) kind_props
  in

  let f_sort (sort', label) =
    if sort' = !sort then
      p b "<li><span class=\"active\">%s</span></li>"
        label
    else
      p b "<a href=\"%s\">%s</a>"
        (url ?browse: !browse ?topic: !topic ?kind: !kind ?author: !author sort')
        label
  in

  p b "<panel><caption><en>%s</en></caption>\n<content><hump-menu2>\n" Messages.select_view;

  p b "<hump-menu2-col><caption><en>%s</en></caption><content><al>\n" Messages.browse ;
  List.iter f_browse (List.map Props.kind_root usable_prop_kinds);
  (
   match (!browse, !topic, !kind, !author) with
     (None, None, None, None) -> 
       p b "<li><span class=\"active\">%s</span></li>" Messages.latest_updates
   | _ ->
       p b "<a href=\"%s\">%s</a>" (url !sort) Messages.latest_updates
  );
  p b "</al></content></hump-menu2-col>\n" ;


  p b "<hump-menu2-col><caption><en>%s</en></caption><content><al>\n" Messages.sort_by ;
  List.iter f_sort 
    (
     [ Sname, Messages.contrib_names ; 
       Sdate, Messages.contrib_dates ;
       Sstatus, Messages.contrib_status ;
     ] 
     @ prop_kinds_pairs 
    );
  p b "</al></content></hump-menu2-col>\n" ;

  p b "</hump-menu2></content>\n</panel>\n"

let print_panel_related_props b prop =
  p b "<panel><caption><en>%s</en></caption>\n<content><al>\n" 
    (title_of_kind prop.Db.Props.kind);
  let f_url browse = url ?browse ?topic: !topic ?kind: !kind ?author: !author !sort in
  let f_opt = Props.father prop in
  let children = 
    List.map (fun t -> (f_url (Some t), t.Db.Props.name)) (Props.children prop) 
  in
  let p_children l =
    List.iter 
      (fun (href, label) ->
	p b "<a href=\"%s\">%s</a>\n" href label)
      l
  in
  let p_prop_with_brothers () =
    let l = 
      match f_opt with
	None -> [prop]
      |	Some f -> Props.children f
    in
    List.iter 
      (fun prop2 ->
	if prop2 = prop then
	  (
	   p b "<li><span class=\"active\">%s</span>\n<al>\n" prop.Db.Props.name ;
	   p_children children;
	   p b "</al></li>\n"
	  )
	else
	  p b "<a href=\"%s\">%s</a>\n" (f_url (Some prop2)) prop2.Db.Props.name
      )
      l	  
  in
  (
   match f_opt with
     None -> 
       p_children children
   | Some f when f = Props.kind_root prop.Db.Props.kind ->
       p_prop_with_brothers ()
   | Some f ->
(*       p b "<a href=\"%s\">%s</a>\n" (f_url None) Messages.latest_updates ;*)
       p b "<li><a href=\"%s\">%s</a>\n<al>\n" (f_url f_opt) f.Db.Props.name ;
       p_prop_with_brothers () ;
       p b "</al></li>\n"
  );
  p b "</al></content>\n</panel>\n"

let print_panel_search b =
  let s =
"<panel>
   <caption>
     <en>Search the Hump</en>
     <fr>Rechercher dans la Bosse</fr>
   </caption>
   <content>
     <form id=\"quick-search\" method=\"get\" action=\"!search\"
      enctype=\"application/x-www-form-urlencoded\">
       <div>
         <input name=\"corpus\" type=\"hidden\" value=\"hump\" id=\"corpus\"/>
         <input name=\"words\" type=\"text\" value=\"\" id=\"words\" />
         <br/>
         <en><button type=\"submit\">Search</button></en>
         <fr><button type=\"submit\">Rechercher</button></fr>
       </div>
     </form>
     <div style=\"font-size: x-small; font-style: italic; text-align: right;\">
     Powered by <a href=\"http://swish-e.org/\">Swish-E</a>
     </div>
   </content>
 </panel>" in
   Buffer.add_string b s

let browse_by b_menu b_content prop_opt contribs =
  p b_menu "<navigation>";
  print_panel_select_view b_menu;
  (
   match !browse with
     None -> ()
   | Some _ -> 
       print_panel_related_props b_menu 
	 (match prop_opt with
	   None -> Props.kind_root Types.Topic 
	 | Some t -> t
	 );
  );
  print_panel_search b_menu;
  p b_menu "</navigation>";
  print_contribs b_content contribs;
  match prop_opt with
    None -> 
      (
       match !topic, !kind, !author with
	 None, None, None -> Messages.latest_updates
       | _, _, _ ->
	   Printf.sprintf "%s %s %s"
	     (
	      match !kind with
		None -> Messages.contribs
	      |	Some p -> p.Db.Props.name
	     )
	     (
	      match !topic with
		None -> ""
	      |	Some t -> Messages.in_topic (Props.fullname t)
	     )
	     (
	      match !author with
		None -> ""
	      |	Some a -> Messages.by_author (Authors.string_name a)
	     )
      )
  | Some t -> Props.fullname t

let display contribs =
  let b_menu = Buffer.create 256 in
  let b_content = Buffer.create 256 in
  let title = browse_by b_menu b_content (!browse) contribs in
  let alternate_rss = title = Messages.latest_updates in
  let menu = Buffer.contents b_menu in
  let content = Buffer.contents b_content in
  let page = Page.page None ~alternate_rss ~title ~menu ~content () in
  page

(** {2 Displaying a single contrib} *)

let display_single c =
  let b_content = Buffer.create 256 in
  print_contrib ~detail: true b_content c;
  let content = Buffer.contents b_content in
  let page = Page.page None ~title: c.Db.Contribs.name ~content () in
  page

(** {2 Main} *)

let _ = Ocgi.Hooks.content_type := "text/pre-xhtml"
let main () = 
  let content = 
    try
      Args.parse ();
      let env = Ocgi.Env.get_cgi_env () in
      Ocgi.Args.parse env options ;
      let page =
	match !contrib with
	  Some c -> display_single c
	| None ->
	    (
             (* ne pas trier sur ce par quoi on browse *)
	     match (!sort, !browse) with
	       (Sprop p, Some p2) when p2.Db.Props.kind = p.Db.Props.kind ->
		 sort := Sname;
	    | _ -> ()
	    );
	    let contribs = select () in
	    display contribs
      in
      page
    with
    | Failure s ->
	prerr_endline s;
	s
  in
  print_string (Ocgi.Model.html_answer (content, []))

let _ = main ()
