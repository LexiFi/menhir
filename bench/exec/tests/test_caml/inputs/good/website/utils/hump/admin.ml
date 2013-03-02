let _ = Config.dbg "Entering module admin.ml"


(** Main module of the admin cgi *)

module H = Ocgi.Html
module M = Messages

let p = Printf.bprintf 

(** {2 Table creation} *)

let insert_first_user () =
  match Users.list () with
    [] -> 
      let u = Users.create 
          ~login: Config.admin_login
          ~mail: Config.admin_mail
          ~passwd: "admin"
          ()
      in
      ()
  | _ ->
      ()

let insert_root_props () =
  match Props.list () with
    [] ->
      List.iter
	(fun kind ->
	  ignore 
	    (Props.create ~kind ~name: (M.of_prop_kind kind) ())	       
	)
	Types.prop_kinds
  | _ ->
      ()

let create_tables env user_opt =
  let db =  Args.db() in
  let iter f =
    try f db
    with  Failure s -> prerr_endline s
  in
  List.iter iter
    (* please keep the list in alphabetical order, except
       the first tables Agents and Admins, which must
       be created first, since the owner is needed
       to insert other elements.
    *)
    [  
      (fun db -> Db.Users.create db; insert_first_user ());
(*      (fun db -> Db.Dbversion.create db; insert_dbversion_current db);*)
      Db.Assoc_prop.create ;
      Db.Authors.create ;
      Db.Contrib_author.create ;
      Db.Contrib_link.create ;
      Db.Contrib_prop.create ;
      Db.Contribs.create ;
      Db.Link_kinds.create ;
      (fun db -> Db.Props.create db; insert_root_props ()) ;
      Db.Sessions.create ;
    ] ;
  Page.admin_page None ~title: "Table creation" ~content: "Done" ()


(** {2 Login/logout} *)

let default env user_opt () = Page.form_login env user_opt

let login env user_opt =
  try 
    let t = F_login.read env in
    let login = t.F_login.login in
    let passwd = t.F_login.passwd in  
    try
      let u = 
        try Users.get_by_login login
        with Not_found ->
          failwith (Messages.err_unknown_user login)
      in
      if u.Db.Users.passwd <> passwd then
        failwith Messages.err_invalid_password;
      
      let sid = Sessions.create env.Ocgi.Env.remote_host u in
      (
       Page.admin_welcome (Some u) (),
       [Ocgi.Model.Set (Config.session_varname, sid)]
      )
    with
    | Failure errmes ->
        (Page.form_login ~errmes env user_opt, [])
  with
    Ocgi.Args.Read_errors l ->
      (Page.form_login ~errmes: (Misc.html_of_read_errors l) env user_opt, [])

let logout env user_opt =
  match user_opt with
    None -> (default env user_opt (), [])
  | Some u ->
      match Model.M.session_id env with
        None -> (Page.admin_welcome None (), [])
      | Some sid ->     
          let b = Sessions.delete sid u in
          let (cooks, us_opt) = 
            if b then
              (
               [Ocgi.Model.Unset (Config.session_varname, sid)],
               None
              )
            else
              ([], user_opt)
          in
          (Page.admin_welcome us_opt (), cooks)


(** {2 Authors} *)

let authors_list () =
  let s_name a = 
    Printf.sprintf "%s %s" a.Db.Authors.name a.Db.Authors.firstname 
  in
  let f a = H.seq
    [
      (
       match a.Db.Authors.url with 
	 None -> s_name a
       | Some s -> H.link s (s_name a)
      );
      " ";
      (
       match a.Db.Authors.mail with
	 None -> ""
       | Some s -> 
	   Printf.sprintf "[%s]"
	     (H.link (Printf.sprintf "mailto:%s" s) Messages.mail)
      );
      " ";
      H.seq
	[
	  H.span ~cl: "button"
	    (H.link 
	       (Printf.sprintf "%s?callback=%s&amp;%s=%d"
		  Config.loc_cgi_admin
		  Constant.cb_form_modify_author
		  Constant.var_id
		  a.Db.Authors.id
	       )
	       M.modify
	    );
	  H.span ~cl: "button"
	    (H.link 
	       (Printf.sprintf "%s?callback=%s&amp;%s=%d"
		  Config.loc_cgi_admin
		  Constant.cb_remove_author
		  Constant.var_id
		  a.Db.Authors.id
	       )
	       M.remove
	    );
	] ;
      H.br ;
    ] 
  in
  H.seq (List.map f (Authors.sort (Authors.list ())))

let authors user env user_opt =
  let l = authors_list () in
  Page.admin_page user_opt 
    ~title: M.authors
    ~content: l
    ()

let form_add_author ?errmes user env user_opt =
  let content = 
    match errmes with
      None -> 
	F_author.string 
	  ~callback: Constant.cb_add_author
	  ()
    | Some s ->
	F_author.reform
	  ~errmes: s
	  ~callback: Constant.cb_add_author
	  env
  in
  Page.admin_page user_opt ~title: M.add_author ~content ()

let add_author user env user_opt =
  try
    try
      let module F = F_author in
      let t = F.read env in
      let name = t.F.name in
      let firstname = Ocgi.Misc.string_of_opt t.F.firstname in
      let mail = t.F.mail in
      let url = t.F.url in
      ignore (Authors.create ~name ~firstname ~mail ~url ());
      authors user env user_opt
    with
      Ocgi.Args.Read_errors l ->
	failwith (Misc.html_of_read_errors l) 
  with
    Failure errmes ->
      form_add_author ~errmes user env user_opt

let form_modify_author ?errmes user env user_opt =
  let content = 
    match errmes with
      None -> 
	(
	 match Ocgi.Args.int_arg env ~default: Constant.id_other Constant.var_id with
	   id when id = Constant.id_other -> failwith M.err_no_id
	 | id ->
	     let a = Authors.get_or_fail id in
	     F_author.string 
	       ~callback: Constant.cb_modify_author
	       ~id: (string_of_int id)
	       ~name: a.Db.Authors.name
	       ~firstname: a.Db.Authors.firstname
	       ?mail: a.Db.Authors.mail
	       ?url: a.Db.Authors.url
	       ()
	)
    | Some s ->
	F_author.reform
	  ~errmes: s
	  ~callback: Constant.cb_modify_author
	  env
  in
  Page.admin_page user_opt ~title: M.modify_author ~content ()

let modify_author user env user_opt =
  try
    try
      let module F = F_author in
      let t = F.read env in
      match t.F.id with
	None -> failwith M.err_no_id
      |	Some id ->
	  let a = Authors.get_or_fail id in
	  ignore
	    (Authors.update 
	       ~name: t.F.name
	       ~firstname: (Ocgi.Misc.string_of_opt t.F.firstname)
	       ~mail: t.F.mail
	       ~url: t.F.url
	       id
	    );
	  authors user env user_opt
    with
      Ocgi.Args.Read_errors l ->
	failwith (Misc.html_of_read_errors l) 
  with
    Failure s ->
      form_modify_author ~errmes: s user env user_opt

let remove_author user env user_opt =
  try
    let id = Ocgi.Args.int_arg env 
        ~err: Messages.err_no_id Constant.var_id
    in
    let author = Authors.get_or_fail id in
    match Ocgi.Args.optional env Constant.var_confirm with
      None ->
        let yes = 
          Printf.sprintf "%s?callback=%s&amp;%s=%d&amp;%s=yes"
            Config.loc_cgi_admin
            Constant.cb_remove_author
            Constant.var_id
            id
            Constant.var_confirm
        in
        let no = Printf.sprintf "%s?callback=%s" 
            Config.loc_cgi_admin
            Constant.cb_admin_authors
        in
        Page.ask_for_confirmation
	  ~page: user_opt
          ~yes ~no
          ~message: (Messages.really_remove_author (Authors.string_name author))
          ()
    | Some _ ->
        Authors.delete author;
        authors user env user_opt
  with
    Ocgi.Args.Missing_argument (_, s) 
  | Failure s -> 
      Page.admin_error env user_opt s

(** {Properties} *)

let props_tree l =
  let f_html p =
    H.seq
      [
(*
	(match p.Db.Props.icon with
	  None -> ""
	| Some src ->
	    (H.tag "img" 
	      ~atts: [ "src", Config.ico src ;
		       "alt",  p.Db.Props.name]
	      "") ^ " "
	);
*)
	H.link 
	  (Printf.sprintf "%s?callback=%s&amp;%s=%d"
	     Config.loc_cgi_admin
	     Constant.cb_form_modify_property
	     Constant.var_id
	     p.Db.Props.id
	  )
	  p.Db.Props.name ;

	if List.mem p l then
	  Printf.sprintf " [%s]" (M.of_prop_kind p.Db.Props.kind)
	else
	  "" ;
      ]	
  in
  let rec f p =
    H.tag "li" 
      (H.seq
         [(f_html p);
          match Props.sort (Props.children p) with
	    [] -> ""
          | l -> H.ul (List.map f l)
        ]
      )
  in
  H.ul (List.map f l)

let properties user env user_opt =  
  let l = Props.roots () in
  let content = props_tree l in
  Page.admin_page user_opt 
    ~title: M.properties
    ~content
    ()

let form_add_property ?errmes user env user_opt =
  try
    let kind = Conv.prop_kind_of_int
	(Ocgi.Args.int_arg env ~err: M.err_no_kind Constant.var_kind)
    in
    let father_choices = ("","")::(Props.choices ~kind ()) in
    let s_kind = Conv.string_of_prop_kind kind in
    let content = 
      match errmes with
	None ->
	  F_property.string
	    ~callback: Constant.cb_add_property 
	    ~kind: s_kind
	    ~father_choices
	    ()
      | Some errmes ->
	  F_property.reform ~errmes
	    ~callback: Constant.cb_add_property
	    ~kind: s_kind
	    ~father_choices
	    env
    in
    Page.admin_page user_opt ~title: (M.add_property kind) ~content ()
  with
    Ocgi.Args.Missing_argument (_,s) 
  | Failure s ->
      Page.admin_error env user_opt s


let add_property user env user_opt =
  try
    try
      let module F = F_property in
      let t = F.read env in
      let kind = t.F.kind in
      let name = t.F.name in
      let father = 
	match t.F.father with
	  None -> Some (Props.kind_root kind)
	| f -> f
      in
      let url = t.F.url in
      let icon = t.F.icon in
      ignore (Props.create ?icon ~kind ~name ?url ?father ());
      properties user env user_opt
    with
      Ocgi.Args.Read_errors l ->
	failwith (Misc.html_of_read_errors l) 
  with
    Failure errmes ->
      form_add_property ~errmes user env user_opt

let form_modify_property ?errmes user env user_opt =
  try
    let remove_link id =
      H.link
	(Printf.sprintf "%s?callback=%s&amp;%s=%d"
	   Config.loc_cgi_admin
	   Constant.cb_remove_property
	   Constant.var_id
	   id
	)
	M.remove
    in
    let content = 
      match errmes with
	None -> 
	  (
	   match Ocgi.Args.int_arg env 
	       ~default: Constant.id_other Constant.var_id 
	   with
	     id when id = Constant.id_other -> failwith M.err_no_id
	   | id ->
	       let p = Props.get_or_fail id in
	       let kind = p.Db.Props.kind in
	       let (father_choices, father) = 
		 match Props.father p with
		   None -> ([], "")
		 | Some f -> 
		     (("", "") :: (Props.choices ~kind ()), string_of_int f.Db.Props.id)
	       in
	       F_property.string 
		 ~callback: Constant.cb_modify_property
		 ~remove_link: (remove_link id)
		 ~id: (string_of_int id)
		 ~kind: (Conv.string_of_prop_kind p.Db.Props.kind)
		 ~father_choices
		 ~father
		 ~name: p.Db.Props.name
		 ?icon: p.Db.Props.icon
		 ?url: p.Db.Props.url
		 ()
	  )
      | Some s ->
	  let p = Props.get_or_fail 
	      (Ocgi.Args.int_arg env ~err: M.err_no_id Constant.var_id)
	  in
	  let kind = p.Db.Props.kind in
	  let (father_choices, father) = 
	    match Props.father p with
	      None -> let s = string_of_int p.Db.Props.id in ([s,""], s)
	    | Some f -> 
		(Props.choices ~kind (), string_of_int f.Db.Props.id)
	  in
	  F_property.reform
	    ~errmes: s
	    ~callback: Constant.cb_modify_property
	    ~remove_link: (remove_link p.Db.Props.id)
	    ~kind: (Conv.string_of_prop_kind p.Db.Props.kind)
	    ~father_choices
	    ~father
	    env
    in
    Page.admin_page user_opt ~title: M.modify_property ~content ()
  with
  | Ocgi.Args.Missing_argument (_,s)
  | Failure s ->
      Page.admin_error env user_opt s

let modify_property user env user_opt =
  try
    try
      let module F = F_property in
      let t = F.read env in
      match t.F.id with
	None -> failwith M.err_no_id
      |	Some id ->
	  let p = Props.get_or_fail id in
	  let father =
	    match Props.father p with
	      None -> None
	    | Some father -> 
		match t.F.father with
		| Some f when f.Db.Props.id = p.Db.Props.id ->
		    Some father
		| x -> x
	  in
	  ignore
	    (Props.update 
	       ~name: t.F.name
	       ?father: father
	       ~icon: t.F.icon
	       ~url: t.F.url
	       id
	    );
	  properties user env user_opt
    with
      Ocgi.Args.Read_errors l ->
	failwith (Misc.html_of_read_errors l) 
  with
    Failure s ->
      form_modify_property ~errmes: s user env user_opt

let remove_property user env user_opt =
  try
    let id = Ocgi.Args.int_arg env 
        ~err: Messages.err_no_id Constant.var_id
    in
    let p = Props.get_or_fail id in
    match Ocgi.Args.optional env Constant.var_confirm with
      None ->
        let yes = 
          Printf.sprintf "%s?callback=%s&amp;%s=%d&amp;%s=yes"
            Config.loc_cgi_admin
            Constant.cb_remove_property
            Constant.var_id
            id
            Constant.var_confirm
        in
        let no = Printf.sprintf "%s?callback=%s" 
            Config.loc_cgi_admin
            Constant.cb_admin_properties
        in
        Page.ask_for_confirmation
	  ~page: user_opt
          ~yes ~no
          ~message: (Messages.really_remove_property p.Db.Props.name)
          ()
    | Some _ ->
        Props.delete p;
        properties user env user_opt
  with
    Ocgi.Args.Missing_argument (_, s) 
  | Failure s -> 
      Page.admin_error env user_opt s

(** {2 Link kinds} *)

let link_kinds_list () =
  let cols =
    [
      (Some M.name, None, None, fun a -> a.Db.Link_kinds.name) ;
      (Some M.icon, None, None, fun a -> a.Db.Link_kinds.icon) ;
      (None, None, None,
       fun a ->
	 H.seq
	   [
	     H.span ~cl: "button"
	       (H.link 
		  (Printf.sprintf "%s?callback=%s&amp;%s=%d"
		     Config.loc_cgi_admin
		     Constant.cb_form_modify_link_kind
		     Constant.var_id
		     a.Db.Link_kinds.id
		  )
		  M.modify
	       );
	     H.span ~cl: "button"
	       (H.link 
		  (Printf.sprintf "%s?callback=%s&amp;%s=%d"
		     Config.loc_cgi_admin
		     Constant.cb_remove_link_kind
		     Constant.var_id
		     a.Db.Link_kinds.id
		  )
		  M.remove
	       )
	   ] 
	   )
    ] 
  in
  H.list_in_table 
    cols
    (Link_kinds.sort (Link_kinds.list ()))

let link_kinds user env user_opt =
  let l = link_kinds_list () in
  Page.admin_page user_opt 
    ~title: M.link_kinds
    ~content: l
    ()

let form_add_link_kind ?errmes user env user_opt =
  let content = 
    match errmes with
      None -> 
	F_link_kind.string 
	  ~callback: Constant.cb_add_link_kind
	  ()
    | Some s ->
	F_link_kind.reform
	  ~errmes: s
	  ~callback: Constant.cb_add_link_kind
	  env
  in
  Page.admin_page user_opt ~title: M.add_link_kind ~content ()

let add_link_kind user env user_opt =
  try
    try
      let module F = F_link_kind in
      let t = F.read env in
      let name = t.F.name in
      let icon = t.F.icon in
      ignore (Link_kinds.create ~name ~icon ());
      link_kinds user env user_opt
    with
      Ocgi.Args.Read_errors l ->
	failwith (Misc.html_of_read_errors l) 
  with
    Failure errmes ->
      form_add_link_kind ~errmes user env user_opt

let form_modify_link_kind ?errmes user env user_opt =
  let content = 
    match errmes with
      None -> 
	(
	 match Ocgi.Args.int_arg env ~default: Constant.id_other Constant.var_id with
	   id when id = Constant.id_other -> failwith M.err_no_id
	 | id ->
	     let t = Link_kinds.get_or_fail id in
	     F_link_kind.string 
	       ~callback: Constant.cb_modify_link_kind
	       ~id: (string_of_int id)
	       ~name: t.Db.Link_kinds.name
	       ~icon: t.Db.Link_kinds.icon
	       ()
	)
    | Some s ->
	F_link_kind.reform
	  ~errmes: s
	  ~callback: Constant.cb_modify_link_kind
	  env
  in
  Page.admin_page user_opt ~title: M.modify_link_kind ~content ()

let modify_link_kind user env user_opt =
  try
    try
      let module F = F_link_kind in
      let t = F.read env in
      match t.F.id with
	None -> failwith M.err_no_id
      |	Some id ->
	  let a = Link_kinds.get_or_fail id in
	  ignore
	    (Link_kinds.update 
	       ~name: t.F.name
	       ~icon: t.F.icon
	       id
	    );
	  link_kinds user env user_opt
    with
      Ocgi.Args.Read_errors l ->
	failwith (Misc.html_of_read_errors l) 
  with
    Failure s ->
      form_modify_link_kind ~errmes: s user env user_opt

let remove_link_kind user env user_opt =
  try
    let id = Ocgi.Args.int_arg env 
        ~err: Messages.err_no_id Constant.var_id
    in
    let link_kind = Link_kinds.get_or_fail id in
    match Ocgi.Args.optional env Constant.var_confirm with
      None ->
        let yes = 
          Printf.sprintf "%s?callback=%s&amp;%s=%d&amp;%s=yes"
            Config.loc_cgi_admin
            Constant.cb_remove_link_kind
            Constant.var_id
            id
            Constant.var_confirm
        in
        let no = Printf.sprintf "%s?callback=%s" 
            Config.loc_cgi_admin
            Constant.cb_admin_link_kinds
        in
        Page.ask_for_confirmation
	  ~page: user_opt
          ~yes ~no
          ~message: (Messages.really_remove_link_kind
		       link_kind.Db.Link_kinds.name)
          ()
    | Some _ ->
        Link_kinds.delete link_kind;
        link_kinds user env user_opt
  with
    Ocgi.Args.Missing_argument (_, s) 
  | Failure s -> 
      Page.admin_error env user_opt s

(** {2 Contribs} *)

let contribs_list ?letter () =
  let print_contrib b c =
    p b "<hump-item\n";
    (
     match Contribs.props ~kind: Types.Kind c with
       [] -> ()
     | x :: _ -> 
	 p b "  kind=\"%s\"\n" x.Db.Props.name;
	 match x.Db.Props.icon with
           None -> ()
	 | Some f ->
	     p b "  icon=\"%s\"\n" f
    );
    p b "  title=\"%s\"\n" c.Db.Contribs.name;
    p b "  detail=\"%s\"\n" (Contribs.detail_url c);
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
    p b "<file type=\"website\" href=\"%s\"/>\n" 
      (Ocgi.Misc.escape_entities c.Db.Contribs.url) ;
    p b "<desc>%s</desc>\n" 
      (H.seq
	   [
	     H.span ~cl: "button"
	       (H.link 
		  (Printf.sprintf "%s?callback=%s&amp;%s=%d"
		     Config.loc_cgi_admin
		     Constant.cb_form_modify_contrib
		     Constant.var_id
		     c.Db.Contribs.id
		  )
		  M.modify
	       );

	     H.span ~cl: "button"
	       (H.link 
		  (Printf.sprintf "%s?callback=%s&amp;%s=%d"
		     Config.loc_cgi_admin
		     Constant.cb_remove_contrib
		     Constant.var_id
		     c.Db.Contribs.id
		  )
		  M.remove
	       );
	     H.span ~cl: "button"
	       (H.link 
		  (Printf.sprintf "%s?callback=%s&amp;%s=%d"
		     Config.loc_cgi_admin
		     Constant.cb_set_contrib_date
		     Constant.var_id
		     c.Db.Contribs.id
		  )
		  M.set_date
	       )
	   ] 
      );
    p b "</hump-item>\n"
  in
  let buf = Buffer.create 256 in
  p buf "<hump-list>\n";
  let l = Contribs.sort (Contribs.list ()) in
  let l = match letter with
    None -> l
  | Some c ->
      let c = Char.lowercase c in
      let rec iter acc = function
	[] -> List.rev acc
	| h :: q ->
	    let name = h.Db.Contribs.name in
	    let len = String.length name in
	    if len > 0 then
	      match Pervasives.compare (Char.lowercase name.[0]) c with
		0 -> iter (h :: acc) q
	      |	n when n < 0 -> iter acc q
	      |	_ -> List.rev acc
	    else
	      iter acc q
      in
      iter [] l
  in
  List.iter (print_contrib buf) l;
  p buf "</hump-list>\n";
  let content = Buffer.contents buf in
    let letters = Array.to_list
      (Array.init 26 (fun i -> Char.chr (Char.code 'a' + i)))
  in
  let f c =
    H.link 
      (Printf.sprintf "%s?callback=%s&amp;%s=%c"
	 Config.loc_cgi_admin
	 Constant.cb_admin_contribs
	 Constant.var_letter
	 c
      )
      (String.make 1 c)
  in
  H.seq
    [
      H.tag "hr" "";
      H.concat ~sep: " " (List.map f letters) ;
      H.tag "hr" "";
      content
    ]

let form_add_contrib_prop c kind =
  let content = H.seq
      [
	H.input H.Hidden ~value: Constant.cb_add_contrib_prop "callback" ;
	H.input H.Hidden ~value: (string_of_int c.Db.Contribs.id) Constant.var_id ;
	H.select Constant.var_idprop (Props.choices ~kind ()) Constant.s_id_other ;
	H.submit_button M.add ;
      ]	
  in
  H.form ~met: H.Post Config.loc_cgi_admin
    content

let contrib_props ?errmes c =

  let t = Hashtbl.create 13 in
  let f p =
    try
      let r = Hashtbl.find t p.Db.Props.kind in
      r := p :: !r
    with
      Not_found ->
	Hashtbl.add t p.Db.Props.kind (ref [p])
  in
  List.iter f (Contribs.props c);
  let prop_cols =
    let props k = 
      try !(Hashtbl.find t k)
      with Not_found -> []
    in
    [
      (None, None, None, fun kind -> H.h3 (M.of_prop_kind kind)) ;
      (None, None, None, 
       fun kind ->
	 H.seq
	   (List.map
	      (fun p ->
		H.seq 
		  [
		    Props.fullname p;
		    H.span ~cl: "button"
		      (H.link
			 (Printf.sprintf "%s?callback=%s&amp;%s=%d&amp;%s=%d"
			    Config.loc_cgi_admin
			    Constant.cb_remove_contrib_prop
			    Constant.var_id
			    c.Db.Contribs.id
			    Constant.var_idprop
			    p.Db.Props.id
		      )
			 M.remove
		      );
		    H.br;
		  ] 
	      )
	      (props kind)
	   )
      ) ;
      (None, None, None, fun kind -> form_add_contrib_prop c kind);
    ]	
  in
  H.seq
    [
      (
       match errmes with
	 None -> ""
       | Some s ->
	   H.div ~cl: "error" s
      ) ;
      H.h2 M.properties ;
      H.list_in_table prop_cols Types.prop_kinds;
    ] 

let form_add_contrib_author c =
  let content = H.seq
      [
	H.input H.Hidden ~value: Constant.cb_add_contrib_author "callback" ;
	H.input H.Hidden ~value: (string_of_int c.Db.Contribs.id) Constant.var_id ;
	H.select Constant.var_idauthor (Authors.choices ()) Constant.s_id_other ;
	H.submit_button M.add ;
      ]	
  in
  H.form ~met: H.Post Config.loc_cgi_admin content

let contrib_authors ?errmes c =
  H.seq
    [
      (
       match errmes with
	 None -> ""
       | Some s ->
	   H.div ~cl: "error" s
      ) ;
      H.h2 M.authors ;
      H.seq
	(List.map
	   (fun a ->
	     H.seq 
	       [
		 Authors.string_name a;
		 H.span ~cl: "button"
		   (H.link
		      (Printf.sprintf "%s?callback=%s&amp;%s=%d&amp;%s=%d"
			 Config.loc_cgi_admin
			 Constant.cb_remove_contrib_author
			 Constant.var_id
			 c.Db.Contribs.id
			 Constant.var_idauthor
			 a.Db.Authors.id
		      )
		      M.remove
		   );
		 H.br
	       ] 
	   )
	   (Contribs.authors c)
	);
      H.br ;
      form_add_contrib_author c ;
    ] 

let contribs ?letter user env user_opt =
  let content =
    let letter =
      match letter with
	Some c -> c
      |	None ->
	  match Ocgi.Args.optional env Constant.var_letter with
	    None -> '0'
	  | Some s when s = "" -> '0'
	  | Some s ->
	      match s.[0] with 
		'a' .. 'z' | 'A' .. 'Z' -> Char.lowercase s.[0]
	      | _ -> '0'
    in
    contribs_list ~letter () 
  in
  Page.admin_page user_opt ~title: M.contribs ~content ()

let status_choices () =
  ("", "") :: (List.map (fun s -> (Conv.string_of_status s, M.of_status s)) Types.status)

let form_add_contrib ?errmes user env user_opt =
  let content = 
    match errmes with
      None -> 
	F_contrib.string 
	  ~callback: Constant.cb_add_contrib
	  ~status_choices: (status_choices ())
	  ~status: (Conv.string_of_status Types.Pre_alpha)
	  ()
    | Some s ->
	F_contrib.reform
	  ~errmes: s
	  ~callback: Constant.cb_add_contrib
	  ~status_choices: (status_choices ())
	  ~status: (Conv.string_of_status Types.Pre_alpha)
	  env
  in
  Page.admin_page user_opt ~title: M.add_contrib ~content ()



let form_modify_contrib ?errmes c =
  F_contrib.string
    ~callback: Constant.cb_modify_contrib
    ~id: (string_of_int c.Db.Contribs.id)
    ~name: c.Db.Contribs.name
    ?status: (Misc.map_opt Conv.string_of_status c.Db.Contribs.status)
    ~status_choices: (status_choices ())
    ~version: (Ocgi.Misc.string_of_opt c.Db.Contribs.version)
    ~description: c.Db.Contribs.description
    ~url: (Ocgi.Misc.escape_entities c.Db.Contribs.url)
    ~homepage_indexed: c.Db.Contribs.homepage_indexed
    ()



let page_modify_contrib ?id ?errmes ?errmes_prop ?errmes_author
    user env user_opt =
  try
    let id = 
      match id with
	None -> Ocgi.Args.int_arg env ~err: M.err_no_id Constant.var_id
      | Some n -> n
    in
    let c = Contribs.get_or_fail id in
    let content = H.seq
	[
	  form_modify_contrib ?errmes c ;
	  contrib_props ?errmes: errmes_prop c ;
	  contrib_authors ?errmes: errmes_author c ;
	]
    in
    Page.admin_page user_opt ~title: M.modify_contrib ~content ()
  with
    Ocgi.Args.Missing_argument (_, s) 
  | Failure s -> 
      Page.admin_error env user_opt s

let add_contrib user env user_opt =
  try
    try
      let module F = F_contrib in
      let t = F.read env in
      let name = t.F.name in
      let description = Misc.string_of_opt t.F.description in
      let version = t.F.version in
      let url = t.F.url in
      let status = t.F.status in
      let homepage_indexed = t.F.homepage_indexed in
      let date = 
	match t.F.date with
	  None -> None
	| Some s ->
	    let make year mon mday =
	      let t = 
		{
		  Unix.tm_year = year - 1900 ;
		  Unix.tm_mon = mon - 1;
		  Unix.tm_mday = mday ;
		  Unix.tm_wday = 0 ;
		  Unix.tm_hour = 1 ;
		  Unix.tm_min = 0 ;
		  Unix.tm_sec = 0 ;
		  Unix.tm_isdst = false ;
		  Unix.tm_yday = 0;
		} 
	      in
	      fst (Unix.mktime t)
	    in
	    try Some (Scanf.sscanf s "%d-%d-%d" make)
	    with e -> None
      in
      let c = Contribs.create ~name ?status ~version 
	  ~description ~url ?date ~homepage_indexed () 
      in
      page_modify_contrib ~id: c.Db.Contribs.id user env user_opt
    with
      Ocgi.Args.Read_errors l ->
	failwith (Misc.html_of_read_errors l) 
  with
    Failure errmes ->
      form_add_contrib ~errmes user env user_opt

let modify_contrib user env user_opt =
  try
    try
      let module F = F_contrib in
      let t = F.read env in
      match t.F.id with
	None -> failwith M.err_no_id
      |	Some id ->
	  let a = Contribs.get_or_fail id in
	  ignore
	    (Contribs.update 
	       ~name: t.F.name
	       ~description: (Misc.string_of_opt t.F.description)
	       ~version: t.F.version
	       ~status: t.F.status
	       ~url: t.F.url
	       ~homepage_indexed: t.F.homepage_indexed
	       id
	    );
	  let letter = 
	    if String.length t.F.name > 0 then
	      Some (Char.lowercase t.F.name.[0])
	    else
	      None
	  in
	  contribs ?letter user env user_opt
    with
      Ocgi.Args.Read_errors l ->
	failwith (Misc.html_of_read_errors l) 
  with
    Failure s ->
      prerr_endline s;
      page_modify_contrib ~errmes: s user env user_opt

let remove_contrib user env user_opt =
  try
    let id = Ocgi.Args.int_arg env 
        ~err: Messages.err_no_id Constant.var_id
    in
    let contrib = Contribs.get_or_fail id in
    match Ocgi.Args.optional env Constant.var_confirm with
      None ->
        let yes = 
          Printf.sprintf "%s?callback=%s&amp;%s=%d&amp;%s=yes"
            Config.loc_cgi_admin
            Constant.cb_remove_contrib
            Constant.var_id
            id
            Constant.var_confirm
        in
        let no = Printf.sprintf "%s?callback=%s" 
            Config.loc_cgi_admin
            Constant.cb_admin_contribs
        in
        Page.ask_for_confirmation
	  ~page: user_opt
          ~yes ~no
          ~message: (Messages.really_remove_contrib contrib.Db.Contribs.name)
          ()
    | Some _ ->
        Contribs.delete contrib;
        contribs user env user_opt
  with
    Ocgi.Args.Missing_argument (_, s) 
  | Failure s -> 
      Page.admin_error env user_opt s

let set_contrib_date user env user_opt =
  try
    let id = Ocgi.Args.int_arg env 
        ~err: Messages.err_no_id Constant.var_id
    in
    let contrib = Contribs.get_or_fail id in
    let date = Unix.time () in
    ignore (Contribs.update ~date contrib.Db.Contribs.id);
    contribs user env user_opt
  with
    Ocgi.Args.Missing_argument (_, s) 
  | Failure s -> 
      Page.admin_error env user_opt s

let add_contrib_prop user env user_opt =
  try
    try
      let c = Contribs.get_or_fail 
	  (Ocgi.Args.int_arg env ~err: M.err_no_id Constant.var_id)
      in
      let p = Props.get_or_fail 
	  (Ocgi.Args.int_arg env ~err: M.prop_mandatory Constant.var_idprop)
      in
      Contribs.add_prop c p ;
      page_modify_contrib ~id: c.Db.Contribs.id user env user_opt
    with
      Ocgi.Args.Missing_argument (_, errmes_prop) ->
	let id = Ocgi.Args.int_arg env 
            ~err: Messages.err_no_id Constant.var_id
	in
	let contrib = Contribs.get_or_fail id in
	page_modify_contrib ~id ~errmes_prop user env user_opt
  with
  | Failure s
  | Ocgi.Args.Missing_argument (_, s) ->
      Page.admin_error env user_opt s

let remove_contrib_prop user env user_opt =
  try
      let c = Contribs.get_or_fail 
	  (Ocgi.Args.int_arg env ~err: M.err_no_id Constant.var_id)
      in
      let p = Props.get_or_fail 
	  (Ocgi.Args.int_arg env ~err: M.prop_mandatory Constant.var_idprop)
      in
      Contribs.remove_prop c p ;
      page_modify_contrib ~id: c.Db.Contribs.id user env user_opt
  with
  | Failure s
  | Ocgi.Args.Missing_argument (_, s) ->
      Page.admin_error env user_opt s

let add_contrib_author user env user_opt =
  try
    try
      let c = Contribs.get_or_fail 
	  (Ocgi.Args.int_arg env ~err: M.err_no_id Constant.var_id)
      in
      let a = Authors.get_or_fail 
	  (Ocgi.Args.int_arg env ~err: M.author_mandatory Constant.var_idauthor)
      in
      Contribs.add_author c a ;
      page_modify_contrib ~id: c.Db.Contribs.id user env user_opt
    with
      Ocgi.Args.Missing_argument (_, errmes_author) ->
	let id = Ocgi.Args.int_arg env 
            ~err: Messages.err_no_id Constant.var_id
	in
	let contrib = Contribs.get_or_fail id in
	page_modify_contrib ~id ~errmes_author user env user_opt
  with
  | Failure s
  | Ocgi.Args.Missing_argument (_, s) ->
      Page.admin_error env user_opt s

let remove_contrib_author user env user_opt =
  try
      let c = Contribs.get_or_fail 
	  (Ocgi.Args.int_arg env ~err: M.err_no_id Constant.var_id)
      in
      let a = Authors.get_or_fail 
	  (Ocgi.Args.int_arg env ~err: M.author_mandatory Constant.var_idauthor)
      in
      Contribs.remove_author c a ;
      page_modify_contrib ~id: c.Db.Contribs.id user env user_opt
  with
  | Failure s
  | Ocgi.Args.Missing_argument (_, s) ->
      Page.admin_error env user_opt s

(** {2 Main} *)

let _ = Model.main 
    [ 
      (Constant.cb_form_login, Page.form_login ?errmes: None )  ;
      (Constant.cb_create_tables, create_tables )  ;

      (Constant.cb_admin_authors, Model.connected authors )  ;
      (Constant.cb_form_add_author, Model.connected (form_add_author ?errmes: None)) ;
      (Constant.cb_add_author, Model.connected add_author) ;
      (Constant.cb_form_modify_author, Model.connected (form_modify_author ?errmes: None)) ;
      (Constant.cb_modify_author, Model.connected modify_author) ;
      (Constant.cb_remove_author, Model.connected remove_author) ;

      (Constant.cb_admin_properties, Model.connected properties) ;
      (Constant.cb_form_add_property, Model.connected (form_add_property ?errmes: None));
      (Constant.cb_add_property, Model.connected add_property);
      (Constant.cb_form_modify_property, Model.connected (form_modify_property ?errmes: None)) ;
      (Constant.cb_modify_property, Model.connected modify_property) ;
      (Constant.cb_remove_property, Model.connected remove_property) ;

      (Constant.cb_admin_link_kinds, Model.connected link_kinds) ;
      (Constant.cb_form_add_link_kind, Model.connected (form_add_link_kind ?errmes: None));
      (Constant.cb_add_link_kind, Model.connected add_link_kind);
      (Constant.cb_form_modify_link_kind, Model.connected (form_modify_link_kind ?errmes: None)) ;
      (Constant.cb_modify_link_kind, Model.connected modify_link_kind) ;
      (Constant.cb_remove_link_kind, Model.connected remove_link_kind) ;

      (Constant.cb_admin_contribs, Model.connected (contribs ?letter: None) )  ;
      (Constant.cb_form_add_contrib, Model.connected (form_add_contrib ?errmes: None)) ;
      (Constant.cb_add_contrib, Model.connected add_contrib) ;
      (Constant.cb_form_modify_contrib, Model.connected (page_modify_contrib ?errmes: None)) ;
      (Constant.cb_modify_contrib, Model.connected modify_contrib) ;
      (Constant.cb_remove_contrib, Model.connected remove_contrib) ;
      (Constant.cb_set_contrib_date, Model.connected set_contrib_date) ;
      (Constant.cb_add_contrib_prop, Model.connected add_contrib_prop) ;
      (Constant.cb_remove_contrib_prop, Model.connected remove_contrib_prop) ;
      (Constant.cb_add_contrib_author, Model.connected add_contrib_author) ;
      (Constant.cb_remove_contrib_author, Model.connected remove_contrib_author) ;
    ]
    [ (Constant.cb_login, login); 
      (Constant.cb_logout, logout) 
    ]
    (fun e us -> (default e us (), []))

