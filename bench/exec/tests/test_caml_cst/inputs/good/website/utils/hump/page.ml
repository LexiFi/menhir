
module H = Ocgi.Html

type user = Db.Users.t

let admin_menu_links =
  (List.map
    (fun (label,t,opt_t_add,t_add_list) ->
      Printf.sprintf "<li><a href=\"%s?callback=%s\">%s</a> [%s%a]</li>"
        Config.loc_cgi_admin t label 
        (match opt_t_add with
          None -> "Add"
        | Some t_add ->
            Printf.sprintf "<a href=\"%s?callback=%s\">Add</a>"
              Config.loc_cgi_admin t_add
        )
        (fun () l -> String.concat "\n" (List.map (function label, t ->
          Printf.sprintf " <a href=\"%s?callback=%s\">%s</a>"
            Config.loc_cgi_admin t label ) l)
        ) t_add_list
    )
    [ Messages.authors, 
      Constant.cb_admin_authors, 
      Some Constant.cb_form_add_author,
      [];

      Messages.contribs, 
      Constant.cb_admin_contribs, 
      Some Constant.cb_form_add_contrib,
      [];

      Messages.properties,
      Constant.cb_admin_properties,
      None,
      List.map (fun kind -> 
        Messages.of_prop_kind kind, 
        Printf.sprintf "%s&amp;%s=%d" 
          Constant.cb_form_add_property 
          Constant.var_kind
          (Conv.int_of_prop_kind kind)
        ) Types.prop_kinds
    ]
  )


(*
  List.map
    (fun (s,t) ->
      (Printf.sprintf "%s?callback=%s" Config.loc_cgi_admin s, t)
    )
    (
     [
       Constant.cb_admin_authors, Messages.authors ;
       Constant.cb_form_add_author, Messages.add_author ;
       Constant.cb_admin_contribs, Messages.contribs ;
       Constant.cb_form_add_contrib, Messages.add_contrib ;
       Constant.cb_admin_link_kinds, Messages.link_kinds ;
       Constant.cb_form_add_link_kind, Messages.add_link_kind ;
       Constant.cb_admin_properties, Messages.properties ;
     ] @
     (List.map
	(fun kind ->
	  Printf.sprintf "%s&amp;%s=%d" 
	    Constant.cb_form_add_property
	    Constant.var_kind
	    (Conv.int_of_prop_kind kind), 
	  Messages.add_property kind
	)
	Types.prop_kinds
     )
    )
*)
let admin_menu = Printf.sprintf "\
<navigation>
<panel>
<caption><en>Menu</en></caption>
<content><al>
%s
</al></content>
</panel></navigation>"
    (String.concat "\n" admin_menu_links)

let admin_page user_opt ?errmes ~title ~content () =
  let (connected, conn_link) = 
    match user_opt with
      None -> 
	(Messages.not_connected,
	 H.link
	   (Printf.sprintf "%s?callback=%s" Config.loc_cgi_admin Constant.cb_form_login) 
	   Messages.connection
	)

    | Some u -> 
	(Messages.connected_as u.Db.Users.login,
	 H.link
	   (Printf.sprintf "%s?callback=%s" Config.loc_cgi_admin Constant.cb_logout) 
	   Messages.disconnection
	)
  in
  let date = Messages.string_of_date 
      ~wday: true ~hours: true ~secs: true (Unix.time()) 
  in
  let conn_info = 
    Printf.sprintf "<connection_info>%s - %s - %s</connection_info>" 
      connected date 
      conn_link
  in
  P_page.string 
    ?errmes
    ~conn_info
    ~authors: (Printf.sprintf
		 "<author name=\"Maxence Guesdon\" url=\"http://maxence-g.net/\"/>")
    ~title
    ~menu: admin_menu
    ~content ()

let form_login ?errmes env user_opt =
  admin_page user_opt 
    ~title: Messages.login_title
    ~content: (F_login.string ?errmes ()) ()

let page user_opt ?errmes ?(alternate_rss=false) ~title ?menu ~content () =
  let alternate_rss =
    if alternate_rss then
      Some "alternate-rss-fr=\"&Caml;hump.rss\" alternate-rss-en=\"&Caml;hump.rss\""
    else
      None
  in
  P_page.string 
    ?errmes
    ?alternate_rss
    ~authors: (Printf.sprintf
		 "<author name=\"Maxence Guesdon\" url=\"http://maxence-g.net/\"/>")
    ~title ?menu ~content ()

let error_message s =
  Ocgi.Html.div ~cl: Messages.error s
    
let admin_error env user_opt s =
  let content = 
    F_login.string ~errmes: s ()
  in
  admin_page user_opt ~title: Messages.error ~content ()

let error user_opt s =
  page user_opt ~title: Messages.error ~content: (error_message s) ()

let admin_welcome user_opt ?errmes () = (* TODO *)
  admin_page ?errmes user_opt ~title: Messages.welcome 
    ~content: "" ()

let welcome user_opt ?errmes () = (* TODO *)
  page ?errmes user_opt ~title: Messages.welcome ~content: "" ()

(* misc *)

let ask_for_confirmation =
  let f_page = admin_page in
  fun ?page ~yes ~no ~message () ->
  let module H = Ocgi.Html in
  let content =
    H.seq
      [ 
	message ;
	H.br ;
	H.br ;
	H.span ~cl: "button" (H.link yes Messages.yes) ;
	H.span ~cl: "button" (H.link no Messages.no) ;
      ] 
  in
  match page with
    None -> content
  | Some user_opt -> f_page user_opt ~title: Messages.confirmation ~content ()

let make_string_edit =
  let f_page = admin_page in
  fun ?page ?(errmes="") ?size ~action ~callback 
      params ~cancel ~label ~name ~value ~message () ->
    let module H = Ocgi.Html in
    let content =
      let hidden = 
	List.map 
	  (fun (name,value) -> H.input H.Hidden ~value name)
	  (("callback", callback) :: params)
      in
      H.form action
	(
	 H.seq
	   [ 	  
	     message ;
	     H.br ;
	     H.div ~cl: "error" errmes ;
	     H.br ;
	     H.seq hidden ;
	     Misc.field label ;
	     H.input H.Text ?size ~value name ;
	     H.submit_button Messages.update ;
	     H.span ~cl: "button" (H.link cancel Messages.cancel) ;
	   ] 
	)
    in
    match page with
      None -> content
    | Some user_opt -> f_page user_opt ~title: "" ~content ()

