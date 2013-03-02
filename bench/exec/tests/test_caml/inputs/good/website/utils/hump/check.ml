let _ = Config.dbg "Entering module dump.ml"

(** Main module of the hump-check utility. *)

let url_timeout = 60
let errors_before_mail = 3

let mail_url_error c url =
  let module M = Ocgi.Mail in
  let mail = {
    M.mail_to = [Config.admin_mail] ;
    M.mail_cc = [] ;
    M.mail_from = Config.admin_mail ;
    M.mail_subject = 
      Printf.sprintf "[Caml Hump]Url error";
    M.mail_body = 
      Printf.sprintf
        "Contrib %s contains the following url not responding:\n %s"
        c.Db.Contribs.name
        url;
  }
  in
  M.sendmail Config.smtp_server Config.smtp_port mail

let incr_contrib_url_error_number c url =
  let db = Args.db () in
  match Db.Contrib_url_check.select db
      ~idcontrib: c.Db.Contribs.id ~url ()
  with
    [] ->
      Db.Contrib_url_check.insert db
	~idcontrib: c.Db.Contribs.id ~url ~cpt: 1 ();
      1
  | t :: _ ->
      let cpt = t.Db.Contrib_url_check.cpt + 1 in 
      Db.Contrib_url_check.update db
	~key_idcontrib: c.Db.Contribs.id ~key_url: url 
	~cpt ();
      cpt

let test_url c url =
  let com = Printf.sprintf "wget --spider -T %d %s 2> /dev/null" 
      url_timeout
      (Filename.quote url)
  in
  match Sys.command com with
    0 -> ()
  | _ -> 
      let nb_errors = incr_contrib_url_error_number c url in
      if nb_errors mod errors_before_mail = 0 then
	mail_url_error c url	
      else
	()

let urls_of_contrib c =
  let db = Args.db () in
  c.Db.Contribs.url ::
  (List.map
     (fun cl -> cl.Db.Contrib_link.url)
     (Db.Contrib_link.select db ~idcontrib: c.Db.Contribs.id ()))  

let check_contrib c =
  let urls = urls_of_contrib c in
  List.iter (test_url c) urls

let check_contribs () =
  let contribs = Contribs.list () in
  List.iter check_contrib contribs

(** {2 Main} *)

let options = 
  []
    
let main () =
  List.iter Args.add_option options;

  Args.parse ();
  (
   try Db.Contrib_url_check.create (Args.db())
   with _ -> ()
  );
  check_contribs ()

let _ = main ()
    
