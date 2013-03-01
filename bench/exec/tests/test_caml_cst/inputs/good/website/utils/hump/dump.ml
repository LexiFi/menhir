let _ = Config.dbg "Entering module dump.ml"

(** Main module of the hump_dump utility. *)

(** {2 Swish-e dump} *)

let p = Printf.bprintf

let print_contrib c =
  let (@) = List.rev_append in
  let tm = Unix.gmtime c.Db.Contribs.date in
  let date =
    Printf.sprintf "%04i-%02i-%02i"
      (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
  in
  [
    "title", c.Db.Contribs.name;
    "desc", c.Db.Contribs.description;
    "detail", Contribs.detail_url c ;
    "url", c.Db.Contribs.url;
    "date", date;
  ] 
  @ begin match c.Db.Contribs.version with
     None -> []
   | Some v -> ["version", v]
  end 
  @ begin match c.Db.Contribs.status with
     None -> []
   | Some st -> ["status", Messages.of_status st]
  end 
  @ begin List.map (function auth ->
    ("author", Authors.string_name auth)
  ) (Contribs.authors c) end 
  @ begin List.map (function prop ->
    String.lowercase (Messages.of_prop_kind prop.Db.Props.kind),
    prop.Db.Props.name
  ) (Contribs.props c) end

let dump_swishe () =
  let contribs = Contribs.list () in
  List.iter (function c ->
    Swishe_index.output stdout (string_of_int c.Db.Contribs.id)
      (Swishe_index.Fields (print_contrib c))
  ) contribs;
  flush stdout



(** {2 Rss dump} *)

let dump_rss ?(raw=false) () =
  let contribs = Contribs.sort_by_date (Contribs.list ()) in
  let l = fst (Misc.get_n_first_ele Constant.latest_updates_number contribs) in
  let item_of_contrib c =
    let date =
      let tm = Unix.gmtime c.Db.Contribs.date in
      { Rss.year = tm.Unix.tm_year + 1900;
	Rss.month = tm.Unix.tm_mon + 1;
	Rss.day = tm.Unix.tm_mday ;
	Rss.hour = tm.Unix.tm_hour ;
	Rss.minute = tm.Unix.tm_min ;
	Rss.second = tm.Unix.tm_sec ;
	Rss.zone = 0 ;                (** in minutes; 60 = UTC+0100 *)
	Rss.week_day = tm.Unix.tm_wday ;
      } 
    in
    Rss.item 
      ~title: (c.Db.Contribs.name^
	       (match c.Db.Contribs.version with None -> "" | Some s -> " "^s))
      ~link: (if raw then Contribs.raw_detail_url c else Contribs.detail_url c)
      ~pubdate: date
      ()
  in
  let items = List.map item_of_contrib l in
  let date =
    let tm = Unix.gmtime (Unix.time()) in
    { Rss.year = tm.Unix.tm_year + 1900;
      Rss.month = tm.Unix.tm_mon + 1;
      Rss.day = tm.Unix.tm_mday ;
      Rss.hour = tm.Unix.tm_hour ;
      Rss.minute = tm.Unix.tm_min ;
      Rss.second = tm.Unix.tm_sec ;
      Rss.zone = 0 ;                (** in minutes; 60 = UTC+0100 *)
      Rss.week_day = tm.Unix.tm_wday ;
    } 
  in
  let date_fmt = if raw then None else Some "%Y-%m-%dT%H:%M:%S" in
  let channel = Rss.channel
      ~title: "The Caml Humps"
      ~link: (if raw then Config.raw_loc_cgi_hump else Config.loc_cgi_hump)
      ~desc: "Releases/updates in the Humps" 
      ~managing_editor: "hump@caml.inria.fr"
      ~webmaster: "hump@caml.inria.fr"
      ~pubdate: date
      ~last_build_date: date
      ~ttl: (24 * 60)
      ~image: { Rss.image_url = Config.loc_www^"pub/logos/caml-inria-fr.128x58.gif" ;
                Rss.image_title = "The Caml Humps" ;
                Rss.image_link =  (if raw then Config.raw_loc_cgi_hump else Config.loc_cgi_hump);
                Rss.image_height = None;
                Rss.image_width = None ;
                Rss.image_desc = None ;
              } 
(*
      ~text_input: { Rss.ti_title = "Search";
                     Rss.ti_desc = "Search the Caml humps";
                     Rss.ti_name = "words";
                     Rss.ti_link = "http://camlcvs.inria.fr/cgi-bin/osearch-humps";
                   } 
*)
      ~language: "en"
      items
   in
   let fmt = Format.formatter_of_out_channel stdout in
   Rss.print_channel ?date_fmt fmt channel ;
   Format.pp_print_flush fmt ()


(** {2 Main} *)

type dump = Swishe | Rdf | Rss of bool (* raw rss or not *)

let dump = ref None

let options = 
  [
    "-swi", Arg.Unit (fun () -> dump := Some Swishe), " dump swish-e index on stdout";
    "-rdf", Arg.Unit (fun () -> dump := Some Rdf), " dump RDF (for mozcaml bar) on stdout";
    "-rss", Arg.Unit (fun () -> dump := Some (Rss false)), " dump rss feed on stdout";
    "-raw-rss", Arg.Unit (fun () -> dump := Some (Rss true)), " dump raw rss feed on stdout";
  ] 
    
let main () =
  List.iter Args.add_option options;

  Args.parse ();
  match !dump with 
    None -> ()
  | Some Swishe -> dump_swishe ()
  | Some Rdf -> Hump_rdf.generate stdout
  | Some (Rss raw) -> dump_rss ~raw ()

let _ = main ()
    
