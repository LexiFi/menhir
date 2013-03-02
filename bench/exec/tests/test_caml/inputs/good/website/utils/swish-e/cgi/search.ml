(**********************************************************************)
(*                                                                    *)
(*                        The Caml Web Site                           *)
(*                                                                    *)
(*        Vincent Simonet, projet Cristal, INRIA Rocquencourt         *)
(*                                                                    *)
(*   Copyright 2004 Institut National de Recherche en Informatique    *)
(*   et en Automatique.  All rights reserved.                         *)
(*                                                                    *)
(**********************************************************************)



open Data


(*************************************************************************)
(** Miscellaneous functions *)

let convert_option x = function
    None -> x
  | Some y -> y

let convert_int x = function
    None -> x
  | Some y ->
      try int_of_string y
      with _ -> x

let map_sublist f pos len list =
  let rec keep len list =
    match len, list with 
      0, _ | _, [] -> []
    | n, hd :: tl -> (f hd) :: (keep (n - 1) tl)
  in
  let rec skip pos list =
    match pos, list with
      0, _ | _, [] -> list
    | n, hd :: tl -> skip (n - 1) tl
  in
  keep len (skip pos list)
    


(*************************************************************************)
(** Input *)

type mode =
    Default
  | Sidebar

let string_of_mode = function
    Default -> "default"
  | Sidebar -> "sidebar"

let convert_mode d = function
    Some "sidebar" -> Sidebar
  | Some "default" -> Default
  | _ -> d

type query = {
    words: string;
    corpus: corpus;
    mode: mode;
    mutable page: int;
    number: int;
  }

let default_query = {
  words = "";
  corpus = default_corpus;
  mode = Default;
  page = 1;
  number = 10
}


let read_query env =
  let r = P_results.read env in
  { words = convert_option default_query.words r.P_results.words;
    corpus = 
    begin 
      try
        begin match (List.assoc "corpus" env.Ocgi.Env.arguments)
            .Ocgi.Env.arg_value
        with
          None -> raise Not_found
        | Some s -> Hashtbl.find corpus_by_name s
        end
      with
        Not_found -> default_corpus
    end;
    mode = convert_mode default_query.mode r.P_results.mode;
    page = convert_int default_query.page r.P_results.page;
    number = convert_int default_query.number r.P_results.number
  }



(*************************************************************************)
(** Generic output functions *)

let make_url q =
  Printf.sprintf
    "search${cgi}?words=%s&amp;corpus=%s&amp;page=%s&amp;number=%s%s"
    (Ocgi.Encoding.encode_string q.words)
    (Ocgi.Encoding.encode_string q.corpus.corpus_name)
    (Ocgi.Encoding.encode_string (string_of_int q.page))
    (Ocgi.Encoding.encode_string (string_of_int q.number))
    begin
      if q.mode = Default then ""
      else Printf.sprintf "&amp;mode=%s" (string_of_mode q.mode)
    end



let make_page_numbers query hits =
  if hits = 0 then
    ""
  else begin
    let count = 
      hits / query.number + (if hits mod query.number = 0 then 0 else 1)
    in
    if query.page <= 0 || query.page > count then query.page <- 1;
    
    let buf = Buffer.create 7 in
    Buffer.add_string buf "[page ";
    let lb = (max 1 (query.page - 9)) in
    for i = lb to (min count (query.page + 9)) do
      if i > lb then Buffer.add_string buf " - ";
      Buffer.add_string buf begin
        if i = query.page then
          Printf.sprintf "<b>%i</b>" i
        else
          Printf.sprintf "<a href=\"%s\">%i</a>"
            (make_url {query with page = i})
            i
      end
    done;
    Buffer.add_string buf "]";
    Buffer.contents buf
  end



let make_excerpt query stg =
  let buf = Buffer.create 7 in
  Swishe_excerpt.map_excerpt 256 query 
    (fun s -> Buffer.add_string buf (Ocgi.Misc.escape_entities s))
    (fun s ->
      Buffer.add_string buf "<b>";
      Buffer.add_string buf (Ocgi.Misc.escape_entities s);
      Buffer.add_string buf "</b>")
    stg;
  Buffer.contents buf



(*************************************************************************)

let make_result answer result =

  let rank = 
    try int_of_string (Swishe_api.property result "swishrank")
    with _ -> -1
  in 
  let description = Swishe_api.property result "swishdescription"
  and index_file = Swishe_api.property result "swishdbfile"
  in

  let corpus =
    try Hashtbl.find index_by_file (Filename.basename index_file)
    with Not_found -> unknown_index
  in

  D_result.string
    ~rank:(string_of_int (rank/10)) 
    ~href:(Ocgi.Misc.escape_quotes (corpus.get_href result))
    ~title:(Ocgi.Misc.escape_entities (corpus.get_title result))
    ~desc:(make_excerpt answer.Swishe_api.parsed_words description)
    ~ref:(corpus.get_refs result)
    ()



let make_corpus_list query =
  String.concat "" begin
    List.map (function corpus ->
      Printf.sprintf 
        "<option value=\"%s\" class=\"search-%i\"%s>%s</option>"
        corpus.corpus_name
        corpus.level 
        (if query.corpus == corpus then " selected=\"selected\"" else "")
        (i10n_to_string corpus.corpus_title)
    ) Data.corpus_list
  end

let http_answer query head tail results =
  let page =
    P_results.string
      ~words:query.words
      ~corpus:(make_corpus_list query) (* TEMPORARY *)
      ~sidebar:(if query.mode = Sidebar then "yes" else "no")
      ~page:"1"
      ~mode:(string_of_mode query.mode)
      ~number:(string_of_int query.number)
      ~head ~tail ~results:(String.concat "\n" results) ()
  in

  Ocgi.Hooks.content_type := "text/pre-xhtml";
  print_string (Ocgi.Model.html_answer (page, []));
  exit 0



let google_redirect query =
  let url =
     "http://www.google.com/custom?sa=Google+Search&cof=GALT%3A%23CC6666%3BGL%3A0%3BVLC%3A%236666FF%3BAH%3Acenter%3BBGC%3Awhite%3BLH%3A60%3BLC%3A%236666FF%3BL%3Ahttp%3A%2F%2Frivesaltes.inria.fr%2Fpub%2Flogos%2Fthe-caml-language.468x60.gif%3BALC%3A%236666FF%3BLW%3A468%3BT%3Ablack%3BGIMP%3A%23CC6666%3BAWFID%3Ae31c4f7be5771b9d%3B&domains=caml.inria.fr&sitesearch=caml.inria.fr&q=" ^ 
      (Ocgi.Encoding.encode_string query.words)
  in
  Printf.printf "Content-type: text/plain\r\n";
  Printf.printf "Status: 303 See Other\r\n";
  Printf.printf "Location: %s\r\n" url;
  Printf.printf "\r\n";
  exit 0


(*************************************************************************)
(** Main *)

exception Error of string

let error exn query =
  let msg =
    match exn with
      Swishe_api.Error (code, s) ->
        Printf.sprintf "%s %s" (Swishe_api.string_of_error code) s
    | _ ->
        "<en>Uncaught exception:</en><fr>Exception non rattrapée\160:</fr> "
        ^ (Printexc.to_string exn)
  in

  http_answer query begin
    Printf.sprintf
      "<en>An error occurred while serving your request.</en>\
      <fr>Une erreur est survenue lors du traitement de votre requête.</fr>\
      <br/>%s"
      (Ocgi.Misc.escape_entities msg)
  end
    ""
    []



let main () =

  let env = Ocgi.Env.get_cgi_env () in
  let query = read_query env in

  try

    if query.corpus.corpus_name = "google" then
      google_redirect query;

    if query.words <> "" then begin
      (* Interrogation Swish-e *)
      let handle = Swishe_api.init (List.map (function index ->
        Filename.concat Config.index_dir index.index_file
        ) query.corpus.indexes)
      in

      let query_string =
        query.words ^ query.corpus.query
      in

      let answer = Swishe_api.query handle query_string in

      (* Numérotation des pages *)
      let pages = make_page_numbers query answer.Swishe_api.hits in

      let info =
        D_info_results.string ~words:query.words
          ~corpus:(i10n_to_string query.corpus.corpus_title)
          ~results_count:(string_of_int answer.Swishe_api.hits)
          ~results_plural:(if answer.Swishe_api.hits > 1 then "s" else "")
          ~pages ()
      in

      let results = 
        map_sublist (make_result answer)
          ((query.page - 1) * query.number)  
          query.number
          answer.Swishe_api.results
      in

      http_answer query info pages results
    end;

    http_answer query "" "" []

  with
    exn -> error exn query



let () = 
  try
    main ()
  with
    exn -> error exn default_query
