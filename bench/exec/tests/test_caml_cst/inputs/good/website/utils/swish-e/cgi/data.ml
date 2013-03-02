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


open Printf



(*************************************************************************)
(** Internationalization *)

type i10n =
    { en: string;
      fr: string
    }

let i10n_to_string r =
  sprintf "<en>%s</en><fr>%s</fr>" 
    (Ocgi.Misc.escape_entities r.en)
    (Ocgi.Misc.escape_entities r.fr)



(*************************************************************************)
(** Indexes *)

type index =
    { index_name: string;
      index_file: string;
      title: i10n;
      get_title: (Swishe_api.result -> string);
      get_href: (Swishe_api.result -> string);
      get_refs: (Swishe_api.result -> string);
   }

let index_by_name = Hashtbl.create 7
let index_by_file = Hashtbl.create 7

let reg_index c = 
  Hashtbl.add index_by_name c.index_name c;
  Hashtbl.add index_by_file c.index_file c


let site_index =
  { index_name = "site";
    index_file = "site.index";
    title = { en = "the Caml Site"; fr = "le site Caml" };
    get_href = (fun r -> "~/" ^ Swishe_api.property r "swishdocpath");
    get_title = (fun r -> Swishe_api.property r "swishtitle");
    get_refs = begin fun r ->
      sprintf "%s - %ik"
        begin match Swishe_api.property r "booktitle" with
          "" -> "<fr>Site Web Caml</fr><en>Caml Web Site</en>"
        | s -> Ocgi.Misc.escape_entities s
        end
        ((Swishe_api.property_int r "swishdocsize" + 512) / 1024)
    end
  }

let ml_archives_index =
  { index_name = "ml_archives";
    index_file = "ml-archives.index";
    title = { en = "Mailing Lists"; fr = "Listes de diffusion" };
    get_href = (fun r -> "~/" ^ (Swishe_api.property r "swishdocpath") ^ "$");
    get_title = (fun r -> Swishe_api.property r "swishtitle");
    get_refs = begin fun r ->
      let author = Swishe_api.property r "message.author"
      and date_time = Swishe_api.property r "message.date"
      and booktitle = Swishe_api.property r "booktitle"
      in
      let date =
        try
          String.sub date_time 0 (String.index date_time 'T')
        with
          Not_found -> date_time
      in
      sprintf "Message <en>from</en><fr>de</fr> %s - %s - %s"
        (Ocgi.Misc.escape_entities author)
        (Ocgi.Misc.escape_entities date)
        (Ocgi.Misc.escape_entities booktitle)
    end
  }

let hump_index =
  { index_name = "hump";
    index_file = "hump.index";
    title = { en = "The Caml Hump"; fr = "La Bosse Caml" };
    get_href = (fun r -> Config.hump_cgi ^ Swishe_api.property r "swishdocpath");
    get_title = (fun r -> Swishe_api.property r "swishtitle");
    get_refs = begin fun r ->
      let author = Swishe_api.property r "author"
      and date = Swishe_api.property r "date"
      and status = Swishe_api.property r "status"
      and version = Swishe_api.property r "version"
      in
      sprintf "<en>The Caml Hump</en><fr>La Bosse Caml</fr> - %s - [%s%s%s]"
        (Ocgi.Misc.escape_entities author)
        (Ocgi.Misc.escape_entities date)
        (if version = "" then "" else sprintf ", version %s" 
          (Ocgi.Misc.escape_entities version))
        (if status = "" then "" else sprintf ", %s" 
          (Ocgi.Misc.escape_entities status))
    end
  }


let library_ocaml_index =
  { index_name = "library-ocaml";
    index_file = "library-ocaml.index";
    title = { en = "Objective Caml library"; fr = "Librairie d'Objective Caml" };
    get_href = (fun r -> 
      Config.ocaml_library_url ^ Swishe_api.property r "swishdocpath");
    get_title = (fun r -> Swishe_api.property r "swishtitle");
    get_refs = begin fun r ->
      let t = 
        match Swishe_api.property r "type" with
          "" -> ""
        | s -> sprintf " %s: %s" (Swishe_api.property r "ident") s
      in
      sprintf 
        "<en>Objective Caml Library</en><fr>Librairie d'Objective Caml</fr> | \
        <tt>%s%s</tt>" (Swishe_api.property r "kind") t
    end
  }

let unknown_index =
    { index_name = "unknown";
      index_file = "unknown";
      title = { en = "unknown"; fr = "unknown" };
      get_href = (fun r -> "");
      get_title = (fun r -> "");
      get_refs = (fun r -> "");
    }

let _ = 
  reg_index site_index;
  reg_index ml_archives_index;
  reg_index library_ocaml_index;
  reg_index hump_index



(*************************************************************************)
(** Corpus *)

type corpus =
    { corpus_name: string;
      corpus_title: i10n;
      level: int;
      indexes: index list;
      query: string
    }

let path s =
  sprintf " AND (swishdocpath = (%s))" s


(** Lorsque l'on change cette liste, il faut penser à mettre à jour la
    sidebar mozilla ! *)
let corpus_list =
  [ 
    { corpus_name = "all";
      corpus_title = { en = "The entire web site";
                       fr = "Tout le site web" };
      level = 1;
      indexes = [site_index; hump_index; ml_archives_index];
      query = ""
    };
    { corpus_name = "hump";
      corpus_title = { en = "The Caml Hump";
                       fr = "La Bosse Caml" };
      level = 1;
      indexes = [hump_index];
      query = ""
    };
    { corpus_name = "ml-archives";
      corpus_title = { en = "Mailing lists archives";
                       fr = "Archives des listes de diffusion" };
      level = 1;
      indexes = [ml_archives_index];
      query = ""
    };
    { corpus_name = "caml-list";
      corpus_title = { en = "caml-list";
                       fr = "caml-list" };
      level = 2;
      indexes = [ml_archives_index];
      query = " AND (listname = caml-list)"
    };
    { corpus_name = "ocaml-beginners";
      corpus_title = { en = "ocaml-beginners";
                       fr = "ocaml-beginners" };
      level = 2;
      indexes = [ml_archives_index];
      query = " AND (listname = ocaml-beginners)"
    };
    { corpus_name = "doc";
      corpus_title = { en = "Documentation";
                       fr = "Documentation" };
      level = 1;
      indexes = [site_index];
      query = path "pub/docs/"
    };
    { corpus_name = "manual-ocaml";
      corpus_title = { en = "Objective Caml manual";
                       fr = "Manuel d'Objective Caml" };
      level = 2;
      indexes = [site_index];
      query = path "pub/docs/manual-ocaml"
    };
    { corpus_name = "manual-caml-light";
      corpus_title = { en = "Caml Light manual";
                       fr = "Manuel de Caml Light" };
      level = 2;
      indexes = [site_index];
      query = path "pub/docs/manual-caml-light"
    };
    { corpus_name = "oreilly-book";
      corpus_title = { en = "Developing Applications with OCaml";
                       fr = "Développement d'Applications avec OCaml" };
      level = 2;
      indexes = [site_index];
      query = path "pub/docs/oreilly-book"
    };
    { corpus_name = "u3-ocaml";
      corpus_title = { en = "Using, Understanding, and Unraveling...";
                       fr = "Using, Understanding, and Unraveling..." };
      level = 2;
      indexes = [site_index];
      query = path "pub/docs/u3-ocaml"
    };
    { corpus_name = "library-ocaml";
      corpus_title = { en = "Objective Caml Library";
                       fr = "Librairie d'Objective Caml" };
      level = 1;
      indexes = [library_ocaml_index];
      query = ""
    };
    { corpus_name = "google";
      corpus_title = { en = "Google index for caml.inria.fr/";
                       fr = "L'index Google pour caml.inria.fr/" };
      level = 1;
      indexes = [];
      query = ""
    }
  ]

let default_corpus = List.hd corpus_list

let corpus_by_name = Hashtbl.create 7
let _ =
  List.iter (function corpus ->
    Hashtbl.add corpus_by_name corpus.corpus_name corpus
  ) corpus_list
