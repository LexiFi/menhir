open Printf

let header f =
  fprintf
    f
    ";; This file has been auto-generated. Please do not edit it.\n\
     ;; Instead, edit [dune.ml] and run [ocaml dune.ml].\n\n"


let ls dir = Array.to_list (Sys.readdir dir)

let filter_suffix files suffix =
  List.filter (fun f -> Filename.check_suffix f suffix) files


let remove_filter_suffix files suffix =
  List.map Filename.remove_extension @@ filter_suffix files suffix


let backends = remove_filter_suffix (ls "backends") ".flags"

type sentence =
  { name : string
  ; seed : int option
  ; size : int
  }

let small_sentences =
  List.init 10 (fun i ->
      { name = sprintf "sentence_1000_%d" i; seed = Some i; size = 1000 } )


let big_sentences =
  List.init 2 (fun i ->
      { name = sprintf "sentence_100000_%d" i; seed = Some i; size = 100000 } )


let huge_sentences =
  List.init 1 (fun i ->
      { name = sprintf "sentence_1000000_%d" i; seed = Some i; size = 1000000 } )


let sentences = small_sentences @ big_sentences @ huge_sentences

let rule_sentence file { name; seed; size } =
  let init =
    match seed with
    | None ->
        "--random-self-init"
    | Some i ->
        sprintf "--random-seed %d" i
  in
  fprintf
    file
    {|
(rule
 (deps
  (:parser ../src/parser.mly))
 (target %s.tokens)
 (action
  (with-stdout-to
   %%{target}
   (run
    menhir
    %%{parser}
    --random-sentence main
    %s
    --random-sentence-length %d
   ))))
|}
    name
    init
    size


let rule_global file backends sentences =
  let sentences =
    String.concat
      ""
      (List.map
         (fun { name } -> sprintf "sentences/%s.tokens\n" name)
         sentences )
  in
  let backends =
    String.concat
      ""
      (List.map
         (fun name -> sprintf "backends/%s.backend/main.exe\n" name)
         backends )
  in
  fprintf
    file
    {|
(executable
 (name speed)
 (libraries unix))

(rule
 (alias benchmark)
 (deps
  ; backend/*.backend/main.exe
  %s
  ; sentences/*.tokens
  %s
  speed.sh
  speed.exe)
 (action
  (run bash speed.sh)))
|}
    backends
    sentences


let template_dune_file = open_out "dune"

let sentences_dune_file = open_out "sentences/dune"

let () =
  header sentences_dune_file;
  header template_dune_file


let () = List.iter (rule_sentence sentences_dune_file) sentences

let () = rule_global template_dune_file backends sentences
