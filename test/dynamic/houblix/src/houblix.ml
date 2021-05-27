(** The Hopix programming language. *)

let name = "houblix"

type ast = AST.t

let parse lexer_init input =
  SyntacticAnalysis.process
    ~lexer_init
    ~lexer_fun:(fun buf ->
      let tok = Lexer.token buf in
      (* Printf.printf "%s\n" (HopixASTHelper.string_of_token tok) ;*)
      tok )
    ~parser_fun:(fun lexer lexbuf ->
      try Parser.program lexer lexbuf with
      | Parser.Error ->
          Error.error "parsing" (Position.cpos lexbuf) "Syntax error." )
    ~input


let file_content filename =
  let cin = open_in filename in
  let b = Buffer.create 24 in
  let rec read () =
    try
      Buffer.add_channel b cin 1;
      read ()
    with
    | End_of_file ->
        ()
  in
  read ();
  close_in cin;
  Buffer.contents b


let parse_filename ?(from_sexp = false) filename =
  if from_sexp
  then file_content filename |> Sexplib.Sexp.of_string |> AST.program_of_sexp
  else parse (Lexing.from_channel ~with_positions:true) (open_in filename)


let extension = ".hopix"

let executable_format = false

let parse_string = parse Lexing.from_string

let print_ast ?(to_sexp = false) ast =
  if to_sexp
  then AST.sexp_of_program ast |> Sexplib.Sexp.to_string_hum ~indent:2
  else PrettyPrinter.(to_string program ast)


let print_expression e = PrettyPrinter.(to_string expression e)

let () =
  let file = Sys.argv.(2) in
  let to_sexp =
    match Sys.argv.(1) with
    | "--sexp" ->
        true
    | "--pretty" ->
        false
    | s ->
        Printf.eprintf "Unknown option %s.\n" s;
        exit 1
  in
  print_string @@ print_ast ~to_sexp (parse_filename file);
  if to_sexp then print_newline ()
