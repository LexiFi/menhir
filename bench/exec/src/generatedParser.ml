open Util
open GeneratorsDB

module Generated = struct
  type status =
    | NotYet
    | GeneratedNotCompiled
    | GenerationSkipped
    | CompilationSkipped
    | GenerationError
    | CompilationError
    | OK
  
  type t =
    {
      directory : string;
      basename : string;
      libraries : string list;
      additionalfiles : string list;
      generator : GeneratorsDB.Generator.t;
      parser : ParsersDB.MlyParser.t;
      istrace : bool;
      mutable lexer : string;
      mutable status : status;
      mutable ml_size : int;
      mutable cmo_size : int;
      mutable o_size : int;
      mutable generation_time : float;
      mutable compilation_time : float;
    }
  
  (****************************** ACCESSORS ********************************)
  let get_directory p =
    p.directory
  
  let get_libraries p opt =
    (List.map (lib2obj opt) p.libraries)
  
  let get_basename p =
    p.basename
  
  let get_ml_filename p =
    p.basename ^ ".ml"
  
  let get_mli_filename p =
    p.basename ^ ".mli"
  
  let get_cmo_filename p =
    p.basename ^ ".cmo"
  
  let get_o_filename p =
    p.basename ^ ".o"
  
  let get_lexer_mll p =
    p.lexer
  
  let get_lexer_basename p =
    (Filename.chop_extension p.lexer)
  
  let get_lexer_ml p =
    (get_lexer_basename p) ^ (".ml")
  
  let get_absolute_basename p =
    Filename.concat p.directory p.basename
  
  let get_absolute_ml_filename p =
    (get_absolute_basename p) ^ ".ml"
  
  let get_absolute_mli_filename p =
    (get_absolute_basename p) ^ ".mli"
  
  let get_absolute_cmo_filename p =
    (get_absolute_basename p) ^ ".cmo"
  
  let get_absolute_o_filename p =
    (get_absolute_basename p) ^ ".o"
  
  let get_parser_directory p =
    (ParsersDB.MlyParser.get_directory p.parser)
  
  let get_lexer_name p =
    p.lexer
  
  let get_linked_executable opt p =
    execsuffix opt (get_basename p)
  
  let get_lexer_ml_name p =
    (Filename.chop_extension p.lexer) ^ ".ml"
  
  let get_ml_size p =
    p.ml_size
  
  let get_cmo_size p =
    p.cmo_size
  
  let get_o_size p =
    p.o_size
  
  let get_additionals p =
    p.additionalfiles
  
  let get_generation_time p =
    p.generation_time
  
  let get_compilation_time p =
    p.compilation_time
  
  let get_generator p =
    p.generator
  
  let get_parser p =
    p.parser
  
  let get_istrace p =
    p.istrace
  
  let get_sources_for_compile p =
    (get_additionals p)
    @ [(get_ml_filename p)]
    @ [(get_lexer_ml p)]
  ;;
  
  let get_sources_for_link p opt =
    List.map (Util.src2obj opt)
      (
        (List.filter (fun ml -> Filename.check_suffix ml ".ml")
            (get_additionals p)
        )
        @ [(get_ml_filename p)]
        @ [(get_lexer_ml p)]
      )
  ;;
  
  (********************************** STORAGE ********************************)
  let trace_generated_parsers = Hashtbl.create 10
  
  let notrace_generated_parsers = Hashtbl.create 10
  
  let get_generated_parsers_trace generator parser =
    Hashtbl.find trace_generated_parsers (generator, parser)
  ;;
  
  let get_generated_parsers_notrace generator parser =
    Hashtbl.find notrace_generated_parsers (generator, parser)
  ;;
  
  let add_generated_parser_trace generator parser generated =
    assert (not (Hashtbl.mem trace_generated_parsers (generator, parser)));
    Hashtbl.add trace_generated_parsers (generator, parser) generated
  ;;
  
  let add_generated_parser_notrace generator parser generated =
    assert (not (Hashtbl.mem notrace_generated_parsers (generator, parser)));
    Hashtbl.add notrace_generated_parsers (generator, parser) generated
  ;;
  
  (********************************* ITERATIONS ******************************)
  let iter_trace f =
    GeneratorsList.iter (
        fun generator ->
            ParsersDB.iter (
                fun parser ->
                    f (get_generated_parsers_trace generator parser)
              )
      )
  
  let iter_notrace f =
    GeneratorsList.iter (
        fun generator ->
            ParsersDB.iter (
                fun parser ->
                    f (get_generated_parsers_notrace generator parser)
              )
      )
  
  let iter_all f =
    iter_trace f;
    iter_notrace f
  
  (* ****************************** CREATORS *********************************)
  
  let make generator parser trace =
    let result =
      {
        directory = Filename.concat (ParsersDB.MlyParser.get_directory parser)
            Settings.generated_dir;
        basename = Printf.sprintf "%s_%s%s"
            (ParsersDB.MlyParser.get_basename parser)
            (GeneratorsDB.Generator.get_suffix generator)
            (if trace then "_trace" else "");
        lexer = ParsersDB.MlyParser.get_lexer parser;
        libraries = GeneratorsDB.Generator.get_libraries generator;
        additionalfiles = ParsersDB.MlyParser.get_additionals parser;
        status = NotYet;
        generation_time = 0.0;
        compilation_time = 0.0;
        ml_size = 0;
        cmo_size = 0;
        o_size = 0;
        generator = generator;
        parser = parser;
        istrace = trace;
      } in
    if trace then
      add_generated_parser_trace generator parser result
    else
      add_generated_parser_notrace generator parser result;
  ;;
  
  let set_ml_size p size =
    p.ml_size <- size
  
  let set_cmo_size p size =
    p.cmo_size <- size
  
  let set_o_size p size =
    p.o_size <- size
  
  let set_generation_time p time =
    p.generation_time <- time
  
  let set_compilation_time p time =
    p.compilation_time <- time
  
  let set_status p status =
    p.status <- status
  
  let set_lexer p lexer =
    p.lexer <- lexer
  
  let to_string p =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf (Printf.sprintf "directory       = %s\n" p.directory);
    Buffer.add_string buf (Printf.sprintf "basename        = %s\n" p.basename);
    Buffer.add_string buf (Printf.sprintf "libraries         = %s\n" (String.concat " " p.libraries));
    Buffer.add_string buf (Printf.sprintf "additionalfiles = %s\n"
          (String.concat ", " p.additionalfiles));
    Buffer.add_string buf (Printf.sprintf "lexer           = %s\n" p.lexer);
    Buffer.add_string buf (Printf.sprintf "ml_size         = %d\n" p.ml_size);
    Buffer.add_string buf (Printf.sprintf "cmo_size        = %d\n" p.cmo_size);
    Buffer.add_string buf (Printf.sprintf "o_size          = %d\n" p.o_size);
    Buffer.add_string buf (Printf.sprintf "generation time = %f\n" p.generation_time);
    Buffer.contents buf
  ;;
end
;;
