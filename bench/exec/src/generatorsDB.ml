module Generator = struct
  type t =
    {
      title : string;
      path : string;
      command : string;
      traceflag : string;
      traceenv : string * string;
      parsersuffix : string;
      libraries : string list;
    }
  
  (****************************** GETTERS *********************************)
  let get_title gen =
    gen.title
  
  let get_path gen =
    gen.path
  
  let get_command gen =
    gen.command
  
  let get_full_command gen =
    Filename.concat gen.path gen.command
  
  let get_suffix gen =
    gen.parsersuffix
  
  let get_libraries gen =
    gen.libraries
  
  let get_traceflag gen =
    gen.traceflag
  
  let get_envparam gen =
    gen.traceenv
  
  (****************************** SETTERS *********************************)
  let make title path command traceflag traceenv parsersuffix libraries =
    Log.log2 (Printf.sprintf "INFO: loading generator configuration '%s'\n" title);
    {
      title = title;
      path = path;
      command = command;
      traceflag = traceflag;
      traceenv = traceenv;
      parsersuffix = parsersuffix;
      libraries = libraries;
    }
  
  (* FIXME new xml format *)
  let from_xml xml =
    let title = ref ""
    and path = ref ""
    and command = ref ""
    and traceflag = ref ""
    and traceenv = ref ("","")
    and parsersuffix = ref ""
    and libraries = ref [] in
    (
      match xml with
      | Xml.Element ("generator", [], childs ) ->
          (List.iter
              (fun elem ->
                    match elem with
                    | Xml.Element ("title", [], [pcdata]) -> title := Xml.pcdata pcdata
                    | Xml.Element ("path", [], [pcdata]) -> path := Xml.pcdata pcdata
                    | Xml.Element ("path", [], []) -> ()
                    | Xml.Element ("command", [], [pcdata]) -> command := Xml.pcdata pcdata
                    | Xml.Element ("traceflag", [], [pcdata]) -> traceflag := Xml.pcdata pcdata
                    | Xml.Element ("traceflag", [], []) -> ()
                    | Xml.Element ("parsersuffix", [], [pcdata]) -> parsersuffix := Xml.pcdata pcdata
                    | Xml.Element ("libraries", [], libs) ->
                        (libraries :=
                          List.map (
                              fun listelem ->
                                  (
                                    match listelem with
                                    | Xml.Element ("library", [], [child]) -> Xml.pcdata child
                                    | _ -> failwith "malformed xml : expected 'library element'"
                                  )
                            )
                            libs
                        )
                    | Xml.Element ("traceenv", attriblist, []) ->
                        (match attriblist with
                          | [] -> failwith "malformed xml : expected 'traceenv element'"
                          | attrib::[] -> traceenv := attrib
                          | _ -> failwith "malformed xml : expected only 1 'traceenv element'"
                        )
                    | Xml.PCData str ->
                        failwith (Printf.sprintf "%s%s here pcdata: %s"
                              "malformed xml : expected ('title'|'path'|"
                              "'command'|'traceflag'|'traceenv'|'parsersuffix'|'libraries')"
                              str)
                    | Xml.Element (str, _, _) ->
                        failwith (Printf.sprintf "%s%s here tagname: '%s'"
                              "malformed xml : expected ('title'|'path'|"
                              "'command'|'traceflag'|'traceenv'|'parsersuffix'|'libraries')"
                              str)
              )
              childs
          )
      | _ -> failwith "malformed xml : expected 'generator element'"
    );
    {
      title = !title;
      path = !path;
      command = !command;
      traceflag = !traceflag;
      traceenv = !traceenv;
      parsersuffix = !parsersuffix;
      libraries = !libraries;
    }
  ;;
  
  (*************************** FORMATTERS ***********************************)
  let to_string gen =
    let buf = Buffer.create 1024 in
    Buffer.add_string buf"{\n";
    Buffer.add_string buf (Printf.sprintf "\ttitle     = %s\n" (get_title gen));
    Buffer.add_string buf (Printf.sprintf "\tpath       = %s\n" (get_path gen));
    Buffer.add_string buf (Printf.sprintf "\tcommand   = %s\n" (get_command gen));
    Buffer.add_string buf (Printf.sprintf "\tsuffix    = %s\n" (get_suffix gen));
    Buffer.add_string buf (Printf.sprintf "\tlibrary   = %s\n" (String.concat " " (get_libraries gen)));
    Buffer.add_string buf (Printf.sprintf "\ttraceflag = %s\n" (get_traceflag gen));
    Buffer.add_string buf (Printf.sprintf "\tenvparam  = [%s=%s]\n" (fst (get_envparam gen)) (snd (get_envparam gen)));
    Buffer.add_string buf "}\n\n";
    Buffer.contents buf
  
  let to_xml generator =
    let title = Xml.Element("title", [], [Xml.PCData (generator.title)])
    and path = Xml.Element ("path", [], [Xml.PCData (generator.path)])
    and command = Xml.Element ("command",[], [Xml.PCData (generator.command)])
    and suffix = Xml.Element ("parsersuffix", [], [Xml.PCData (generator.parsersuffix)])
    and traceflag = Xml.Element ("traceflag", [], [Xml.PCData (generator.traceflag)])
    and traceenv = Xml.Element("traceenv", [generator.traceenv], [])
    and lib = Xml.Element
      ("libraries", [],
        (List.map (fun x -> Xml.Element("library", [], [Xml.PCData x])
            )
            (generator.libraries))
      ) in
    if ((String.length (fst generator.traceenv)) > 0) then
     Xml.Element ("generator", [], [title; path; command; traceflag; traceenv; suffix; lib])
     else 
     Xml.Element ("generator", [], [title; path; command; traceflag; suffix; lib])
  ;;
  
  let input_lines chan prompt =
    let rec loop acc iter =
      Printf.printf "%s %d : " prompt iter; flush stdout;
      match input_line chan with
      | "" -> acc
      | s -> loop (s:: acc) (iter + 1) in
    loop [] 0
  ;;
  
  let from_stdin () =
    print_endline "Enter the title of the new generator.";
    print_endline "It will only be used in reports";
    print_string "\ttitle : ";
    flush stdout;
    let title = input_line stdin in
    print_endline "Enter the path of the new generator, ";
    print_endline "or press enter to search in the environment path";
    print_string "\tpath : ";
    flush stdout;
    let path = input_line stdin in
    print_endline "Enter the command to invoke the generator,";
    print_endline "e.g. : 'menhir --table'";
    print_string "\tcommand : ";
    flush stdout;
    let command = input_line stdin in
    print_endline "Enter the suffix to apply to files generated";
    print_endline "using this generator. The suffix must be unique";
    print_string "\tsuffix : ";
    flush stdout;
    let suffix = input_line stdin in
    print_endline "Enter the libraries that must be linked with";
    print_endline "the parsers produced by this generator";
    print_endline "an example is menhir, which must be linked with";
    print_endline "menhirLib when using tables backend";
    print_endline "terminate the list by pressing enter on an empty line";
    let libraries = input_lines stdin "\tlibrary" in
    print_endline "Enter the environment variables to change in order to";
    print_endline "generate a trace, this variable is OCAMLRUNPARAM for ocamlyacc";
    print_string "\tenv variable : ";
    flush stdout;
    let envvar = input_line stdin in
    print_endline "Enter the value of the environnement variable that must be passed";
    print_string "\tenv value : ";
    flush stdout;
    let envval = input_line stdin in
    print_endline "Enter the flag in order to generate a parser that contains";
    print_endline "specific trace informations";
    print_string "\ttraceflag : ";
    flush stdout;
    let traceflag = input_line stdin in
    make title path command traceflag (envvar, envval) suffix libraries
  
end

module GeneratorsList = struct
  (*************************** GLOBAL Generators List ************************)
  
  type t =
    | Generator of Generator.t
    | GeneratorGroup of Generator.t * Generator.t
  
  (* the global list of generators *)
  let generators = ref []
  
  let apply f elem =
    match elem with
    | Generator g -> f g
    | GeneratorGroup (gen1, gen2) -> f gen1; f gen2
  ;;
  
  let apply_group f group =
    match group with
    | Generator _ -> ()
    | GeneratorGroup (gen1, gen2) -> f (gen1, gen2)
  
  let iter f =
    List.iter (apply f) !generators
  ;;
  
  let iter_group f =
    List.iter (apply_group f) !generators
  (* convert a generators list to a xml document *)
  let to_xml generators_list =
    Xml.Element("generators", [], (List.map Generator.to_xml
          generators_list))
  
  (* print the list of generators *)
  let print () =
    iter (fun x -> Printf.printf "%s\n" (Generator.to_string x))
  ;;
  
  let add generator =
    generators := (Generator generator) :: !generators
  
  let add_group gen1 gen2 =
    generators := (GeneratorGroup (gen1, gen2)) :: !generators
  
  (************************* XML Manipulation *********************************)
  (* convert a generators list to a xml document *)
  let to_xml generators_list =
    Xml.Element("generators", [], (List.map
          (fun x ->
                match x with
                | Generator g -> Generator.to_xml g
                | GeneratorGroup (g1, g2) -> Xml.Element("generatorgroup", [], (Generator.to_xml g1:: Generator.to_xml g2::[]))
          )
          !generators
      ))
  
  let add_generator_xml elem =
    add (Generator.from_xml elem)
  
  let add_group_xml xml =
    let childs = Xml.children xml in
    match childs with
    | [] -> failwith "malformed xml"
    | elem1:: elem2::[] -> add_group (Generator.from_xml elem1)
          (Generator.from_xml elem2)
    | _ -> failwith "malformed xml"
  
  (* adds generators to the generators list from the xml document *)
  let from_xml xml =
    Xml.iter (fun elem ->
            match Xml.tag elem with
            | "generator" -> add_generator_xml elem
            | "generatorgroup" -> add_group_xml elem
            | _ -> failwith "malformed xml"
      ) xml
  ;;
  
  (* read generators list from xml file generators_db.xml*)
  (* and fill the global generators list *)
  let read () =
    let xml = Xml.parse_file Settings.generators_file in
    from_xml xml;
    Log.log1 (Printf.sprintf "-> %d generators configurations loaded\n" (List.length !generators));
  ;;
  
  (* export the global generators list to xml file : generators_db *)
  let write () =
    let f = open_out Settings.generators_file in
    let xml = to_xml !generators in
    output_string f (Xml.to_string_fmt xml);
    close_out f
  ;;
  
  (* FIXME DEPRECATED *)
  let generators_name () =
    let lst = ref [] in
    iter (fun x -> lst := (Generator.get_title x):: (!lst));
    Array.of_list (List.rev ( !lst))
  ;;
  
  let list_remove liste elem = 
  let rec loop lst acc = 
    match lst with
    | [] -> acc
    | a ::tl when a = elem -> loop tl (acc)
    | a :: tl -> loop tl (a::acc)
    in 
    loop liste []
    
    
    let remove generator = 
      generators := list_remove !generators (Generator generator)
      ;;
      
  
end


