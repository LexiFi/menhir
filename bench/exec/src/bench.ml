open Actions;;

let test_all () =
  (* read databases *)
  Log.log1 "Loading generators configuration...\n";


  (try
  GeneratorsDB.GeneratorsList.read();
    
  with Xml.Error _ -> 
  ( 
  Log.loge "Seems to have an empty generators xml file. Please add generators\n";
        exit 1
       ));
         Log.log1 "Loading parsers configuration...\n";
         ParsersDB.read ();
      (* generate parsers *)
      Log.log1 "Parsers generation... This may take a moment\n";
      if (Settings.need_notrace_generation) then
        generate_all false Settings.forced_generation;
      if (Settings.need_trace_generation) then
        generate_all true Settings.forced_generation;
      (* compile parsers *)
      if (Settings.need_compilation) then
        (
          Log.log1 "Parsers prerequisites compilation...\n";
          if (Settings.bytecode_compilation) then
            compile_additionals false Settings.forced_compilation;
          if (Settings.native_compilation) then
            compile_additionals true Settings.forced_compilation;
          Log.log1 "Parsers compilation... This may (also) take a moment\n";
          if (Settings.bytecode_compilation) then
            ( if Settings.need_trace_generation then
                compile_trace false Settings.forced_compilation;
              compile_notrace false Settings.forced_compilation);
          if (Settings.native_compilation) then
            (*compile_all true Settings.forced_compilation;*)
            ( if Settings.need_trace_generation then
                compile_trace true Settings.forced_compilation;
              compile_notrace true Settings.forced_compilation);
        );
      (* compile lexers and additionals and link *)
      if (Settings.need_linking) then
        (
          Log.log1 "Lexers compilation and full linking...\n";
          if (Settings.bytecode_compilation) then
            (*(link_all false Settings.force_link);*)
            (if Settings.need_trace_generation then
                link_trace false Settings.force_link;
              link_notrace false Settings.force_link);
          if (Settings.native_compilation) then
            (*(link_all true Settings.force_link);*)
            (if Settings.need_trace_generation then
                link_trace true Settings.force_link;
              link_notrace true Settings.force_link);
        );
      
      if Settings.do_all_sizes_comparison then
        (Log.log1 "Calculing all sizes\n";
          GeneratedParser.Generated.iter_notrace calc_all_sizes;
          Log.log1 "Finished Calculing all sizes\n")
      else
        (
          if Settings.do_ml_size_comparison then
            (Log.log1 "Calculing ml sizes\n";
              GeneratedParser.Generated.iter_notrace calc_ml_size;
              Log.log1 "Finished Calculing ml sizes\n");
          if Settings.do_cmo_size_comparison then
            (Log.log1 "Calculing cmo sizes\n";
              GeneratedParser.Generated.iter_notrace calc_cmo_size;
              Log.log1 "Finished Calculing cmo sizes\n");
          if Settings.do_o_size_comparison then
            (Log.log1 "Calculing o sizes\n";
              GeneratedParser.Generated.iter_notrace calc_o_size;
              Log.log1 "Finished Calculing o sizes\n")
        );
      
      (* run speed tests *)
      if (Settings.do_execution_time_calculation) then
        (
          Log.log1 "Scanning for inputs files... This will be short\n";
          GeneratedParser.Generated.iter_notrace (Inputs.scan_inputs);
          (*Inputs.print ();*)
          Log.log1 "Performing execution speed comparisons... you can prepare a coffee...\n";
          if (Settings.bytecode_compilation) then
            (run_speeds_all false);
          if (Settings.native_compilation) then
            (run_speeds_all true);
        );
      (* run trace tests *)
      if (Settings.do_trace_comparison) then
        (
          Log.log1 "Scanning for inputs files... This will be short\n";
          GeneratedParser.Generated.iter_trace (Inputs.scan_inputs);
          Log.log1 "Performing execution with traces... please be patient...\n";
          if (Settings.bytecode_compilation) then
            (run_traces_all false);
          if (Settings.native_compilation) then
            (run_traces_all true);
          Log.log1 "Performing comparisons of the generated traces... please wait...\n";
          if (Settings.bytecode_compilation) then
            (compare_traces_all false);
          if (Settings.native_compilation) then
            (compare_traces_all true)
        )
(* build report here *)
(*
let module T = Actions.GenerateReport (struct end) in
let module A = Report.Run(T) in
A.make_report();
let module B = Plot.Run(T) in
B.make_plot ()
*)
(*
compile_with_lexer ();

ParsersDB.iter_generated (calc_all_sizes);
ParsersDB.iter_generated (GeneratedParser.run_speed speed_inputs);
ParsersDB.iter_generated (fun p -> Printf.printf "%s\n" (GeneratedParser.to_string p));
(* re-generate with trace *)
(* generate true;
compile_with_lexer ();
ParsersDB.iter_generated (GeneratedParser.run_trace trace_inputs);
ParsersDB.iter_generated (fun p -> Printf.printf "%s\n" (GeneratedParser.to_string p));
*)
let module T = Actions.GenerateReport (struct end) in
let module A = Report.Run(T) in
A.make_report();
let module B = Plot.Run(T) in
B.make_plot ();
;;

let only_size () =
(* read databases *)
GeneratorsDB.read();
ParsersDB.read ();
(* generate all without trace and calc speed *)
generate false;
ParsersDB.iter_generated (calc_all_sizes);
ParsersDB.iter_generated (fun p -> Printf.printf "%s\n" (GeneratedParser.to_string p));
let module T = Actions.Generate (struct end) in
let module A = Report.Run(T) in
A.make_report();
let module B = Plot.Run(T) in
B.make_plot ()
;;

*)



let add_group () =
  (try
    GeneratorsDB.GeneratorsList.read();
  with Xml.Error _ ->
      (
        Log.loge "Seems to have an empty generators xml file. Please add generators\n";
        exit 1
      )
      );
      let htable = Hashtbl.create 10 in
      let i = ref 0 in
      GeneratorsDB.GeneratorsList.iter
        (fun gen -> Printf.printf "\t%d) %s\n"
                !i
                (GeneratorsDB.Generator.get_title gen);
              Hashtbl.add htable !i gen; i:= !i + 1
        );
      print_endline "You create generators group in order to compare the traces";
      print_endline "generated by parsers created with this generators";
      print_endline "enter the numbers (left) of the two generators to group";
      print_string "\tfirst generator : ";
      flush stdout;
      let firstn = int_of_string(input_line stdin) in
      let first = Hashtbl.find htable firstn in
      print_string "\tsecond generator : ";
      flush stdout;
      let secondn = int_of_string(input_line stdin) in
      let second = Hashtbl.find htable secondn in
      GeneratorsDB.GeneratorsList.remove first;
      GeneratorsDB.GeneratorsList.remove second;
      GeneratorsDB.GeneratorsList.add_group first second;
      GeneratorsDB.GeneratorsList.write()

let add_generator () =
  (try
    GeneratorsDB.GeneratorsList.read();
  with Xml.Error _ -> ()
  );
  let gen = GeneratorsDB.Generator.from_stdin() in
    GeneratorsDB.GeneratorsList.add gen;
    GeneratorsDB.GeneratorsList.write()

let status () =
  GeneratorsDB.GeneratorsList.read();
  ParsersDB.read();
;;

let usage progname =
  Printf.printf "usage: %s [options] command\n" progname;
  Printf.printf "   available commands :\n";
  Printf.printf "       test\n";
  Printf.printf "       add_generator\n";
  Printf.printf "       status\n";
  Printf.printf "       addgenerator\n";
  Printf.printf "       addgroup\n";
  Printf.printf "       clean\n"
;;

(*
if Sys.file_exists "stdout" then
Sys.remove "stdout"
;;

if Sys.file_exists "stderr" then
Sys.remove "stderr"
;;
*)

let argv = Sys.argv in
match (Array.length argv) with
| 1 -> usage argv.(0); exit 0
| 2 ->
    (match argv.(1) with
      | "test" ->
          (
            try
              test_all ();
              exit 0;
            with a -> Log.logclose(); raise a
          )
      | "addgenerator" -> add_generator()
      | "addgroup" -> add_group()
      | "status" -> status (); exit 0
      | "clean" -> exit 0
      | _ -> failwith "not yet : hum hum"
    )
| _ -> failwith "not yet"
;;

