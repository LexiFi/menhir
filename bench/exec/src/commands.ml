open Util
open GeneratedParser

open Printf
open BenchTypes


let run_pretty_command command title showtime =
  Log.logc command;
  Log.log2 (sprintf " %-60s" title);
  let _ = tick() in
  if (Sys.command command) != 0 then
    (
      let t = tick() in
      Log.log2 ("Error\n");
      Log.logf ("=> ERROR\n");
      Error,t
    )
  else
    (
      let t = tick() in
      if showtime then Log.log2 (sprintf "OK : %.1f ms\n" (1000.0 *. t)) else Log.log2 ("OK\n");
      Log.logf ("=> OK\n");
      OK,t
    )

(* generate a given .mly parser with a generator *)
(* the generated parser is added to the list of [generatedparsers] of the mly *)
let generate force generated_parser =
  check_create_dir (Generated.get_directory generated_parser);
  Log.log_up();
  Log.logi (sprintf "generating parser %s\n" (Generated.get_basename generated_parser));
  let needed = (newer (ParsersDB.MlyParser.get_absolute_filename
            (Generated.get_parser generated_parser))
        (Generated.get_absolute_ml_filename generated_parser)) in
  if (force || needed)
  then
    (
      let generator = Generated.get_generator generated_parser in
      let parser = Generated.get_parser generated_parser in
      let command = sprintf "%s -b %s %s %s 2>> stderr 1>> stdout"
          (GeneratorsDB.Generator.get_full_command generator)
          (GeneratedParser.Generated.get_absolute_basename generated_parser)
          (if GeneratedParser.Generated.get_istrace generated_parser then
              (GeneratorsDB.Generator.get_traceflag generator)
            else "")
          (ParsersDB.MlyParser.get_absolute_filename parser)
      in
      let title = Generated.get_basename generated_parser in
      match run_pretty_command command title false with
      | OK, deltatime ->
          Generated.set_generation_time generated_parser deltatime;
          Generated.set_status generated_parser Generated.GeneratedNotCompiled
      | Error,deltatime ->
          Generated.set_status generated_parser Generated.GenerationError
    )
  else
    (
      Log.logi ("generation not necessary... skipping\n");
      Log.log2 (sprintf "\t%-40s%s\n" (Generated.get_basename generated_parser) "seems already generated");
      Generated.set_status generated_parser Generated.GenerationSkipped
    )
;;

let ocaml_compile sourcefile libraries flags opt =
  let command = sprintf "%s %s -c %s %s 2>> stderr 1>> stdout"
      (if opt then "ocamlopt" else "ocamlc")
      (String.concat " " flags)
      (String.concat " " libraries)
      sourcefile in
  run_pretty_command command sourcefile false
;;

let ocaml_link sourcefiles output flags libraries opt =
  let command = sprintf "%s %s %s %s -o %s.%s 2>> stderr 1>> stdout"
      (if opt then "ocamlopt" else "ocamlc")
      (String.concat " " flags)
      (String.concat " " libraries)
      (String.concat " " sourcefiles)
      output
      (if opt then "opt" else "byte") in
  run_pretty_command command output false
;;

let compile opt force generated =
  let sourcefile = Generated.get_ml_filename generated in
  let directory = Generated.get_directory generated in
  let libraries = Generated.get_libraries generated opt in
  pushd directory;
  let needed = newer sourcefile (src2obj opt sourcefile) in
  Log.log_up();
  Log.logi (sprintf "compiling parser %s\n" (Generated.get_basename generated));
  if (force || needed) then
    (
      (* FIXME hazardous assertion on existence of mli *)
      match ocaml_compile ((ml2mli sourcefile) ^ " " ^ sourcefile)
        libraries Settings.ocamlc_flags opt with
      | OK, time ->
          Generated.set_compilation_time generated time;
          Generated.set_status generated Generated.OK
      | Error,_ ->
          Generated.set_status generated Generated.CompilationError
    )
  else
    (
      Log.logi (sprintf "compilation not necessary... skipping\n");
      Log.log2 (sprintf "\t%-40s%s\n" (Generated.get_basename generated) "seems already generated");
      Generated.set_status generated Generated.CompilationSkipped
    );
  popd()
;;

exception LexerCompilationException of string;;
exception LexerAdaptationException of string;;

(* this adapts a generic lexer found in a directory, replacing *)
(* @INCLUDE_PARSER@ tag in the file with the name of the parser we want to generate *)
let adapt_lexer sourcelexer parserbasename =
  let destlexer = sprintf "%s_%s.mll"
      (Filename.chop_extension sourcelexer)
      (parserbasename) in
  let title = sprintf "adapting lexer %s to %s" sourcelexer destlexer in
  Log.logi title;
  if newer sourcelexer destlexer then
    (
      let command = sprintf "sed -e \"s/%s/%s/g\" %s > %s"
          "@INCLUDE_PARSER@"
          (String.capitalize(parserbasename))
          (sourcelexer)
          (destlexer) in
      match (run_pretty_command command title false) with
      | OK, _ -> Log.logi "adaptation went OK\n"
      | Error, _ -> Log.logi "adaptation raised Error\n"
    )
  else Log.logi ("adaptation not necessary.. skipping\n");
  destlexer
;;

(* we assume the existence of lexer.mll in the current directory *)
let generate_lexer opt force generated =
  let lexer = adapt_lexer
      (Generated.get_lexer_mll generated)
      (Generated.get_basename generated) in
  Generated.set_lexer generated lexer;
  if (force || newer lexer (Generated.get_lexer_ml generated)) then
    (
      let command = sprintf "ocamllex %s > /dev/null" lexer in
      Log.logi "lexer generation\n";
      match (run_pretty_command command lexer false) with
      | OK, _ -> Log.logi "lexer generation went OK\n"
      | Error,_ -> Log.logi "lexer generation raised Error\n";
    )
  else
    Log.logi "lexer generation not necessary... skipping\n";
;;

let copy_if_newer source destination =
  assert (Sys.file_exists source);
  Log.log_up();
  Log.logi (sprintf "copying %s to %s\t" source destination);
  if (not (Sys.file_exists destination)) || (newer source destination) then
    (
      let src = open_in source
      and dest = open_out destination in
      let buf = String.create 1024 in
      let rec loop () =
        let l = input src buf 0 1024 in
        if l != 0 && l != 1024 then
          (
            output_string dest (String.sub buf 0 l);
            loop();
          )
        else if l = 1024 then
          (
            output_string dest buf;
            loop();
          ) in
      loop ();
      close_in src;
      close_out dest;
      assert ((file_size source) = (file_size destination));
      Log.logf "=> OK\n"
    )
  else Log.logf "=> Not necessary... Skipping\n"

let copy_additionals parser =
  let additionals = ParsersDB.MlyParser.get_additionals parser in
  List.iter (fun file ->
          let directory = (ParsersDB.MlyParser.get_directory parser) in
          let source = Filename.concat directory file in
          let destination = Filename.concat
              (Filename.concat directory Settings.generated_dir)
              file in
          copy_if_newer source destination
    )
    additionals;
  let lexersource = Filename.concat
      (ParsersDB.MlyParser.get_directory parser)
      (ParsersDB.MlyParser.get_lexer parser) in
  let lexerdest = Filename.concat
      (ParsersDB.MlyParser.get_directory parser)
      (Filename.concat Settings.generated_dir (ParsersDB.MlyParser.get_lexer parser)) in
  copy_if_newer lexersource lexerdest
;;

(* compilation of parser prerequisites *)
let compile_additionals opt force parser =
  pushd (ParsersDB.MlyParser.get_generated_directory parser);
  Log.logi "Compilation of additional files\n";
  Log.log2 (sprintf "- for parser %s\n" (ParsersDB.MlyParser.get_filename parser));
  let additionals = ParsersDB.MlyParser.get_additionals parser in
  List.iter (fun file ->
          if newer file (src2obj opt file) then
            ignore(ocaml_compile file [] Settings.ocamlc_flags opt)
          else
            (
              Log.logi (sprintf "compiling '%s' => not necessary...skipping\n" file);
              Log.log2 (sprintf "\t%-40snot necessary...skipping\n" file);
            )
    ) additionals;
  popd ()

let compile_lexer opt force generated =
  let file = Generated.get_lexer_ml generated in
  if (newer file (src2obj opt file) || force )then
    ignore(ocaml_compile file [] Settings.ocamlc_flags opt)
  else
    Log.log2 (sprintf "\t%-40snot necessary... skipping\n" file)

let link_with_lexer generated opt =
  let target = execsuffix opt (Generated.get_basename generated) in
  let sources = Generated.get_sources_for_link generated opt in
  if has_newer sources target then
    ignore(ocaml_link sources
          (Generated.get_basename generated)
          (Settings.ocamlc_flags)
          (Generated.get_libraries generated opt)
          opt)
  else
    Log.log2 (sprintf "\t%-40snot necessary... skipping\n" target)
;;

let link opt force generated =
  pushd (Generated.get_directory generated);
  generate_lexer opt force generated;
  compile_lexer opt force generated;
  link_with_lexer generated opt;
  popd()
;;

let run_speed generated opt input =
  pushd (Generated.get_directory generated);
  let file = Inputs.Input.get_fullname input in
  let title = (sprintf "%20s with %-40s"
        (Generated.get_basename generated)
        (Inputs.Input.get_filename input)) in
  let command = sprintf "./%s.%s < ../%s 1> /dev/null 2>&1"
      (Generated.get_basename generated)
      (if opt then "opt" else "byte")
      file in
  (
    match run_pretty_command command title true with
    | OK, time -> ParseResult.set_time generated input BenchTypes.OK (Some time);
    | Error,time -> ParseResult.set_time generated input BenchTypes.Error (Some time);
  );
  popd()
;;

(* FIXME : we should recreate input dirs hierarchy to avoid file conflicts *)
(* TODO try not cd'ing to directory *)
let run_trace generated opt input =
  let trace = Traces.build generated input in
  let tracename = Traces.get_filename trace
  and executable = Generated.get_linked_executable opt generated
  and inputfile = Inputs.Input.get_fullname input 
  and inputfile_base = Inputs.Input.get_filename input in
  let title = sprintf "%s with %s" (executable) (inputfile_base) in
  Log.log_up();
  Log.logi (sprintf "running trace '%s' (%s)\n" tracename executable); 
  check_create_dir (Traces.get_directory trace);
  pushd (Generated.get_directory generated);
  (* FIXME hardcoded ocamlrunparam *)
  Unix.putenv "OCAMLRUNPARAM" "p";
  let command = sprintf "./%s < ../%s 1> ../%s/%s 2>&1"
      executable
      inputfile
      Settings.trace_dir
      tracename in
  (
    match run_pretty_command command title false with
    | OK,_ -> ParseResult.set_trace generated input BenchTypes.OK (Some trace)   
    | Error,_ -> ParseResult.set_trace generated input BenchTypes.Error (Some trace)
  );
  popd()
;;

exception NoTrace;;

let diff_traces generated1 generated2 opt input =
  let inputfile = Inputs.Input.get_filename input in
  let generatedgroup = generated1, generated2 in
  let tracegroup  =  (ParseResult.get_trace generated1 input,
     ParseResult.get_trace generated2 input) in
  let title = sprintf "diff (%s,%s) with input :  %s "
                                (Generated.get_linked_executable opt generated1)
                                (Generated.get_linked_executable opt generated2)
                                (inputfile) in
  try
    let trace1, trace2 = match tracegroup with
    | Some t1, Some t2 -> t1, t2
    | _ -> Log.loge title ; raise NoTrace in
    pushd (Traces.get_directory trace1);
    let file1 = Traces.get_filename trace1
    and file2 = Traces.get_filename trace2
    and result = sprintf "diff_%s" inputfile in
    let command = sprintf "diff %s %s > %s" file1 file2 result in
    (
      match run_pretty_command command title false with
      | OK, _ -> TraceDiff.add generatedgroup 
                               input 
                               (TraceDiff.Correct (file_size result))
      | Error, _ -> TraceDiff.add generatedgroup 
                               input 
                               (TraceDiff.Incorrect result)
    );
    popd()
  with NoTrace -> Log.loge title
;;

