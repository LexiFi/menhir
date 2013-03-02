open GeneratedParser

type t =
  {
    input     : Inputs.Input.t;
    directory : string;
    filename  : string;
  }

let get_directory trace = 
  trace.directory

let get_filename trace = 
  trace.filename

let get_input trace = 
  trace.input

let htable_traces = Hashtbl.create 100

let associate generated trace =
  let input = trace.input in
  assert (not (Hashtbl.mem htable_traces (generated, input)));
  Hashtbl.add htable_traces (generated, input) trace
;;

let make directory filename input =
  {
    input     = input;
    directory = directory;
    filename  = filename;
  }

let build generated input =
  let directory =
    Filename.concat
      (Generated.get_parser_directory generated)
      Settings.trace_dir in
  let filename = Printf.sprintf "%s_%s" (Generated.get_basename generated)
      (Inputs.Input.get_filename input) in
  let trace = make directory filename input in
  assert (not (String.contains  filename '/'));
  associate generated trace;
  trace
