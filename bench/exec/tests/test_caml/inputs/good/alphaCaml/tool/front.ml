open Printf

let version =
  ref false

let options = [
  "--verbose", Arg.Set Error.verbose, "Enable verbose diagnostics";
  "--version", Arg.Set version, "Print version number and exit";
]

let usage =
  sprintf "Usage: %s <options> <filename>" Sys.argv.(0)

let set_filename name =
  match !Error.filename with
  | None ->
      if Filename.check_suffix name ".mla" then
	Error.filename := Some name
      else begin
	fprintf stderr "Error: the argument file name should end in .mla.\n";
	exit 1
      end
  | Some _ ->
      fprintf stderr "%s\n" usage;
      exit 1

let () =
  Arg.parse options set_filename usage

let filename = 
  if !version then begin
    printf "alphaCaml, version %s\n" Version.version;
    exit 0
  end;
  match !Error.filename with
  | None ->
      fprintf stderr "%s\n" usage;
      exit 1
  | Some filename ->
      filename

let basename =
  Filename.chop_extension filename

let channel =
  open_in filename

let () =
  Error.file := Some channel

let prologue, declarations =
  Parser.phrase Lexer.token (Lexing.from_channel channel)

let () =
  Error.file := None;
  close_in channel

