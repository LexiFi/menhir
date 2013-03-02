open Printf

(* Prepare for parsing the command line. *)

let verbose =
  ref 0

let filename = 
  ref None

let insert name = 
  filename := Some name

let cyclic =
  ref false

let depth =
  ref 2

let chunk =
  ref 16

let options = Arg.align [
  "--chunk", Arg.Set_int chunk, sprintf "<size> Sets expansion chunk size (default: %d)" !chunk;
  "--cyclic", Arg.Set cyclic, sprintf " Allows speculative ill-founded cycles (default: %b)" !cyclic;
  "--depth", Arg.Set_int depth, sprintf "<depth> Sets refuter's exploration depth (default: %d)" !depth;
  "--verbose", Arg.Set_int verbose, sprintf "<level> Sets verbosity level (default: %d)" !verbose;
  "-c", Arg.Set_int chunk, "<size> Synonymous for --chunk <size>";
  "-d", Arg.Set_int depth, "<depth> Synonymous for --depth <depth>";
  "-v", Arg.Set_int verbose, "<level> Synonymous for --verbose <level>";
]

let usage =
  Printf.sprintf "Usage: %s <options> <filename>" Sys.argv.(0)

(* Parse the command line. *)

let () =
  Arg.parse options insert usage

(* Export the settings. *)

let verbose =
  !verbose

let filename =
  match !filename with
  | None ->
      Arg.usage options usage;
      exit 1
  | Some filename ->
      filename

let basename =
  Filename.chop_suffix filename ".grm"

let dotname =
  basename ^ ".dot"

let proofname =
  basename ^ ".prf"

let cyclic =
  !cyclic

let depth =
  !depth

let chunk =
  !chunk

