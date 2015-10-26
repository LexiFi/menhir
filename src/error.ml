open Printf

(* ---------------------------------------------------------------------------- *)

(* Global state. *)

let get_initialized_ref ref =
  match !ref with
  | None ->
      assert false
  | Some contents ->
      contents

let filename =
  ref (None : string option)

let filemark =
  ref Mark.none

(* 2011/10/19: do not use [Filename.basename]. The [#] annotations that
   we insert in the [.ml] file must retain their full path. This does
   mean that the [#] annotations depend on how menhir is invoked -- e.g.
   [menhir foo/bar.mly] and [cd foo && menhir bar.mly] will produce
   different files. Nevertheless, this seems useful/reasonable. *)

(* This also influences the type error messages produced by [--infer]. *)

let set_filename name =
  filename := Some name;
  filemark := Mark.fresh()

let get_filename () =
  get_initialized_ref filename

let get_filemark () =
  !filemark

let file_contents =
  ref (None : string option)

let get_file_contents () =
  get_initialized_ref file_contents

(* ---------------------------------------------------------------------------- *)

(* Logging and log levels. *)

let log kind verbosity msg =
  if kind >= verbosity then
    Printf.fprintf stderr "%t%!" msg

let logG =
  log Settings.logG

let logA =
  log Settings.logA

let logC =
  log Settings.logC

(* ---------------------------------------------------------------------------- *)

(* Errors and warnings. *)

let errors =
  ref false

let display continuation header positions format =
  List.iter (fun position -> 
    fprintf stderr "%s:\n" (Positions.string_of_pos position)
  ) positions;
  Printf.kfprintf
    continuation
    stderr
    (header ^^ format ^^ "\n%!")

let error positions format =
  display
    (fun _ -> exit 1)
    "Error: "
    positions format 

let signal positions format =
  display
    (fun _ -> errors := true)
    "Error: "
    positions format 

let warning positions format =
  display
    (fun _ -> ())
    "Warning: "
    positions format 

let errors () =
  !errors

let errorp v =
  error [ Positions.position v ]

let grammar_warning =
  if Settings.strict then signal else warning

