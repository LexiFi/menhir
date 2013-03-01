open Printf
open Lexing

let print_location = function
  | Some (pos1, pos2) ->
      let file = pos1.pos_fname in
      let line = pos1.pos_lnum in
      let char1 = pos1.pos_cnum - pos1.pos_bol in
      let char2 = pos2.pos_cnum - pos1.pos_bol in (* intentionally [pos1.pos_bol] *)
      fprintf stderr "File \"%s\", line %d, characters %d-%d:\n" file line char1 char2
	(* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)
  | None ->
      ()

let signaled =
  ref false

let signal locs message =
  List.iter print_location locs;
  fprintf stderr "%s\n%!" message;
  signaled := true

let error locs message =
  signal locs message;
  exit 1

let signalv v message =
  signal [ Annotation.get v ] message

let errorv v message =
  error [ Annotation.get v ] message

let errorb lexbuf msg =
  error [ Some (Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf) ] msg

let signaled () =
  if !signaled then
    exit 1

