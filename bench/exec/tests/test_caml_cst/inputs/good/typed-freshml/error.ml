open Printf
open Lexing

let log verbosity msg =
  if Settings.verbose >= verbosity then
    fprintf stderr "%t%!" msg

let positions pos1 pos2 =
  let file = pos1.pos_fname in
  let line = pos1.pos_lnum in
  let char1 = pos1.pos_cnum - pos1.pos_bol in
  let char2 = pos2.pos_cnum - pos1.pos_bol in (* intentionally [pos1.pos_bol] *)
  fprintf stderr "File \"%s\", line %d, characters %d-%d:\n" file line char1 char2
    (* use [char1 + 1] and [char2 + 1] if *not* using Caml mode *)

let signaled =
  ref false

let signals tvs message =
  List.iter (fun tv ->
    let startpos, endpos = Annotation.get tv in
    positions startpos endpos
  ) tvs;
  fprintf stderr "%s\n%!" message;
  signaled := true

let errors tvs message =
  signals tvs message;
  exit 1

let signal tv message =
  signals [ tv ] message

let error tv message =
  errors [ tv ] message

let error2 pos1 pos2 message =
  error (Annotation.make (pos1, pos2) ()) message

let signaled () =
  if !signaled then
    exit 1

