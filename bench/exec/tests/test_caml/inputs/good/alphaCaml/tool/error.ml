open Printf
open Lexing

let verbose =
  ref false

let get_initialized_ref ref =
  match !ref with
  | None ->
      assert false
  | Some contents ->
      contents

let filename =
  ref (None : string option)

let get_filename () =
  get_initialized_ref filename

let signal (line, char1, char2) message =
  fprintf stderr "File \"%s\", line %d, characters %d-%d:\n%s\n" (get_filename()) line char1 char2 message

let one_position position =
  let line = position.pos_lnum in
  let char1 = position.pos_cnum - position.pos_bol in
  line, char1, char1

let two_positions position1 position2 =
  (* Both positions assumed to lie on the same line. *)
  let line = position1.pos_lnum in
  let char1 = position1.pos_cnum - position1.pos_bol in
  let char2 = position2.pos_cnum - position2.pos_bol in
  line, char1, char2

let error1 position message =
  signal (one_position position) message;
  exit 1

let error2 position1 position2 message =
  signal (two_positions position1 position2) message;
  exit 1

let signal2 position1 position2 message =
  signal (two_positions position1 position2) message

let file =
  ref (None : in_channel option)

let get_file () =
  get_initialized_ref file

