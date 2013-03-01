open Printf
open Lexing

let character p =
  fprintf stderr "character %d"
    (p.pos_cnum - p.pos_bol)

let characters p1 p2 =
  fprintf stderr "characters %d-%d"
    (p1.pos_cnum - p1.pos_bol)
    (p2.pos_cnum - p1.pos_bol) (* intentionally [p1.pos_bol] *)

let line p =
  fprintf stderr "File \"%s\", line %d, "
    p.pos_fname
    p.pos_lnum

let position p =
  line p;
  character p

let range p1 p2 =
  line p1;
  characters p1 p2

let signaled =
  ref false

let signal m =
  fprintf stderr "Error: %s\n%!" m;
  signaled := true

let signal1 p m =
  position p;
  fprintf stderr ":\nError: %s\n%!" m;
  signaled := true

let signal2 p1 p2 m =
  range p1 p2;
  fprintf stderr ":\nError: %s\n%!" m;
  signaled := true

let check () =
  if !signaled then
    exit 1

let error m =
  signal m;
  exit 1

let error1 p m =
  signal1 p m;
  exit 1

let error2 p1 p2 m =
  signal2 p1 p2 m;
  exit 1

