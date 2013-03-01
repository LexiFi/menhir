(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/flags.ml,v 1.27.4.7 1999/02/20 01:30:23 francois Exp $ *)
(*

Runtime flags to allow enabling or disabling optimizations without recompiling. Command line arguments are parsed
right here so that the flags immediately have their correct value.

*)

let verbose = ref false;;
let explicit = ref false;;
let teX = ref false;;
let showcode = ref false;;
let exec = ref false;;
let noguards = ref false;;

let filename = ref None;;

let flags = [
  "verbose", verbose;
  "explicit", explicit;
  "tex", teX;
  "showcode", showcode;
  "exec", exec;
  "noguards", noguards
];;

Arg.parse [
  "-verbose", Arg.Set verbose, "displays lots of timing (and other internal) information";
  "-explicit", Arg.Set explicit, "turns on detailed printing mode";
  "-tex", Arg.Set teX, "switches to TeX output";
  "-showcode", Arg.Set showcode, "shows the output of the pattern matching compiler";
  "-exec", Arg.Set exec, "executes the code, in addition to typechecking it";
  "-noguards", Arg.Set noguards, "prevents the use of guarded constraints"
] (function anonymous -> filename := Some anonymous) "More details in README, and in the source code!"
;;
