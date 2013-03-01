(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/chrono.ml,v 1.5.4.3 1998/08/31 17:21:19 fpottier Exp $ *)
(*

Timing a function's execution.

*)

open Unix

let enabled = ref false;;
let start() = enabled := true;;
let stop() = enabled := false;;

type clock = (float * int) ref

let create () =
  ref (0.0, 0)
;;

let reset clock =
  clock := (0.0, 0)
;;

let diff clock1 clock2 =
  let time1, count1 = !clock1
  and time2, count2 = !clock2 in
  ref (time1 -. time2, count1 - count2)
;;

let chrono clock action =
  if !enabled then begin
    let start_time = (Unix.times()).tms_utime in
    try
      let result = action() in
      let stop_time = (Unix.times()).tms_utime in
      let (previous_time, previous_count) = !clock in
      clock := (previous_time +. stop_time -. start_time, succ previous_count);
      result
    with error ->
      let stop_time = (Unix.times()).tms_utime in
      let (previous_time, previous_count) = !clock in
      clock := (previous_time +. stop_time -. start_time, succ previous_count);
      raise error
  end
  else action()
;;

let print clock name want_count =
  let (time, count) = !clock in
  print_string name;
  if want_count then begin
    print_string " was called ";
    print_int count;
    print_string " times and";
  end;
  print_string " took ";
  print_float time;
  print_endline " seconds.";
  flush Pervasives.stdout
;;
