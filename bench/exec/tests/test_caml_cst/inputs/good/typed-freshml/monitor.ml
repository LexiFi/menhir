open Printf
open Print

let monitor enabled domain codomain name f x =
  if enabled then
    w (fun b ->
      bprintf b "Invoking %s with argument:\n%a\n%!" name domain x
    );
  let y =
    f x
  in
  if enabled then
    w (fun b ->
      bprintf b "Invoking %s returns:\n%a\n%!" name codomain y
    );
  y

open Unix

let now () =
  (times()).tms_utime

let origin =
  now()

let runtime () =
  now() -. origin

let fraction duration =
  sprintf "%.0f%%" (100.0 *. duration /. (runtime()))

let time task f =
  let total = ref 0.0 in
  let f x =
    let start = now() in
    let y =
      f x
    in
    total := now() -. start +. !total;
    y
  and display () =
    let duration = !total in
    fprintf Pervasives.stderr "%s took %.02f seconds so far (%s of total time).\n%!"
      task
      duration
      (fraction duration)
  in
  f, display

