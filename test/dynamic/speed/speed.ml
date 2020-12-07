open Scanf
open Printf
let out = printf

(* This is the information we gather about one run. *)

type measurement = {
  tokens: float;   (* number of tokens parsed *)
  time: float;     (* time spent *)
  minor: float;    (* number of words allocated in minor heap *)
  major: float;    (* number of words allocated in major heap *)
  promoted: float;
}

(* Reading a measurement produced by src/gene.ml. *)

let read f =
  let c = Scanning.from_file f in
  Scanf.bscanf c "tokens: %f\ntime: %f\nminor: %f\nmajor: %f\npromoted: %f\n"
  (fun tokens time minor major promoted ->
    Scanning.close_in c;
    { tokens; time; minor; major; promoted })

(* Printing a measurement. *)

let print m =
  out "Time : %.1f seconds per billion tokens.\n"
    (m.time *. 1000000000.0 /. m.tokens);
  out "Space: %.1f words per token (minor) and %.1f words per token (major).\n"
    (m.minor /. m.tokens) (m.major /. m.tokens);
  out "\n";
  ()

(* Read three measurements performed by separate processes. *)

let code =
  read "src/code.time"
let table =
  read "src/table.time"
let ocamlyacc =
  read "src/ocamlyacc.time"

(* Display a comparison. *)

let () =
  out "Code back-end:\n";
  print code;
  out "Table back-end:\n";
  print table;
  out "ocamlyacc:\n";
  print ocamlyacc;

  out "The table back-end is %.1f times slower than the code back-end.\n"
    (table.time /. code.time);
  out "ocamlyacc          is %.1f times slower than the code back-end.\n"
    (ocamlyacc.time /. code.time);
  out "ocamlyacc          is %.1f times faster than the table back-end.\n"
    (table.time /. ocamlyacc.time);
  out "\n";

  out "The table back-end allocates %.1f times more memory \
       than the code back-end.\n"
    (table.minor /. code.minor);

  flush stdout
