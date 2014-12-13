open Printf

(* Read one floating-point in a file. *)
let read f =
  let c = open_in f in
  let s = input_line c in
  close_in c;
  float_of_string s

let dry =
  read "gene/dry.time"
let code =
  read "gene/code.time"
let table =
  read "gene/table.time"
let ocamlyacc =
  try
    Some (read "gene/ocamlyacc.time")
  with Sys_error _ ->
    None

let optionally o f =
  match o with Some x -> f x | None -> ()

let () =
  printf "Test input generation takes %.2f seconds.\n" dry;
  printf "Parsing with the code back-end takes %.2f seconds.\n" (code -. dry);
  printf "Parsing with the table back-end takes %.2f seconds.\n" (table -. dry);
  optionally ocamlyacc (fun ocamlyacc ->
    printf "Parsing with ocamlyacc takes %.2f seconds.\n" (ocamlyacc -. dry);
  );
  printf "The table back-end is %.1f times slower than the code back-end.\n" ((table -. dry) /. (code -. dry));
  optionally ocamlyacc (fun ocamlyacc ->
    printf "ocamlyacc is %.1f times slower than the code back-end.\n" ((ocamlyacc -. dry) /. (code -. dry));
    printf "ocamlyacc is %.1f times faster than the table back-end.\n" ((table -. dry) /. (ocamlyacc -. dry));
  );
  flush stdout

