open Scanf
open Printf

let out = printf

(* This is the information we gather about one run. *)

type measurement =
  { (* number of tokens parsed *)
    tokens : float
  ; (* time spent *)
    time : float
  ; (* number of words allocated in minor heap *)
    minor : float
  ; (* number of words allocated in major heap *)
    major : float
  ; promoted : float
  ; ast : float
  }

(* Reading a measurement produced by src/gene.ml. *)

let zero =
  { tokens = 0.; time = 0.; minor = 0.; major = 0.; promoted = 0.; ast = 0. }


let ( + ) m m' =
  { tokens = m.tokens +. m'.tokens
  ; time = m.time +. m'.time
  ; minor = m.minor +. m'.minor
  ; major = m.major +. m'.major
  ; promoted = m.promoted +. m'.promoted
  ; ast = m.ast +. m'.ast
  }


let ( / ) m n =
  { tokens = m.tokens /. n
  ; time = m.time /. n
  ; minor = m.minor /. n
  ; major = m.major /. n
  ; promoted = m.promoted /. n
  ; ast = m.ast /. n
  }


let average ml = List.fold_left ( + ) zero ml / (float_of_int @@ List.length ml)

let read f =
  let c = Scanning.from_file f in
  try
    Scanf.bscanf
      c
      "tokens: %f\ntime: %f\nminor: %f\nmajor: %f\npromoted: %f\nast: %f\n"
      (fun tokens time minor major promoted ast ->
        Scanning.close_in c;
        { tokens; time; minor; major; promoted; ast } )
  with
  | Scanf.Scan_failure _ ->
      eprintf "File %s does not conform to expected format.\n" f;
      eprintf "%s :" f;
      let c = open_in f in
      ( try
          while true do
            eprintf "%s\n" (input_line c)
          done
        with
      | End_of_file ->
          () );
      exit 1


(* Printing a measurement. *)

let print m =
  out
    "    Time : %.1f seconds per billion tokens.\n"
    (m.time *. 1000000000.0 /. m.tokens);
  out
    "    Space: %.2f words per token (minor) and %.2f words per token (major)\n"
    (m.minor /. m.tokens)
    (m.major /. m.tokens);
  out
    "    A total of %.3f words per tokens were allocated\n"
    ((m.minor +. m.major -. m.promoted) /. m.tokens);
  out
    "    Among these %.2f words per token were used for the AST.\n"
    (m.ast /. m.tokens);
  out "\n";
  ()


let backends =
  List.sort_uniq
    compare
    (List.map
       Filename.remove_extension
       (List.map
          (fun s -> Filename.chop_suffix (Filename.basename s) ".time")
          (List.filter
             (fun s -> Filename.check_suffix s ".backend")
             (Array.to_list (Sys.readdir "backends")) ) ) )


let m backend =
  let Unix.{ st_size; _ } =
    let ( / ) = Filename.concat in
    Unix.stat
      ( "backends"
      / (backend ^ ".backend")
      / ".main.eobjs"
      / "native"
      / "dune__exe__Parser.o" )
  in
  let ( / ) = Int.div in
  let ko_size = st_size / 1000 in
  let time_suffix = backend ^ ".time" in
  let bases =
    Array.to_list (Sys.readdir "times")
    |> List.filter (fun s -> Filename.check_suffix s time_suffix)
    |> List.map (fun s -> Filename.chop_suffix s time_suffix)
    |> List.map Filename.remove_extension
    |> List.sort_uniq compare
  in
  ( ko_size
  , average
      (List.map
         (fun base -> read (sprintf "times/%s.%s.time" base backend))
         bases ) )


(* Display a comparison. *)

let () =
  let name = Sys.argv.(1) in
  out "[ Start of %s benchmark ]\n" name;
  List.iter
    (fun backend ->
      let m = m backend in
      out "  %s back-end:\n" backend;
      out "    parser.o size : %d ko\n" (fst m);
      print (snd m) )
    backends;
  out "[ End of %s benchmark ]\n" name;
  flush stdout
