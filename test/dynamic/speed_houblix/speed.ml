open Scanf
open Printf

let out = printf

(* This is the information we gather about one run. *)

type measurement =
  { (* number of tokens parsed *)
    tokens: float
  ; (* time spent *)
    time: float
  ; (* number of words allocated in minor heap *)
    minor: float
  ; (* number of words allocated in major heap *)
    major: float
  ; promoted: float }

(* Reading a measurement produced by src/gene.ml. *)

let zero = {tokens= 0.; time= 0.; minor= 0.; major= 0.; promoted= 0.}

let ( + ) m m' =
  { tokens= m.tokens +. m'.tokens
  ; time= m.time +. m'.time
  ; minor= m.minor +. m'.minor
  ; major= m.major +. m'.major
  ; promoted= m.promoted +. m'.promoted }

let ( / ) m n =
  { tokens= m.tokens /. n
  ; time= m.time /. n
  ; minor= m.minor /. n
  ; major= m.major /. n
  ; promoted= m.promoted /. n }

type measurement_agg =
  {code: measurement; old_code: measurement; table: measurement}

let ( + ) m m' =
  { code= m.code + m'.code
  ; old_code= m.old_code + m'.old_code
  ; table= m.table + m'.table }

let ( / ) m n = {code= m.code / n; old_code= m.old_code / n; table= m.table / n}

let zero = {code= zero; old_code= zero; table= zero}

let average ms = List.fold_left ( + ) zero ms / float_of_int (List.length ms)

let read f =
  let c = Scanning.from_file f in
  Scanf.bscanf c "tokens: %f\ntime: %f\nminor: %f\nmajor: %f\npromoted: %f\n"
    (fun tokens time minor major promoted ->
      Scanning.close_in c ;
      {tokens; time; minor; major; promoted})

(* Printing a measurement. *)

let print m =
  out  "Time : %.1f seconds per billion tokens.\n"
    (m.time *. 1000000000.0 /. m.tokens) ;
  out "Space: %.2f words per token (minor) and %.2f words per token (major).\n"
    (m.minor /. m.tokens) (m.major /. m.tokens) ;
  out "\n" ;
  ()

let bases =
  List.sort_uniq compare
    (List.map Filename.remove_extension
       (List.map
          (fun s -> Filename.chop_suffix s ".time")
          (List.filter
             (fun s -> Filename.check_suffix s ".time")
             (Array.to_list (Sys.readdir "src")))))

(* Read three measurements performed by separate processes. *)
let m =
  average
    (List.map
       (fun base ->
         let code = read (sprintf "src/%s.code.time" base) in
         let table = read (sprintf "src/%s.table.time" base) in
         let old_code = read (sprintf "src/%s.old_code.time" base) in
         {code; old_code; table})
       bases)

(* Display a comparison. *)

let () =
  out " ----- Houblix benchmark ----- \n" ;
  out "Code back-end:\n" ;
  print m.code ;
  out "Old code back-end:\n" ;
  print m.old_code ;
  out "Table back-end:\n" ;
  print m.table ;
  out "The old code back-end is %.1f times slower than the new code back-end.\n"
    (m.old_code.time /. m.code.time) ;
  out "The table back-end is %.1f times slower than the new code back-end.\n"
    (m.table.time /. m.code.time) ;
  out "\n" ;
  out
    "The table back-end allocates %.1f times more memory than the new code \
     back-end.\n"
    (m.table.minor /. m.code.minor) ;
  flush stdout
