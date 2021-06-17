open Printf

(* ---------------------------------------------------------------------------- *)

(* Parse the command line. *)

(* [--dry-run] offers a choice between running just the generator, or both the
   generator and the parser. *)

let dry_run = ref false

(* [--seed] allows the random seed to be set via the command line. *)

let seed = ref 61112962

(* [--runs] allows the desired number of runs to be set via the command line. *)

let runs = ref 10

(* [--size] allows the size of the randomly-generated expression to be
   set via the command line. *)

let input_file = ref ""

let options =
  Arg.align
    [ ( "--runs"
      , Arg.Set_int runs
      , sprintf "<runs> Set the number of runs (%d)" !runs )
    ; ( "--input"
      , Arg.Set_string input_file
      , "<filename> Sets the input filename" )
    ]


let usage = sprintf "Usage: %s <options>" Sys.argv.(0)

let () = Arg.parse options (fun _ -> ()) usage

let input_file = !input_file

let () = assert (input_file <> "")

let () = Random.self_init ()

(* ---------------------------------------------------------------------------- *)

(* Run. *)

let count_newlines file =
  let inch = open_in file in
  let buffer = Bytes.create 1024 in
  let n = ref 0 in
  let len = ref 0 in
  while
    let read = input inch buffer 0 1024 in
    len := read;
    read <> 0
  do
    for i = 0 to !len - 1 do
      if Bytes.get buffer i = '\n' then n := !n + 1
    done
  done;
  !n


open Lexing

type token = Parser.token

let tokens =
  let lexfun = Simple_lexer.token in
  let lexbuf = Lexing.from_channel (open_in input_file) in
  let stack = ref [] in
  try
    while true do
      stack := lexfun lexbuf :: !stack
    done;
    failwith "Loop did not end"
  with
  | Simple_lexer.ExnEOF ->
      !stack


let tokens : token array = Array.of_list @@ List.rev tokens

let () = Gc.full_major ()

let () = if !dry_run then exit 0

let lexer lexbuf =
  let pos = lexbuf.lex_curr_pos in
  (* As long as we parse well-formed sequences of tokens, we cannot hit the
     end of the array. *)
  assert (pos < Array.length tokens);
  let token = Array.unsafe_get tokens pos in
  lexbuf.lex_curr_pos <- pos + 1;
  token


let new_lexbuf () =
  let lexbuf = from_string "" in
  lexbuf.lex_start_p <- dummy_pos;
  lexbuf.lex_curr_p <- dummy_pos;
  lexbuf.lex_curr_pos <- 0;
  lexbuf


let () =
  (* Running the parser several times in succession (without re-generating the
     random data, and without explicitly invoking the GC between runs) should
     allow us to obtain slightly more stable timings. *)
  printf "Going to perform %d runs\n" !runs;
  let gc1 = Gc.quick_stat () in
  let times1 = Unix.times () in
  for run = 1 to !runs - 1 do
    ignore (Parser.main lexer (new_lexbuf ()));
    printf "%d\n%!" run
  done;
  let ast =
    let ast = Parser.main lexer (new_lexbuf ()) in
    printf "%d\n%!" !runs;
    ast
  in
  let times2 = Unix.times () in
  let elapsed = times2.tms_utime -. times1.tms_utime in
  let gc2 = Gc.quick_stat () in
  let ast_size = float_of_int (Obj.reachable_words (Obj.repr ast) * !runs) in
  let minor = gc2.minor_words -. gc1.minor_words
  and major = gc2.major_words -. gc1.major_words
  and promoted = gc2.promoted_words -. gc1.promoted_words in
  let runs = float_of_int !runs in
  eprintf "tokens: %d\n" (Array.length tokens);
  eprintf "time: %f\n" (elapsed /. runs);
  eprintf "minor: %f\n" (minor /. runs);
  eprintf "major: %f\n" (major /. runs);
  eprintf "promoted: %f\n" (promoted /. runs);
  eprintf "ast: %f\n" (ast_size /. runs)
