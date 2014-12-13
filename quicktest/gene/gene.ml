open Printf
open Parser
open Stream
open Generator

(* ---------------------------------------------------------------------------- *)

(* A token printer, for debugging. *)

let print_token = function
  | INT i ->
      printf "%d" i
  | PLUS ->
      printf " + "
  | MINUS ->
      printf " - "
  | TIMES ->
      printf " * "
  | DIV ->
      printf " / "
  | LPAREN ->
      printf "("
  | RPAREN ->
      printf ")"
  | EOL ->
      printf "\n"

let print_token_stream =
  iter print_token

(* ---------------------------------------------------------------------------- *)

(* Parse the command line. *)

(* [--dry-run] offers a choice between running just the generator, or both the
   generator and the parser. *)

let dry_run =
  ref false

(* [--seed] allows the random seed to be set via the command line. *)

let seed =
  ref 61112962

(* [--size] allows the size of the randomly-generated expression to be
   set via the command line. *)

let size =
  ref 10000000

let options = Arg.align [
  "--dry-run", Arg.Set dry_run, "Run only the generator, not the parser";
  "--seed", Arg.Set_int seed, sprintf "<seed> Set the random seed (%d)" !seed;
  "--size", Arg.Set_int size, sprintf "<size> Set the size of the test (%d)" !size;
]

let usage =
  sprintf "Usage: %s <options>" Sys.argv.(0)

let () =
  Arg.parse options (fun _ -> ()) usage

(* ---------------------------------------------------------------------------- *)

(* Run. *)

let wrap token =
  (token, Lexing.dummy_pos, Lexing.dummy_pos)

let () =
  Random.init !seed

let () =
  let tks : token stream = produce !size in
  let tks = fresh (map wrap tks) in
  if !dry_run then begin
    let _ = find (fun _ -> false) tks in
    printf "Done.\n"
  end
  else begin
    let i : int = MenhirLib.Convert.Simplified.traditional2revised Parser.main tks in
    printf "%d\n%!" i
  end

