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

let options = Arg.align [
  "--dry-run", Arg.Set dry_run, "Run only the generator, not the parser";
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
  let seed = 61112962 in
  Random.init seed

let () =
  let tks : token stream = produce 10000000 in
  let tks = fresh (map wrap tks) in
  if !dry_run then begin
    let _ = find (fun _ -> false) tks in
    printf "Done.\n"
  end
  else begin
    let i : int = MenhirLib.Convert.Simplified.traditional2revised Parser.main tks in
    printf "%d\n%!" i
  end

