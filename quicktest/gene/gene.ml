open Printf
open Parser
open Stream
open Generator

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

(* Hooking the stream to the parser. *)

let parse xs =
  MenhirLib.Convert.Simplified.traditional2revised Parser.main
    (fresh (map (fun token -> (token, Lexing.dummy_pos, Lexing.dummy_pos)) xs))

let () =
  let i : int = parse (produce 10000000) in
  printf "%d\n%!" i

