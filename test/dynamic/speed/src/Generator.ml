(* A random generator of well-formed arithmetic expressions, for use as test
   input for the parser. *)

open Sek
open Parser

(* [split n] produces two numbers [n1] and [n2] comprised between [0] and [n]
   (inclusive) whose sum is [n]. *)

let split n =
  let n1 = Random.int (n + 1) in
  let n2 = n - n1 in
  n1, n2

(* [produce n buffer] produces a random arithmetic expression with exactly [n]
   internal nodes. The tokens are prepended in front of the sequence [buffer].
   (We choose to prepend, instead of appending, so as to produce exactly the
   same result as in the previous formulation of this code, where we used a
   linked stream instead of a sequence.) *)

(* We do not produce divisions because they are of no grammatical
   interest (we already have multiplication) and they can cause
   early termination of the parser due to a division by zero (the
   parser performs evaluation on the fly!). *)

let rec produce (buffer : token E.t) n : unit =
  let push token = E.push front buffer token
  and produce n = produce buffer n in
  if n = 0 then
    let i = Random.int 10 in
    push (INT i)
  else
    match Random.int 5 with
    | 0 ->
        (* Parentheses. *)
        push RPAREN;
        produce (n - 1);
        push LPAREN
    | 1 ->
        (* Plus. *)
        let n1, n2 = split (n - 1) in
        produce n2;
        push PLUS;
        produce n1
    | 2 ->
        (* Minus. *)
        let n1, n2 = split (n - 1) in
        produce n2;
        push MINUS;
        produce n1
    | 3 ->
        (* Times. *)
        let n1, n2 = split (n - 1) in
        produce n2;
        push TIMES;
        produce n1
    | 4 ->
        (* Unary minus. *)
        produce (n - 1);
        push MINUS
    | _ ->
        assert false

(* We finish with an EOL. *)

let produce n : token array =
  let dummy = EOL in
  let buffer = E.create dummy in
  produce buffer n;
  E.push back buffer EOL;
  E.to_array buffer
