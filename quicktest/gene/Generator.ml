(* A generator of well-formed arithmetic expressions, for use as test input
   for the parser. *)

(* -------------------------------------------------------------------------- *)

(* A tiny library of finite or infinite streams. *)

type 'a stream =
  unit -> 'a answer

and 'a answer =
  | Done
  | More of 'a * 'a stream

let empty () =
  Done

let cons x ys () =
  More (x, ys)

let singleton x =
  cons x empty

let rec (++) xs ys () =
  match xs() with
  | More (x, xs) ->
      More (x, xs ++ ys)
  | Done ->
      ys()

(* -------------------------------------------------------------------------- *)

(* A random generator of well-formed arithmetic expressions. *)

open Parser

(* [split n] produces two numbers [n1] and [n2] comprised between [0] and [n]
   (inclusive) whose sum is [n]. *)

let split n =
  let n1 = Random.int (n + 1) in
  let n2 = n - n1 in
  n1, n2

(* [produce n k] produces a random arithmetic expression with exactly
   [n] internal nodes. It is produced directly as a stream of tokens,
   and is concatenated with the stream [k] -- this allows an efficient
   formulation that does not use the stream concatenation operator. *)

let rec produce n (k : token stream) : token stream =
  if n = 0 then
    cons (INT 0) k
  else
    match Random.int 6 with
    | 0 ->
        (* Parentheses. *)
        cons LPAREN (produce (n - 1) (cons RPAREN k))
    | 1 ->
        (* Plus. *)
        let n1, n2 = split (n - 1) in
        produce n1 (cons PLUS (produce n2 k))
    | 2 ->
        (* Minus. *)
        let n1, n2 = split (n - 1) in
        produce n1 (cons MINUS (produce n2 k))
    | 3 ->
        (* Times. *)
        let n1, n2 = split (n - 1) in
        produce n1 (cons TIMES (produce n2 k))
    | 4 ->
        (* Div. *)
        let n1, n2 = split (n - 1) in
        produce n1 (cons DIV (produce n2 k))
    | 5 ->
        (* Unary minus. *)
        cons MINUS (produce (n - 1) k)
    | _ ->
        assert false

