(* This module implements sort inference. *)

(* -------------------------------------------------------------------------- *)

(* The syntax of sorts is:

     sort ::= (sort, ..., sort) -> *

   where the arity (the number of sorts on the left-hand side of the arrow)
   can be zero. *)

module S = struct

  type 'a structure =
    | Arrow of 'a list

  let map f (Arrow xs) =
    Arrow (List.map f xs)

  let iter f (Arrow xs) =
    List.iter f xs

  exception Iter2

  let iter2 f (Arrow xs1) (Arrow xs2) =
    let n1 = List.length xs1
    and n2 = List.length xs2 in
    if n1 = n2 then
      List.iter2 f xs1 xs2
    else
      raise Iter2 (* TEMPORARY *)

end

include S

(* -------------------------------------------------------------------------- *)

(* Instantiate the unification algorithm with the above signature. *)

include Unifier.Make(S)

type sort = term =
  | TVar of int
  | TNode of sort structure

(* -------------------------------------------------------------------------- *)

(* Sort constructors. *)

let arrow (args : variable list) : variable =
  fresh (Some (Arrow args))

let star : variable =
  arrow []

let fresh () =
  fresh None

(* Sort accessors. *)

let domain (x : variable) : variable list option =
  match structure x with
  | Some (Arrow xs) ->
      Some xs
  | None ->
      None

(* -------------------------------------------------------------------------- *)

(* A printer. *)

let rec print (b : Buffer.t) (sort : sort) =
  match sort with
  | TVar x ->
      Printf.bprintf b "?sort%02d" x
  | TNode (S.Arrow []) ->
      Printf.bprintf b "*"
  | TNode (S.Arrow (sort :: sorts)) ->
      (* Always parenthesize the domain, so there is no ambiguity. *)
      Printf.bprintf b "(%a%a) -> *"
        print sort
        print_comma_sorts sorts

and print_comma_sorts b sorts =
  List.iter (print_comma_sort b) sorts

and print_comma_sort b sort =
  Printf.bprintf b ", %a" print sort

let print sort : string =
  let b = Buffer.create 32 in
  print b sort;
  Buffer.contents b
