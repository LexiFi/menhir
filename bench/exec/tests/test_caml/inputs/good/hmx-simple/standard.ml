(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/standard.ml,v 1.1 2000/06/15 17:01:11 fpottier Exp $ *)

(* Additions to Objective Caml's standard library. *)

(* [fork f l] applies the function [f] to every element of the list [l], producing a pair; it returns two lists,
   obtained by collecting the first (resp. second) components of these pairs. The order of the result lists is
   unspecified. *)

let fork f =
  let rec fork lleft lright = function
    | [] ->
	lleft, lright
    | element :: rest ->
	let left, right = f element in
	fork (left :: lleft) (right :: lright) rest in
  fork [] []

