(* ------------------------------------------------------------------------- *)

(* Propositional formulae. *)

type 'leaf formula =
  | FFalse
  | FTrue
  | FLeaf of 'leaf
  | FNot of 'leaf formula
  | FAnd of 'leaf formula * 'leaf formula
  | FOr of 'leaf formula * 'leaf formula

(* ------------------------------------------------------------------------- *)

(* Building up formulae (smart constructors). *)

let falsity =
  FFalse

let truth =
  FTrue

let leaf x =
  FLeaf x

let negation = function
  | FTrue ->
      FFalse
  | FFalse ->
      FTrue
  | f ->
      FNot f

let disjunction f1 f2 =
  match f1, f2 with
  | FFalse, f
  | f, FFalse ->
      f
  | FTrue, _
  | _, FTrue ->
      FTrue
  | _, _ ->
      FOr (f1, f2)

let conjunction f1 f2 =
  match f1, f2 with
  | FFalse, _
  | _, FFalse ->
      FFalse
  | FTrue, f
  | f, FTrue ->
      f
  | _, _ ->
      FAnd (f1, f2)

let implication f1 f2 =
  disjunction (negation f1) f2

let nonimplication f1 f2 =
  conjunction f1 (negation f2)

let iff f1 f2 =
  conjunction (implication f1 f2) (implication f2 f1)

let conditional f f1 f2 =
  disjunction
    (conjunction f f1)
    (conjunction (negation f) f2)

let fold_map operator neutral f xs =
  List.fold_left (fun accu x ->
    operator accu (f x)
  ) neutral xs

let conjunction_map f xs =
  fold_map conjunction truth f xs

let disjunction_map f xs =
  fold_map disjunction falsity f xs

(* ------------------------------------------------------------------------- *)

(* Converting a closed formula to a Boolean value. This function fails
   if the formula is not closed. *)

let distinguish = function
  | FTrue ->
      true
  | FFalse ->
      false
  | _ ->
      assert false

(* ------------------------------------------------------------------------- *)

(* Conversion to NNF -- pushes all negations to the leaves. *)

let rec nnf (f : 'leaf formula) =
  match f with
  | FFalse
  | FTrue
  | FLeaf _ ->
      f
  | FNot f ->
      nnfnot f
  | FAnd (f1, f2) ->
      FAnd (nnf f1, nnf f2)
  | FOr (f1, f2) ->
      FOr (nnf f1, nnf f2)

and nnfnot (f : 'leaf formula) =
  match f with
  | FFalse ->
      FTrue
  | FTrue ->
      FFalse
  | FLeaf _ ->
      FNot f
  | FNot f ->
      nnf f
  | FAnd (f1, f2) ->
      FOr (nnfnot f1, nnfnot f2)
  | FOr (f1, f2) ->
      FAnd (nnfnot f1, nnfnot f2)

(* ------------------------------------------------------------------------- *)

(* Printing formulae (using the syntax of Boolean algebra). *)

open Print
open Printf

let rec print0 (pl : 'leaf printer) buffer (f : 'leaf formula) =
  match f with
  | FFalse ->
      bprintf buffer "false"
  | FTrue ->
      bprintf buffer "true"
  | FLeaf x ->
      bprintf buffer "(%a)" pl x
  | FNot f ->
      bprintf buffer "not %a" (print0 pl) f
  | _ ->
      bprintf buffer "(%a)" (print pl) f

and print1 pl buffer f =
  match f with
  | FAnd (f1, f2) ->
      bprintf buffer "%a and %a" (print1 pl) f1 (print1 pl) f2
  | _ ->
      print0 pl buffer f

and print2 pl buffer f =
  match f with
  | FOr (f1, f2) ->
      bprintf buffer "%a or %a" (print2 pl) f1 (print2 pl) f2
  | _ ->
      print1 pl buffer f

and print pl buffer f =
  print2 pl buffer f

(* ------------------------------------------------------------------------- *)

(* Substitution of formulas for leaves. *)

type ('a, 'b) substitution =
    'a -> 'b formula

let rec substitute (s : ('a, 'b) substitution) (f : 'a formula) : 'b formula =
  match f with
  | FFalse ->
      FFalse
  | FTrue ->
      FTrue
  | FLeaf x ->
      s x
  | FNot f ->
      negation (substitute s f)
  | FAnd (f1, f2) ->
      conjunction (substitute s f1) (substitute s f2)
  | FOr (f1, f2) ->
      disjunction (substitute s f1) (substitute s f2)

let map (s : 'a -> 'b) (f : 'a formula) : 'b formula =
  substitute (fun x ->
    leaf (s x)
  ) f

type ('a, 'b) polarized_substitution =
    bool -> 'a -> 'b formula

let rec polarized_substitute (s : ('a, 'b) polarized_substitution) (p : bool) (f : 'a formula) : 'b formula =
  match f with
  | FFalse ->
      FFalse
  | FTrue ->
      FTrue
  | FLeaf x ->
      s p x
  | FNot f ->
      negation (polarized_substitute s (not p) f)
  | FAnd (f1, f2) ->
      conjunction (polarized_substitute s p f1) (polarized_substitute s p f2)
  | FOr (f1, f2) ->
      disjunction (polarized_substitute s p f1) (polarized_substitute s p f2)

(* ------------------------------------------------------------------------- *)

(* Iteration over all leaves. *)

let rec fold g f accu =
  match f with
  | FFalse
  | FTrue ->
      accu
  | FLeaf x ->
      g x accu
  | FNot f ->
      fold g f accu
  | FAnd (f1, f2)
  | FOr (f1, f2) ->
      fold g f1 (fold g f2 accu)

