(* Sequences with constant time concatenation and linear-time conversion
   to an ordinary list. *)

type 'a seq =
| SZero
| SOne of 'a
| SConcat of 'a seq * 'a seq

let empty =
  SZero

let singleton x =
  SOne x

let append xs ys =
  SConcat (xs, ys)

let rec elements xs accu =
  match xs with
  | SZero ->
      accu
  | SOne x ->
      x :: accu
  | SConcat (xs1, xs2) ->
      elements xs1 (elements xs2 accu)

let elements xs =
  elements xs []
