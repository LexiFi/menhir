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

let rec fold_left accu f xs =
  match xs() with
  | Done ->
      accu
  | More (x, xs) ->
    let accu = f accu x in
    fold_left accu f xs

let iter f xs =
  fold_left () (fun () x -> f x) xs

let rec map f xs () =
  match xs() with
  | Done ->
      Done
  | More (x, xs) ->
      More (f x, map f xs)

(* An infinite, imperative stream. *)

type 'a infinite_imperative_stream =
  unit -> 'a

let fresh (xs : 'a stream) : 'a infinite_imperative_stream =
  let r = ref xs in
  fun () ->
    match !r() with
    | Done ->
        raise End_of_file
    | More (x, xs) ->
        r := xs;
        x

