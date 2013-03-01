let force =
  Lazy.force

(* A data structure for streams with memoization. *)

type 'a stream =
  'a cell Lazy.t

and 'a cell =
  | Nil
  | Cons of 'a * 'a stream

(* Construction. *)

let nil : 'a stream =
  lazy Nil

let cons (x : 'a) (xs : 'a stream) : 'a stream =
  lazy (Cons (x, xs))

(* Destruction. *)

let extract xs =
  match force xs with
  | Nil ->
      raise Not_found
  | Cons (x, xs) ->
      x, xs

(* Reversal. *)

(* [rev r f] is the reverse of [r] concatenated with [f]. Its cost
   (which is paid up front) is linear in the size of [r]. *)

let rec rev (r : 'a list) (f : 'a cell) : 'a cell =
  match r with
  | [] ->
      f
  | x :: r ->
      rev r (Cons (x, lazy f))

(* [rev r] is the reverse of [r]. The cost of forcing the first
   cell is linear in the size of [r], while the cost of forcing
   every other cell is zero. *)

let rev (r : 'a list) : 'a stream =
  lazy (rev r Nil)

(* Concatenation. *)

let rec (++) (xs : 'a stream) (ys : 'a stream) : 'a stream =
  lazy (
    match force xs with
    | Cons (x, xs) ->
	Cons (x, xs ++ ys)
    | Nil ->
	force ys
  )

(* Truncation. *)

let rec take (n : int) (xs : 'a stream) : 'a stream =
  if n = 0 then
    nil
  else lazy (
    match force xs with
    | Nil ->
	Nil
    | Cons (x, xs) ->
	Cons (x, take (n-1) xs)
  )

let rec drop (n : int) (xs : 'a stream) : 'a cell =
  match n, force xs with
  | 0, c ->
      c
  | _, Nil ->
      Nil
  | n, Cons (x, xs) ->
      drop (n-1) xs

let drop (n : int) (xs : 'a stream) : 'a stream =
  lazy (drop n xs)

