(* This is an enriched version of [CompletedNat], where we compute not just
   numbers, but also lists of matching length. During the fixed point
   computation, instead of manipulating actual lists, we manipulate only
   recipes for constructing lists. These recipes can be evaluated by the user
   after the fixed point has been reached. *)

(* A property is either [Finite (n, xs)], where [n] is a natural number and
   [xs] is a (recipe for constructing a) list of length [n]; or [Infinity]. *)

type 'a t =
| Finite of int * 'a list Lazy.t
| Infinity

let equal p1 p2 =
  match p1, p2 with
  | Finite (i1, _), Finite (i2, _) ->
      i1 = i2
  | Infinity, Infinity ->
      true
  | _, _ ->
      false

let bottom =
  Infinity

let is_maximal p =
  match p with
  | Finite (0, _) ->
      true
  | _ ->
      false

let min p1 p2 =
  match p1, p2 with
  | Finite (i1, _), Finite (i2, _) ->
      if i1 <= i2 then p1 else p2
  | p, Infinity
  | Infinity, p ->
      p

let min_lazy p1 p2 =
  match p1 with
  | Finite (0, _) ->
      p1
  | _ ->
      min p1 (Lazy.force p2)

let add p1 p2 =
  match p1, p2 with
  | Finite (i1, xs1), Finite (i2, xs2) ->
      Finite (
        i1 + i2,
        (* The only list operation in the code! *)
        lazy (Lazy.force xs1 @ Lazy.force xs2)
      )
  | _, _ ->
      Infinity

let add_lazy p1 p2 =
  match p1 with
  | Infinity ->
      Infinity
  | _ ->
      add p1 (Lazy.force p2)

let print conv p =
  match p with
  | Finite (i, xs) ->
      string_of_int i ^ " " ^
      String.concat " " (List.map conv (Lazy.force xs))
  | Infinity ->
      "infinity"
