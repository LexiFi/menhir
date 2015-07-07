(* This is an enriched version of [CompletedNat], where we compute not just
   numbers, but also sequences of matching length. *)

(* A property is either [Finite (n, xs)], where [n] is a natural number and
   [xs] is a sequence of length [n]; or [Infinity]. *)

type 'a t =
| Finite of int * 'a Seq.seq
| Infinity

let equal p1 p2 =
  match p1, p2 with
  | Finite (i1, _), Finite (i2, _) ->
      i1 = i2
  | Infinity, Infinity ->
      true
  | _, _ ->
      false

let compare p1 p2 =
  match p1, p2 with
  | Finite (i1, _), Finite (i2, _) ->
      if i1 < i2 then -1 else if i1 = i2 then 0 else 1
  | Infinity, Infinity ->
      0
  | Finite _, Infinity ->
      -1
  | Infinity, Finite _ ->
      1

let bottom =
  Infinity

let epsilon =
  Finite (0, Seq.empty)

let singleton x =
  Finite (1, Seq.singleton x)

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
      min p1 (p2())

let min_cutoff p1 p2 =
  match p1 with
  | Finite (0, _) ->
      p1
  | Finite (i1, _) ->
      (* Pass [i1] as a cutoff value to [p2]. *)
      min p1 (p2 i1)
  | Infinity ->
      (* Pass [max_int] to indicate no cutoff. *)
      p2 max_int

let add p1 p2 =
  match p1, p2 with
  | Finite (i1, xs1), Finite (i2, xs2) ->
      Finite (i1 + i2, Seq.append xs1 xs2)
  | _, _ ->
      Infinity

let add_lazy p1 p2 =
  match p1 with
  | Infinity ->
      Infinity
  | _ ->
      add p1 (p2())

let add_cutoff cutoff p1 p2 =
  match p1 with
  | Infinity ->
      Infinity
  | Finite (i1, _) ->
      if cutoff <= i1 then
        (* Cut. No need to evaluate [p2]. *)
        Infinity
      else
        (* Evaluate [p2], with an adjusted cutoff value. *)
        add p1 (p2 (cutoff - i1))

let print conv p =
  match p with
  | Finite (i, xs) ->
      string_of_int i ^ " " ^
      String.concat " " (List.map conv (Seq.elements xs))
  | Infinity ->
      "infinity"

let to_int p =
  match p with
  | Finite (i, _) ->
      i
  | Infinity ->
      max_int

let extract p =
  match p with
  | Finite (_, xs) ->
      Seq.elements xs
  | Infinity ->
      assert false
