type 'a t =
| Reachable of 'a list Lazy.t
| Unreachable

let equal p1 p2 =
  match p1, p2 with
  | Reachable _, Reachable _
  | Unreachable, Unreachable ->
      true
  | _, _ ->
      false

let bottom =
  Unreachable

let epsilon =
  Reachable (lazy [])

let singleton x =
  Reachable (lazy [x])

let is_maximal p =
  match p with
  | Reachable _ ->
      true
  | _ ->
      false

let min p1 p2 =
  match p1, p2 with
  | Reachable _, _ ->
       p1
  | Unreachable, p ->
      p

let min_lazy p1 p2 =
  match p1 with
  | Reachable _ ->
      p1
  | Unreachable ->
      Lazy.force p2

let add p1 p2 =
  match p1, p2 with
  | Reachable xs1, Reachable xs2 ->
      Reachable (
        (* The only list operation in the code! *)
        lazy (Lazy.force xs1 @ Lazy.force xs2)
      )
  | _, _ ->
      Unreachable

let add_lazy p1 p2 =
  match p1 with
  | Unreachable ->
      Unreachable
  | Reachable _ ->
      add p1 (Lazy.force p2)

let print conv p =
  match p with
  | Reachable xs ->
      String.concat " " (List.map conv (Lazy.force xs))
  | Unreachable ->
      "unreachable"
