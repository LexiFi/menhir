(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/set7.ml,v 1.11.6.9 1999/04/05 16:45:39 francois Exp $ *)

(* Sets over ordered types *)

open Errors
open Baltree7

(* Sets are represented by AVL trees. *)

type 'a t = {
    tree: 'a Baltree7.t;
    mutable order: ('a -> 'a -> int) option
  }

let mko = function
    Some f ->
      f
  | None ->
      raise (CantHappen "No ordering in Set7.mko.")

let get_order s =
  mko s.order

let empty ord =
  { tree = Empty; order = Some ord }

let is_empty = function
    { tree = Empty } -> true
  | _ -> false

let mem x { tree = tree; order = order } =
  Baltree7.contains ((mko order) x) tree;;

let add x ({ tree = tree; order = order } as s) =
  let tree' = Baltree7.add ((mko order) x) x tree in
  if tree == tree' then s
  else { tree = tree'; order = order }

exception StrictAdd

let strict_add x { tree = tree; order = order } =
  try
    { tree = Baltree7.strict_add ((mko order) x) x tree; order = order }
  with Baltree7.StrictAdd ->
    raise StrictAdd;;

let smart_add decision x ({ tree = tree; order = order } as s) =
  let tree' = Baltree7.smart_add ((mko order) x) decision x tree in
  if tree == tree' then s
  else { tree = tree'; order = order }

let remove x ({ tree = tree; order = order } as s) =
  let tree' = Baltree7.remove ((mko order) x) tree in
  if tree == tree' then s
  else { tree = tree'; order = order }

let union s1 s2 =
  let order = get_order s1 in
  let rec union = fun t1 t2 -> match (t1, t2) with
    (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
      let (l2, _, r2) = Baltree7.split (order v1) t2 in
      Baltree7.join (union l1 l2) v1 (union r1 r2) in
  { tree = union s1.tree s2.tree; order = s1.order };;

let smart_union f s1 s2 =
  let order = get_order s1 in
  let rec union = fun t1 t2 -> match (t1, t2) with
    (Empty, t2) -> t2
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
      let (l2, v2, r2) = Baltree7.split (order v1) t2 in
      let v = match v2 with
        Nothing -> v1
      | Something v2 -> f v1 v2 in
      Baltree7.join (union l1 l2) v (union r1 r2) in
  { tree = union s1.tree s2.tree; order = s1.order };;

let inter s1 s2 =
  let order = get_order s1 in
  let rec inter = fun t1 t2 -> match (t1, t2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
      match Baltree7.split (order v1) t2 with
        (l2, Nothing, r2) ->
          Baltree7.concat (inter l1 l2) (inter r1 r2)
      | (l2, Something _, r2) ->
          Baltree7.join (inter l1 l2) v1 (inter r1 r2) in
  { tree = inter s1.tree s2.tree; order = s1.order };;

let smart_inter f s1 s2 =
  let order = get_order s1 in
  let rec inter = fun t1 t2 -> match (t1, t2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> Empty
  | (Node(l1, v1, r1, _), t2) ->
      match Baltree7.split (order v1) t2 with
        (l2, Nothing, r2) ->
          Baltree7.concat (inter l1 l2) (inter r1 r2)
      | (l2, Something v2, r2) ->
          Baltree7.join (inter l1 l2) (f v1 v2) (inter r1 r2) in
  { tree = inter s1.tree s2.tree; order = s1.order };;

let smart_subset link witness accu s1 s2 =
  let order = get_order s1 in
  let rec incl accu t1 t2 = match (t1, t2) with
    (Empty, t2) -> accu
  | (Node(_, v, _, _), Empty) -> witness v
  | (Node(l1, v1, r1, _), t2) ->
      match Baltree7.split (order v1) t2 with
        (_, Nothing, _) ->
	  witness v1
      | (l2, Something v2, r2) ->
          incl (incl (link accu v1 v2) l1 l2) r1 r2 in
  incl accu s1.tree s2.tree;;

let diff s1 s2 =
  let order = get_order s1 in
  let rec diff = fun t1 t2 -> match (t1, t2) with
    (Empty, t2) -> Empty
  | (t1, Empty) -> t1
  | (Node(l1, v1, r1, _), t2) ->
      match Baltree7.split (order v1) t2 with
        (l2, Nothing, r2) ->
          Baltree7.join (diff l1 l2) v1 (diff r1 r2)
      | (l2, Something _, r2) ->
          Baltree7.concat (diff l1 l2) (diff r1 r2) in
  { tree = diff s1.tree s2.tree; order = s1.order };;

let compare s1 s2 =
  Baltree7.compare (get_order s1) s1.tree s2.tree;;

let equal s1 s2 =
  compare s1 s2 == 0;;

let smart_equal eq s1 s2 =
  Baltree7.equal eq s1.tree s2.tree;;

let iter f s =
  let rec iter = function
    Empty -> ()
  | Node(l, v, r, _) -> iter l; f v; iter r
  in iter s.tree;;

let fold f s init =
  let rec fold accu = function
    Empty -> accu
  | Node(l, v, r, _) -> fold (f v (fold accu r)) l
  in fold init s.tree;;

let elements s =
  let rec elements accu = function
    Empty -> accu
  | Node(l, v, r, _) -> elements (v :: elements accu r) l
  in elements [] s.tree;;

let choose s =
  match s.tree with
    Empty -> raise Not_found
  | Node(_, v, _, _) -> v

(* Home-brewed additions, by Francois Pottier *)

let choosep predicate s =
  let rec choose_aux = function
    Empty -> raise Not_found
  | Node(l, v, r, _) ->
      if (predicate v) then v
      else try
        choose_aux l
      with Not_found ->
        choose_aux r
  in choose_aux s.tree
;;

let cardinality s =
  let rec count_aux = function
    Empty -> 0
  | Node(l, v, r, _) ->
      count_aux l + 1 + count_aux r
  in count_aux s.tree
;;

let memp f s =
  Baltree7.find f s.tree
;;

let memp_greatest f s =
  Baltree7.find_greatest f s.tree
;;

let map new_order f s =
  fold (fun elem accu -> add (f elem) accu) s (empty new_order)
;;

let endo_map f s =
  fold (fun elem accu -> add (f elem) accu) s (empty (get_order s))
;;

let monotonous_map new_order f s =
  let rec map = function
      Empty -> Empty
    | Node(l, v, r, h) -> Node(map l, f v, map r, h) in
  { order = Some new_order; tree = map s.tree }
;;

let monotonous_endo_map f s =
  let rec map = function
      Empty -> Empty
    | Node(l, v, r, h) as node ->
	let l' = map l
        and r' = map r
        and v' = f v in
	if (l == l') & (r == r') & (v == v') then node else Node(l', v', r', h) in
  let tree = s.tree in
  let tree' = map tree in
  if tree == tree' then s else { order = s.order; tree = tree' }
;;

let filter predicate s =
  let emptyset = empty (get_order s) in

  let unchanged, yes = fold (fun elem (unchanged, yes) ->
    if predicate elem then
      unchanged, add elem yes
    else
      false, yes
  ) s (true, emptyset) in

  if unchanged then s else yes (* This is supposed to help preserve sharing and enhance GC behavior. *)
;;

let smallest s =
  let rec smallest_aux = function
    Empty ->
      raise Not_found
  | Node(Empty, v, _, _) ->
      v
  | Node(l, _, _, _) ->
      smallest_aux l
  
  in smallest_aux s.tree
;;

exception NonEmpty

let subset s1 s2 =
  let order = get_order s1 in
  let rec diff t1 t2 = match (t1, t2) with
    (Empty, t2) -> ()
  | (t1, Empty) -> raise NonEmpty
  | (Node(l1, v1, r1, _), t2) ->
      match Baltree7.split (order v1) t2 with
        (_, Nothing, _) ->
          raise NonEmpty
      | (l2, Something _, r2) ->
          diff l1 l2;
	  diff r1 r2 in
  try
    diff s1.tree s2.tree;
    true
  with NonEmpty ->
    false
;;

let freeze s =
  s.order <- None
;;

let unfreeze s order =
  s.order <- Some order
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Checking whether the intersection of two sets is non-empty. This could be done by using inter and is_empty, but we
would allocate memory needlessly. Using smart_inter would be a lot better. We can do even slightly better by partially
evaluating our call to smart_inter. This is fun.

*)

exception Nonempty

let disjoint s1 s2 =
  let order = get_order s1 in

  let rec look t1 t2 = match (t1, t2) with
    (Empty, t2) -> ()
  | (t1, Empty) -> ()
  | (Node(l1, v1, r1, _), t2) ->
      match Baltree7.split (order v1) t2 with
        (l2, Nothing, r2) ->
	  look l1 l2;
	  look r1 r2
      | (l2, Something v2, r2) ->
	  raise Nonempty in

  try
    look s1.tree s2.tree;
    true
  with Nonempty ->
    false
;;

let fold_aux f s init =
  let rec fold accu = function
    Empty -> accu
  | Node(l, v, r, _) -> fold (f v (fold accu r)) l
  in fold init s
;;

let apply_assoc_op1 operator s =
  match s.tree with
    Empty ->
      raise Not_found
  | Node(l, elem, r, _) ->
      fold_aux operator l (fold_aux operator r elem)
;;

let isolate s =
  let e = choose s in
  e, remove e s
;;

