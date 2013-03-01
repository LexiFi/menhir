(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/baltree7.ml,v 1.3.8.6 1999/02/25 14:08:52 francois Exp $ *)

(* Height-balanced binary trees.
   These are binary trees such that the heights of the children
   differ by at most 2. *)

type 'a t = Empty | Node of 'a t * 'a * 'a t * int;;

type 'a contents = Nothing | Something of 'a;;

(* Compute the height of a tree. *)

let height = function
    Empty -> 0
  | Node(_, _, _, h) -> h;;

(* Creates a new node with left son l, value x and right son r.
   l and r must be balanced and | height l - height r | <= 2.
   Inline expansion of height for better speed. *)

let create l x r =
  let sl = match l with Empty -> 0 | Node(_,_,_,s) -> s in
  let sr = match r with Empty -> 0 | Node(_,_,_,s) -> s in
  Node(l, x, r, (if sl >= sr then succ sl else succ sr));;

(* Same as create, but performs one step of rebalancing if necessary.
   Assumes l and r balanced.
   Inline expansion of create for better speed in the most frequent case
   where no rebalancing is required. *)

let bal l x r =
  let sl = match l with Empty -> 0 | Node(_,_,_,s) -> s in
  let sr = match r with Empty -> 0 | Node(_,_,_,s) -> s in
  if sl > sr + 2 then begin
    match l with
      Empty -> invalid_arg "baltree__bal"
    | Node(ll, lv, lr, _) ->
        if height ll >= height lr then
          create ll lv (create lr x r)
        else begin
          match lr with
            Empty -> invalid_arg "baltree__bal"
          | Node(lrl, lrv, lrr, _)->
              create (create ll lv lrl) lrv (create lrr x r)
        end
  end else if sr > sl + 2 then begin
    match r with
      Empty -> invalid_arg "baltree__bal"
    | Node(rl, rv, rr, _) ->
        if height rr >= height rl then
          create (create l x rl) rv rr
        else begin
          match rl with
            Empty -> invalid_arg "baltree__bal"
          | Node(rll, rlv, rlr, _) ->
              create (create l x rll) rlv (create rlr rv rr)
        end
  end else
    Node(l, x, r, (if sl >= sr then succ sl else succ sr));;

(* Same as bal, but repeat rebalancing until the final result is balanced. *)

let rec join l x r =
  match bal l x r with
    Empty -> invalid_arg "baltree__join"
  | Node(l', x', r', _) as t' ->
      let d = height l' - height r' in
      if d < -2 or d > 2 then join l' x' r' else t'
;;

(* Merge two trees l and r into one.
   All elements of l must precede the elements of r.
   Assumes | height l - height r | <= 2. *)

let rec merge t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      bal l1 v1 (bal (merge r1 l2) v2 r2)
;;

(* Same as merge, but does not assume anything about l and r. *)

let rec concat t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
      join l1 v1 (join (concat r1 l2) v2 r2)
;;

(* Insertion *)

let add searchpred x t =
  let rec add = function
    Empty ->
      Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
      let c = searchpred v in
      if c == 0 then t else
      if c < 0 then begin
	let l' = add l in
	if l == l' then t else bal l' v r
      end
      else begin
	let r' = add r in
	if r == r' then t else bal l v r'
      end
  in add t
;;

exception StrictAdd

let strict_add searchpred x t =
  let rec add = function
    Empty ->
      Node(Empty, x, Empty, 1)
  | Node(l, v, r, _) as t ->
      let c = searchpred v in
      if c == 0 then raise StrictAdd else
      if c < 0 then bal (add l) v r else bal l v (add r)
  in add t
;;

let smart_add searchpred decision x t =
  let rec add = function
    Empty ->
      Node(Empty, x, Empty, 1)
  | Node(l, v, r, height) as t ->
      let c = searchpred v in
      if c == 0 then
	let choice = decision v x in
	if choice == v then t
	else Node(l, choice, r, height)
      else
	if c < 0 then begin
	  let l' = add l in
	  if l == l' then t else bal l' v r
	end
	else begin
	  let r' = add r in
	  if r == r' then t else bal l v r'
	end
  in add t
;;

(* Membership *)

let contains searchpred t =
  let rec contains = function
    Empty -> false
  | Node(l, v, r, _) ->
      let c = searchpred v in
      if c == 0 then true else
      if c < 0 then contains l else contains r
  in contains t
;;

(* Search *)

let find searchpred t =
  let rec find = function
    Empty ->
      raise Not_found
  | Node(l, v, r, _) ->
      let c = searchpred v in
      if c == 0 then v else
      if c < 0 then find l else find r
  in find t
;;

let find_greatest searchpred t =
  let rec find = function
    Empty ->
      raise Not_found
  | Node(l, v, r, _) ->
      let c = searchpred v in
      if c == 0 then begin
	try
	  find r
	with Not_found ->
	  v
      end
      else
      if c < 0 then find l else find r
  in find t
;;

(* Deletion *)

let remove searchpred t =
  let rec remove = function
    Empty ->
      Empty
  | Node(l, v, r, _) as t ->
      let c = searchpred v in
      if c == 0 then merge l r else
      if c < 0 then begin
	let l' = remove l in
	if l == l' then t else bal l' v r
      end
      else begin
	let r' = remove r in
	if r == r' then t else bal l v r'
      end
  in remove t
;;

(* Modification *)

let modify searchpred modifier t =
  let rec modify = function
    Empty ->
      begin match modifier Nothing with
        Nothing -> Empty
      | Something v -> Node(Empty, v, Empty, 1)
      end
  | Node(l, v, r, s) as t ->
      let c = searchpred v in
      if c == 0 then
        begin match modifier(Something v) with
          Nothing -> merge l r
        | Something v' -> if v == v' then t else Node(l, v', r, s)
        end
      else if c < 0 then begin
	let l' = modify l in
	if l == l' then t else bal l' v r
      end
      else begin
	let r' = modify r in
	if r == r' then t else bal l v r'
      end
  in modify t
;;

(* Splitting *)

let split searchpred =
  let rec split = function
    Empty ->
      (Empty, Nothing, Empty)
  | Node(l, v, r, _) ->
      let c = searchpred v in
      if c == 0 then (l, Something v, r)
      else if c < 0 then
        let (ll, vl, rl) = split l in (ll, vl, join rl v r)
      else
        let (lr, vr, rr) = split r in (join l v lr, vr, rr)
  in split
;;

(* Comparison (by lexicographic ordering of the fringes of the two trees). *)

let compare cmp s1 s2 =
  let rec compare_aux l1 l2 =
  match (l1, l2) with
    ([], []) -> 0
  | ([], _)  -> -1
  | (_, []) -> 1
  | (Empty::t1, Empty::t2) ->
      compare_aux t1 t2
  | (Node(Empty, v1, r1, _) :: t1, Node(Empty, v2, r2, _) :: t2) ->
      let c = cmp v1 v2 in
      if c != 0 then c else compare_aux (r1::t1) (r2::t2)
  | (Node(l1, v1, r1, _) :: t1, t2) ->
      compare_aux (l1 :: Node(Empty, v1, r1, 0) :: t1) t2
  | (t1, Node(l2, v2, r2, _) :: t2) ->
      compare_aux t1 (l2 :: Node(Empty, v2, r2, 0) :: t2)
  in
    compare_aux [s1] [s2];;

let equal eq s1 s2 =
  let rec equal_aux l1 l2 =
  match (l1, l2) with
    ([], []) -> true
  | ([], _)
  | (_, []) -> false
  | (Empty::t1, Empty::t2) ->
      equal_aux t1 t2
  | (Node(Empty, v1, r1, _) :: t1, Node(Empty, v2, r2, _) :: t2) ->
      if eq v1 v2 then equal_aux (r1::t1) (r2::t2)
      else false
  | (Node(l1, v1, r1, _) :: t1, t2) ->
      equal_aux (l1 :: Node(Empty, v1, r1, 0) :: t1) t2
  | (t1, Node(l2, v2, r2, _) :: t2) ->
      equal_aux t1 (l2 :: Node(Empty, v2, r2, 0) :: t2)
  in
    equal_aux [s1] [s2];;
