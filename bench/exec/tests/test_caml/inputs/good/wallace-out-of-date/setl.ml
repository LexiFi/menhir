(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/Attic/setl.ml,v 1.1.2.5 1999/04/05 16:45:40 francois Exp $ *)
(*

Sets over ordered types, implemented using ordered lists.

*)

open Errors

type 'a t = {
    mutable order: ('a -> 'a -> int) option;
    list: 'a list
  } 

let mko = function
    Some f ->
      f
  | None ->
      raise (CantHappen "No ordering in Setl.mko.")

let get_order s =
  mko s.order

let empty order =
  { order = Some order; list = [] }

let is_empty s =
  s.list == []

let mem x s =
  let order = get_order s in
  List.exists (fun y -> order x y = 0) s.list

let add x ({ list = list; order = order } as s) =
  let ord = mko order in
  let rec add = function
      [] ->
	[x]
    | (elem :: rest) as l ->
	let result = ord x elem in
        if result < 0 then x :: l
	else if result = 0 then l
	else begin
	  let rest' = add rest in
	  if rest == rest' then l
	  else elem :: rest'
	end in
  let list' = add list in
  if list == list' then s else { order = order; list = list' }

exception StrictAdd

let strict_add x s =
  let order = get_order s in
  let rec add = function
      [] ->
	[x]
    | (elem :: rest) as l ->
	let result = order x elem in
        if result < 0 then x :: l
	else if result = 0 then raise StrictAdd
	else elem :: (add rest) in
  { order = s.order; list = add s.list }

let smart_add decision x ({ list = list; order = order } as s) =
  let ord = mko order in
  let rec add = function
      [] ->
	[x]
    | (elem :: rest) as l ->
	let result = ord x elem in
        if result < 0 then x :: l
	else if result = 0 then
	  let choice = decision elem x in
	  if choice == elem then l
	  else x :: rest
	else begin
	  let rest' = add rest in
	  if rest == rest' then l
	  else elem :: rest'
	end in
  let list' = add list in
  if list == list' then s else { order = order; list = list' }

let remove x ({ list = list; order = order } as s) =
  let ord = mko order in
  let rec remove = function
      [] ->
	raise Not_found
    | (elem :: rest) as l ->
	let result = ord x elem in
	if result < 0 then raise Not_found
	else if result = 0 then rest
	else begin
	  let rest' = remove rest in
	  if rest == rest' then l
	  else elem :: rest'
	end in
  let list' = remove list in
  if list == list' then s else { order = order; list = list' }

let union s1 s2 =
  let order = get_order s1 in
  let rec union l1 l2 =
    match (l1, l2) with
      _, [] -> l1
    | [], _ -> l2
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let result = order elem1 elem2 in
	if result < 0 then elem1 :: (union rest1 l2)
	else if result = 0 then elem1 :: (union rest1 rest2)
	else elem2 :: (union l1 rest2) in
  { order = s1.order; list = union s1.list s2.list }

let inter s1 s2 =
  let order = get_order s1 in
  let rec inter l1 l2 =
    match (l1, l2) with
      _, []
    | [], _ ->
	[]
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let result = order elem1 elem2 in
	if result < 0 then inter rest1 l2
	else if result = 0 then elem1 :: (inter rest1 rest2)
	else inter l1 rest2 in
  { order = s1.order; list = inter s1.list s2.list }

exception NonEmpty

let disjoint s1 s2 =
  let order = get_order s1 in
  let rec inter l1 l2 =
    match (l1, l2) with
      _, []
    | [], _ ->
	()
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let result = order elem1 elem2 in
	if result < 0 then inter rest1 l2
	else if result = 0 then raise NonEmpty
	else inter l1 rest2 in
  try
    inter s1.list s2.list;
    true
  with NonEmpty ->
    false

let diff s1 s2 =
  let order = get_order s1 in
  let rec diff l1 l2 =
    match (l1, l2) with
      [], _ -> []
    | _, [] -> l1
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let result = order elem1 elem2 in
	if result < 0 then elem1 :: (diff rest1 l2)
	else if result = 0 then diff rest1 rest2
	else diff l1 rest2 in
  { order = s1.order; list = diff s1.list s2.list }

let subset s1 s2 =
  let order = get_order s1 in
  let rec diff l1 l2 =
    match (l1, l2) with
      [], _ -> ()
    | _, [] -> raise NonEmpty
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let result = order elem1 elem2 in
	if result < 0 then raise NonEmpty
	else if result = 0 then diff rest1 rest2
	else diff l1 rest2 in
  try
    diff s1.list s2.list;
    true
  with NonEmpty ->
    false

let smart_union f s1 s2 =
  let order = get_order s1 in
  let rec union l1 l2 =
    match (l1, l2) with
      _, [] -> l1
    | [], _ -> l2
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let result = order elem1 elem2 in
	if result < 0 then elem1 :: (union rest1 l2)
	else if result = 0 then (f elem1 elem2) :: (union rest1 rest2)
	else elem2 :: (union l1 rest2) in
  { order = s1.order; list = union s1.list s2.list }

let smart_inter f s1 s2 =
  let order = get_order s1 in
  let rec inter l1 l2 =
    match (l1, l2) with
      _, []
    | [], _ ->
	[]
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let result = order elem1 elem2 in
	if result < 0 then inter rest1 l2
	else if result = 0 then (f elem1 elem2) :: (inter rest1 rest2)
	else inter l1 rest2 in
  { order = s1.order; list = inter s1.list s2.list }

let smart_subset link witness accu s1 s2 =
  let order = get_order s1 in
  let rec diff accu l1 l2 =
    match (l1, l2) with
      [], _ -> accu
    | (elem1 :: _), [] -> witness elem1
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let result = order elem1 elem2 in
	if result < 0 then witness elem1
	else if result = 0 then diff (link accu elem1 elem2) rest1 rest2
	else diff accu l1 rest2 in
  diff accu s1.list s2.list

exception NotEqual

let smart_equal equality s1 s2 =
  let rec check l1 l2 =
    match (l1, l2) with
      [], [] -> ()
    | (elem1 :: rest1), (elem2 :: rest2) ->
	if equality elem1 elem2 then check rest1 rest2
	else raise NotEqual
    | _, _ ->
	raise NotEqual in
  try
    check s1.list s2.list;
    true
  with NotEqual ->
    false

let equal s1 s2 =
  let order = get_order s1 in
  smart_equal (fun x y -> (order x y = 0)) s1 s2

let compare s1 s2 =
  let order = get_order s1 in
  let rec compare l1 l2 =
    match (l1, l2) with
      [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let result = order elem1 elem2 in
	if result = 0 then compare rest1 rest2
	else result in
  compare s1.list s2.list

let elements s =
  s.list

let iter action s =
  List.iter action s.list

let fold action s accu =
  List.fold_right action s.list accu

let choose s =
  match s.list with
    [] ->
      raise Not_found
  | elem :: _ ->
      elem

let choosep predicate s =
  let rec choosep = function
      [] ->
	raise Not_found
    | elem :: rest ->
	if predicate elem then elem
	else choosep rest in
  choosep s.list

let cardinality s =
  List.length s.list

let memp searchp s =
  let rec memp = function
      [] ->
	raise Not_found
    | elem :: rest ->
	let result = searchp elem in
	if result < 0 then raise Not_found
	else if result = 0 then elem
	else memp rest in
  memp s.list

let memp_greatest searchp s =
  let rec memp = function
      [] ->
	raise Not_found
    | elem :: rest ->
	let result = searchp elem in
	if result < 0 then raise Not_found
	else if result = 0 then begin
	  try
	    memp rest
	  with Not_found ->
	    elem
	end
	else memp rest in
  memp s.list

let map new_order f s =
  {
    order = Some new_order;
    list = Sort.list (fun x y -> (new_order x y < 0)) (List.map f s.list)
  }

let endo_map f s =
  let order = get_order s in
  {
    order = s.order;
    list = Sort.list (fun x y -> (order x y < 0)) (List.map f s.list)
  }

let monotonous_map new_order f s =
  {
    order = Some new_order;
    list = List.map f s.list
  }

let monotonous_endo_map f s =
  {
    order = s.order;
    list = Standard.list_endo_map f s.list
  }

let filter predicate s =
  {
    order = s.order;
    list = Standard.list_filter predicate s.list
} 

let smallest s =
  match s.list with
    [] ->
      raise Not_found
  | elem :: _ ->
      elem

let freeze s =
  s.order <- None

let unfreeze s order =
  s.order <- Some order

let apply_assoc_op1 operator s =
  match s.list with
    [] ->
      raise Not_found
  | elem :: rest ->
      List.fold_left operator elem rest

let isolate { list = list; order = order } =
  match list with
    [] ->
      raise Not_found
  | elem :: rest ->
      elem, { list = rest; order = order }
