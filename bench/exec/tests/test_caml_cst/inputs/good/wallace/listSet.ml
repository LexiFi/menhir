(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/listSet.ml,v 1.13 2000/04/12 09:46:35 fpottier Exp $ *)

(* This module provides a naïve, list-based implementation of sets. It might be useful when dealing with very small
   sets, because it has very little overhead. *)

type 'a t = 'a list

(* An ordering is a two-argument function [f] such that [f e1 e2] is zero if the elements [e1] and [e2] are equal,
   strictly negative if [e1] is smaller than [e2], and strictly positive if [e1] is greater than [e2]. For instance,
   a suitable ordering function for integers is [(-)]. The generic comparison function [Pervasives.compare] is also
   a suitable ordering. *)

type 'a ordering = 'a -> 'a -> int

(* The empty set. *)

let empty =
  []

(* [is_empty s] tells whether [s] is the empty set. *)

let is_empty = function
  | [] ->
      true
  | _ :: _ ->
      false

(* [singleton x] returns a singleton set containing [x] as its only element. *)

let singleton x =
  [ x ]

(* [is_singleton s] returns [x] if [s] is a singleton containing [x] as its only element; otherwise, it raises
   [NotSingleton]. *)

exception NotSingleton

let is_singleton = function
  | [ x ] ->
      x
  | _ ->
      raise NotSingleton

(* [make2 o x y] creates a set whose elements are [x] and [y]. [x] and [y] need not be distinct according
   to the ordering [o]. *)

let make2 compare x y =
  let c = compare x y in
  if c = 0 then [ x ]
  else if c < 0 then [ x; y ]
  else [ y; x ]

(* [cardinal s] returns the cardinal of [s]. *)

let cardinal =
  List.length

(* [mem o x s] returns [true] if and only if some value equal to [x] modulo [o] appears in the set [s]. The ordering
   [o] must be consistent with the set [s]. *)

let rec mem o x = function
  | [] ->
      false
  | y :: rest ->
      let c = o x y in
      if c = 0 then true
      else if c < 0 then false
      else mem o x rest

(* [memp ox s] looks for an element equal to [x] within the set [s], if [ox] is the partial application of the set's
   ordering, [o], to some value [x]. If such an element exists, it is returned. Otherwise, the call raises
   [Not_found]. *)

let rec memp ox = function
  | [] ->
      raise Not_found
  | y :: rest ->
      let c = ox y in
      if c = 0 then y
      else if c < 0 then raise Not_found
      else memp ox rest

(* [add o x s] returns a set whose elements are all elements of [s], plus [x]. The ordering [o] must be consistent
   with the set [s]. If the set [s] already contains an element equal to [x] (according to [o]) then [Unchanged] is
   raised. *)

exception Unchanged

let strict_add o x s =

  let rec strict_add = function
    | [] ->
	[ x ]
    | (y :: rest) as list ->
	let c = o x y in
	if c < 0 then x :: list
	else if c = 0 then raise Unchanged
	else y :: (strict_add rest) in

  strict_add s

(* [add o x s] returns a set whose elements are all elements of [s], plus [x]. The ordering [o] must be consistent
   with the set [s]. *)

let add o x s =
  try
    strict_add o x s
  with Unchanged ->
    s

(* [fine_add o decide x s] returns a set whose elements are all elements of [s], plus [x]. If an element $x_0$ equal
   to [x] (according to [o]) already existed in [s], then [decide] is called. It is passed both elements (first
   $x_0$, then [x]) and must return the element which shall appear in the final set. This element may be different
   from [x] and from $x_0$, but must be equal to both according to [o]. As usual, the ordering [o] must be
   consistent with the set [s]. *)

type 'a decision = 'a -> 'a -> 'a

let fine_add o decision x s =

  let rec strict_add = function
    | [] ->
	[ x ]
    | (y :: rest) as list ->
	let c = o x y in
	if c < 0 then x :: list
	else if c = 0 then begin
	  let z = decision y x in
	  if z == y then raise Unchanged else z :: rest
	end
	else y :: (strict_add rest) in

  try
    strict_add s
  with Unchanged ->
    s

(* [remove o x s] returns a set whose elements are all elements of [s], except [x]. The ordering [o] must be
   consistent with the set [s]. *)

let remove o x s =

  let rec strict_remove = function
    | [] ->
	raise Not_found
    | y :: rest ->
	let c = o x y in
	if c < 0 then raise Not_found
	else if c = 0 then rest
	else y :: (strict_remove rest) in

  try
    strict_remove s
  with Not_found ->
    s

(* [removep ox s] looks for an element equal to [x] within the set [s], if [ox] is the partial application of the
   set's ordering, [o], to some value [x]. If such an element exists, it is returned, together with the set deprived
   of it. Otherwise, the call raises [Not_found]. *)

let removep search s =

  let rec strict_remove = function
    | [] ->
	raise Not_found
    | y :: rest ->
	let c = search y in
	if c < 0 then raise Not_found
	else if c = 0 then y, rest
	else begin
	  let y', rest' = strict_remove rest in
	  y', y :: rest'
	end in

  strict_remove s

(* [union o s1 s2] returns the union of the sets [s1] and [s2]. The ordering [o] must be consistent with both
   of these sets. *)

let rec union o l1 l2 =
  match (l1, l2) with
  | _, [] -> l1
  | [], _ -> l2
  | (elem1 :: rest1), (elem2 :: rest2) ->
      let c = o elem1 elem2 in
      if c < 0 then elem1 :: (union o rest1 l2)
      else if c = 0 then elem1 :: (union o rest1 rest2)
      else elem2 :: (union o l1 rest2)

(* [fine_union o decide s1 s2] returns the union of the sets [s1] and [s2]. If equal (according to [o]) elements
   [x1] and [x2] appear in [s1] and [s2], respectively, then [decide] is called. It is passed both elements (first
   [x1], then [x2]) and must return the element which shall appear in the final set. This element may be different
   from [x1] and from [x2], but must be equal to both according to [o]. As usual, the ordering [o] must be
   consistent with the set [s]. *)

let fine_union o decision l1 l2 =

  let rec union l1 l2 =
    match (l1, l2) with
    | _, [] -> l1
    | [], _ -> l2
    | (elem1 :: rest1), (elem2 :: rest2) ->
	let c = o elem1 elem2 in
	if c < 0 then elem1 :: (union rest1 l2)
	else if c = 0 then (decision elem1 elem2) :: (union rest1 rest2)
	else elem2 :: (union l1 rest2) in

  union l1 l2

(* [diff o s t] returns the set difference of [s] and [t], that is, $s\setminus t$. The ordering [o] must
   be consistent with both of these sets. *)

let rec diff o l1 l2 =
  match (l1, l2) with
  | _, [] ->
      l1
  | [], _ ->
      []
  | elem1 :: rest1, elem2 :: rest2 ->
      let c = o elem1 elem2 in
      if c < 0 then elem1 :: (diff o rest1 l2)
      else if c = 0 then diff o rest1 rest2
      else diff o l1 rest2

(* [corestrict o s1 s2] assumes [s1] is a function graph (i.e. a set of pairs), and [s2] is some subset of [s1]'s
   domain space. It returns the graph of the co-restriction of [s1] with respect to [s2]. In other words, it
   returns the set of all pairs in [s1] whose first component is \emph{not} in [s2]. The ordering [o] must
   be consistent with both of these sets. *)

let rec corestrict o l1 l2 =
  match (l1, l2) with
  | _, [] ->
      l1
  | [], _ ->
      []
  | ((key1, _) as elem1) :: rest1, elem2 :: rest2 ->
      let c = o key1 elem2 in
      if c < 0 then elem1 :: (corestrict o rest1 l2)
      else if c = 0 then corestrict o rest1 rest2
      else corestrict o l1 rest2

(* [disjoint o s1 s2] returns [true] if and only if the sets [s1] and [s2] are disjoint, i.e. iff their intersection
   is empty. The ordering [o] must be consistent with both of these sets. *)

let rec disjoint o l1 l2 =
  match (l1, l2) with
  | _, []
  | [], _ ->
      true
  | elem1 :: rest1, elem2 :: rest2 ->
      let c = o elem1 elem2 in
      if c < 0 then disjoint o rest1 l2
      else if c = 0 then false
      else disjoint o l1 rest2

(* [iter f s] invokes [f x], in turn, for each element [x] of the set [s]. Elements are presented to [f] in
   increasing order according to the set's ordering. *)

let iter = List.iter

(* [fold f s seed] invokes [f x accu], in turn, for each element [x] of the set [s]. Elements are presented to [f] in
   increasing order according to the set's ordering. The initial value of [accu] is [seed]; then, at each new call,
   its value is the value returned by the previous invocation of [f]. The value returned by [fold] is the final
   value of [accu]. In other words, if $s = \{ x_1, x_2, \ldots, x_n \}$, where $x_1 < x_2 < \ldots < x_n$, then
   [fold f s seed] computes $([f]\,x_n\,\ldots\,([f]\,x_2\,([f]\,x_1\,[seed]))\ldots)$. *)

let rec fold f list accu =
  match list with
  | [] ->
      accu
  | y :: rest ->
      fold f rest (f y accu)

(* [fold_rev] performs exactly the same job as [fold], but presents elements to [f] in the opposite order. *)

let fold_rev =
  List.fold_right

(* It is valid to evaluate [iter2 f s1 s2] if and only if [s1] and [s2] are equal sets, according to their natural
   ordering(s). Doing so invokes [f x1 x2], in turn, for each pair of equal elements [x1] and [x2]. Elements are
   presented to [f] in increasing order according to the sets' ordering(s).

   Note that this implementation may silently produce a meaningless result if [s1] and [s2] are not equal. *)

let iter2 =
  List.iter2

(* [iterator s] returns a stateful iterator over the set [s]. That is, if $s = \{ x_1, x_2, \ldots, x_n \}$, where
   $x_1 < x_2 < \ldots < x_n$, then [iterator s] is a function which, when invoked for the $k^{\text{th}}$ time,
   returns [Some ]$x_k$, if $k\leq n$, and [None] otherwise. *)

let iterator list =

  let remainder = ref list in

  let rec next () =
    match !remainder with
    | [] ->
	None
    | elem :: rest ->
	remainder := rest;
	Some elem in

  next

(* [exists p s] returns [true] if and only if some element of [s] matches the predicate [p]. *)

exception Exists

let exists p s =
  try
    iter (fun x ->
      if p x then
	raise Exists
    ) s;
    false
  with Exists ->
    true

(* [compare o] is an ordering over sets whose ordering is [o]. In other words, if [s1] and [s2] have ordering [o],
   then [compare o s1 s2] compares them and returns an integer code. *)

let rec compare o l1 l2 =
  match (l1, l2) with
  | [], [] -> 0
  | [], _ -> -1
  | _, [] -> 1
  | (elem1 :: rest1), (elem2 :: rest2) ->
      let c = o elem1 elem2 in
      if c = 0 then compare o rest1 rest2
      else c

(* [equal o] implements equality over sets whose ordering is [o]. In other words, if [s1] and [s2] have ordering
   [o], then [equal o s1 s2] compares them and returns [true] if and only if they have the same elements. *)

let equal o l1 l2 =
  compare o l1 l2 = 0

(* [subset o] implements the subset predicate over sets whose ordering is [o]. In other words, if [s] and [t] have
   ordering [o], then [subset o s t] compares them and returns [true] if and only if $s\subseteq t$. *)

let rec subset o l1 l2 =
  match (l1, l2) with
  | [], _ ->
      true
  | _ :: _, [] ->
      false
  | elem1 :: rest1, elem2 :: rest2 ->
      let c = o elem1 elem2 in
      if c < 0 then false
      else if c = 0 then subset o rest1 rest2
      else subset o l1 rest2

(* [filter o p s] returns the subset of [s] formed by all elements which satisfy the predicate [p]. The ordering [o]
   must be consistent with [s]. *)

let filter _ predicate list =
  let rec filter = function
    | [] ->
	[]
    | (elem :: rest) as list ->
	let rest' = filter rest in
	if predicate elem then
	  if rest == rest' then list else elem :: rest'
	else
	  rest' in
  filter list

(* [map o f s] computes the image of [s] through [f]. [o] becomes the ordering of the result set. *)

let rec eliminate_duplicates o list =
  match list with
  | []
  | [ _ ] ->
      list
  | x1 :: ((x2 :: _) as rest1) ->
      if o x1 x2 = 0 then eliminate_duplicates o rest1
      else x1 :: (eliminate_duplicates o rest1)

let map o f list =
  eliminate_duplicates o (Sort.list (fun x y -> (o x y <= 0)) (List.map f list))

(* [monotone_map f s] computes the image of [s] through [f], which must be a monotone function, i.e. preserve the
   elements' \emph{strict} ordering. *)

let monotone_map =
  List.map

(* [endo_map] is similar to [map], but attempts to physically share its result with its input. This saves
   memory when [f] is the identity function. *)

let rec endo_map f list =
  match list with
  | [] ->
      list
  | elem :: rest ->
      let elem' = f elem
      and rest' = endo_map f rest in
      if (elem == elem') & (rest == rest') then list
      else elem' :: rest'

