(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/baltree.ml,v 1.22 2000/04/12 09:46:34 fpottier Exp $ *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Definition} *)

(* A tree is either empty, or a binary node, which (in addition to its sons) carries a piece of data, as well as
   balance information. The precise meaning of the balance information is not specified here; it may represent height
   or weight information. Other kinds of information could be used, but [singleton], [is_singleton] and [union] will
   have to be modified slightly, because they assume a set is a singleton if and only if its associated integer is
   1. [make2] will need to be modified as well; it assumes the information associated to a 2-element set is 2. *)

type 'a tree =
  | Empty
  | Node of 'a tree * 'a * 'a tree * int

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Generic tree functions}

   We begin with functions which do not depend on the chosen balancing scheme. The code is parameterized over a
   small set of balancing primitives. *)

module Make (Balance : sig

  (* [cardinal t] returns the number of nodes in the tree [t]. Merely walking the tree and counting nodes is
     inefficient in the case of weight-balanced trees, which is why we do not provide a default implementation
     of this function. *)

  val cardinal: 'a tree -> int

  (* [bal] is a ``smart constructor'': it creates a new node with the specified left son, element and right son. It
     performs one step of rebalancing if necessary. *)

  val bal: 'a tree -> 'a -> 'a tree -> 'a tree

  (* [concat3] performs the same function as [bal], but performs as much rebalancing as necessary. In other words,
     it creates a balanced tree from a tree [l], a value [x] and a tree [r], provided [l], [x] and [r] are ordered. *)

  val concat3: 'a tree -> 'a -> 'a tree -> 'a tree

end) = struct

  type 'a t = 'a tree

  (* An ordering is a two-argument function [f] such that [f e1 e2] is zero if the elements [e1] and [e2] are equal,
     strictly negative if [e1] is smaller than [e2], and strictly positive if [e1] is greater than [e2]. For instance,
     a suitable ordering function for integers is [(-)]. The generic comparison function [Pervasives.compare] is also
     a suitable ordering. *)

  type 'a ordering = 'a -> 'a -> int

  (* The empty set. *)

  let empty =
    Empty

  (* [is_empty s] tells whether [s] is the empty set. *)

  let is_empty = function
    | Empty ->
	true
    | _ ->
	false

  (* [singleton x] returns a singleton set containing [x] as its only element. Beware: this code assumes that the
     balance information associated to a singleton is 1. This is true for height and weight balanced trees, but could
     be false with other kinds of trees. *)

  let singleton x =
    Node(Empty, x, Empty, 1)

  (* [is_singleton s] returns [x] if [s] is a singleton containing [x] as its only element; otherwise, it raises
     [NotSingleton]. Beware: this code assumes that if the balance information associated to a set is 1, then it is a
     singleton. This is true for height and weight balanced trees, but could be false with other kinds of trees. *)

  exception NotSingleton

  let is_singleton = function
    | Node(_, x, _, 1) ->
	x
    | _ ->
	raise NotSingleton

  (* [make2 o x y] creates a set whose elements are [x] and [y]. [x] and [y] need not be distinct according to the
     ordering [o]. Beware: this code assumes the information associated to a 2-element set is 2. This is true for
     height and weight balanced trees, but could be false with other kinds of trees. *)

  let make2 compare x y =
    let c = compare x y in
    if c = 0 then
      singleton x
    else if c < 0 then
      Node(singleton x, y, Empty, 2)
    else
      Node(Empty, x, singleton y, 2)

  (* [cardinal s] returns the cardinal of [s]. Its implementation is provided in this functor's argument, [Balance]. *)

  let cardinal =
    Balance.cardinal

  (* [mem o x s] returns [true] if and only if some value equal to [x] modulo [o] appears in the set [s]. The ordering
     [o] must be consistent with the set [s]. *)

  let rec mem compare x = function
    | Empty ->
	false
    | Node(l, v, r, _) ->
	let c = compare x v in
	(c = 0) || (mem compare x (if c < 0 then l else r))

  (* [memp ox s] looks for an element equal to [x] within the set [s], if [ox] is the partial application of the set's
     ordering, [o], to some value [x]. If such an element exists, it is returned. Otherwise, the call raises
     [Not_found]. *)

  let rec memp search = function
    | Empty ->
	raise Not_found
    | Node(l, v, r, _) ->
	let c = search v in
	if c = 0 then v
	else memp search (if c < 0 then l else r)

  (* [add o x s] returns a set whose elements are all elements of [s], plus [x]. The ordering [o] must be consistent
     with the set [s]. If the set [s] already contains an element equal to [x] (according to [o]) then [Unchanged] is
     raised.

     Our implementation closely follows O'Caml's standard library's, but eases the load on the garbage collector by
     preserving physical equality whenever possible. *)

  exception Unchanged

  let strict_add compare x s =

    let rec strict_add = function
      | Empty ->
	  singleton x
      | Node(l, v, r, _) as t ->
	  let c = compare x v in
	  if c = 0 then raise Unchanged
	  else if c < 0 then Balance.bal (strict_add l) v r
	  else Balance.bal l v (strict_add r) in

    strict_add s

  (* [add o x s] returns a set whose elements are all elements of [s], plus [x]. The ordering [o] must be consistent
     with the set [s]. *)

  let add compare x s =
    try
      strict_add compare x s
    with Unchanged ->
      s

  (* [fine_add o decide x s] returns a set whose elements are all elements of [s], plus [x]. If an element $x_0$ equal
     to [x] (according to [o]) already existed in [s], then [decide] is called. It is passed both elements (first
     $x_0$, then [x]) and must return the element which shall appear in the final set. This element may be different
     from [x] and from $x_0$, but must be equal to both according to [o]. As usual, the ordering [o] must be
     consistent with the set [s]. *)

  type 'a decision = 'a -> 'a -> 'a

  let fine_add compare decision x s =

    let rec strict_add = function
      | Empty ->
	  singleton x
      | Node(l, v, r, info) as t ->
	  let c = compare x v in
	  if c = 0 then begin
	    let w = decision v x in
	    if w == v then raise Unchanged
	    else Node(l, w, r, info)
	  end
	  else if c < 0 then Balance.bal (strict_add l) v r
	  else Balance.bal l v (strict_add r) in

    try
      strict_add s
    with Unchanged ->
      s

  (* [merge] merges two trees [l] and [r] into one. All elements of [l] must precede the elements of [r]. [l] and
     [r] must of comparable sizes, i.e. a tree formed by adding a node on top of them would be balanced. The
     code is taken from O'Caml's standard library. *)

  let rec merge t1 t2 =
    match (t1, t2) with
    | Empty, t -> t
    | t, Empty -> t
    | Node(l1, v1, r1, h1), Node(l2, v2, r2, h2) ->
	Balance.bal l1 v1 (Balance.bal (merge r1 l2) v2 r2)

  (* [concat2] merges two trees [l] and [r] into one. All elements of [l] must precede the elements of [r]. The
     code is taken from O'Caml's standard library. *)

  let rec concat2 t1 t2 =
    match (t1, t2) with
    | Empty, _ -> t2
    | _, Empty -> t1
    | Node(l1, v1, r1, _), Node(l2, v2, r2, _) ->
	Balance.concat3 l1 v1 (Balance.concat3 (concat2 r1 l2) v2 r2)

  (* [remove o x s] returns a set whose elements are all elements of [s], except [x]. The ordering [o] must be
     consistent with the set [s]. *)

  let remove compare x s =
    
    let rec strict_remove = function
      | Empty ->
	  raise Not_found
      | Node(l, v, r, _) ->
	  let c = compare x v in
	  if c = 0 then merge l r
	  else if c < 0 then Balance.bal (strict_remove l) v r
	  else Balance.bal l v (strict_remove r) in

    try
      strict_remove s
    with Not_found ->
      s

  (* [removep ox s] looks for an element equal to [x] within the set [s], if [ox] is the partial application of the
     set's ordering, [o], to some value [x]. If such an element exists, it is returned, together with the set deprived
     of it. Otherwise, the call raises [Not_found]. *)

  let removep search s =
    
    let rec strict_remove = function
      | Empty ->
	  raise Not_found
      | Node(l, v, r, _) ->
	  let c = search v in
	  if c = 0 then v, merge l r
	  else if c < 0 then begin
	    let v', l' = strict_remove l in
	    v', Balance.bal l' v r
	  end
	  else begin
	    let v', r' = strict_remove r in
	    v', Balance.bal l v r'
	  end in

    strict_remove s

  (* [split x t] returns a triple of
     \begin{itemize}
     \item a tree containing all elements strictly less than [x] in [t];
     \item the element equal to [x] in [t], if there is one;
     \item a tree containing all elements strictly greater than [x] in [t].
     \end{itemize}
     All comparisons are performed using the total ordering [compare]. *)

  let rec split compare x = function
      Empty ->
	(Empty, None, Empty)
    | Node(l, v, r, _) ->
	let c = compare x v in
	if c = 0 then (l, Some v, r)
	else if c < 0 then
	  let (ll, vl, rl) = split compare x l in (ll, vl, Balance.concat3 rl v r)
	else
	  let (lr, vr, rr) = split compare x r in (Balance.concat3 l v lr, vr, rr)

  (* [union o s1 s2] returns the union of the sets [s1] and [s2]. The ordering [o] must be consistent with both
     of these sets.

     The implementation is essentially O'Caml's standard library's. It resembles Adams', but adds a couple of
     enhancements. First, the union algorithm explicitly degenerates to an [add] when there only remains one
     element in one of the sets, thus saving some time---little time, I would guess. Second, the divide-and-conquer
     algorithm splits the smaller sub-tree, whereas Adams always splits the left one.

     Lastly, our implementation eases the load on the garbage collector by preserving physical equality whenever
     possible. TEMPORARY This might be useless; check.

     Beware: this code assumes that if the balance information associated to a set is 1, then it is a singleton. This
     is true for height and weight balanced trees, but could be false with other kinds of trees. *)

  let rec union compare s1 s2 =
    match (s1, s2) with
    | (Empty, t2) -> t2
    | (t1, Empty) -> t1
    | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
	if h1 >= h2 then
	  if h2 = 1 then add compare v2 s1
	  else begin
	    let (l2, _, r2) = split compare v1 s2 in
	    let l' = union compare l1 l2
	    and r' = union compare r1 r2 in
	    if (l1 == l') & (r1 == r') then s1 else Balance.concat3 l' v1 r'
	  end
	else
	  if h1 = 1 then add compare v1 s2
	  else begin
	    let (l1, _, r1) = split compare v2 s1 in
	    let l' = union compare l1 l2
	    and r' = union compare r1 r2 in
	    if (l2 == l') & (r2 == r') then s2 else Balance.concat3 l' v2 r'
	  end

  (* [fine_union o decide s1 s2] returns the union of the sets [s1] and [s2], like [union], but uses a decision
     function to choose between equal elements. *)

  let reverse decision elem1 elem2 =
    decision elem2 elem1

  let fine_union compare decision s1 s2 =

    let rec union s1 s2 =
      match (s1, s2) with
      | (Empty, t2) -> t2
      | (t1, Empty) -> t1
      | (Node(l1, v1, r1, h1), Node(l2, v2, r2, h2)) ->
	  if h1 >= h2 then
	    if h2 = 1 then fine_add compare decision v2 s1
	    else begin
	      let (l2, v2, r2) = split compare v1 s2 in
	      let l' = union l1 l2
	      and r' = union r1 r2 in
	      match v2 with
	      |	None ->
		  if (l1 == l') & (r1 == r') then s1
		  else Balance.concat3 l' v1 r'
	      |	Some v2 ->
		  let v' = decision v1 v2 in
		  if (l1 == l') & (v1 == v') & (r1 == r') then s1
		  else Balance.concat3 l' v' r'
	    end
	  else
	    if h1 = 1 then fine_add compare (reverse decision) v1 s2
	    else begin
	      let (l1, v1, r1) = split compare v2 s1 in
	      let l' = union l1 l2
	      and r' = union r1 r2 in
	      match v1 with
	      |	None ->
		  if (l2 == l') & (r2 == r') then s2
		  else Balance.concat3 l' v2 r'
	      |	Some v1 ->
		  let v' = decision v1 v2 in
		  if (l2 == l') & (v2 == v') & (r2 == r') then s2
		  else Balance.concat3 l' v' r'
	    end in

    union s1 s2

  (* [diff o s t] returns the set difference of [s] and [t], that is, $s\setminus t$. The ordering [o] must
     be consistent with both of these sets. *)

  let rec diff o s1 s2 =
    match (s1, s2) with
    | Empty, _
    | _, Empty ->
	s1
    | Node(l1, v1, r1, _), _ ->
	match split o v1 s2 with
	| (l2, None, r2) ->
	    Balance.concat3 (diff o l1 l2) v1 (diff o r1 r2)
	| (l2, Some _, r2) ->
	    concat2 (diff o l1 l2) (diff o r1 r2)

  (* [corestrict o s1 s2] assumes [s1] is a function graph (i.e. a set of pairs), and [s2] is some subset of [s1]'s
     domain space. It returns the graph of the co-restriction of [s1] with respect to [s2]. In other words, it
     returns the set of all pairs in [s1] whose first component is \emph{not} in [s2]. The ordering [o] must
     be consistent with both of these sets. *)

  let rec corestrict o s1 s2 =
    match (s1, s2) with
    | Empty, _ ->
	Empty
    | _, Empty ->
	s1
    | Node(l1, ((key1, _) as v1), r1, _), _ ->
	match split o key1 s2 with
	| (l2, None, r2) ->
	    Balance.concat3 (corestrict o l1 l2) v1 (corestrict o r1 r2)
	| (l2, Some _, r2) ->
	    concat2 (corestrict o l1 l2) (corestrict o r1 r2)

  (* [disjoint o s1 s2] returns [true] if and only if the sets [s1] and [s2] are disjoint, i.e. iff their intersection
     is empty. The ordering [o] must be consistent with both of these sets. *)

  exception NotDisjoint

  let disjoint o s1 s2 =
    
    let rec disjoint s1 s2 =
      match (s1, s2) with
      | Empty, _
      | _, Empty ->
	  ()
      | Node(l1, v1, r1, _), _ ->
          match split o v1 s2 with
          | (l2, None, r2) ->
	      disjoint l1 l2;
	      disjoint r1 r2
          | (_, Some _, _) ->
	      raise NotDisjoint in

    try
      disjoint s1 s2;
      true
    with NotDisjoint ->
      false

  (* [iter f s] invokes [f x], in turn, for each element [x] of the set [s]. Elements are presented to [f] in
     increasing order according to the set's ordering. *)

  let rec iter f = function
    | Empty ->
	()
    | Node(l, v, r, _) ->
	iter f l; f v; iter f r

  (* [fold f s seed] invokes [f x accu], in turn, for each element [x] of the set [s]. Elements are presented to [f] in
     increasing order according to the set's ordering. The initial value of [accu] is [seed]; then, at each new call,
     its value is the value returned by the previous invocation of [f]. The value returned by [fold] is the final
     value of [accu]. In other words, if $s = \{ x_1, x_2, \ldots, x_n \}$, where $x_1 < x_2 < \ldots < x_n$, then
     [fold f s seed] computes $([f]\,x_n\,\ldots\,([f]\,x_2\,([f]\,x_1\,[seed]))\ldots)$. *)

  let rec fold f s seed =
    match s with
    | Empty ->
	seed
    | Node(l, v, r, _) ->
	fold f r (f v (fold f l seed))

  (* [fold_rev] performs exactly the same job as [fold], but presents elements to [f] in the opposite order. *)

  let rec fold_rev f s seed =
    match s with
    | Empty ->
	seed
    | Node(l, v, r, _) ->
	fold_rev f l (f v (fold_rev f r seed))

  (* [iterator s] returns a stateful iterator over the set [s]. That is, if $s = \{ x_1, x_2, \ldots, x_n \}$, where
     $x_1 < x_2 < \ldots < x_n$, then [iterator s] is a function which, when invoked for the $k^{\text{th}}$ time,
     returns [Some ]$x_k$, if $k\leq n$, and [None] otherwise. Such a function can be useful when one wishes to
     iterate over a set's elements, without being restricted by the call stack's discipline.

     Because the call stack is not used to store information about which part of the set remains to be walked, an
     explicit (heap-allocated) structure has to be used. It is defined below. Note that it is essentially a list
     of (element, right-tree) pairs, which corresponds to the information which would be stored in the call stack,
     if it were used.

     It would be possible to implement a straightforward iterator by first turning the set into a list, then walking
     the list. Our implementation is more economical, because it only allocates a structure of size $O(\log n)$ (the
     call stack) at a given time, and because only one walk is necessary, thus reaping a small constant factor. *)

  type 'a remainder =
    | Nothing
    | ElemThenRightThenParent of 'a * 'a t * 'a remainder

  let iterator s =

    (* When asked to create an iterator for s, we allocate a piece of state, which shall allow the iterator to remember
       which part of the tree remains to be walked. It consists of a tree and a remainder, which are to be walked in
       order. At first, the whole tree [s] has to be walked, and there is no remainder.

       Note that [next] does not work in worst-case constant time, since it may have to travel arbitrarily far down
       the current sub-tree's left spine. However, it does work in amortized constant time; in other words, iterating
       over a complete tree still takes worst-case linear time. *)

    let tree = ref s
    and remainder = ref Nothing in

    let rec next () =
      match !tree, !remainder with
      | Empty, Nothing ->
	  None
      | Empty, ElemThenRightThenParent (elem, right, parent) ->
	  tree := right;
	  remainder := parent;
	  Some elem
      | Node(l, v, r, _), parent ->
	  tree := l;
	  remainder := ElemThenRightThenParent(v, r, parent);
	  next () in

    next

  (* It is valid to evaluate [iter2 f s1 s2] if and only if [s1] and [s2] are equal sets, according to their natural
     ordering(s). Doing so invokes [f x1 x2], in turn, for each pair of equal elements [x1] and [x2]. Elements are
     presented to [f] in increasing order according to the sets' ordering(s).

     Note that this implementation may silently produce a meaningless result if [s1] and [s2] are not equal. *)

  let iter2 f s1 s2 =
    let iterator2 = iterator s2 in
    iter (function elem1 ->
      match iterator2() with
      |	Some elem2 ->
	  f elem1 elem2
      |	None ->
	  assert false
    ) s1

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

  exception Got of int

  let compare o s1 s2 =
    let iterator2 = iterator s2 in
    try
      iter (fun x1 ->
	match iterator2() with
	| None ->
	    raise (Got 1)
	| Some x2 ->
	    let c = o x1 x2 in
	    if c <> 0 then
	      raise (Got c)
      ) s1;
      match iterator2() with
      |	None ->
	  0
      |	Some _ ->
	  -1
    with Got c ->
      c

  (* [equal o] implements equality over sets whose ordering is [o]. In other words, if [s1] and [s2] have ordering
     [o], then [equal o s1 s2] compares them and returns [true] if and only if they have the same elements. *)

  let equal o s1 s2 =
    compare o s1 s2 = 0

  (* [subset o] implements the subset predicate over sets whose ordering is [o]. In other words, if [s] and [t] have
     ordering [o], then [subset o s t] compares them and returns [true] if and only if $s\subseteq t$.

     This code is taken from O'Caml's standard library, with one extra optimization. *)

  let rec subset o s1 s2 =
    match (s1, s2) with
    | Empty, _ ->
        true
    | _, Empty ->
        false
    | Node (l1, v1, r1, _), Node (l2, v2, r2, _) ->
        let c = o v1 v2 in
        if c = 0 then
          subset o l1 l2 && subset o r1 r2
        else if c < 0 then
	  match r1 with
	  | Empty ->
              subset o s1 l2
	  | Node _ ->
              subset o (Node (l1, v1, Empty, 0)) l2 && subset o r1 s2
        else
	  match l1 with
	  | Empty ->
	      subset o s1 r2
	  | Node _ ->
              subset o (Node (Empty, v1, r1, 0)) r2 && subset o l1 s2

  (* [filter o p s] returns the subset of [s] formed by all elements which satisfy the predicate [p]. The ordering [o]
     must be consistent with [s]. *)

  let filter o predicate s =
    let modified = ref false in

    let subset = fold (fun element subset ->
      if predicate element then
	add o element subset
      else begin
	modified := true;
	subset
      end
    ) s Empty in

    if !modified then
      subset
    else
      s

  (* [map o f s] computes the image of [s] through [f]. [o] becomes the ordering of the result set. *)

  let map o f s =
    fold (fun element accu ->
      add o (f element) accu
    ) s Empty

  (* [monotone_map f s] computes the image of [s] through [f], which must be a monotone function, i.e. preserve the
     elements' \emph{strict} ordering. *)

  let rec monotone_map f = function
    | Empty ->
	Empty
    | Node(l, v, r, h) ->
	Node(monotone_map f l, f v, monotone_map f r, h)

  (* [endo_map] is similar to [map], but attempts to physically share its result with its input. This saves
     memory when [f] is the identity function. *)

  let rec endo_map f tree =
    match tree with
      | Empty ->
	  tree
      |	Node(l, v, r, h) ->
	  let l' = endo_map f l
	  and v' = f v
	  and r' = endo_map f r in
	  if (l == l') & (v == v') & (r == r') then tree
	  else Node(l', v', r', h)

end  

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Height balanced trees}

   This module implements height-balanced binary trees (the heights of each node's children differ by at most 2). The
   code is taken from O'Caml's standard library. *)

module HeightBalance = struct

  (* [cardinal s] returns the cardinal of [s]. *)

  let rec cardinal = function
    | Empty ->
	0
    | Node(l, _, r, _) ->
	cardinal l + 1 + cardinal r

  (* [height t] returns the height of a tree [t]. *)

  let height = function
    | Empty -> 0
    | Node(_, _, _, h) -> h

  (* [create l x r] creates a new node with left son [l], value [x] and right son [r]. [l] and [r] must be balanced and
     $\norm{[height l] - [height r]} \leq 2$ must hold. Calls to [height] are manually inlined for better
     speed. *)

  let create l x r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    Node(l, x, r, (if hl >= hr then hl + 1 else hr + 1))

  (* [bal] performs the same function as [create], but performs one step of rebalancing if necessary. [l] and [r] must
     be balanced. Calls to [create] are manually inlined, for better speed, in the most frequent case where no
     rebalancing is required. *)

  let bal l x r =
    let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h in
    let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h in
    if hl > hr + 2 then begin
      match l with
      | Empty -> assert false
      | Node(ll, lv, lr, _) ->
	  if height ll >= height lr then
	    create ll lv (create lr x r)
	  else begin
	    match lr with
	    | Empty -> assert false
	    | Node(lrl, lrv, lrr, _)->
		create (create ll lv lrl) lrv (create lrr x r)
	  end
    end else if hr > hl + 2 then begin
      match r with
      | Empty -> assert false
      | Node(rl, rv, rr, _) ->
	  if height rr >= height rl then
	    create (create l x rl) rv rr
	  else begin
	    match rl with
	    | Empty -> assert false
	    | Node(rll, rlv, rlr, _) ->
		create (create l x rll) rlv (create rlr rv rr)
	  end
    end else
      Node(l, x, r, (if hl >= hr then hl + 1 else hr + 1))

  (* [concat3] performs the same function as [bal], but keeps rebalancing until the final result is balanced. *)

  let rec concat3 l x r =
    match bal l x r with
    | Empty -> assert false
    | Node(l', x', r', _) as t' ->
	let d = height l' - height r' in
	if d < -2 or d > 2 then concat3 l' x' r' else t'

end

module Height =
  Make(HeightBalance)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Weight balanced trees}

   This module implements weight-balanced binary trees (the weights of each node's children differ by at most a
   constant ratio). The code is taken from Adam's paper, entitled ``Efficient sets---a balancing act,'' published in
   the \emph{Journal of Functional Programming} in 1993, with a few modifications. *)

module WeightBalance = struct

  (* [cardinal t] returns the cardinal (i.e. the weight) of a tree [t]. *)

  let cardinal = function
    | Empty -> 0
    | Node(_, _, _, n) -> n

  (* [create l v r] is a ``smart constructor'': it creates a new node with left son [l], value [v] and right son [r].
     Weight information for the new node is computed automatically. *)

  let create l v r =
    let ln = match l with Empty -> 0 | Node(_,_,_,n) -> n in
    let rn = match r with Empty -> 0 | Node(_,_,_,n) -> n in
    Node(l, v, r, ln + rn + 1)

  (* The balancing ratio. This constant can probably be changed, although this isn't clear from Adams' paper. *)

  let ratio = 5

  (* [bal] performs the same function as [create], but performs one step of rebalancing if necessary. *)

  let bal l v r =
    let ln = match l with Empty -> 0 | Node(_,_,_,n) -> n in
    let rn = match r with Empty -> 0 | Node(_,_,_,n) -> n in
    if ln + rn < 2 then
      Node(l, v, r, ln + rn + 1)
    else if rn > ratio * ln then begin
      match r with
      |	Empty -> assert false
      |	Node(rl, rv, rr, _) ->
	  if cardinal rl < cardinal rr then
	    create (create l v rl) rv rr
	  else begin
	    match rl with
	    | Empty -> assert false
	    | Node(rll, rlv, rlr, _) ->
		create (create l v rll) rlv (create rlr rv rr)
	  end
    end
    else if ln > ratio * rn then begin
      match l with
      | Empty -> assert false
      | Node(ll, lv, lr, _) ->
	  if cardinal lr < cardinal ll then
	    create ll lv (create lr v r)
	  else begin
	    match lr with
	    | Empty -> assert false
	    | Node(lrl, lrv, lrr, _)->
		create (create ll lv lrl) lrv (create lrr v r)
	  end
    end
    else
      Node(l, v, r, ln + rn + 1)

  (* Citing Adams' paper: ``[concat3] is the natural successor to [create] and [bal] in a hierarchy of smart
     constructors. This function forms a balanced tree from two non-overlapping trees of arbitrary size and a third
     item, an element which lies between the values in the first tree and those in the second tree. It runs in time
     proportional to the difference in the heights of the trees, so it is very fast for similar sized trees, and
     degenerates to an insert when joining a tiny tree to a big tree.'' This property allows [split] to take time
     $O(\log n)$, and [union] to run in time $O(n+m)$.

     Adams' implementation of [concat3] calls [add] when one of its arguments is an empty tree. This is slightly
     inefficient: because the element to be added is either smaller or greater than all elements in the tree, no
     comparisons have to be performed. We prefer to write two special-purpose versions of [add], called [add_left]
     and [add_right]. They perform no comparisons. This saves speed; furthermore, they are able to work without
     knowledge of the tree's ordering function, which is not the case of [add]. Having to pass the ordering around
     would be a nuisance here. *)

  let rec add_left x = function
    | Empty ->
	Node(Empty, x, Empty, 1)
    | Node(l, v, r, _) as t ->
	bal (add_left x l) v r

  let rec add_right x = function
    | Empty ->
	Node(Empty, x, Empty, 1)
    | Node(l, v, r, _) as t ->
	bal l v (add_right x r)

  let rec concat3 l v r =
    match (l, r) with
    | Empty, _ ->
	add_left v r
    | _, Empty ->
	add_right v l
    | Node(ll, lv, lr, ln), Node(rl, rv, rr, rn) ->
	if ratio * ln < rn then
	  bal (concat3 l v rl) rv rr
	else if ratio * rn < ln then
	  bal ll lv (concat3 lr v r)
	else
	  create l v r

end

module Weight =
  Make(WeightBalance)

