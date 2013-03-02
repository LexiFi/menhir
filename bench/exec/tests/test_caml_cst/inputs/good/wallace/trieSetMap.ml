(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/trieSetMap.ml,v 1.3 2000/02/11 16:15:54 fpottier Exp $ *)

(* This module, parameterized over an implementation of sets, provides maps whose keys are sets. As its name implies,
   its implementation uses tries. *)

module Make (X : sig

  type 'a t
  type 'a ordering = 'a -> 'a -> int

  val make2: 'a ordering -> 'a -> 'a -> 'a t
  val add: 'a ordering -> 'a -> 'a t -> 'a t
  val union: 'a ordering -> 'a t -> 'a t -> 'a t
  val iter: ('a -> unit) -> 'a t -> unit
  val iterator: 'a t -> unit -> 'a option

end) = struct

  (* Our [Set] component is simply [X]. *)

  module Set = X 

  module Map = struct

    (*i ----------------------------------------------------------------------------------------------------------- i*)
    (*s \mysection{Data structure} *)

    (* Maps are binary tries. A trie is either the empty trie, representing an empty map, a leaf carrying a piece
       of data, or a binary node, carrying an element and two sons. The left son contains all keys (i.e. all sets)
       which do not this element, and the right son contains all keys (i.e. all sets) which do contain it. The
       elements found along a single branch of the trie are sorted in increasing order. If the trie does not contain
       a node for a certain element, then this element is understood as missing from all keys. *)

    type ('a, 'b) t =
      |	Empty
      |	Leaf of 'b
      |	Node of 'a * ('a, 'b) t * ('a, 'b) t

    type 'a ordering = 'a X.ordering

    (*i ----------------------------------------------------------------------------------------------------------- i*)
    (*s \mysection{The empty map} *)

    let empty =
      Empty

    (*i ----------------------------------------------------------------------------------------------------------- i*)
    (*s \mysection{Searching} *)

    (* Assuming comparing basic set elements takes time $O(1)$, the search is performed in time $O(N)$,
       where $N$ is the trie's depth (i.e. the size of the biggest set added to the trie so far). *)

    let find o key m =

      (* The key is a set. We iterate over its elements, sorted in increasing order. This describes a path within the
       trie, which we follow. *)

      let next = X.iterator key in

      let rec find element_option m =

	(* Obtain the set's next element. We may already have read in a previous invocation of this recursive
	   function. If so, we keep it. Otherwise, we ask for a new one from the iterator.

	   Using a stackless iterator allows us to iterate over the set and over the trie at the same time, while
	   using the call stack to follow the latter structure. *)

	let element_option =
	  match element_option with
	  | Some _ ->
	      element_option
	  | None ->
	      next() in

	(* Two cases, depending on whether we have exhausted the set or not. *)

	match element_option with
	| Some element -> (

	    (* We are looking at the set's current element. *)

	    match m with
	    | Empty
	    | Leaf _ ->
	    
    	        (* We are presented with one more element, but the map either is empty or contains exactly one key,
		   a set which does not contain the current element. Fail. *)

		raise Not_found

	    | Node (element', left, right) ->

		let c = o element element' in
		if c < 0 then

		  (* We are presented with an element smaller than the current node's. Because elements are sorted
		     along each branch of the trie, this means that this element does not have a node along this
		     branch. Thus, no eligible key contains this element. Fail. *)

		  raise Not_found

		else if c = 0 then

		  (* We are presented with an element equal to the current node's. Discard the current element, and
		     look into the node's right son, which corresponds to the sub-map whose keys do contain this
		     element. *)

		  find None right

		else

		  (* We are presented with an element greater than the current's node. Because elements of the set
		     being looked for are presented in increasing order, this set does not contain the current node's
		     element. Keep the current element, and look into the node's left son, which corresponds to the
		     sub-map whose keys do not contain this element. *)

		  find (Some element) left

	  )
	| None ->
	    find_nothing m

      (* [find_nothing m] is equivalent to [find None m], when all elements of the key have been
	 exhausted. Considering that once the iterator has returned [None], it will continue doing so forever,
	 [find_nothing] can directly call itself recursively, rather than going through [find] and performing several
	 redundant checks. *)

      and find_nothing m =

	(* The set has no more elements. *)

	match m with
	| Empty ->
	    raise Not_found
	| Leaf data ->
	    data
	| Node (_, left, _) ->
	    find_nothing left

      in find None m

    (*i ----------------------------------------------------------------------------------------------------------- i*)
    (*s \mysection{Adding a binding} *)

    (* Assuming comparing basic set elements takes time $O(1)$, the addition is performed in time $O(\max(n, N))$,
       where $n$ is the size of the set being added, and $N$ is the trie's depth (i.e. the size of the biggest set
       added to the trie so far). *)

    let add o key data m =

      let next = X.iterator key in

      let rec add element_option m =

	(* Obtain the set's next element. We may already have read in a previous invocation of this recursive
	   function. If so, we keep it. Otherwise, we ask for a new one from the iterator.

	   Using a stackless iterator allows us to iterate over the set and over the trie at the same time, while
	   using the call stack to follow the latter structure. *)

	let element_option =
	  match element_option with
	  | Some _ ->
	      element_option
	  | None ->
	      next() in

	(* Two cases, depending on whether we have exhausted the set or not. *)

	match element_option with
	| Some element -> (

	    (* We are looking at the set's current element. *)

	    match m with
	    | Empty
	    | Leaf _ ->

		(* The trie does not have a node for the current element. Add the following elements to an empty trie;
		   then, add a binary node for the current element on top of the result. *)

		Node(element, m, add None Empty)

	    | Node(element', left, right) ->
		let c = o element element' in

		if c < 0 then

		  (* The trie does not have a node for the current element. Same behavior as above. *)

		  Node(element, m, add None Empty)

		else if c = 0 then

		  (* The trie has a node for the current element. Add the following elements to its right son.
		     The current code does not attempt to preserve sharing, but could do so if desired. *)

		  Node(element', left, add None right)

		else

		  (* The trie has a node for an element which the current key does not have. Add all of our elements,
		     including the one we just read, to its left son. The current code does not attempt to preserve
		     sharing, but could do so if desired. *)

		  Node(element', add (Some element) left, right)

          )
	| None ->
	    add_nothing m

      (* [add_nothing m] is equivalent to [add None m], when all elements of the key have been exhausted. Considering
	 that once the iterator has returned [None], it will continue doing so forever, [add_nothing] can directly call
	 itself recursively, rather than going through [add] and performing several redundant checks. *)

      and add_nothing m =

	(* The set has no more elements. *)

	match m with
	| Empty
	| Leaf _ ->

	    (* If there was no binding, define the new one. If there was a previous binding, discard it silently. The
		current code does not attempt to preserve sharing, but could do so if desired. *)

	    Leaf data

	| Node(element', left, right) ->

	    (* The trie has a node for an element which does not appear in the set. Update its left son. The current
	       code does not attempt to preserve sharing, but could do so if desired. *)

	    Node(element', add_nothing left, right)

      in add None m

  end

end

