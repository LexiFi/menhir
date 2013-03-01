(* $Header: /home/yquem/cristal/fpottier/cvs/pappy/compressedTrie.ml,v 1.1 2004/03/18 15:31:54 fpottier Exp $ *)

(* This module implements a certain flavor of path- and level-compressed
   binary tries. These offer essentially the same programming interface
   as an array, except the key range needs not be fixed in advance. These
   tries have the property of being more space-efficient than arrays when
   they contain few elements whose value differs from the default, and of
   being equally space-efficient when they contain many elements. In other
   words, they may be viewed as sparse arrays. *)

module Make (BitManip : sig

  (* [highest_bit i] returns a bit mask where the highest (most
     significant) bit of [i] is set and all other bits are zero. [i]
     must be nonzero. Because an efficient implementation of this
     function is architecture-dependent, we do not provide one. *)
   val highest_bit: int -> int

end) : sig

  (* Keys. *)
  type key = (* nonnegative *) int

  (* Tries. *)
  type 'a t

  (* [create default] returns a fresh trie, where every key is mapped to
     [default]. *)
  val create: 'a -> 'a t

  (* [get t key] returns the value associated to the key [key] in the
     trie [t]. *)
  val get: 'a t -> key -> 'a

  (* [set t key data] updates the trie [t] with a mapping of [key] to
     [data]. The previous binding for [key] is overridden. *)
  val set: 'a t -> key -> 'a -> unit

end = struct

  type key = int
  type 'a t = {
      default: 'a;
      mutable trie: 'a trie
    } 

  (* A trie consists of leaf nodes and branch nodes.

     Every leaf node contains an array, which stores a segment of the
     virtual array implemented by the trie. The segment is identified
     by an integer prefix, whose [k] lower bits are zero, and a bit
     mask, whose [k] lower bits are one and whose other bits are zero,
     for some value of [k]. The size of the array is $2^k$, which may
     be computed as the mask plus one.

     Every branch node contains an integer prefix, whose [k] lower
     bits are zero, a bit mask, whose [k]th bit is one and whose other
     bits are zero, an approximation of the amount of memory used by
     this node and its sub-nodes, and two sub-tries. *)

  and 'a trie =
    | Leaf of (* prefix: *) int * int (* bits: *) * 'a array
    | Branch of (* prefix: *) int * (* bit: *) int
       * (* mutable words: *) int * (* mutable left: *) 'a trie * (* mutable right: *) 'a trie

  (* This record type is used to update the fields [words], [left],
     and [right] in place in [Branch] nodes. This is somewhat ugly,
     but saves some time and space. *)

  type 'a branch = {
      prefix: int;
      bit: int;
      mutable words: int;
      mutable left: 'a trie;
      mutable right: 'a trie
    } 

  (* [assess trie] returns an approximation of the number of memory words
     used by the trie [trie]. *)

  let assess = function
    | Leaf (_, bits, _) ->
	bits + 6
    | Branch (_, _, words, _, _) ->
	words

  (* [singleton key data] returns a fresh leaf node that maps [key] to
     [data]. *)

  let singleton key data =
    Leaf (key, 0, Array.create 1 data)

  (* [create default] returns a fresh trie, where every key is mapped to
     [default]. *)

  let create default = {
    default = default;
    trie = singleton 0 default (* We have no representation for the empty trie; a singleton will do. *)
  } 

  (* [get t key] returns the value associated to the key [key] in the
     trie [t].

     This implementation follows branches without checking whether the
     key matches the prefix found at the current node. This means that
     a query for a non-existent key shall be detected only when
     finally reaching a leaf, rather than higher up in the tree. This
     strategy is better when (most) queries are expected to be
     successful. *)

  let get { default = default; trie = trie } =

    let rec get trie key =
      match trie with
      | Leaf (prefix, bits, array) ->
	  if prefix = key land (lnot bits) then
	    Array.unsafe_get array (key land bits)
	  else
	    default
      | Branch (_, bit, _, left, right) ->
	  get (if key land bit = 0 then left else right) key

    in
    get trie

  (* The auxiliary function [join p0 trie0 p1 trie1] merges the tries
     [trie0] and [trie1], assuming that their prefixes [p0] and [p1]
     disagree. This is simply a matter of creating a new branch node,
     whose prefix is the longest common prefix of [p0] and [p1], and
     whose subtries are [trie0] and [trie1]. *)

  let join p0 trie0 p1 trie1 =
    let bit = BitManip.highest_bit (p0 lxor p1) in
    let prefix = p0 (* for instance *) land (lnot (2 * bit - 1)) in
    if p0 land bit = 0 then
      Branch (prefix, bit, 0 (* dummy *), trie0, trie1)
    else
      Branch (prefix, bit, 0 (* dummy *), trie1, trie0)

  (* [compress default trie] examines the trie [trie], whose top node
     must be a [Branch] node. It assesses how much memory the trie
     uses, and updates its [words] field. If the trie uses more memory
     than a flat array, then it is turned into one, by creating a
     [Leaf] node; otherwise it is left unchanged. *)

  let compress default trie =
    let branch = (Obj.magic trie : 'a branch) in
    let words = assess branch.left + assess branch.right + 6 in
    branch.words <- words;
    let new_array_size = 2 * branch.bit in
    if (new_array_size > Sys.max_array_length) || (words < new_array_size + 5) then
      trie
    else
      let array = Array.create new_array_size default in
      let bits = new_array_size - 1 in

      let rec iter = function
	| Leaf (prefix, leaf_bits, leaf_array) ->
	    let prefix = prefix land bits in
	    for j = 0 to leaf_bits do
	      Array.unsafe_set array (prefix + j) (Array.unsafe_get leaf_array j)
	    done
	| Branch (_, _, _, left, right) ->
	    iter left;
	    iter right

      in
      iter branch.left;
      iter branch.right;
      Leaf (branch.prefix, bits, array)

  (* [set t key data] updates the trie [t] with a mapping of [key] to
     [data]. The previous binding for [key] is overridden.

     In each case, we check whether the key [key] matches the prefix
     associated with the node at hand. If it does, we destructively
     update the array (for leaf nodes) or the appropriate subtrie (for
     branch nodes). If it does not, we create a new branch node at
     this point.

     After the update, all nodes that lie on the path between the root
     and the leaf that was modified are compressed, if they have
     become too fat. *)

  let set t key data =

    let rec set trie key data =
      match trie with
      | Leaf (prefix, bits, array) when prefix = key land (lnot bits) ->
	  Array.unsafe_set array (key land bits) data;
	  trie
      | Branch (prefix, bit, _, left, right) when prefix = key land (lnot (2 * bit - 1)) ->
	  if key land bit = 0 then
	    (Obj.magic trie : 'a branch).left <- set left key data
	  else
	    (Obj.magic trie : 'a branch).right <- set right key data;
	  compress t.default trie
      | Leaf (prefix, _, _)
      | Branch (prefix, _, _, _, _) ->
	  compress t.default (join key (singleton key data) prefix trie)

    in
    t.trie <- set t.trie key data

end
