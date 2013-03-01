(* This module offers a purely functional implementation of Tarjan's
   data structure for solving the union-find problem. *)

(* ------------------------------------------------------------------------- *)

(* Items. *)

module type Item = sig

  type t

  val equal: t -> t -> bool

  module Map : sig
    type key = t
    type 'a t
    val empty: 'a t
    val find: key -> 'a t -> 'a
    val add: key -> 'a -> 'a t -> 'a t
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

end

(* ------------------------------------------------------------------------- *)

(* Descriptors. *)

module type Desc = sig

  type descriptor

  val default: descriptor

  type accumulator

  val union: descriptor -> descriptor -> accumulator -> descriptor * accumulator

  (* [default] should be a neutral element for [union]. [union] should
     be commutative and associative. *)

end

(* ------------------------------------------------------------------------- *)

(* Here are the operations provided by this module. *)

module type S = sig

  type item

  type descriptor

  type accumulator

  (* A state consists of a partition of the items (or, in other words,
     of an equivalence relation over items), together with a total
     mapping of the partition blocks (or, in other words, of the
     equivalence classes) to descriptors. *)

  type state

  (* In the initial state, each item forms its own block, and each
     block is mapped to the [default] descriptor. *)

  val initial: state

  (* [representative] maps each item to a representative member of its
     block. *)

  val representative: item -> state -> item

  (* [equivalent] tells whether two items are members of the same
     block. *)

  val equivalent: item -> item -> state -> bool

  (* [descriptor] maps (items to) blocks to descriptors. *)

  val descriptor: item -> state -> descriptor

  (* [set] changes the descriptor associated with a block. *)

  val set: item -> descriptor -> state -> state

  (* [union] merges two blocks. The new block's descriptor is obtained
     as the union of the two original descriptors. Computing the union
     of two descriptors can have side effects, reflected via the
     [accumulator] monad. *)

  val union: item -> item -> state -> accumulator -> state * accumulator

end

(* ------------------------------------------------------------------------- *)

(* Implementation. *)

module Make (Item : Item) (Desc : Desc) = struct

  type item =
      Item.t

  type descriptor =
      Desc.descriptor

  type accumulator =
      Desc.accumulator

  type link =
    | Root of int (* weight *) * Desc.descriptor (* descriptor *)
    | Link of Item.t

  type state = {
      mutable forest: link Item.Map.t
    }

  let initial = {
    forest = Item.Map.empty
  }

  let find item state =
    try
      Item.Map.find item state.forest
    with Not_found ->
      assert false

  let rec follow item item' state =
    match find item' state with
    | Root (weight, descriptor) ->
	item', weight, descriptor
    | Link item'' ->
	let (item'', _, _) as answer =
	  follow item' item'' state
	in
	state.forest <- Item.Map.add item (Link item'') state.forest;
	answer

  let examine item state =
    try
      let item, weight, descriptor =
	match Item.Map.find item state.forest with
	| Root (weight, descriptor) ->
	    item, weight, descriptor
	| Link item' ->
	    follow item item' state
      in
      item, weight, descriptor, 0
    with Not_found ->
      item, 0, Desc.default, 1

  let representative item state =
    let item, _, _, _ = examine item state in
    item

  let equivalent item1 item2 state =
    Item.equal (representative item1 state) (representative item2 state)

  let descriptor item state =
    let _, _, descriptor, _ = examine item state in
    descriptor

  let set item descriptor state =
    let item, weight, _, increase = examine item state in
    { forest = Item.Map.add item (Root (weight, descriptor)) state.forest }

  let union item1 item2 state accu =
    let item1, weight1, descriptor1, increase1 = examine item1 state
    and item2, weight2, descriptor2, increase2 = examine item2 state in
    if Item.equal item1 item2 then
      state, accu
    else
      let descriptor, accu = Desc.union descriptor1 descriptor2 accu in
      let root = Root (weight1 + weight2, descriptor) in
      let link1, link2 =
	if weight1 >= weight2 then
	  root,
	  Link item1
	else
	  Link item2,
	  root
      in
      { forest = Item.Map.add item1 link1 (Item.Map.add item2 link2 state.forest) },
      accu

end
