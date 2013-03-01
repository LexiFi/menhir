module type Item = sig

  (* This is the type of items. *)

  type t

  (* This is equality of items. *)

  val equal: t -> t -> bool

  (* Generation of fresh items. *)

  val fresh: unit -> t

  (* This module offers maps over items. *)

  module Map : sig

    type key = t
    type 'a t

    val empty: 'a t
    val find: key -> 'a t -> 'a
    val add: key -> 'a -> 'a t -> 'a t
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

  end

end

module type Desc = sig

  (* This is the type of descriptors. *)

  type descriptor

  (* A default descriptor is implicitly associated with every item. *)

  val default: descriptor

  (* Combining descriptors. *)

  type accumulator

  val union: descriptor -> descriptor -> accumulator -> descriptor * accumulator

end

module type S = sig

  type item

  type descriptor

  type accumulator

  type partition

  val empty: partition

  val representative: item -> partition -> item

  val equivalent: item -> item -> partition -> bool

  val descriptor: item -> partition -> descriptor

  val set: item -> descriptor -> partition -> partition

  val union: item -> item -> partition -> accumulator -> partition * accumulator

  val fusion: partition -> partition -> accumulator -> partition * accumulator

  val fold: (item -> descriptor -> 'a -> 'a) -> partition -> 'a -> 'a

end

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

  type partition = {
      mutable forest: link Item.Map.t;
      cardinal: int
    }

  let empty = {
    forest = Item.Map.empty;
    cardinal = 0
  }

  let find item partition =
    try
      Item.Map.find item partition.forest
    with Not_found ->
      assert false

  let rec follow item item' partition =
    match find item' partition with
    | Root (weight, descriptor) ->
	item', weight, descriptor
    | Link item'' ->
	let (item'', _, _) as answer =
	  follow item' item'' partition
	in
	partition.forest <- Item.Map.add item (Link item'') partition.forest;
	answer

  let examine item partition =
    try
      let item, weight, descriptor =
	match Item.Map.find item partition.forest with
	| Root (weight, descriptor) ->
	    item, weight, descriptor
	| Link item' ->
	    follow item item' partition
      in
      item, weight, descriptor, 0
    with Not_found ->
      item, 0, Desc.default, 1

  let representative item partition =
    let item, _, _, _ = examine item partition in
    item

  let equivalent item1 item2 partition =
    Item.equal (representative item1 partition) (representative item2 partition)

  let descriptor item partition =
    let _, _, descriptor, _ = examine item partition in
    descriptor

  let set item descriptor partition =
    let item, weight, _, increase = examine item partition in
    { forest = Item.Map.add item (Root (weight, descriptor)) partition.forest;
      cardinal = partition.cardinal + increase }

  let union item1 item2 partition accu =
    let item1, weight1, descriptor1, increase1 = examine item1 partition
    and item2, weight2, descriptor2, increase2 = examine item2 partition in
    if Item.equal item1 item2 then
      partition, accu
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
      { forest = Item.Map.add item1 link1 (Item.Map.add item2 link2 partition.forest);
	cardinal = partition.cardinal + increase1 + increase2 },
      accu

  let fusion partition1 partition2 accu =
    let smaller, greater =
      if partition1.cardinal <= partition2.cardinal then
	partition1, partition2
      else
	partition2, partition1
    in
    (* TEMPORARY perform eager path compression over smaller forest
       before doing this *)
    Item.Map.fold (fun item link (partition, accu) ->
      match link with
      | Root (_, descriptor) ->
	  set item descriptor partition, accu
      | Link item' ->
	  let item'', _, _ = follow item item' partition in
	  union item item'' partition accu
    ) smaller.forest (greater, accu)

  let fold f partition accu =
    Item.Map.fold (fun item link accu ->
      match link with
      | Root (_, descriptor) ->
	  f item descriptor accu
      | Link _ ->
	  accu
    ) partition.forest accu

end
