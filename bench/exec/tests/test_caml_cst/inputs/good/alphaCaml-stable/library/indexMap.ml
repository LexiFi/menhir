(* This functor turns an implementation of sets and maps over integers
   into an implementation of sets and maps over an indexed type. An
   indexed type is a type whose values can be mapped to integers in an
   injective manner (that is, no two elements have the same index). *)

module Make (I : sig

  type t

  val index: t -> int

end) (M : GMap.S with type key = int) = struct

  let equal x y =
    I.index x = I.index y

(* ---------------------------------------------------------------------------- *)

  module Set = struct

    (* Sets of elements are represented as maps of integers to elements. *)

    type element =
	I.t

    type t =
	element M.t

    let empty =
      M.empty

    let add a s =
      M.add (I.index a) a s

    let remove a s =
      M.remove (I.index a) s

    let singleton a =
      M.singleton (I.index a) a

    let union =
      M.union

    let mem a s =
      M.mem (I.index a) s

    let decide a b =
      assert (equal a b);
      b

    let inter s1 s2 =
      M.inter decide s1 s2

    let is_empty =
      M.is_empty

    let disjoint s1 s2 =
      M.disjoint s1 s2

    let compare s1 s2 =
      M.compare (fun a b ->
	assert (equal a b);
	0
      ) s1 s2

    let equal s1 s2 =
      compare s1 s2 = 0

    let subset =
      M.subset

    let fold f s accu =
      M.fold (fun _ a accu ->
	f a accu
      ) s accu

    let iter f s =
      M.iter (fun _ a ->
	f a
      ) s

    let diff =
      M.diff

    let cardinal =
      M.cardinal

    let choose s =
      let _, a = M.choose s in
      a

    let iterator s =
      let i = M.iterator s in
      fun () ->
	match i() with
	| None ->
	    None
	| Some (_, a) ->
	    Some a

    let print print_element b s =
      Print.sepiterator Print.comma print_element b (iterator s)

  end

(* ---------------------------------------------------------------------------- *)

  module Map = struct

    (* Maps over elements are represented as maps over integers. The
       image of an integer [i] through the map is a pair of the
       element whose index is [i] and of the actual data. *)

    type key =
	I.t

    type 'a t =
	(key * 'a) M.t

    let empty =
      M.empty

    let is_empty =
      M.is_empty

    let singleton a v =
      M.singleton (I.index a) (a, v)

    let add a v m =
      M.add (I.index a) (a, v) m

    type 'a add_or_lookup =
      | Added of 'a t
      | LookedUp of 'a

    let add_or_lookup a v m =
      match M.add_or_lookup (I.index a) (a, v) m with
      | M.Added m ->
	  Added m
      | M.LookedUp (_, v) ->
	  LookedUp v

    exception Strict of key

    let strict_add a v m =
      try
	M.strict_add (I.index a) (a, v) m
      with M.Unchanged ->
	raise (Strict a)

    let remove a m =
      M.remove (I.index a) m

    let union =
      M.union

    let lookup a m =
      let _, v = M.lookup (I.index a) m in
      v

    let find =
      lookup

    let mem a m =
      M.mem (I.index a) m

    let map f m =
      M.map (fun (a, v) -> (a, f v)) m

    let endo_map f m =
      M.map (fun (a, v) -> (a, f v)) m

    let iter f m =
      M.iter (fun _ (a, v) ->
	f a v
      ) m

    let fold f m accu =
      M.fold (fun _ (a, v) accu ->
	f a v accu
      ) m accu

    let mapi f m =
      fold (fun a v m ->
	add a (f a v) m
      ) m empty

    let cardinal =
      M.cardinal

    let choose m =
      let _, binding = M.choose m in
      binding

    let iterator s =
      let i = M.iterator s in
      fun () ->
	match i() with
	| None ->
	    None
	| Some (_, binding) ->
	    Some binding

    let domain m =
      M.map (fun (a, _) -> a) m

    let print print_key print_data b m =
      iter (fun a v ->
	Printf.bprintf b "%a --> %a\n" print_key a print_data v
      ) m

    (* TEMPORARY implémenter lift, restrict, corestrict? *)

  end

end

