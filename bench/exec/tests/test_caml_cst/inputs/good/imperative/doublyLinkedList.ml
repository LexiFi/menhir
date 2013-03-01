(* This module implements a mutable set with constant time operations. *)

(* ------------------------------------------------------------------------- *)

(* This signature documents the set operations offered by this module. *)

module type S = sig

  (* Elements. *)

  type t

  (* When a new, non-sentinel element [x] is allocated by the client,
     it must be initialized by invoking [init x]. The element [x] is
     initially considered not to be a member of the set. *)

  val init: t -> unit

  (* [mem x] determines whether [x] is a member of the set. [x] must
     not be the sentinel. *)

  val mem: t -> bool

  (* [insert x] inserts [x] into the set. [x] must not be the
     sentinel. *)

  val insert: t -> unit

  (* [remove x] removes [x] from the set. [x] must not be the
     sentinel. *)

  val remove: t -> unit

  (* [is_empty()] determines whether the set is empty. *)

  val is_empty: unit -> bool

  (* [choose()] returns an arbitrary member [x] of the set. The set
     must be non-empty. [x] is not removed from the set. *)

  val choose: unit -> t

  (* [fold f accu] folds [f] over all elements of the set, not
     including the sentinel. [f] is not allowed to modify the set via
     its side effects. *)

  val fold: (t -> 'a -> 'a) -> 'a -> 'a

end

(* ------------------------------------------------------------------------- *)

(* This functor implements the above signature. It relies on a sentinel
   element, allocated by the client. *)

module MakeWithSentinel (X : sig

  (* Elements. *)

  type t

  (* Equality. *)

  val equal: t -> t -> bool

  (* A dummy element is used as a sentinel. *)

  val sentinel: t

  (* Each element, including the sentinel, must carry [prev] and
     [next] fields, which are read and written using the functions
     below. *)

  val get_prev: t -> t
  val set_prev: t -> t -> unit
  val get_next: t -> t
  val set_next: t -> t -> unit

end) : S with type t = X.t = struct

  type t =
      X.t

  open X

  (* Elements are organized in a circular doubly linked list. The list
     is never empty, since the sentinel is always a member of
     it. Initially, the sentinel is the only member of the list. *)

  (* When a non-sentinel element [x] is not a member of the list, its
     [prev] and [next] fields are set to point to [x] itself. (Setting
     them both to [sentinel] would not work -- it would not allow
     telling with certainty that [x] is not in the list.) This allows
     testing for membership in constant time and has the beneficial
     side effect of avoiding space leaks. *)

  let () =
    set_prev sentinel sentinel;
    set_next sentinel sentinel

  (* When a new, non-sentinel element [x] is allocated by the client,
     it must be initialized by invoking [init x]. The element [x] is
     initially considered not to be a member of the set. *)

  let init x =
    assert (not (equal x sentinel));
    set_prev x x;
    set_next x x

  (* [mem x] determines whether [x] is a member of the set. [x] must
     not be the sentinel. *)

  let mem x =
    assert (not (equal x sentinel));
    let successor = get_next x in
    not (equal x successor)

  (* [insert x] inserts [x] into the set. [x] must not be the
     sentinel. *)

  let insert x =
    assert (not (equal x sentinel));
    if not (mem x) then
      let predecessor = sentinel in
      let successor = get_next predecessor in
      assert (not (equal x successor));
      set_prev x predecessor;
      set_next x successor;
      set_next predecessor x;
      set_prev successor x;
      assert (mem x)

  (* [remove x] removes [x] from the set. [x] must not be the
     sentinel. *)

  let remove x =
    assert (not (equal x sentinel));
    if mem x then
      let predecessor = get_prev x
      and successor = get_next x in
      assert (not ((equal x predecessor) or (equal x successor)));
      set_next predecessor successor;
      set_prev successor predecessor;
      set_next x x;
      set_prev x x;
      assert (not (mem x))

  (* [is_empty()] determines whether the set is empty. *)

  let is_empty () =
    let head = get_next sentinel in
    equal sentinel head

  (* [choose()] returns an arbitrary member [x] of the set. The set
     must be non-empty. [x] is not removed from the set. *)

  let choose () =
    let head = get_next sentinel in
    assert (not (equal head sentinel));
    assert (mem head);
    head

  (* [fold f accu] folds [f] over all elements of the set, not
     including the sentinel. [f] is not allowed to modify the set via
     its side effects. *)

  let fold f accu =
    let rec fold x accu =
      if equal x sentinel then
	accu
      else begin
	assert (mem x);
	let accu = f x accu in
	let successor = get_next x in
	fold successor accu
      end
    in
    let head = get_next sentinel in
    fold head accu

end

(* ------------------------------------------------------------------------- *)

(* This functor implements the above signature. It does not require a
   sentinel element, but allocates options instead, which makes it
   possibly less efficient. *)

module MakeWithoutSentinel (X : sig

  (* Elements. *)

  type t

  (* Equality. *)

  val equal: t -> t -> bool

  (* Each element must carry [prev] and [next] fields, which are read
     and written using the functions below. *)

  val get_prev: t -> t option
  val set_prev: t -> t option -> unit
  val get_next: t -> t option
  val set_next: t -> t option -> unit

end) : S with type t = X.t = struct

  module S = MakeWithSentinel (struct

    type t = X.t option

    let equal x1 x2 =
      match x1, x2 with
      | None, None ->
	  true
      | Some x1, Some x2 ->
	  X.equal x1 x2
      | Some _, None
      | None, Some _ ->
	  false

    let sentinel =
      None

    let sentinel_prev =
      ref None

    let get_prev = function
      | None ->
	  !sentinel_prev
      | Some x ->
	  X.get_prev x

    let set_prev x y =
      match x with
      | None ->
	  sentinel_prev := y
      | Some x ->
	  X.set_prev x y

    let sentinel_next =
      ref None

    let get_next = function
      | None ->
	  !sentinel_next
      | Some x ->
	  X.get_next x

    let set_next x y =
      match x with
      | None ->
	  sentinel_next := y
      | Some x ->
	  X.set_next x y

  end)

  type t =
      X.t

  let init x =
    S.init (Some x)

  let mem x =
    S.mem (Some x)

  let insert x =
    S.insert (Some x)

  let remove x =
    S.remove (Some x)

  let is_empty =
    S.is_empty

  let choose () =
    match S.choose() with
    | None ->
	assert false
    | Some x ->
	x

  let fold f accu =
    S.fold (fun x accu ->
      match x with
      | None ->
	  assert false
      | Some x ->
	  f x accu
    ) accu

end

