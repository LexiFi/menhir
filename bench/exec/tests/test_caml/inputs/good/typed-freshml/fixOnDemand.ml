(* -------------------------------------------------------------------------- *)

(* A specification of keys. All we need are maps over keys. *)

module type KEY = sig
  type key
  type 'a t
  val empty: 'a t
  val add: key -> 'a -> 'a t -> 'a t
  val find: key -> 'a t -> 'a
  val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

(* -------------------------------------------------------------------------- *)

(* A specification of lattices. *)

module type LATTICE = sig

  type property

  val bottom: property
  val equal: property -> property -> bool

end

(* -------------------------------------------------------------------------- *)

(* The fixed point computation functor. *)

module Make (K : KEY) (L : LATTICE) (T : sig

  open K
  open L

  val transfer: (key -> property) -> (key -> property)

end) = struct

  type key =
      K.key

  type property =
      L.property

  (* ------------------------------------------------------------------------ *)

  (* Set up an internal data structure for keys. *)

  module rec IKey : sig

    type ikey = {

	(* An integer identity. *)

	identity: int;

	(* The key that this internal key stands for. *)

	key: key;

	(* The current valuation at this key. *)

	mutable state: property;

	(* This key's subjects and observers. *)

	mutable subjects: ikey list;
	mutable observers: IKeySet.t;

      }

  end = struct

    type ikey = {
	identity: int;
	key: key;
	mutable state: property;
	mutable subjects: ikey list;
	mutable observers: IKeySet.t;
      }

  end

  and IKeySet : Set.S with type elt = IKey.ikey = Set.Make (struct
    open IKey
    type t = ikey
    let compare x y = x.identity - y.identity
  end)

  open IKey

  (* ------------------------------------------------------------------------ *)

  (* Set up a worklist of keys waiting to be processed.

     Invariant: a key never appears twice in the worklist. This is
     because a key is inserted into the worklist only when it is
     first initialized or when it is an observer of some other key;
     but, after it is inserted, it becomes initialized, and ceases to
     observe anything, so it cannot be inserted a second time.

     Invariant: whenever we receive control from or return control to
     the client, the worklist is empty.

     Should the worklist be FIFO or LIFO? It seems that it should be
     LIFO, so as to perform depth-first discovery of the graph and
     bottom-up evaluation. This should speed things up. *)

     (* TEMPORARY think more about complexity in practice *)

  module Worklist : sig

    val insert: ikey -> unit
    val work: (ikey -> unit) -> unit
    val is_empty: unit -> bool

  end = struct

    let worklist =
      Stack.create()

    let insert x =
      assert (x.subjects = []);
      Stack.push x worklist

    let rec work f =
      try
	while true do
	  f (Stack.pop worklist)
	done
      with Stack.Empty ->
	()

    let is_empty () =
      Stack.is_empty worklist

  end

  (* ------------------------------------------------------------------------ *)

  (* Subjects and observers.

     Invariant: a key in the worklist has no subjects. (It can have
     observers.) This means that an observer of some key is never in
     the worklist.

     Note: a key can observe itself. Note: a subject list can have
     duplicate elements. *)

  module SO : sig

    val observe: ikey -> ikey -> unit
    val signal: ikey -> unit

  end = struct

    (* [observe o s] makes [o] an observer of subject [s]. *)

    let observe o s =
      o.subjects <- s :: o.subjects;
      s.observers <- IKeySet.add o s.observers

    (* [signal s] sends a signal to all observers of subject [s]. Each
       observer that receives a signal ceases to be an observer of any
       subjects and is inserted into the worklist. *)

    let signal s =
      IKeySet.iter (fun o ->
	List.iter (fun s ->
	  s.observers <- IKeySet.remove o s.observers
        ) o.subjects;
	o.subjects <- [];
	Worklist.insert o
      ) s.observers;
      assert (IKeySet.is_empty s.observers)

  end

  (* ------------------------------------------------------------------------ *)

  (* Set up a mapping of keys to internal keys.

     Absence of a key [x] in this mapping means that [x] has not
     been initialized yet; in that case, a new internal key is
     initialized, and [x] is inserted into the worklist. *)

  module M : sig

    val lookup: key -> ikey
    val consult: key -> ikey option
    val fold: (key -> property -> 'a -> 'a) -> 'a -> 'a

  end = struct

    let state =
      ref K.empty

    let next =
      ref 0

    let consult k =
      try
	Some (K.find k !state)
      with Not_found ->
	None

    let lookup k =
      try
	K.find k !state
      with Not_found ->
	let identity = !next in
	next := identity + 1;
	let x = {
	  identity = identity;
	  key = k;
	  state = L.bottom;
	  subjects = [];
	  observers = IKeySet.empty;
	} in
	state := K.add k x !state;
	Worklist.insert x;
	x

    let fold f accu =
      K.fold (fun k x accu ->
	f k x.state accu
      ) !state accu

  end

  (* ------------------------------------------------------------------------ *)

  (* Worklist processing. *)

  let process () =
    Worklist.work (fun x ->

      (* At this point, [x] has no subjects, since it was in the
         worklist. This is important, since [transfer] will invoke
         [observe x] and add new subjects. *)

      assert (x.subjects = []);

      (* Get the current value at [x]. *)

      let current : property = x.state in

      (* Compute an updated value at [x]. *)

      (* We make sure that every client call to [get] is accompanied
	 with a call to [observe], so that the transfer function
	 automatically observes all of the arguments that it
	 gets. This yields a set of dependencies that can be
	 reasonably fine-grained, and is correct by construction. *)

      let updated : property =
	T.transfer (fun y ->
	  let y = M.lookup y in
	  SO.observe x y;
	  y.state
        ) x.key
      in

      (* If the updated value at [x] differs from the old value,
	 record the updated value and send a signal to all observers
	 of [x]. *)

      if not (L.equal current updated) then begin
	x.state <- updated;
        SO.signal x
      end

    )

  (* ------------------------------------------------------------------------ *)

  (* Requests normally trigger evaluation. *)

  let get k =
    assert (Worklist.is_empty());
    let x = M.lookup k in
    process();
    x.state

  (* When evaluation is forbidden, requests can fail. *)

  let consult k =
    assert (Worklist.is_empty());
    match M.consult k with
    | Some x ->
	Some x.state
    | None ->
	None

  (* ------------------------------------------------------------------------ *)

  (* Iteration over all keys where the least fixed point is currently
     known (thanks to previous calls to [get]). No new computation is
     triggered. *)

  let fold f accu =
    assert (Worklist.is_empty());
    M.fold f accu

end

