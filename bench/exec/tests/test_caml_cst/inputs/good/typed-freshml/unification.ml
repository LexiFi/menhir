(* ------------------------------------------------------------------------- *)

(* Signatures. *)

module type Term = sig

  (* Shallow term formers. *)

  type 'a t

  (* Deep term formers. *)

  type 'a dt =
    | DTVar of 'a
    | DTTerm of 'a dt t

  (* Operations on shallow term formers. *)

  exception Decompose

  val map: ('a -> 'b) -> 'a t -> 'b t

  val decompose: 'a t -> 'a t -> ('a * 'a) list -> ('a * 'a) list

end

module type ShallowUnifier = sig

  type 'a t
  type 'a dt
  type variable

  (* Shallow terms. *)

  type shallow_term =
      variable t

  include UnionFind.S with type item = variable
                       and type accumulator = (variable * variable) list

  val make: partition -> shallow_term -> partition * variable
  val unify: partition -> accumulator -> partition

  (* Deep terms. *)

  type deep_term =
      variable dt

  val unifyd: partition -> deep_term -> deep_term -> partition
  val unchop_variable: partition -> variable -> deep_term
  val unchop_term: partition -> shallow_term -> deep_term

end

module type DeepUnifier = sig

  type 'a t
  type 'a dt
  type variable

  type deep_term =
      variable dt

  include UnionFind.S with type item = variable
                       and type accumulator = (deep_term * deep_term) list
		       and type descriptor = deep_term t option

  val unify: partition -> accumulator -> partition

end

(* ------------------------------------------------------------------------- *)

(* Unification based on shallow terms. *)

module Shallow (Item : UnionFind.Item) (Term : Term) (Desc : sig

  include UnionFind.Desc with type accumulator = (Item.t * Item.t) list

  val inject: Item.t Term.t -> descriptor

  val project: descriptor -> Item.t Term.t option

end) = struct

  type 'a t =
      'a Term.t

  type 'a dt =
      'a Term.dt

  type variable =
      Item.t

  type shallow_term =
      variable t

  module U = UnionFind.Make (Item) (Desc)

  include U

  let make partition term =
    let v = Item.fresh () in
    set v (Desc.inject term) partition, v

  let rec unify partition pending =
    match pending with
    | [] ->
	partition
    | (variable1, variable2) :: pending ->
	let partition, pending = union variable1 variable2 partition pending in
	unify partition pending

  (* Deep terms. *)

  type deep_term =
      variable dt

  let chop partition term =
    let partition =
      ref partition
    in
    let rec chop = function
    | Term.DTVar v ->
	v
    | Term.DTTerm term ->
	let p, v = make !partition (Term.map chop term) in
	partition := p;
	v
    in
    let v = chop term in
    !partition, v

  let unifyd partition term1 term2 =
    let partition, v1 = chop partition term1 in
    let partition, v2 = chop partition term2 in
    unify partition [ v1, v2 ]

  (* [unchop_variable] and [unchop_term] attempt to be inverses of
     [chop]. They turn a variable or a shallow term into a deep
     term. They can loop if the partition is cyclic. They stop at
     variables whose descriptor is empty -- these are just variables
     -- and at variables whose descriptor contains more than one term
     -- these are erroneous terms. *)

  let rec unchop_variable partition v =
    match Desc.project (descriptor v partition) with
    | Some term ->
	unchop_term partition term
    | None ->
	Term.DTVar v

  and unchop_term partition term =
    Term.DTTerm (Term.map (unchop_variable partition) term)

end

(* ------------------------------------------------------------------------- *)

(* To each equivalence class, associate an optional term. Fail
   immediately if two non-unifiable terms are discovered. *)

module ShallowMono (Item : UnionFind.Item) (Term : Term) =
  Shallow (Item) (Term) (struct

    type descriptor =
	Item.t Term.t option

    let default =
      None

    let inject term =
      Some term

    let project d =
      d

    type accumulator =
	(Item.t * Item.t) list

    let union descriptor1 descriptor2 pending =
      match descriptor1, descriptor2 with
      | None, None ->
	  None, pending
      | Some _, None ->
	  descriptor1, pending
      | None, Some _ ->
	  descriptor2, pending
      | Some term1, Some term2 ->
	  descriptor2, Term.decompose term1 term2 pending

  end)

(* ------------------------------------------------------------------------- *)

(* To each equivalence class, associate a list of non-unifiable
   terms. Do not fail if two non-unifiable terms are discovered. No
   attempt at efficiency is made when the lists become long, since
   lists of more than one element represent type errors. *)

module ShallowMulti (Item : UnionFind.Item) (Term : Term) =
  Shallow (Item) (Term) (struct

    type descriptor =
	Item.t Term.t list

    let default =
      []

    let inject term =
      [ term ]

    let project = function
      | [ term ] ->
	  Some term
      | _ ->
	  None

    type accumulator =
	(Item.t * Item.t) list

    let rec insert term1 (descriptor2, pending) =
      match descriptor2 with
      | [] ->
	  [ term1 ], pending
      | term2 :: terms2 ->
	  try
	    descriptor2, Term.decompose term1 term2 pending
	  with Term.Decompose ->
	    let terms2, pending = insert term1 (terms2, pending) in
	    term2 :: terms2, pending

    let union descriptor1 descriptor2 pending =
      List.fold_right insert descriptor1 (descriptor2, pending)

  end)

(* ------------------------------------------------------------------------- *)

(* Unification based directly on deep terms. Does not make use of
   [Item.fresh]. *)

module Deep (Item : UnionFind.Item) (Term : Term) = struct

  type 'a t =
      'a Term.t

  type 'a dt =
      'a Term.dt

  type variable =
      Item.t

  type deep_term =
      variable dt

  module Desc = struct

    (* A descriptor is a non-variable deep term. *)

    type descriptor =
	deep_term t option

    let default =
      None

    type accumulator =
	(deep_term * deep_term) list

    let union descriptor1 descriptor2 pending =
      match descriptor1, descriptor2 with
      | None, None ->
	  None, pending
      | Some _, None ->
	  descriptor1, pending
      | None, Some _ ->
	  descriptor2, pending
      | Some term1, Some term2 ->
	  descriptor2, Term.decompose term1 term2 pending

  end

  module U = UnionFind.Make (Item) (Desc)

  include U

  let rec unify partition pending =
    match pending with
    | [] ->
	partition
    | (Term.DTVar v1, Term.DTVar v2) :: pending ->
	let partition, pending = union v1 v2 partition pending in
	unify partition pending
    | (Term.DTVar v1, Term.DTTerm term2) :: pending
    | (Term.DTTerm term2, Term.DTVar v1) :: pending ->
	let descriptor, pending = Desc.union (descriptor v1 partition) (Some term2) pending in
	unify (set v1 descriptor partition) pending
    | (Term.DTTerm term1, Term.DTTerm term2) :: pending ->
	unify partition (Term.decompose term1 term2 pending)

end
