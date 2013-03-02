open Print

(* -------------------------------------------------------------------------- *)

(* Three-valued Booleans. *)

(* Do not alter the order of the constructors, it is exploited below. *)

type bool3 =
  | BTrue
  | BUndetermined
  | BFalse

let print buffer b =
  Buffer.add_string buffer
    begin match b with
    | BTrue ->
	"proved"
    | BFalse ->
	"refuted"
    | BUndetermined ->
	"undetermined"
    end

let equal (b1 : bool3) (b2 : bool3) =
  b1 = b2

let disjunction (b1 : bool3) (b2 : bool3) =
  min b1 b2

let conjunction (b1 : bool3) (b2 : bool3) =
  max b1 b2

let disjunction_map f xs =
  List.fold_left (fun accu x ->
    match accu with
    | BTrue ->
	BTrue
    | BUndetermined
    | BFalse ->
	disjunction accu (f x)
  ) BFalse xs

let conjunction_map f xs =
  List.fold_left (fun accu x ->
    match accu with
    | BFalse ->
	BFalse
    | BUndetermined
    | BTrue ->
	conjunction accu (f x)
  ) BTrue xs

(* -------------------------------------------------------------------------- *)

(* This signature describes Boolean lattices. *)

module type BLATTICE = sig

  type property =
      bool3

  val bottom: property
  val top: property
  val equal: property -> property -> bool
  val leq: property -> property -> bool

end

(* -------------------------------------------------------------------------- *)

(* The three-valued Boolean lattice where [BTrue] is the least element. *)

module TrueBottom : BLATTICE = struct

  type property =
      bool3

  let bottom =
    BTrue

  let top =
    BFalse

  let equal =
    equal

  let leq (b1 : property) (b2 : property) =
    b1 <= b2

end

(* -------------------------------------------------------------------------- *)

(* The three-valued Boolean lattice where [BFalse] is the least element. *)

module FalseBottom : BLATTICE = struct

  type property =
      bool3

  let bottom =
    BFalse

  let top =
    BTrue

  let equal =
    equal

  let leq (b1 : property) (b2 : property) =
    b1 >= b2

end

