(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/miniAlgebra.ml,v 1.2 2004/03/30 12:09:32 fpottier Exp $ *)

open Sig

module Base = struct

  type symbol =
    | SInt
    | SArrow
    | STuple of int

  type 'a term =
      symbol * 'a list

  type 'a arterm =
    | TVariable of 'a
    | TTerm of ('a arterm) term

  let iter f (_, params) =
    List.iter f params

  let map f (sym, params) =
    (sym, List.map f params)

  let fold f (_, params) accu =
    List.fold_right f params accu

  type label =
    | LArrowL
    | LArrowR
    | LTuple

  let print f = function
    | (SInt, []) ->
	SInt, "int"
    | (SArrow, [domain; codomain]) ->
	SArrow, (f LArrowL domain) ^ " -> " ^ (f LArrowR codomain)
    | (STuple 0, []) ->
	STuple 0, "unit"
    | (STuple k, ts) ->
	assert (k > 1);
	STuple k, List.fold_left (fun s t ->
	  s ^ (if s = "" then "" else " * ") ^ (f LTuple t)
        ) "" ts
    | _ ->
	assert false (* ill-formed type *)

  let parenthesize label sym =
    match label, sym with
    | _, STuple 0 ->
	(* Never parenthesize the unit type. *)
	false
    | (LArrowL | LTuple), SArrow
	(* Parenthesize an arrow type that appears left of an arrow or inside a tuple. *)
    | LTuple, STuple _
        (* Parenthesize a tuple that appears inside a tuple. *)
      ->
	true
    | _, _ ->
	false

  let arity (sym, _) =
    match sym with
    | SInt ->
	0
    | SArrow ->
	2
    | STuple k ->
	k

  let map2 f (sym, params) =
    let rec map2 = function
      |	[] ->
	  [], []
      |	elem :: more ->
	  let elem1, elem2 = f elem in
	  let more1, more2 = map2 more in
	  elem1 :: more1, elem2 :: more2
    in
    let params1, params2 = map2 params in
    (sym, params1), (sym, params2)

  exception Iter2

  let iter2 f (sym1, params1) (sym2, params2) =
    if sym1 <> sym2 then
      raise Iter2;
    List.iter2 f params1 params2

end

module MiniRowLabel : sig

  (** This module maintains a global mapping from identifiers to
      abstract ``labels'', which are internally represented as integers,
      and back. *)

  include Ordered

  (** [import s] associates a unique label with the identifier [s],
      possibly extending the global mapping if [s] was never encountered
      so far. Thus, if [s] and [t] are equal strings, possibly allocated
      in different memory locations, [import s] and [import t] return
      the same label. The identifier [s] is recorded and may be later
      recovered via [export]. *)
  val import: string -> t

  (** [export i] provides access to the inverse of the global mapping,
      that is, associates a unique identifier with every label. The
      identifier associated with a label is the one originally supplied
      to [import]. *)
  val export: t -> string

end = struct

  (** A row label is an object of type [t], that is, an integer. *)
  type t = int

  let compare = (-)

  (** A hash table maps all known identifiers to integer values. It
      provides one direction of the global mapping. *)
  let table =
    Hashtbl.create 1023

  (** An infinite array maps all known integer values to identifiers. It
      provides the other direction of the global mapping. *)
  let array =
    InfiniteArray.make "<BUG>" (* Dummy data. *)

  (** A global counter contains the next available integer label. *)
  let counter =
    ref 0

  (** [import s] associates a unique label with the identifier [s],
      possibly extending the global mapping if [s] was never encountered
      so far. Thus, if [s] and [t] are equal strings, possibly allocated
      in different memory locations, [import s] and [import t] return
      the same label. The identifier [s] is recorded and may be later
      recovered via [export]. *)
  let import s =
    try
      Hashtbl.find table s
    with Not_found ->
      let i = !counter in
      Hashtbl.add table s i;
      InfiniteArray.set array i s;
      counter := i + 1;
      i

  (** [export i] provides access to the inverse of the global mapping,
      that is, associates a unique identifier with every label. The
      identifier associated with a label is the one originally supplied
      to [import]. *)
  let export i =
    assert (i < !counter);
    InfiniteArray.get array i

end

module Row = Row.Make(Base)(MiniRowLabel)

type 'a arterm = 'a Row.arterm

open Row

(* We must lift the term builders of the free algebra up to the row algebra. TEMPORARY comment faire mieux ?*)

let int =
  TTerm (FreeTerm (Base.SInt, []))

let arrow term1 term2 =
  TTerm (FreeTerm (Base.SArrow, [term1; term2]))

let tuple terms =
  let k = List.length terms in
  assert (k <> 1); (* we don't have syntax for tuples of length 1, so better not create them *)
  TTerm (FreeTerm (Base.STuple k, terms))

