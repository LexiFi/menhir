open Source
open Typed

(* ------------------------------------------------------------------------- *)

(* Value unification. *)

module Item = struct

  type t =
      var

  let equal =
    Var.Atom.equal

  let fresh () =
    (* Unification of deep terms does not require fresh variables. *)
    assert false

  module Map = Var.AtomMap

end

module Term = struct

  type 'a t =
    | TBool of bool
    | TTagTuple of tag * 'a structured_tuple * contrainte Location.t

  type 'a dt =
    | DTVar of 'a
    | DTTerm of 'a dt t

  exception Decompose

  let map f term =
    match term with
    | TBool b ->
	TBool b
    | TTagTuple (tag, tuple, guard) ->
	TTagTuple (tag, Layout.map f tuple, guard)

  (* Values are unified by structural decomposition, except in the
     case of abstractions, where structural decomposition would be
     incorrect, since a renaming is involved.

     We cannot express the fact that two values are related by a
     certain renaming, so we just forget equations that involve
     two abstractions. This is a source of incompleteness. *)

  (* TEMPORARY introduce a renaming? how? *)
  (* TEMPORARY or at least remember the original equation between values? *)

  let decompose term1 term2 accu =
    match term1, term2 with
    | TBool b1, TBool b2 ->
	if b1 = b2 then
	  accu
	else
	  raise Decompose
    | TTagTuple (tag1, tuple1, _), TTagTuple (tag2, tuple2, _) ->
	if Datacon.Atom.equal tag1 tag2 then
	  Layout.fold2_except_under_abstractions (fun x1 x2 accu -> (x1, x2) :: accu) tuple1 tuple2 accu
	else
	  raise Decompose
    | TBool _, TTagTuple _
    | TTagTuple _, TBool _ ->
	assert false

end

(* No occurs check is needed because the programming language is
   sufficiently restricted to ensure that cyclic equations cannot
   arise. Indeed, each [case] construct matches a linear pattern,
   whose bound variables are fresh, against pre-existing values. *)

module U = Unification.Deep (Item) (Term)

open Term

(* ------------------------------------------------------------------------- *)

(* Translating values to the unifier's deep terms and back. They are
   isomorphic. *)

let rec import = function
  | VVar x ->
      DTVar x
  | VBool b ->
      DTTerm (TBool b)
  | VTagTuple (tag, tuple, guard) ->
      DTTerm (TTagTuple (tag, Layout.map import tuple, guard))

let rec export = function
  | DTVar x ->
      VVar x
  | DTTerm term ->
      export_term term

and export_term = function
  | TBool b ->
      VBool b
  | TTagTuple (tag, tuple, guard) ->
      VTagTuple (tag, Layout.map export tuple, guard)

(* ------------------------------------------------------------------------- *)

(* Public interface. *)

type 'a consistency =
  | Inconsistent
  | Consistent of 'a

type state =
    U.partition consistency

let empty =
  Consistent U.empty

let unify state v1 v2 =
  match state with
  | Inconsistent ->
      Inconsistent
  | Consistent partition ->
      try
	Consistent (U.unify partition [ import v1, import v2 ])
      with Decompose ->
	Inconsistent

let exploit state =
  match state with
  | Inconsistent ->
      Inconsistent
  | Consistent partition ->
      Consistent (fun x ->
	
	(* If [x] is its own representative, emit an equation that relates
	   [x] to its descriptor. Otherwise, emit an equation that relates
	   [x] to its representative. *)

	let y = U.representative x partition in
	if Item.equal x y then
	  Option.map export_term (U.descriptor x partition)
	else
	  Some (VVar y)
      )

