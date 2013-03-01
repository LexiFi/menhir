(* TEMPORARY comment ce module va-t-il évoluer avec les types paramétrés? *)

open Source

(* ------------------------------------------------------------------------- *)

(* Determine which property types have empty support, inner, outer, or bound. *)

module Empty = struct

  module Data = struct

    type property = {
	support: bool; (* [true] means possibly nonempty, [false] means definitely empty *)
	inner: bool;
	outer: bool;
	bound: bool;
      }

    let bottom = {
      support = false;
      inner = false;
      outer = false;
      bound = false;
    }

    let equal v1 v2 =
      v1.support = v2.support &&
      v1.inner = v2.inner &&
      v1.outer = v2.outer &&
      v1.bound = v2.bound

    let join v1 v2 = {
      support = v1.support || v2.support;
      inner = v1.inner || v2.inner;
      outer = v1.outer || v2.outer;
      bound = v1.bound || v2.bound;
    }

  end

  open Data

  module Transfer = struct

    let transfer get t =
      let _, propertycondefs = SymbolTable.typedef t in

      let rec eval layout =
	match Annotation.content layout with
	| LComponent (_, TData t') ->
	    get t'
	| LComponent (_, (TAtom | TAtomSet)) ->
	    { support = true; inner = false; outer = false; bound = true }
	| LComponent (_, TBool) ->
	    { support = false; inner = false; outer = false; bound = false }
	| LInner layout ->
	    let v = eval layout in
	    { support = v.support; inner = v.support; outer = false; bound = false }
	| LOuter layout ->
	    let v = eval layout in
	    { support = v.support; inner = false; outer = v.support; bound = false }
	| LAbstraction _ ->
	    { support = true; inner = false; outer = false; bound = false }
	| LTuple layouts ->
	    List.fold_left (fun accu layout -> join accu (eval layout)) bottom layouts
      in

      List.fold_left (fun accu (_, details) ->
	let layout, _ = open_dcd details in
	join accu (eval layout)
      ) bottom propertycondefs

  end

  module Fix =
    FixOnDemand.Make (Datatype.AtomMap) (Data) (Transfer)

end

open Empty
open Data
open Fix

(* ------------------------------------------------------------------------- *)

(* Public accessors. *)

let has_empty_support = function
  | TData t ->
      not (get t).support
  | TAtom
  | TAtomSet ->
      false
  | TBool ->
      true

let has_empty_inner = function
  | TData t ->
      not (get t).inner
  | TAtom
  | TAtomSet ->
      true
  | TBool ->
      true

let has_empty_outer = function
  | TData t ->
      not (get t).outer
  | TAtom
  | TAtomSet ->
      true
  | TBool ->
      true

let has_empty_bound = function
  | TData t ->
      not (get t).bound
  | TAtom
  | TAtomSet ->
      false
  | TBool ->
      true

(* ------------------------------------------------------------------------- *)

(* Determine which property types have definitely nonempty support. *)

module Nonempty = struct

  module Data = struct

    type property =
      | VDefinitelyNonempty
      | VPossiblyEmpty

    let bottom =
      VDefinitelyNonempty

    let top =
      VPossiblyEmpty

    let equal =
      (=)

    let product =
      min

    let sum =
      max

  end

  open Data

  module Transfer = struct

    let transfer get t =
      let _, propertycondefs = SymbolTable.typedef t in

      let rec eval layout =
	match Annotation.content layout with
	| LComponent (_, TData t') ->
	    get t'
	| LComponent (_, TAtom) ->
	    VDefinitelyNonempty
	| LComponent (_, (TBool | TAtomSet)) ->
	    VPossiblyEmpty
	| LInner layout
	| LOuter layout ->
	    eval layout
	| LAbstraction _ ->
	    VPossiblyEmpty
	| LTuple layouts ->
	    List.fold_left (fun accu layout -> product accu (eval layout)) top layouts
      in

      List.fold_left (fun accu (_, details) ->
	let layout, _ = open_dcd details in
	sum accu (eval layout)
      ) bottom propertycondefs

  end

  module Fix =
    FixOnDemand.Make (Datatype.AtomMap) (Data) (Transfer)

end

open Nonempty
open Data
open Fix

(* ------------------------------------------------------------------------- *)

(* Public accessors. *)

let has_nonempty_support = function
  | TData t ->
      get t = VDefinitelyNonempty
  | TAtom ->
      true
  | TBool
  | TAtomSet ->
      false

