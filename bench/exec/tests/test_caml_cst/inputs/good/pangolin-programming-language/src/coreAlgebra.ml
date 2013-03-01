(* $Id: coreAlgebra.ml 27 2007-09-28 14:08:27Z yann.regisgianas $ *)

(* Pangolin, a functional programming language for correct programs.
   Copyright (C) 2007, Yann Régis-Gianas, François Pottier.
   Contact: yann.regisgianas@gmail.com

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>. *)

open Misc
open Position

(** [lname] is the type of label. *)
type lname = LName of string

(** This module implements the internal representation of terms. *)

module RowLabel : sig
  
  (** This module maintains a global mapping from identifiers to
      abstract ``labels'', which are internally represented as
      integers, and back. *)
  
  type t

  (** [compare x y] is a total ordering. It returns a negative integer
      if [x] is less than [y], 0 if [x] and [y] are equal, and a
      positive integer if [x] is greater than [y]. *)
  val compare: t -> t -> int

  (** [import s] associates a unique label with the identifier [s],
      possibly extending the global mapping if [s] was never encountered
      so far. Thus, if [s] and [t] are equal strings, possibly allocated
      in different memory locations, [import s] and [import t] return
      the same label. The identifier [s] is recorded and may be later
      recovered via [export]. *)
    val import: lname -> t

    (** [export i] provides access to the inverse of the global mapping,
	that is, associates a unique identifier with every label. The
	identifier associated with a label is the one originally supplied
	to [import]. *)
    val export: t -> lname

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
    let import (LName s) =
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
      LName (InfiniteArray.get array i)

  end

(** The terms of a row algebra include a binary row extension
    constructor for every row label, the unary constant row
    constructor, and the terms of the underlying free algebra. *)
type 'a term =
  | RowCons of RowLabel.t * 'a * 'a
  | RowUniform of 'a
  | App of 'a * 'a
  | Var of 'a
      
(** Terms whose parameters are either leaves of type ['a], or terms.
    [arterm] stands for ``abstract recursive term''. *)
type 'a arterm =
  | TVariable of 'a
  | TTerm of ('a arterm) term
      
let rec iter f = function
  | RowCons (_, hd, tl) ->
      f hd; f tl
  | RowUniform content ->
      f content
  | App (l, r) -> 
      f l; f r
  | Var v -> 
      f v
      
let rec map f = function
  | RowCons (label, hd, tl) ->
      RowCons (label, f hd, f tl)
  | RowUniform content ->
      RowUniform (f content)
  | App (l, r) ->
      App (f l, f r)
  | Var v ->
      Var (f v)
      
let rec fold f term accu =
  match term with
    | RowCons (_, hd, tl) ->
	f hd (f tl accu)
    | RowUniform content ->
	f content accu
    | App (l, r) -> 
	f r (f l accu)
    | Var v ->
	f v accu

let rec fold2 f term term' accu =
  match term, term' with
    | RowCons (_, hd, tl), RowCons (_, hd', tl') ->
	f hd hd' (f tl tl' accu)
    | RowUniform content, RowUniform content' ->
	f content content' accu
    | App (l, r), App (l', r') -> 
	f r r' (f l l' accu)
    | Var v, Var v' ->
	f v v' accu
    | _ -> failwith "fold2"
	  
let app t args = 
  List.fold_left (fun acu x -> TTerm (App (acu, x))) t args
    
exception InvalidSymbolString of string
  
exception InvalidSymbolUse of string * int
  
let rec type_variables' term acu = 
  fold type_variables  term acu
    
and type_variables term acu = 
  match term with
    | TTerm term -> type_variables' term acu
    | TVariable x -> 
	if not (List.memq x acu) then x :: acu else acu
	  
let type_variables term = type_variables term []
  
let rec change_term_vars c = 
  map (change_arterm_vars c)
    
and change_arterm_vars c = 
  function
    | TTerm term -> TTerm (change_term_vars c term)
    | TVariable x -> TVariable (
	try 
	  List.assq x c
	with Not_found -> x)
	
let rec gen_change_term_vars c = 
  map (gen_change_arterm_vars c)
    
and gen_change_arterm_vars c = 
    function
      | TTerm term -> TTerm (gen_change_term_vars c term)
      | TVariable x -> (
	  try 
	    List.assq x c
	  with Not_found -> TVariable x)
	  
let uniform v = 
  TTerm (RowUniform v)

let rowcons label x y = 
  let intern_label = RowLabel.import label in
    TTerm (RowCons (intern_label, x, y))

let n_rowcons typed_labels y = 
  List.fold_left (fun acu (l, t) -> rowcons l t acu) y typed_labels
