(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/row.ml,v 1.4 2004/02/03 17:05:24 fpottier Exp $ *)

(** This module extends a free algebra with rows. *)

open Sig

module Make
    (A : Algebra)
    (RowLabel : RowLabel)
= struct

  (* Repeat the constituents of this row algebra. *)
  module A = A

  module RowLabel = RowLabel

  (** The terms of a row algebra include a binary row extension
      constructor for every row label, the unary constant row
      constructor, and the terms of the underlying free algebra. *)
  type 'a term =
    | RowCons of RowLabel.t * 'a * 'a
    | RowUniform of 'a
    | FreeTerm of 'a A.term

  (** Terms whose parameters are either leaves of type ['a], or terms.
      [arterm] stands for ``abstract recursive term''. *)
  type 'a arterm =
    | TVariable of 'a
    | TTerm of ('a arterm) term

  let iter f = function
    | RowCons (_, hd, tl) ->
	f hd; f tl
    | RowUniform content ->
	f content
    | FreeTerm term ->
	A.iter f term

  let map f = function
    | RowCons (label, hd, tl) ->
	RowCons (label, f hd, f tl)
    | RowUniform content ->
	RowUniform (f content)
    | FreeTerm term ->
	FreeTerm (A.map f term)

  let fold f term accu =
    match term with
    | RowCons (_, hd, tl) ->
	f hd (f tl accu)
    | RowUniform content ->
	f content accu
    | FreeTerm term ->
	A.fold f term accu

  type symbol =
    | SRowCons
    | SRowUniform
    | SFreeTerm of A.symbol

  type label =
    | LRowConsL
    | LRowConsR
    | LRowUniform
    | LFreeTerm of A.label

  let print f = function
    | RowCons (label, hd, tl) ->
	SRowCons,
	RowLabel.export label ^ ": " ^
        f LRowConsL hd ^ "; " ^
	f LRowConsR tl
    | RowUniform content ->
	SRowUniform,
	"\\" ^
	f LRowUniform content
    | FreeTerm term ->
	let symbol, string =
	  A.print (fun label son -> f (LFreeTerm label) son) term in
	SFreeTerm symbol, string

  let parenthesize label symbol =
    match label, symbol with
    | (LRowConsL | LRowUniform), (SRowCons | SRowUniform) ->
	assert false
    | LRowConsL, SFreeTerm symbol ->
	false
    | LRowConsR, (SRowCons | SRowUniform | SFreeTerm _) ->
	false
    | LRowUniform, SFreeTerm symbol ->
	true
    | LFreeTerm _, SRowCons ->
	true
    | LFreeTerm _, SRowUniform ->
	false
    | LFreeTerm label, SFreeTerm symbol ->
	A.parenthesize label symbol

end

