(* This module provides an implementation of words as (hash-consed)
   lists of symbols. *)

(* Symbols are assumed to be integers. This means that they can be
   compared for equality and that they are their own hash key. *)

open Hashcons
open Sigs

module Make
    (Symbol : SYMBOL with type symbol = int)
= struct

  module Symbol = Symbol
  open Symbol

  (* ------------------------------------------------------------------------ *)

  (* The structure of words. *)

  type bare_word =
    | WNil
    | WCons of symbol * word

  and word =
      bare_word hash_consed

  type t =
      word

  (* ------------------------------------------------------------------------ *)

  (* Set up hash-consing. *)

  module T = Hashcons.Make (struct

    type t =
	bare_word

    let equal (bw1 : bare_word) (bw2 : bare_word) =
      match bw1, bw2 with
      | WNil, WNil ->
	  true
      | WCons (s1, w1), WCons (s2, w2) ->
	  s1 == s2 (* symbols are comparable *) && w1 == w2 (* sub-words are already hash-consed *)
      | WNil, WCons _
      | WCons _, WNil ->
	  false

    let hash = function
      | WNil ->
	  0
      | WCons (s, w) ->
	  hash2 s (* symbols are integers *) w.hkey

  end)

  let table =
    T.create 1023

  let hashcons w =
    T.hashcons table w

  (* ------------------------------------------------------------------------ *)

  (* Comparison. *)

  let equal w1 w2 =
    w1 == w2 (* words are hash-consed *)

  let compare w1 w2 =
    compare w1.tag w2.tag

  let hash w =
    w.hkey

  (* ------------------------------------------------------------------------ *)

  (* Construction. *)

  let epsilon =
    hashcons WNil

  let cons s w =
    hashcons (WCons (s, w))

  let construct ss =
    List.fold_right cons ss epsilon

  let rec concat w1 w2 =
    match w1.node with
    | WNil ->
	w2
    | WCons (s1, w1) ->
	cons s1 (concat w1 w2)

  let concat3 w1 w2 w3 =
    concat w1 (concat w2 w3)

  (* ------------------------------------------------------------------------ *)

  (* Iteration. *)

  let rec iter f w =
    match w.node with
    | WNil ->
	()
    | WCons (s, w) ->
	f s;
	iter f w

  let rec fold f w accu =
    match w.node with
    | WNil ->
	accu
    | WCons (s, w) ->
	let accu = f s accu in
	fold f w accu

  let rec elements w =
    match w.node with
    | WNil ->
	[]
    | WCons (s, w) ->
	s :: elements w

  let reverse w =
    fold cons w epsilon

  let rec exists p w =
    match w.node with
    | WNil ->
	false
    | WCons (s, w) ->
	p s || exists p w

  (* ------------------------------------------------------------------------ *)

  (* Length. *)

  let is_epsilon w =
    match w.node with
    | WNil ->
	true
    | WCons _ ->
	false

  let is_singleton w =
    match w.node with
    | WCons (s, w) when is_epsilon w ->
	Some s
    | _ ->
	None

  let rec length w accu =
    match w.node with
    | WNil ->
	accu
    | WCons (s, w) ->
	length w (accu + 1)

  let length w =
    length w 0

  (* ------------------------------------------------------------------------ *)

  (* Access to the constituents. *)

  let array w =
    Array.of_list (List.rev (fold (fun s elements -> s :: elements) w []))

  (* ------------------------------------------------------------------------ *)

  (* Display. *)

  let print b w =
    match w.node with
    | WNil ->
	Buffer.add_string b "!"
    | WCons _ ->
	Print.seplist Print.space Symbol.print b (elements w)

  (* ------------------------------------------------------------------------ *)

  (* Indexing. *)

  module Left = struct

    type symbol =
	Symbol.symbol

    type word =
	t

    (* An index is represented as a pair of a reversed prefix and a suffix. *)

    type index =
	word * word

    let equal (p1, s1) (p2, s2) =
      equal p1 p2
	(* if the two indices are relative to the same word, as they
	   should be, there is no need for comparing the suffixes. *)

    let hash (w1, w2) =
      hash2 (hash w1) (hash w2)

    let start w =
      epsilon, w

    exception AtBoundary

    let next _ (w1, w2) =
      match w2.node with
      | WNil ->
	  raise AtBoundary
      | WCons (s2, w2) ->
	  cons s2 w1, w2

    let get _ (_, w2) =
      match w2.node with
      | WNil ->
	  assert false
      | WCons (s2, _) ->
	  s2

    let prefix _ (w1, _) =
      reverse w1

    let suffix _ (_, w2) =
      w2

    let fold f w accu =
      let rec loop ((w1, w2) as index) accu =
	match w2.node with
	| WNil ->
	    f index accu
	| WCons (s2, w2) ->
	    loop (cons s2 w1, w2) (f index accu)
      in
      loop (epsilon, w) accu

  end

  (* When indexing from right to left, all conventions are reversed,
     so, in particular, [get] returns the symbol found immediately at
     the left of the index. *)

  module Right = struct

    include Left

    let start w =
      epsilon, reverse w   (* Left.start (reverse w) *)

    let prefix _ (w1, _) =
      w1                   (* reverse (Left.prefix w i) *)

    let suffix _ (_, w2) =
      reverse w2           (* reverse (Left.suffix w i) *)

    let fold f w accu =
      fold f (reverse w) accu

  end

end

