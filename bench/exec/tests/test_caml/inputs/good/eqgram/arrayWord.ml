(* This module provides an implementation of words as (hash-consed)
   immutable arrays of symbols. These are probably analogous to Java
   strings. *)

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
      symbol array

  and word =
      bare_word hash_consed

  type t =
      word

  (* ------------------------------------------------------------------------ *)

  (* Set up hash-consing. *)

  module T = Hashcons.Make (struct

    type t =
	bare_word

    let rec equal (bw1 : bare_word) (bw2 : bare_word) i =
      i < 0 || (
        Array.unsafe_get bw1 i == Array.unsafe_get bw2 i (* symbols are comparable *) &&
        equal bw1 bw2 (i-1)
      )

    let equal bw1 bw2 =
      let n = Array.length bw1 in
      n = Array.length bw2 &&
      equal bw1 bw2 (n-1)

    let hash bw =
      let n = Array.length bw in
      let h = ref 0 in
      for i = n-1 downto 0 do
	let s = Array.unsafe_get bw i in
	h := hash2 s (* symbols are integers *) !h
      done;
      !h

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
    hashcons (Array.make 0 0)

  let cons s w =
    let bw = w.node in
    let n = Array.length bw in
    let bw' = Array.make (n+1) s in
    for i = 0 to n-1 do
      Array.unsafe_set bw' (i+1) (Array.unsafe_get bw i)
    done;
    hashcons bw'

  let construct ss =
    hashcons (Array.of_list ss)

  let concat w1 w2 =
    let bw1 = w1.node
    and bw2 = w2.node in
    let n1 = Array.length bw1
    and n2 = Array.length bw2 in
    let n = n1 + n2 in
    let bw = Array.make n 0 in
    for i = 0 to n1-1 do
      Array.unsafe_set bw i (Array.unsafe_get bw1 i)
    done;
    for i = 0 to n2-1 do
      Array.unsafe_set bw (n1+i) (Array.unsafe_get bw2 i)
    done;
    hashcons bw

  let concat3 w1 w2 w3 =
    let bw1 = w1.node
    and bw2 = w2.node
    and bw3 = w3.node in
    let n1 = Array.length bw1
    and n2 = Array.length bw2
    and n3 = Array.length bw3 in
    let n = n1 + n2 + n3 in
    let bw = Array.make n 0 in
    for i = 0 to n1-1 do
      Array.unsafe_set bw i (Array.unsafe_get bw1 i)
    done;
    for i = 0 to n2-1 do
      Array.unsafe_set bw (n1+i) (Array.unsafe_get bw2 i)
    done;
    let base = n1+n2 in
    for i = 0 to n3-1 do
      Array.unsafe_set bw (base+i) (Array.unsafe_get bw3 i)
    done;
    hashcons bw

  (* ------------------------------------------------------------------------ *)

  (* Iteration. *)

  let rec iter f w =
    Array.iter f w.node

  let rec fold f w accu =
    Array.fold_left (fun accu s -> f s accu) accu w.node

  let rec elements w =
    Array.to_list w.node

  let exists p w =
    let bw = w.node in
    let n = Array.length bw in
    let rec exists i =
      i < n && (p bw.(i) || exists (i+1))
    in
    exists 0

  (* ------------------------------------------------------------------------ *)

  (* Length. *)

  let length w =
    Array.length w.node

  let is_epsilon w =
    length w = 0

  let is_singleton w =
    let bw = w.node in
    if Array.length bw = 1 then
      Some bw.(0)
    else
      None

  (* ------------------------------------------------------------------------ *)

  (* Access to the constituents. *)

  let array w =
    w.node

  (* ------------------------------------------------------------------------ *)

  (* Display. *)

  let print b w =
    if length w = 0 then
      Buffer.add_string b "!"
    else
      Print.seplist Print.space Symbol.print b (elements w)

  (* ------------------------------------------------------------------------ *)

  (* Indexing. *)

  module Left = struct

    type symbol =
	Symbol.symbol

    type word =
	t

    (* An index is represented as an integer. *)

    type index =
	int

    let equal (i1 : index) (i2 : index) =
      i1 = i2

    let hash (i : index) =
      i

    let start _ =
      0

    exception AtBoundary

    let next w i =
      let n = Array.length w.node in
      assert (i >= 0 && i <= n);
      if i = n then
	raise AtBoundary
      else
	i+1

    let get w i =
      Array.get w.node i

    let prefix w i =
      hashcons (Array.sub w.node 0 i)

    let suffix w i =
      let n = Array.length w.node in
      hashcons (Array.sub w.node i (n-i))

    let fold f w accu =
      let bw = w.node in
      let n = Array.length bw in
      let rec loop i accu =
	if i > n then
	  accu
	else
	  loop (i+1) (f i accu)
      in
      loop 0 accu

  end

  (* When indexing from right to left, all conventions are reversed,
     so, in particular, [get] returns the symbol found immediately at
     the left of the index. *)

  module Right = struct

    include Left

    let start w =
      Array.length w.node

    let next w i =
      assert (i >= 0);
      if i = 0 then
	raise AtBoundary
      else
	i-1

    let get w i =
      assert (i > 0);
      Array.get w.node (i-1)

    let prefix =
      Left.suffix

    let suffix =
      Left.prefix

    let fold f w accu =
      let bw = w.node in
      let n = Array.length bw in
      let rec loop i accu =
	if i < 0 then
	  accu
	else
	  loop (i-1) (f i accu)
      in
      loop n accu

  end

end

