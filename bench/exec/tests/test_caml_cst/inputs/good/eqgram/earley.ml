(* This module implements a simple Earley parser, following Aycock and
   Horspool's description (`Practical Earley Parsing', the Computer
   Journal, 2002). *)

open Printf
open Print
open Hashcons
open Sigs

module Run
    (Grammar : GRAMMAR)
    (W : sig

      open Grammar.Word

      val left: word
      val right: word

    end)
= struct

  open Grammar
  open Word

  module Index =
    Word.Left

  (* ------------------------------------------------------------------------ *)

  (* An Earley item consists of two parts: an LR(0) item (a production
     with a bullet in its right-hand side) and an index into the token
     array. *)

  type bare_item = {

      (* The production's left-hand side. This can be [None] if we are
	 dealing with the production associated with the new start
	 symbol [S']. *)

      lhs: Symbol.symbol option;

      (* The production's right-hand side. *)

      rhs: Word.word;

      (* The position of the bullet, an index within the right-hand
	 side. *)

      bullet: Index.index;

      (* The backpointer, an index within the token array. *)

      backpointer: int;

    }

  type item =
      bare_item hash_consed

  (* ------------------------------------------------------------------------ *)

  (* Debugging. *)

  let print_lhs b = function
    | None ->
	Buffer.add_string b "?"
    | Some s ->
	Symbol.print b s

  let print b item =
    let item = item.node in
    bprintf b "[%a -> %a . %a, %d]"
      print_lhs item.lhs
      Word.print (Index.prefix item.rhs item.bullet)
      Word.print (Index.suffix item.rhs item.bullet)
      item.backpointer

  (* ------------------------------------------------------------------------ *)

  (* Set up hash-consing of items. *)

  module T = Hashcons.Make (struct

    type t =
	bare_item

    let equal_lhs lhs1 lhs2 =
      match lhs1, lhs2 with
      | None, None ->
	  true
      | Some s1, Some s2 ->
	  Symbol.equal s1 s2
      | Some _, None
      | None, Some _ ->
	  false

    let equal (bi1 : bare_item) (bi2 : bare_item) =
      equal_lhs bi1.lhs bi2.lhs &&
      Word.equal bi1.rhs bi2.rhs &&
      Index.equal bi1.bullet bi2.bullet &&
      bi1.backpointer = bi2.backpointer

    let hash (bi : bare_item) =
      hash3
	(Word.hash bi.rhs)
	(Index.hash bi.bullet)
	bi.backpointer

  end)

  let table =
    T.create 10007

  let hashcons bi : item =
    T.hashcons table bi

  (* ------------------------------------------------------------------------ *)

  (* Set up sets of items. *)

  module ItemSet =
    Set.Make (struct
      type t =
	  item
      let compare item1 item2 =
	item1.tag - item2.tag
    end)

  (* ------------------------------------------------------------------------ *)

  (* Turn the word that should be parsed into an array of symbols, and
     initialize an array of Earley items. *)

  let symbols =
    Word.array W.left

  let n =
    Array.length symbols

  let items =
    Array.make (n+1) ItemSet.empty

  let start = {
    lhs = None;
    rhs = W.right;
    bullet = Index.start W.right;
    backpointer = 0;
  }

  (* ------------------------------------------------------------------------ *)

  (* A queue of items that were newly inserted and whose consequences still
     have to be examined. Items inserted in buckets of lesser index must be
     explored first, which explains the use of a priority queue. *)

  module Q = BinomialQueue.Make (struct
    type t = item * int
    let compare (_, i1) (_, i2) = i1 - i2
  end)

  let queue =
    ref Q.empty

  (* ------------------------------------------------------------------------ *)

  (* [insert item i] inserts an [item] into the item set at index [i] and,
     if it was not already in there, enqueues it for later examination. *)

  let insert item i =
    let item = hashcons item in
    if not (ItemSet.mem item items.(i)) then begin
      if false then (* toggle *)
	w (fun b ->
	  bprintf b "Queueing item %a at index %d.\n%!"
	    print item i
	);
      items.(i) <- ItemSet.add item items.(i);
      queue := Q.insert (item, i) !queue
    end

  (* [examine (item, i)] examines an [item] that was newly inserted at
     index [i] and inserts new items as a consequence. It can signal
     success of the entire parse. *)

  exception Success

  let examine (item, i) =
    let item = item.node in
    try

      let new_bullet = Index.next item.rhs item.bullet in
      let a = Index.get item.rhs item.bullet in
      match classify a with
      | T
      | NO _ ->

	  (* (Scanner.) The bullet lies in front of a terminal or
	     opaque non-terminal symbol. Check whether the same symbol
	     appears in the symbol array at index [i]. If so, add a
	     new Earley item to the item set at [i+1]. Otherwise, do
	     nothing. *)

	  if i < n && Symbol.equal a symbols.(i) then
	    insert { item with bullet = new_bullet } (i+1)

      | NT productions ->

	  (* (Predictor.) The bullet lies in front of a transparent
	     non-terminal symbol. Add one new Earley item to the item
	     set at [i] for each possible expansion of that symbol. *)

	  let lhs = Some a in
	  List.iter (fun (_, rhs) ->
	    insert {
	      lhs = lhs;
	      rhs = rhs;
	      bullet = Index.start rhs;
	      backpointer = i;
	    } i
	  ) productions;

	  (* (Modified predictor.) Furthermore, if [a] is nullable,
	     add one new Earley item to the item set at [i], much as
	     if [a] had just been scanned. *)

	  if nullable_symbol a then
	    insert { item with bullet = new_bullet } i

    with Index.AtBoundary ->
      
      match item.lhs with
      | Some s ->

	  (* (Completer.) The bullet lies at the end of the right-hand side, so we
	     have recognized the symbol [s] found on the left-hand side. Let the
	     index [j] be as given by the backpointer. Fetch all items at [j]
	     whose bullet lies in front of an occurrence of [s], and add updated
	     versions of these items at the current index [i]. *)

	  let j = item.backpointer in
	  ItemSet.iter (fun (old : item) ->
	    let old = old.node in
	    try
	      let new_bullet = Index.next old.rhs old.bullet in
	      if Symbol.equal s (Index.get old.rhs old.bullet) then
		insert { old with bullet = new_bullet } i
	    with Index.AtBoundary ->
	      ()
	  ) items.(j)

      | None ->

	  (* (Success.) The bullet lies at the end of the right-hand side, and
	     the left-hand side is [None], so we have recognized the desired
	     word [W.right]. If the current index [i] corresponds to the end
	     of the token array, then the parse is successful. *)

	  assert (item.backpointer = 0);
	  if i = n then
	    raise Success

  (* The parser's main loop. *)

  let success =
    insert start 0;
    try
      let rec loop () =
	let head, tail = Q.extract !queue in
	queue := tail;
	examine head;
	loop()
      in
      loop()
    with
    | Success ->
	true
    | Not_found ->
	false

end

(* -------------------------------------------------------------------------- *)

(* Packaging. *)

module Make (Grammar : GRAMMAR) = struct

  (* Currently, no precomputation is performed when the grammar is made
     available. *)

  let parse left right =
    if false then (* toggle *)
      w (fun b ->
	bprintf b "Entering Earley parser with %a < %a.\n%!"
	  Grammar.Word.print left
	  Grammar.Word.print right
      );
    let module R = Run (Grammar) (struct
      let left = left
      let right = right
    end) in
    if false then (* toggle *)
      w (fun b ->
	bprintf b "Exiting Earley parser with %b.\n%!" R.success
      );
    R.success

end

