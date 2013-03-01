(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/unifier.ml,v 1.11 2004/02/03 13:05:11 fpottier Exp $ *)

(* TEMPORARY commenter *)

(** This module implements unification of (ranked) multi-equations
    over a row algebra, that is, an algebra obtained by extending a
    free algebra [A] with rows (see module {!Row}).

    For the purposes of this module, a rank is an element of an
    arbitrary total order. A rank is associated with every
    multi-equation. When two multi-equations are merged, the smaller
    rank is kept.

    It is understood that finite and infinite terms are legal -- no
    occur check is performed here. *)

open Sig

module Make
    (RowAlgebra : RowAlgebra)
    (Rank : Rank)
= struct

  (** The algebra associated with this unifier. *)
  module A = RowAlgebra

  open A

  (** The universe of ranks associated with this unifier. *)
  module Rank = Rank

  (* Multi-equations partition variables into equivalence classes: two variables
     are deemed equivalent if and only if they belong to the same multi-equation.
     For this reason, we encode multi-equations using a union/find data structure.

     With every equivalence class of variables, i.e. with every
     multi-equation, we associate some additional information. The
     [structure] field holds an optional term, which (if present) is
     understood as also being part of the multi-equation. The [rank]
     field gives the multi-equation's rank. The mutable [mark] field
     is unused; it is provided for our client's convenience. *)

  type variable =
      descriptor UnionFind.point

  and descriptor = {
      mutable structure: variable term option;
      mutable rank: Rank.t;
      mutable mark: Mark.t
    } 

  (** [variable()] creates a new variable, whose rank is [none]. *)
  let variable () =
    UnionFind.fresh {
      structure = None;
      rank = Rank.none;
      mark = Mark.none
    } 

  (* [unify register v1 v2] equates the variables [v1] and [v2]. That
     is, it adds the equation [v1 = v2] to the constraint which it
     maintains, then rewrites it in a number of ways until an
     inconsistency is found or a solved form is reached. If the
     former, then [Inconsistency] is raised.

     Any variables which are freshly created during the process are
     passed to [register], so as to make the caller aware of their
     existence. *)

  exception Inconsistency

  let unify register =

    (* Define an auxiliary function which creates a fresh variable,
       found within a multi-equation of its own, with specified
       rank and structure. *)

    let fresh rank structure =
      let v = UnionFind.fresh {
        structure = structure;
	rank = rank;
	mark = Mark.none
      }	in
      register v;
      v in

    (* The main function only has two parameters; [register] remains
       unchanged through recursive calls. *)

    let rec unify v1 v2 =

      (* If the two variables already belong to the same multi-equation,
	 there is nothing to do. This check is not just an optimization;
	 it is essential in guaranteeing termination, since we are
	 dealing with potentially cyclic structures. *)

      if not (UnionFind.equivalent v1 v2) then

	(* Before performing a recursive call, we will merge the
	   multi-equations associated with [v1] and [v2]. We can't
	   do this right here and now, however, because we need to
	   look at their structure to determine which descriptor it
	   is best (more economical) to keep. *)

	let desc1 = UnionFind.find v1
	and desc2 = UnionFind.find v2 in

	(* Our first step is to compare the ranks of the two multi-equations,
	   so as to compute the minimum rank.

	   This enables us to give correct and efficient versions of a number
	   of auxiliary functions:

	   [fresh] specializes [fresh] (defined above) with the minimum rank.
	   [merge] merges the multi-equations, keeping an arbitrary structure.
	   [merge1] merges the multi-equations, keeping the former's structure.
	   [merge2] merges the multi-equations, keeping the latter's structure. *)

	let fresh, merge, merge1, merge2 =
	  let rank1 = desc1.rank
	  and rank2 = desc2.rank in
	  if Rank.compare rank1 rank2 < 0 then
	    let merge1 () =
	      UnionFind.union v2 v1
	    and merge2 () =
	      UnionFind.union v2 v1;
	      desc1.structure <- desc2.structure in
	    fresh rank1, merge1, merge1, merge2
	  else
	    let merge1 () =
	      UnionFind.union v1 v2;
	      desc2.structure <- desc1.structure
	    and merge2 () =
	      UnionFind.union v1 v2 in
	    fresh rank2, merge2, merge1, merge2 in
	
	(* Another auxiliary function, for syntactic convenience. *)

	let filter v term =
	  unify v (fresh (Some term)) in

	(* Now, let us look at the structure of the two multi-equations. *)

	match desc1.structure, desc2.structure, merge1, merge2 with
	  
	(* Neither multi-equation contains a term. Merge them; we're done. *)

	| None, None, _, _ ->
	    merge()

	(* Exactly one multi-equation contains a term; keep it. *)

	| None, _, _, merge2
	| _, None, merge2, _ ->
	    merge2()

	(* Both multi-equations contain terms whose head symbol belong
	   to the free algebra [A]. Merge the multi-equations
	   (dropping one of the terms), then decompose the equation
	   that arises between the two terms. Signal an error if the
	   terms are incompatible, i.e. do not have the same head
	   symbol. *)

	| Some (FreeTerm term1), Some (FreeTerm term2), _, _ ->
	    begin
	      merge();
	      try
		A.iter2 unify term1 term2
	      with A.Iter2 ->
		raise Inconsistency
	    end

	(* Both multi-equations contain a uniform row term. Merge the
	   multi-equations (dropping one of the terms), then decompose
	   the equation that arises between the two terms. *)

	| Some (RowUniform content1), Some (RowUniform content2), _, _ ->
	    merge();
	    unify content1 content2

	(* Both multi-equations contain a ``row cons'' term. Compare
	   their labels. *)

	| Some (RowCons (label1, hd1, tl1)),
	  Some (RowCons (label2, hd2, tl2)), _, _ ->
	    let c = RowLabel.compare label1 label2 in
	    if c = 0 then begin

	      (* The labels coincide. This is the cheapest
		 case. Proceed as in the case of free terms above. *)

	      merge();
	      unify hd1 hd2;
	      unify tl1 tl2

	    end
	    else begin

	      (* The labels do not coincide. We must choose which
		 descriptor (i.e. which term) to keep. We choose to
		 keep the one that exhibits the smallest label
		 (according to an arbitrary, fixed total order on
		 labels). This strategy favors a canonical
		 representation of rows, where smaller labels come
		 first. This should tend to make the cheap case above
		 more frequent, thus allowing rows to be unified in
		 quasi-linear time. *)

	      if c < 0 then merge1() else merge2();

	      (* Decompose the equation that arises between the two
		 terms. We must create an auxiliary row variable, as
		 well as two auxiliary row terms. Because their value
		 is logically determined by that of [v1] or [v2], it
		 is appropriate to give them the same rank. *)

	      let tl = fresh None in
	      filter tl1 (RowCons (label2, hd2, tl));
	      filter tl2 (RowCons (label1, hd1, tl))

	    end

	(* The left-hand multi-equation contains a ``row cons'' term,
	   while the right-hand one contains a ``free'' term; or the
	   converse. *)

	| Some (RowCons (label1, hd1, tl1)), Some (FreeTerm term2), merge1, merge2
	| Some (FreeTerm term2), Some (RowCons (label1, hd1, tl1)), merge2, merge1 ->

	    (* We have a choice between keeping a row whose entries
	       are terms, or a term whose sub-terms are rows. It is
	       simple to see that the former is more economical,
	       space-wise, if the term's arity is greater than 2,
	       while the latter is more economical if it is smaller
	       than 2. Both have the same cost when the term's arity
	       is exactly 2. *)

	    if A.arity term2 >= 2 then merge1() else merge2();

	    (* Decompose the equation that arises between the two
	       terms. To do so, split every son of [term2] into a row
	       which exhibits [label1]. Then, create two terms with
	       the same head symbol as [term2], one of which describes
	       the row's value at [label1], the other of which
	       describes its remainder, and equate them with [hd1] and
	       [tl1], respectively. *)

	    let hd2, tl2 = A.map2 (fun son2 ->
	      let hd = fresh None
	      and tl = fresh None in
	      filter son2 (RowCons (label1, hd, tl));
	      hd, tl
	    ) term2 in

	    filter hd1 (FreeTerm hd2);
	    filter tl1 (FreeTerm tl2)

	(* The left-hand multi-equation contains a ``free'' term,
	   while the right-hand one contains a uniform row term; or
	   the converse. *)

	| Some (FreeTerm term1), Some (RowUniform content2), merge1, merge2
	| Some (RowUniform content2), Some (FreeTerm term1), merge2, merge1 ->

	    (* We have a choice between keeping a uniform row whose
	       content is a term, or a term whose sub-terms are
	       uniform rows. The former is more economical, unless the
	       term's arity is 0. *)

	    if A.arity term1 > 0 then merge2() else merge1();

	    (* Decompose the equation that arises between the two
	       terms. To do so, create a term which describes every
	       entry of the uniform row [term1], and equate it with
	       [content2]. *)

	    let content1 = A.map (fun son1 ->
	      let content = fresh None in
	      filter son1 (RowUniform content);
	      content
	    ) term1 in

	    filter content2 (FreeTerm content1)

	(* The left-hand multi-equation contains a ``row cons'' term,
	   while the right-hand one contains a uniform row term; or
	   the converse. *)

	| Some (RowCons (label1, hd1, tl1)), Some (RowUniform content2), _, merge2
	| Some (RowUniform content2), Some (RowCons (label1, hd1, tl1)), merge2, _ ->

	    (* Keep the uniform representation, which is more compact. *)

	    merge2();

	    (* Decompose the equation that arises between the two
	       terms. To do so, equate [hd1] with [content2], then
	       equate [tl1] with a fresh uniform row whose content is
	       [content2].

	       Note that, instead of creating the latter fresh, one
	       may wish to re-use [v2], which is also a uniform row
	       whose content is [content2]. However, these two terms
	       do not have the same sort. Although this optimization
	       would most likely be correct, its proof of correctness
	       would be more involved, requiring a single variable to
	       simultaneously possess several sorts. *)

	    unify hd1 content2;
	    filter tl1 (RowUniform content2)

    in unify

end

