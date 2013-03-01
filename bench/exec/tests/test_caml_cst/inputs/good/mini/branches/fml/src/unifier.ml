(* $Id$ *)

(** This module implements unification of (ranked) multi-equations
    over a row algebra, that is, an algebra obtained by extending a
    free algebra [A] with rows (see module {!Row}).

    For the purposes of this module, a rank is an element of an
    arbitrary total order. A rank is associated with every
    multi-equation. When two multi-equations are merged, the smaller
    rank is kept.

    It is understood that finite and infinite terms are legal -- no
    occur check is performed here. *)

open Misc
open Sig
open Positions

module Make 
  (MultiEquation : MultiEquation)
  (TypingExceptions: TypingExceptions 
   with module MultiEquation = MultiEquation)
= struct

  (** The multi-equation module associated with this unifier. *)
  module MultiEquation = MultiEquation
  open MultiEquation.Algebra
  open MultiEquation
  module TypingExceptions = TypingExceptions
  open TypingExceptions

  (** The universe of ranks associated with this unifier. *)
  module Rank = MultiEquation.Rank

  (* [unify register v1 v2] equates the variables [v1] and [v2]. That
     is, it adds the equation [v1 = v2] to the constraint which it
     maintains, then rewrites it in a number of ways until an
     inconsistency is found or a solved form is reached. If the
     former, then [Inconsistency] is raised.

     Any variables which are freshly created during the process are
     passed to [register], so as to make the caller aware of their
     existence. *)

  let unify ?tracer pos register =

    let tracer = default (fun s -> ignore) tracer in

    (* Define an auxiliary function which creates a fresh variable,
       found within a multi-equation of its own, with specified
       rank and structure. *)

    let fresh kind rank structure =
      let v = UnionFind.fresh {
        structure = structure;
        rank = rank;
        mark = Mark.none;
        kind = kind;
        name = None;
        pos = Some pos;
        var = None;
        variance = None;
      } in
        register v;
        v in

    (* The main function only has two parameters; [register] remains
       unchanged through recursive calls. *)

    let rec unify pos (v1: variable) (v2: variable) =

      tracer v1 v2;

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
           [merge2] merges the multi-equations, keeping the latter's structure.
        *)

        let fresh, merge, merge1, merge2 =
          let kind = 
            match desc1.kind, desc2.kind with
                k1, k2 when is_rigid v1 -> k1
              | k1, k2 when is_rigid v2 -> k2
              | _ -> Flexible
          in
          let rank1 = desc1.rank
          and rank2 = desc2.rank in
            if Rank.compare rank1 rank2 < 0 then
              let merge1 () =
                UnionFind.union v2 v1
              and merge2 () =
                UnionFind.union v2 v1;
                desc1.structure <- desc2.structure in
                fresh kind rank1, merge1, merge1, merge2
            else
              let merge1 () =
                UnionFind.union v1 v2;
                desc2.structure <- desc1.structure
              and merge2 () =
                UnionFind.union v1 v2 in
                fresh kind rank2, merge2, merge1, merge2 in

        (* Another auxiliary function, for syntactic convenience. *)

        let filter v term =
          unify pos v (fresh (Some term)) in

          (* Now, let us look at the structure of the two multi-equations. *)

          match desc1.structure, desc2.structure, merge1, merge2 with
              
            (* Neither multi-equation contains a term. 
               Merge them; we're done. *)
              
            (* Rigid variables can only be unified with themselves.
               As the two variables are not equivalent, it must 
               be a unification error. *)

            | None, None, _, _ 
                when desc1.kind = Flexible || desc2.kind = Flexible ->
                merge()

            | None, Some (Var v), _, _ when desc1.kind = Flexible ->
                UnionFind.union v1 v2

            | Some (Var v), None, _, _ when desc2.kind = Flexible ->
                UnionFind.union v2 v1

            | Some (Var v), _, _, _ ->
                unify pos v v2

            | _, Some (Var v), _, _ ->
                unify pos v v1
                
            (* Exactly one multi-equation contains a term; keep it. *)
            | None, _, _, _ when is_rigid v1 ->
                raise (CannotUnify (pos, TVariable v1, TVariable v2))

            | _, None, _, _ when is_rigid v2 ->
                raise (CannotUnify (pos, TVariable v2, TVariable v1))
                  
            | None, _, _, merge2
            | _, None, merge2, _ ->
                merge2()

                  
            (* Both multi-equations contain terms whose head symbol belong
               to the free algebra [A]. Merge the multi-equations
               (dropping one of the terms), then decompose the equation
               that arises between the two terms. Signal an error if the
               terms are incompatible, i.e. do not have the same head
               symbol. *)

            | Some (App (term1, term2)), Some (App (term1', term2')), _, _ ->
                begin
                  merge();
                  unify pos term1 term1';
                  unify pos term2 term2'
                end

            (* Both multi-equations contain a uniform row term. Merge the
               multi-equations (dropping one of the terms), then decompose
               the equation that arises between the two terms. *)

            | Some (RowUniform content1), Some (RowUniform content2), _, _ ->
                merge();
                unify pos content1 content2

            (* Both multi-equations contain a ``row cons'' term. Compare
               their labels. *)

            | Some (RowCons (label1, hd1, tl1)),
              Some (RowCons (label2, hd2, tl2)), _, _ ->
                let c = RowLabel.compare label1 label2 in
                  if c = 0 then begin

                    (* The labels coincide. This is the cheapest
                       case. Proceed as in the case of free terms above. *)

                    merge();
                    unify pos hd1 hd2;
                    unify pos tl1 tl2

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

            | Some (RowCons (label1, hd1, tl1)), 
              Some (App (term, term') as term2), 
              merge1, merge2 
            | Some (App (term, term') as term2), 
              Some (RowCons (label1, hd1, tl1)), 
              merge2, merge1 -> 
                let attach son = 
                  let hd, tl = twice fresh None None in
                    filter son (RowCons (label1, hd, tl));
                    hd, tl
                in
                let hd, tl = attach term
                and hd', tl' = attach term' 
                in
                  filter hd1 (App (hd, hd'));
                  filter tl1 (App (tl, tl'))

            (* The left-hand multi-equation contains a ``free'' term,
               while the right-hand one contains a uniform row term; or
               the converse. *)

            | Some (RowUniform content2), Some (App (term2, term2') as term1), 
              merge1, merge2
            | Some (App (term2, term2') as term1), Some (RowUniform content2), 
              merge2, merge1 ->
                merge1 ();
                let attach son =
                  let content = fresh None in 
                    filter son (RowUniform content); 
                    content
                in
                let content1 = App (attach term2, attach term2')
                in
                  filter content2 content1


            | Some (RowCons (label1, hd1, tl1)), Some (RowUniform content2), 
              _, merge2
            | Some (RowUniform content2), Some (RowCons (label1, hd1, tl1)), 
              merge2, _ ->
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

                unify pos hd1 content2;
                filter tl1 (RowUniform content2)

    in unify pos

end

