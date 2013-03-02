(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/walk.ml,v 1.19.2.14 1999/04/08 13:25:07 francois Exp $ *)

open Types
open Errors
open Rowsig

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A generic function to walk a type and produce a modified, but mostly unchanged, type. This function has two
advantages: hiding the repetitive code and enhancing sharing with the old type as much as possible.

*)

let walk_leaf behavior sign leaf =
  match normalize_leaf leaf with
    TVar v -> (
      match normalize_link v with
	Some row ->
	  let entries = row.entries
	  and remainder = row.remainder in
	  let entries' = RowMap.map (behavior sign) entries
	  and remainder' = behavior sign remainder in
	  if (entries == entries') & (remainder == remainder') then leaf
	  else begin
	    let dummy = freshV v in
	    dummy.link <- Some { entries = entries'; remainder = remainder' };
	    TVar dummy
	  end
      |	None ->
	  behavior sign leaf
    )
  | TLUB _
  | TGLB _ ->
      behavior sign leaf

let walk behavior sign ty =
  match ty with
  | TAbstract (stamp, args) ->
      let unchanged, args' = List.fold_right2 (fun variance arg (unchanged, args') ->
	let arg' = walk_leaf behavior (if variance then sign else not sign) arg in
	unchanged & (arg == arg'), arg' :: args'
      ) (Abstract.get_variance stamp) args (true, []) in
      if unchanged then ty else TAbstract (stamp, args')
  | TRPresent arg ->
      let arg' = walk_leaf behavior sign arg in
      if arg == arg' then ty else TRPresent arg'
  | TRMaybe arg ->
      let arg' = walk_leaf behavior sign arg in
      if arg == arg' then ty else TRMaybe arg'
  | TVPresent arg ->
      let arg' = walk_leaf behavior sign arg in
      if arg == arg' then ty else TVPresent arg'
  | TBottom
  | TTop
  | TRBottom
  | TRMissing
  | TRTop
  | TVAbsent
  | TVTop ->
      ty

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

[groksa action sign ty accu] walks down [ty] recursively. At each (clean) variable node, [action] is called. It is
passed the current value of [sign], the variable found at the node, and the accumulator, and must return an updated
accumulator. The call to groksa then returns the final value of the accumulator.

groks is a version of groksa which doesn't deal with an accumulator; grok is a version which deals neither with an
accumulator, nor with signs. These versions are provided for better efficiency.

*)

let groksa_clean_leaf action sign leaf accu =
  match leaf with
    TVar v ->
      action sign v accu
  | TLUB vset ->
      Set7.fold (action sign) vset accu
  | TGLB vset ->
      Set7.fold (action sign) vset accu

let groksa_variable action sign v accu =
  match normalize_link v with
    Some row ->
      let accu = groksa_clean_leaf action sign row.remainder accu in
      RowMap.fold (fun _ ty accu -> groksa_clean_leaf action sign ty accu) row.entries accu
  |	None ->
      action sign v accu

let groksa_leaf action sign leaf accu =
  match normalize_leaf leaf with
    TVar v ->
      groksa_variable action sign v accu
  | TLUB vset ->
      Set7.fold (action sign) vset accu
  | TGLB vset ->
      Set7.fold (action sign) vset accu

let groksa action sign term accu =
  match term with
    TBottom
  | TTop
  | TRTop
  | TRMissing
  | TVAbsent
  | TRBottom
  | TVTop ->
      accu
  | TAbstract(stamp, args) ->
      List.fold_left2 (fun accu variance arg ->
	groksa_leaf action (if variance then sign else not sign) arg accu
      ) accu (Abstract.get_variance stamp) args
  | TRPresent ty ->
      groksa_leaf action sign ty accu
  | TRMaybe ty ->
      groksa_leaf action sign ty accu
  | TVPresent ty ->
      groksa_leaf action sign ty accu

let groka_clean_leaf action term accu =
  match term with
    TVar v ->
      action v accu
  | TLUB vset ->
      Set7.fold action vset accu
  | TGLB vset ->
      Set7.fold action vset accu

let groka_variable action v accu =
  match normalize_link v with
    Some row ->
      let accu = groka_clean_leaf action row.remainder accu in
      RowMap.fold (fun _ ty accu -> groka_clean_leaf action ty accu) row.entries accu
  | None ->
      action v accu

let groka_leaf action leaf accu =
  match normalize_leaf leaf with
    TVar v ->
      groka_variable action v accu
  | TLUB vset ->
      Set7.fold action vset accu
  | TGLB vset ->
      Set7.fold action vset accu

let groka action term accu =
  match term with
    TBottom
  | TTop
  | TRTop
  | TRMissing
  | TVAbsent
  | TRBottom
  | TVTop ->
      accu
  | TAbstract(stamp, args) ->
      List.fold_right (groka_leaf action) args accu
  | TRPresent ty ->
      groka_leaf action ty accu
  | TRMaybe ty ->
      groka_leaf action ty accu
  | TVPresent ty ->
      groka_leaf action ty accu

let grok_clean_leaf action = function
    TVar v ->
      action v
  | TLUB vset ->
      Set7.iter action vset
  | TGLB vset ->
      Set7.iter action vset

let grok_variable action v =
  match normalize_link v with
    Some row ->
      grok_clean_leaf action row.remainder;
      RowMap.iter (grok_clean_leaf action) row.entries
  | None ->
      action v

let grok_leaf action leaf =
  match normalize_leaf leaf with
    TVar v ->
      grok_variable action v
  | TLUB vset ->
      Set7.iter action vset
  | TGLB vset ->
      Set7.iter action vset

let grok action = function
    TBottom
  | TTop
  | TRTop
  | TRMissing
  | TVAbsent
  | TRBottom
  | TVTop ->
      ()
  | TAbstract(stamp, args) ->
      List.iter (grok_leaf action) args
  | TRPresent ty ->
      grok_leaf action ty
  | TRMaybe ty ->
      grok_leaf action ty
  | TVPresent ty ->
      grok_leaf action ty

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Walking a scheme by traversing the constraint graph. These functions use the primitives provided by Types.Traverse,
as well as grok. In addition to being directly useful, they are good canonical examples of how to walk constraint
graphs.

TEMPORARY call groka_variable on guarded constraints, or explain why this isn't done

*)

let fold_scheme action (Scheme(context, body, effect)) accu =

  Traverse.start();

  let rec loop v accu =
    if not (Traverse.is_marked v) then begin
      Traverse.mark v;
      let accu = groka loop v.lo accu in
      let accu = Set7.fold loop v.loset accu in
      let accu = Set7.fold loop v.hiset accu in
      let accu = groka loop v.hi accu in
      let accu = Set7.fold (fun (_, v1, v2) accu -> loop v2 (loop v1 accu)) v.guards accu in
      let accu = action v accu in
      accu
    end
    else
      accu in

  let accu = loop body accu in
  let accu = loop effect accu in
  Set7.fold (fun (_, ty) accu -> loop ty accu) context accu
;;

let iter_scheme action (Scheme(context, body, effect)) =

  Traverse.start();

  let rec loop v =
    if not (Traverse.is_marked v) then begin
      Traverse.mark v;
      grok loop v.lo;
      Set7.iter loop v.loset;
      Set7.iter loop v.hiset;
      grok loop v.hi;
      Set7.iter (fun (_, v1, v2) -> loop v1; loop v2) v.guards;
      action v
    end in

  loop body;
  loop effect;
  Set7.iter (fun (_, ty) -> loop ty) context
;;

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Copying type schemes.

Copy_scheme simply returns a copy of the scheme (i.e. the same scheme with all variables replaced with fresh
ones). The "representative" field is used to maintain a constant-time mapping from each variable to its copy.
Variables which haven't been copied yet can be recognized by the fact that they are their own representative.
Of course, for this convention to work, we have to reset the "representative" fields of the original scheme
after the copy operation is complete; "fvlist" is used to speed this up.

*)

let fvlist =
  ref []
;;

let rec copy_var v =
  if v != v.representative then
    v.representative
  else begin
    let v' = freshV v in
    v.representative <- v';
    fvlist := v :: !fvlist;
    v'.lo <- copy_type v.lo;
    v'.loset <- copy_vset v.loset;
    v'.hiset <- copy_vset v.hiset;
    v'.hi <- copy_type v.hi;
    v'.guards <- copy_guards v.guards;
    v'
  end

and copy_vset vset =
  Set7.fold (fun v vset -> Set7.add (copy_var v) vset) vset empty_variable_set

and copy_clean_leaf = function
    TVar v ->
      TVar (copy_var v)
  | TLUB vset ->
      TLUB (copy_vset vset)
  | TGLB vset ->
      TGLB (copy_vset vset)

and copy_leaf leaf =
  match normalize_leaf leaf with
    TVar v ->
      if v != v.representative then
	TVar v.representative
      else begin
	match normalize_link v with
	  Some row ->
	    let v' = freshV v in
	    v.representative <- v';
	    fvlist := v :: !fvlist;
	    let row = {
	      entries = RowMap.map copy_clean_leaf row.entries;
	      remainder = copy_clean_leaf row.remainder
	    } in
	    v'.link <- Some row;
	    TVar v'
	| None ->
	    TVar (copy_var v)
      end
  | TLUB vset ->
      TLUB (copy_vset vset)
  | TGLB vset ->
      TGLB (copy_vset vset)	  

and copy_type = function
  (TBottom | TTop | TRTop | TRMissing | TVAbsent | TRBottom | TVTop) as ty ->
    ty
| TAbstract(stamp, args) ->
    TAbstract(stamp, List.map copy_leaf args)
| TRPresent ty ->
    TRPresent (copy_leaf ty)
| TRMaybe ty ->
    TRMaybe (copy_leaf ty)
| TVPresent ty ->
    TVPresent (copy_leaf ty)

and copy_guards guards =
  Set7.fold (fun (head, v1, v2) guards -> Set7.add (head, copy_var v1, copy_var v2) guards) guards empty_guard_set

and copy_context context =
  Set7.fold (function (identifier, ty) -> Set7.add (identifier, copy_var ty)) context empty_context

let copy_scheme (Scheme(context, body, effect) as scheme) =
  let scheme' = Scheme(copy_context context, copy_var body, copy_var effect) in
  List.iter (fun v ->
    v.representative <- v
  ) !fvlist;
  fvlist := [];
  scheme'

