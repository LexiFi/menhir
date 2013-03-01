(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/connexity.ml,v 1.23.4.12 1999/04/08 13:25:06 francois Exp $ *)

open Errors
open Types
open Rowsig

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Computing polarities.

*)

let make_sign_valid v =

  (* According to the convention below, if the variable isn't marked, its sign is reset first. As a side effect,
     the variable is marked. *)

  if not (Traverse.is_marked v) then begin
    Traverse.mark v;
    v.sign <- Neutral
  end

let rec give_sign sign v =
  match v.link with
    None ->

      (* The condition block in the following test does the following.

	 First, make the variable's sign valid if necessary. (We don't use make_sign_valid here, although we could,
	 because we inline the call and partially evaluate it to combine it with what follows.)

	 Then, check whether the variable already has this sign. If so, there is nothing to do. Otherwise, give it
	 this sign and prepare to proceed. (Again, we do not use set_sign here, because we partially evaluate the
	 call.) *)

      if begin
	if not (Traverse.is_marked v) then begin
	  Traverse.mark v;
	  v.sign <- (if sign then Positive else Negative);
	  true
	end
	else begin
	  match (sign, v.sign) with
	    true, Negative ->
	      v.sign <- Bipolar;
	      true
	  | true, Neutral ->
	      v.sign <- Positive;
	      true
	  | false, Positive ->
	      v.sign <- Bipolar;
	      true
	  | false, Neutral ->
	      v.sign <- Negative;
	      true
	  | _ ->
	      false
	end
      end
      then begin

	(* Give signs to the constructed bound. Furthermore, if this is a negative variable, give signs to the
	   variables mentioned in the guarded constraints. *)

	if sign then
	  groks true v.lo
	else begin
	  groks false v.hi;
	  Set7.iter (fun (_, v1, v2) ->
	    give_sign true v1;
	    give_sign false v2
	  ) v.guards
	end;

	(* We must make sure that all variables linked to this one have valid signs, because the garbage collection
	   phase will check them to determine whether these constraints should be removed. *)

	Set7.iter make_sign_valid v.loset;
	Set7.iter make_sign_valid v.hiset

      end

  | Some row ->

      give_sign_leaf sign row.remainder;
      RowMap.iter (give_sign_leaf sign) row.entries

and give_sign_leaf sign = function
    TVar v ->
      give_sign sign v
  | TLUB vset ->
      Set7.iter (give_sign sign) vset
  | TGLB vset ->
      Set7.iter (give_sign sign) vset

and groks sign = function
    TBottom
  | TTop
  | TRTop
  | TRMissing
  | TVAbsent
  | TRBottom
  | TVTop ->
      ()
  | TAbstract(stamp, args) ->
      List.iter2 (fun variance arg ->
	give_sign_leaf (if variance then sign else not sign) arg
      ) (Abstract.get_variance stamp) args
  | TRPresent ty ->
      give_sign_leaf sign ty
  | TRMaybe ty ->
      give_sign_leaf sign ty
  | TVPresent ty ->
      give_sign_leaf sign ty

let polarity (Scheme(context, body, effect) as scheme) =

  (* We use a clever trick to avoid having to reset all signs prior to computing the fixpoint. The advantage of 
     our method is that it eliminates one pass and makes the collection time proportional to the output graph's
     size, instead of the input graph's size. Besides, it's cool.

     The trick is to adopt the convention that if a variable is marked, then its sign is valid, otherwise its
     sign is invalid and should be reset before proceeding. This means that when the function completes, all
     marked variables have their correct signs, and all unmarked variables are unreachable.

     When we subsequently perform the actual collection phase, we shall need to re-use the marks to avoid looping.
     At first glance, this is a problem because we have no way of telling whether a variable's sign is valid.
     However, the collection phase will walk the graph in such a way that only variables with valid signs are
     traversed, so we needn't worry about this.

     A quick measurement shows that this trick makes the polarity phase twice faster, while the collection phase
     is unaffected. *)

  Traverse.start();

  give_sign true body;
  give_sign true effect;
  Set7.iter (fun (_, ty) -> give_sign false ty) context

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Timers.

*)

let garbage_polarity_clock = Chrono.create()
let garbage_collection_clock = Chrono.create()

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The actual garbage collection code.

Only three kinds of constraints are kept:

  negative variable < positive variable
  constructed term < positive variable
  negative variable < constructed term

Guarded constraints are kept only if the selector variable is negative.

We need to reset the marks to avoid looping while collecting. However, we walk the constraint graph in such a way that
we only visit variables whose sign has been correctly set by the polarity phase.

*)

let rec collect v =
  match v.link with
    None ->

      if not (Traverse.is_marked v) then begin
	Traverse.mark v;

	match v.sign with
	  Positive ->
	    grok v.lo;
	    v.loset <- Set7.filter is_negative v.loset;
	    Set7.iter collect v.loset;
	    v.hiset <- empty_variable_set;
	    let _, hi = tautologous_bounds_kind (kind_of_variable v) in
	    v.hi <- hi;
	    v.guards <- empty_guard_set
	| Negative ->
	    let lo, _ = tautologous_bounds_kind (kind_of_variable v) in
	    v.lo <- lo;
	    v.loset <- empty_variable_set;
	    v.hiset <- Set7.filter is_positive v.hiset;
	    Set7.iter collect v.hiset;
	    grok v.hi;
	    Set7.iter (fun (_, v1, v2) -> collect v1; collect v2) v.guards
	| Bipolar ->
	    grok v.lo;
	    v.loset <- Set7.filter is_negative v.loset;
	    Set7.iter collect v.loset;
	    v.hiset <- Set7.filter is_positive v.hiset;
	    Set7.iter collect v.hiset;
	    grok v.hi;
	    Set7.iter (fun (_, v1, v2) -> collect v1; collect v2) v.guards
	| Neutral ->
	    raise (CantHappen "Connexity.garbage")

      end

  | Some row ->

      collect_leaf row.remainder;
      RowMap.iter collect_leaf row.entries

and collect_leaf = function
    TVar v ->
      collect v
  | TLUB vset ->
      Set7.iter collect vset
  | TGLB vset ->
      Set7.iter collect vset

and grok = function
    TBottom
  | TTop
  | TRTop
  | TRMissing
  | TVAbsent
  | TRBottom
  | TVTop ->
      ()
  | TAbstract(stamp, args) ->
      List.iter collect_leaf args
  | TRPresent ty ->
      collect_leaf ty
  | TRMaybe ty ->
      collect_leaf ty
  | TVPresent ty ->
      collect_leaf ty

let garbage (Scheme(context, body, effect) as scheme) =

  Chrono.chrono garbage_polarity_clock (fun () ->

    polarity scheme

  );
  Chrono.chrono garbage_collection_clock (fun () ->

    Traverse.start();
    collect body;
    collect effect;
    Set7.iter (fun (_, ty) -> collect ty) context

  )

