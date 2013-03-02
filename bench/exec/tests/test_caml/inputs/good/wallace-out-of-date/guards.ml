(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/Attic/guards.ml,v 1.1.2.4 1999/04/08 12:43:45 francois Exp $ *)
(*

Dealing with expanded variables.

Here, we implement a global normalization phase, performed at the end of the closure computation. Garbage collection
can be run without prior normalization; however, canonization and minimization require it. The purpose of this phase
is to eliminate any partially expanded variables. Some such variables may remain, even after using appropriate
normalization calls, for several reasons:

 + A variable's constructed bound may involve expanded variables, because expanding a variable does not affect the
   terms in which it appears.

 + Guarded constraints may involve expanded variables, because expanding a variable does not affect the guarded
   constraints in which it appears.

 + GLUBs may involve expanded variables, because expanding a variable does not affect the GLUBs in which it appears.

In all three cases, the reason why expansion isn't performed greedily is the absence of a back pointer. Subtyping
constraints between variables, on the other hand, are symmetric, so two variables related by a constraint must carry
the same explicit fields at the end of closure.

The normalization phase requires walking the type scheme and performing all necessary expansions. One walk is not
enough -- one must repeat the process until a fix-point is reached.

*)

open Types
open Rowsig

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

A custom graph traversal function. The action function, instead of being called last, is called first; if it causes
the current variable to be expanded, then this fact is taken into account by the traversal function.

In fact, detecting newly created variables as quickly as possible is not necessary for correctness; one could wait
until the next graph traversal. However, by dealing with them right now, one may have a better chance of terminating
in one swoop, thus achieving better overall speed.

*)

let iter_scheme action (Scheme(context, body, effect)) =

  Traverse.start();

  let rec loop v =
    if not (Traverse.is_marked v) then begin
      Traverse.mark v;

      action v;

      match normalize_link v with
	Some _ ->
	  Walk.grok_variable loop v
      |	None ->
	  Walk.grok loop v.lo;
	  Set7.iter loop v.loset;
	  Set7.iter loop v.hiset;
	  Walk.grok loop v.hi;
	  Set7.iter (fun (_, v1, v2) -> Walk.grok_variable loop v1; Walk.grok_variable loop v2) v.guards
    end in

  loop body;
  loop effect;
  Set7.iter (fun (_, ty) -> loop ty) context

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The expansion function.

*)

let unstable =
  ref false

let check v =

  (* Determine whether the variable should be expanded, and if so, which fields should be added. The fields to be
     added are any fields explicitly carried by the constructed bounds, as well as any fields carried by the members
     of the guarded constraints. *)

  let requested_fields = accumulate_small_term v.lo (accumulate_small_term v.hi RowSet.empty) in

  let requested_fields = Set7.fold (fun (_, v1, v2) accu ->
    accumulate_variable v2 (accumulate_variable v1 accu)
  ) v.guards requested_fields in

  (* If the variable must be expanded, do it. *)

  if not (RowSet.is_empty requested_fields) then begin
    let _ = expand_clean_variable requested_fields v in
    unstable := true
  end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

The main normalization function.

The constraint graph is repeatedly traversed, until the condition evaluates to false, i.e. until we are able to
perform a whole traversal without expanding any type variables. The complexity of this algorithm isn't very clear;
hopefully it terminates very quickly. Its correctness is easy to check; when the algorithm terminates, a whole graph
traversal has been performed without finding any expanded variables, so normalization has been performed.

*)

let rec normalize scheme =
  unstable := false;
  iter_scheme check scheme;
  if !unstable then
    normalize scheme

let normalize scheme =
  Errors.trace "Guards.normalize" (fun () ->
    normalize scheme
  )

