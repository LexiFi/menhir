(******************************************************************************)
(*                                                                            *)
(*                                    Menhir                                  *)
(*                                                                            *)
(*   Copyright Inria. All rights reserved. This file is distributed under     *)
(*   the terms of the GNU General Public License version 2, as described in   *)
(*   the file LICENSE.                                                        *)
(*                                                                            *)
(******************************************************************************)

open Printf
open StackLang

module Make
  (Data : sig
     include Order.S
     val default : t
   end)
  (S : sig
     type label' = label * Data.t
     val spec_label : label' -> label
     val spec_tblock :
       (* jump: *) (label' -> block) ->
       label' -> typed_block ->
       typed_block
   end)
  (X : sig val program : program end)
: sig val program : program end
= struct

open S
open X

(* A queue of [(label, data)] pairs, waiting to be processed. Each such pair
   represents a request to specialize block [label] for data [data]. This is
   a queue-set, so a pair can be inserted into the queue at most once. *)

module Q =
  QueueSet.Make(Order.Pair(Label)(Data))

let queue : Q.t =
  Q.create()

(* A set of the labels that have been specialized. This set is used for
   counting purposes only. *)

let labels =
  ref Label.Set.empty

(* [enqueue label'] inserts the label [label'] into the queue. *)

let enqueue label' =
  let label, _data = label' in
  if Q.add label' queue then
    labels := Label.Set.add label !labels

(* [jump label'] inserts the label [label'] into the queue and returns
   a [JUMP] instruction towards this label. *)

let jump label' =
  enqueue label';
  IJump (spec_label label')

(* The main computation. *)

let program =
  (* Phase 1. Enqueue the entry points. *)
  program.entry |> StringMap.iter begin fun _ label ->
    enqueue (label, Data.default)
  end;
  (* Phase 2. As long as the queue contains specialization requests,
     service these requests by creating new specialized blocks.
     The transformed control flow graph keeps growing. *)
  let cfg = ref Label.Map.empty in
  Q.repeatedly queue begin fun label' ->
    let label, _data = label' in
    let tblock = lookup program label in
    let tblock = spec_tblock jump label' tblock in
    cfg := Label.Map.add (spec_label label') tblock !cfg
  end;
  (* Done. *)
  (* The map of the entry points need not be rebuilt: our assumption
     about [spec_label] guarantees that the names of the entry points
     do not change. *)
  { program with cfg = !cfg }

(* Log an information message. *)

let () =
  Error.logC 1 (fun f ->
    fprintf f "%d specialized copies of %d functions have been created.\n"
      (Q.count queue)
      (Label.Set.cardinal !labels)
  )

end (* Make *)
