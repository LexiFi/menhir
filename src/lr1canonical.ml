(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

(* This module formulates the construction of the canonical LR(1) automaton as
   a least fixed point computation. *)

(* It is currently *not* used; the code in [Lr1construction] also constructs
   the canonical LR(1) automaton when [--canonical] is passed on the command
   line. *)

(* The code in this module has been tested and seems correct. Furthermore, it
   appears to be roughly as efficient as the code in [Lr1construction]. There
   are some outliers, though; sometimes one is 2x faster than the other,
   sometimes it is the converse. *)

(* This formulation may be deemed somewhat naive: because a "variable" is an
   LR(0) state and a "property" is a set of LR(1) states, every time the
   algorithm revisits an LR(0) state [s], it recomputes *all* of the LR(1)
   states that correspond to [s]. *)

type lr0state =
  Lr0.node

type lr1state =
  Lr0.lr1state

open Grammar

(* -------------------------------------------------------------------------- *)

(* A property is a set of core-compatible LR(1) states. In fact, for each
   LR(0) core [c], we have a different space of properties, namely the sets of
   LR(1) states whose core is [c]. *)

module P = struct

  module S = struct

    include Set.Make(struct
      type t = lr1state
      let compare = Lr0.compare
    end)

    (* [map] is part of the output of [Set.Make] since 4.04 only, so we have
       to define it by hand. *)

    let map (f : elt -> elt) (s : t) : t =
      fold (fun x accu ->
        add (f x) accu
      ) s empty

  end

  type property =
    S.t

  let bottom =
    S.empty

  let equal =
    S.equal

  let is_maximal _p =
    false

  let singleton =
    S.singleton

  let iter =
    S.iter

  let map =
    S.map

  let big_union (xs : 'a list) (f : 'a -> property) : property =
    List.fold_left (fun accu x ->
      S.union accu (f x)
    ) S.empty xs

end

(* -------------------------------------------------------------------------- *)

(* Instantiate [Fix] to compute a mapping of LR(0) states to properties. *)

module F =
  Fix.Make
    (Maps.ArrayAsImperativeMaps(Lr0))
    (P)

(* -------------------------------------------------------------------------- *)

module Run () = struct

(* -------------------------------------------------------------------------- *)

(* Define the desired least fixed point. *)

let mu : lr0state -> P.property =

  F.lfp (fun (c : lr0state) get ->

    (* Test whether [c] is a start state of the LR(0) automaton. *)
    match Lr0.incoming_symbol c with
    | None ->
        (* [c] is a start state. We need the corresponding LR(1) start state,
           and nothing else. *)
        P.singleton (Lr0.start c)
    | Some symbol ->
        (* [c] is not a start state. For every edge of [b] to [c] in the LR(0)
           automaton, we must find out which LR(1) states whose core is [b]
           currently exist, and compute their successors along [symbol]. This
           yields a set of LR(1) states. *)
        P.big_union (Lr0.incoming_edges c) (fun b ->
          P.map (Lr0.transition symbol) (get b)
        )
  )

(* -------------------------------------------------------------------------- *)

(* We now force the least fixed computation to take place. (This is implicit;
   it is done just by applying the function [mu] to every LR(0) state). At the
   same time, we count the states of the automaton, and assign a unique number
   to each of them. We build the mapping of states to numbers and the reverse
   mapping of numbers to states. *)

(* For every LR(0) state [c], the indices [start.(c)] and [finish c] delimit a
   semi-open interval. The indices within this interval correspond to the
   LR(1) states whose core is [c]. *)

type node =
  int

module N =
  Fix.Numbering.ForOrderedType(Lr0.Lr1StateAsOrderedType)
  (* This yields [N.encode : lr1state -> node]
            and  [N.current : unit -> node]. *)

let start =
  Array.make Lr0.n 0 (* dummy *)
    (* TEMPORARY get rid of this array? *)

let () =
  Misc.iteri Lr0.n (fun c ->
    start.(c) <- N.current();
    P.iter (fun s -> ignore (N.encode s)) (mu c)
  )

include N.Done()
  (* This defines [n : int],
                  [encode : lr1state -> node],
                  [decode : node -> lr1state]. *)

let state : node -> lr1state =
  decode

(* -------------------------------------------------------------------------- *)

(* Expose the entry states of the LR(1) automaton. *)

let entry : node ProductionMap.t =
  ProductionMap.map (fun (c : Lr0.node) ->
    (* Exactly one state in the canonical LR(1) automaton corresponds to the
       LR(0) state [c]. *)
    start.(c)
  ) Lr0.entry

(* -------------------------------------------------------------------------- *)

(* Expose the transitions of the LR(1) automaton. *)

let transition symbol (i : node) : node =
  encode (Lr0.transition symbol (state i))

let outgoing_symbols (i : node) =
  Lr0.outgoing_symbols (Lr0.core (state i))

let transitions (i : node) : node SymbolMap.t =
  SymbolMap.init (fun symbol ->
    transition symbol i
  ) (outgoing_symbols i)

(* -------------------------------------------------------------------------- *)

(* Expose the numbering of the states of the LR(1) automaton. *)

let number (i : node) : int =
  i

let node (i : int) : node =
  i

(* -------------------------------------------------------------------------- *)

end (* Run *)
