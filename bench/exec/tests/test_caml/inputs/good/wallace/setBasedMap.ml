(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/setBasedMap.ml,v 1.10 2000/02/11 16:15:51 fpottier Exp $ *)

(* This module turns any implementation of general-purpose sets into an implementation of general-purpose maps. *)

module Make (X : GSet.S) = struct

  (* Maps are simply sets of pairs, ordered according to their first component. *)
  
  type ('a, 'b) t =
      ('a * 'b) X.t

  let cp o (key1, _) (key2, _) =
    o key1 key2

  (* An ordering is a two-argument function [f] such that [f e1 e2] is zero if the elements [e1] and [e2] are equal,
     strictly negative if [e1] is smaller than [e2], and strictly positive if [e1] is greater than [e2]. For instance,
     a suitable ordering function for integers is [(-)]. The generic comparison function [Pervasives.compare] is also
     a suitable ordering. *)

  type 'a ordering = 'a -> 'a -> int

  (* The empty map. *)

  let empty =
    X.empty

  (* A map is empty if and only if its underlying set is. *)

  let is_empty =
    X.is_empty

  (* A map's cardinal is the cardinal of its underlying set. *)

  let cardinal =
    X.cardinal

  (* [lookup o k m] looks up the value associated to the key [k] in the map [m], and raises [Not_found] if no value is
     bound to [k]. The ordering [o] must be consistent with the map [m]. *)

  let lookup o key map =
    let _, data = X.memp (fun (key2, _) -> o key key2) map in
    data

  (* [lookup_and_remove o k m] looks up the value [v] associated to the key [k] in the map [m], and raises [Not_found]
     if no value is bound to [k]. The ordering [o] must be consistent with the map [m]. The call returns the value
     [v], together with the map [m] deprived from the binding from [k] to [v]. *)

  let lookup_and_remove o key map =
    let (_, data), map = X.removep (fun (key2, _) -> o key key2) map in
    data, map

  (* [add o k d m] returns a map whose bindings are all bindings in [m], plus a binding of the key [k] to the datum
     [d]. If a binding already exists for [k], it is overridden. The ordering [o] must be consistent with the set
     [m]. *)

  let add o key data map =
    X.fine_add (cp o) (fun old_binding new_binding -> new_binding) (key, data) map

  (* [singleton k d] returns a map whose only binding is from [k] to [d]. *)

  let singleton key data =
    X.singleton (key, data)

  (* [union o m1 m2] returns the union of the maps [m1] and [m2]. As usual, the ordering [o] must be consistent with
     the map [m]. *)

  let union o m1 m2 =
    X.union (cp o) m1 m2

  (* [fine_union o decide m1 m2] returns the union of the maps [m1] and [m2]. If a key [k] is bound to [x1]
     (resp. [x2]) within [m1] (resp. [m2]), then [decide] is called. It is passed [x1] and [x2], and must return the
     value which shall bound to [k] in the final map. As usual, the ordering [o] must be consistent with the map
     [m]. *)

  type 'a decision = 'a -> 'a -> 'a

  let fine_union o decide map1 map2 =
    X.fine_union
      (cp o)
      (fun (key, data1) (_ (* key *), data2) -> (key, decide data1 data2))
      map1
      map2

  (* [iter f m] invokes [f k x], in turn, for each binding from key [k] to element [x] in the map [m]. Keys are
     presented to [f] in increasing order according to the map's ordering. *)

  let iter f map =
    X.iter (fun (key, data) -> f key data) map

  (* [fold f m seed] invokes [f k d accu], in turn, for each binding from key [k] to datum [d] in the map
     [m]. Keys are presented to [f] in increasing order according to the map's ordering. The initial value of
     [accu] is [seed]; then, at each new call, its value is the value returned by the previous invocation of [f]. The
     value returned by [fold] is the final value of [accu]. *)

  let fold f m accu =
    X.fold (fun (key, data) accu ->
      f key data accu
    ) m accu

  (* [fold_rev] performs exactly the same job as [fold], but presents keys to [f] in the opposite order. *)

  let fold_rev f m accu =
    X.fold_rev (fun (key, data) accu ->
      f key data accu
    ) m accu

  (* It is valid to evaluate [iter2 f m1 m2] if and only if [m1] and [m2] have the same domain, according to their
     natural key ordering(s). Doing so invokes [f k x1 x2], in turn, for each key [k] bound to [x1] in [m1] and
     to [x2] in [m2]. Bindings are presented to [f] in increasing order according to the maps' key ordering(s). *)

  let iter2 f m1 m2 =
    X.iter2 (fun (key1, data1) (_, data2) ->
      f key1 data1 data2
    ) m1 m2

  (* [map f m] returns the map obtained by composing the map [m] with the function [f]; that is, the map
     $k\mapsto f(m(k))$. *)

  let map f m =
    X.monotone_map (fun (key, data) -> (key, f data)) m

  (* [endo_map] is similar to [map], but attempts to physically share its result with its input. This saves
     memory when [f] is the identity function. *)

  let endo_map f m =
    X.endo_map (fun ((key, data) as binding) ->
      let data' = f data in
      if data == data' then binding
      else (key, data')
    ) m

  (* A map's domain is a set. Thus, to be able to perform operations on domains, we need set operations, provided by
     the [Domain] sub-module. The two-way connection between maps and their domains is given by two additional
     functions, [domain] and [lift]. [domain m] returns [m]'s domain. [lift f ks] returns the map $k\mapsto f(k)$,
     where $k$ ranges over a set of keys [ks]. *)

  module Domain = X
  
  let domain m =
    X.monotone_map (fun (key, _) -> key) m

  let lift f s =
    X.monotone_map (fun key -> (key, f key)) s

  (* [corestrict o m d] performs a co-restriction of the map [m] to the domain [d]. That is, it returns the map
     $k\mapsto m(k)$, where $k$ ranges over all keys bound in [m] but \emph{not} present in [d]. As usual, the
     ordering [o] must be consistent with the domain [d] and the map [m]. *)

  let corestrict o m d =
    X.corestrict o m d

end

