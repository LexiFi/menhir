(* This module provides a simple-minded implementation of first-order
   unification over an arbitrary signature. *)

(* -------------------------------------------------------------------------- *)

(* The signature must be described by the client, as follows. *)

module type STRUCTURE = sig

  (* The type ['a structure] should be understood as a type of shallow terms
     whose leaves have type ['a]. *)
  type 'a structure

  val map: ('a -> 'b) -> 'a structure -> 'b structure

  val iter: ('a -> unit) -> 'a structure -> unit

  (* [iter2] fails if the head constructors differ. *)
  exception Iter2
  val iter2: ('a -> 'b -> unit) -> 'a structure -> 'b structure -> unit

end

(* -------------------------------------------------------------------------- *)

(* The unifier. *)

module Make (S : STRUCTURE) = struct

type 'a structure = 'a S.structure

(* The data structure maintained by the unifier is as follows. *)

(* A unifier variable is a point of the union-find algorithm. *)

type variable =
    descriptor UnionFind.point

and descriptor = {

  (* Every equivalence class carries a globally unique identifier. When
     a new equivalence class is created, a fresh identifier is chosen,
     and when two classes are merged, one of the two identifiers is kept.
     This identifier can be used as a key in a hash table. One should be
     aware, though, that identifiers are stable only as long as no unions
     are performed. *)

  id : int;

  (* Every equivalence class carries a structure, which is either [None],
     which means that the variable is just that, a variable; or [Some t],
     which means that the variable represents (has been equated with) the
     term [t]. *)

  structure : variable structure option;

  (* Every equivalence class carries a mutable mark, which is used only by the
     occurs check. We could also remove this field altogether and use a
     separate hash table, where [id]s serve as keys, but this should be
     faster. The occurs check is performed eagerly, so this could matter. *)

  mutable mark : Mark.t;

}

(* -------------------------------------------------------------------------- *)

(* Accessors. *)

let id v =
  (UnionFind.get v).id

let structure v =
  (UnionFind.get v).structure

(* -------------------------------------------------------------------------- *)

(* [r++]. *)

let postincrement r =
  let v = !r in
  r := v + 1;
  v

(* -------------------------------------------------------------------------- *)

(* [fresh] creates a fresh variable with specified structure. *)

let fresh =
  let c = ref 0 in
  fun structure ->
    let id = postincrement c in
    let mark = Mark.none in
    UnionFind.fresh { id; structure; mark }

(* -------------------------------------------------------------------------- *)

(* [occurs_check x y] checks that [x] does not occur within [y]. *)

exception Occurs of variable * variable

let occurs_check x y =
  (* Generate a fresh color for this particular traversal. *)
  let black = Mark.fresh () in
  (* The traversal code -- a depth-first search. *)
  let rec visit z =
    let desc = UnionFind.get z in
    if not (Mark.same desc.mark black) then begin
      desc.mark <- black;
      (* We are looking for [x]. *)
      if UnionFind.equivalent x z then
        raise (Occurs (x, y))
      else
        Option.iter (S.iter visit) desc.structure
    end
  in
  (* The root is [y]. *)
  visit y

(* -------------------------------------------------------------------------- *)

(* The internal function [unify v1 v2] equates the variables [v1] and [v2] and
   propagates the consequences of this equation until a cycle is detected, an
   inconsistency is found, or a solved form is reached. The exceptions that
   can be raised are [Occurs] and [S.Iter2]. *)

let rec unify (v1 : variable) (v2 : variable) : unit =
  if not (UnionFind.equivalent v1 v2) then begin
    (* Prevent the creation of cycles. *)
    occurs_check v1 v2;
    occurs_check v2 v1;
    (* Read the two descriptors BEFORE calling [union]. *)
    let desc1 = UnionFind.get v1
    and desc2 = UnionFind.get v2 in
    (* Recursively unify the two descriptors. *)
    UnionFind.set v1 (unify_descriptors desc1 desc2);
    (* Merge the equivalence classes. Do this LAST so we get more meaningful
       output if unification fails and we have to print the two terms. *)
    UnionFind.union v1 v2
  end

(* -------------------------------------------------------------------------- *)

(* [unify_descriptors desc1 desc2] combines the descriptors [desc1] and
   [desc2], producing a descriptor for the merged equivalence class. *)

and unify_descriptors desc1 desc2 =
  let structure = unify_structures desc1.structure desc2.structure in
  { desc1 with structure }
    (* the choice of [id] is arbitrary; the value of [mark] is irrelevant *)

(* -------------------------------------------------------------------------- *)

(* [unify_structures structure1 structure2] combines two structures. If one of
   them is undefined, we just keep the other. If both are defined, we unify
   them recursively. *)

and unify_structures structure1 structure2 =
  Option.multiply (fun t1 t2 ->
    S.iter2 unify t1 t2;
    t2 (* arbitrary; [t1] and [t2] are now equal anyway *)
  ) structure1 structure2

(* -------------------------------------------------------------------------- *)

(* The public version of [unify]. *)

exception Unify of variable * variable

let unify v1 v2 =
  try
    unify v1 v2
  with S.Iter2 ->
    raise (Unify (v1, v2))

(* -------------------------------------------------------------------------- *)

(* Decoding an acyclic graph as a deep term. *)

(* This is a simple-minded version of the code, where sharing is lost. Its
   cost could be exponential if there is a lot of sharing. In practice, its
   use is usually appropriate, especially in the scenario where the term is
   meant to be printed as a tree. *)

type term =
  | TVar of int
  | TNode of term structure

let rec decode (v : variable) : term =
  match structure v with
  | None ->
     TVar (id v)
  | Some t ->
     TNode (S.map decode t)

(* -------------------------------------------------------------------------- *)

end
