(******************************************************************************)
(*                                                                            *)
(*                                    Fix                                     *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU Library General Public License version 2, with a         *)
(*  special exception on linking, as described in the file LICENSE.           *)
(*                                                                            *)
(******************************************************************************)

open Sigs

let force o =
  match o with Some x -> x | None -> assert false

module Make (M : IMPERATIVE_MAPS) = struct

  type t = M.key

  (* Create a generator of fresh integers. *)

  open Gensym

  let g =
    generator()

  let current () =
    current g

  (* Memoizing the function [fun _ -> fresh g] yields the function [encode],
     which maps keys to unique integers. We use [visibly_memoize] so as to
     have access to the memoization table. This allows us to use operations
     such as [M.find] and [M.iter] below. *)

  let (encode : t -> int), (table : int M.t) =
    let module Memo = Memoize.Make(M) in
    Memo.visibly_memoize (fun (_ : t) -> fresh g)

  (* Testing whether a key has been encountered already. *)

  let has_been_encoded (x : t) : bool =
    (* We do not have [M.mem], so we re-implement it in terms of [M.find]. *)
    try
      let _ = M.find x table in
      true
    with Not_found ->
      false

  (* Building a mapping of integer codes back to keys. *)

  let reverse_mapping () : t array =
    let n = current() in
    let reverse : t option array = Array.make n None in
    M.iter (fun x i ->
      reverse.(i) <- Some x
    ) table;
    Array.map force reverse

  module Done () = struct

    type t = M.key

    let n = current()

    let encode x =
      (* It would be an error to try and encode new keys now. Thus, if
         [x] has not been encountered before, the client is at fault.
         Fail with a nice informative message. *)
      if has_been_encoded x then
        encode x
      else
        Printf.sprintf
          "Fix.Numbering: invalid argument passed to \"encode\".\n%s\n"
          __LOC__
        |> invalid_arg

    let reverse =
      reverse_mapping()

    let decode i =
      if 0 <= i && i < n then
        reverse.(i)
      else
        Printf.sprintf
          "Fix.Numbering: invalid argument passed to \"decode\".\n\
           The index %d is not in the range [0, %d).\n%s\n"
          i n __LOC__
        |> invalid_arg

  end

end

module ForOrderedType (T : OrderedType) =
  Make(Glue.PersistentMapsToImperativeMaps(Map.Make(T)))

module ForHashedType (T : HashedType) =
  Make(Glue.HashTablesAsImperativeMaps(T))

module ForType (T : TYPE) =
  ForHashedType(Glue.TrivialHashedType(T))

module Typed = struct

  (** A value of type [c : n cardinal] witnesses the fact that the set [n] has
      cardinal [c].
      A [cardinal] is always greater than or equal to 0.
  *)
  type 'n cardinal = int lazy_t
  let cardinal (lazy x : 'n cardinal) : int = x

  (** A value of type [i : n index] is an integer that is guaranteed to belong
      to the set [n].
      If [c : n cardinal], then [0 <= i < c].

      Note: elements of a finite set are called [index] because their main
      purpose is to index information in fixed-size vectors.
      See [Vector] sub-module below.
  *)
  type 'n index = int

  (** Type-level sets are introduced by modules (to create fresh type names).
      A new set is represented by a pair of a fresh abstract type [n] and a
      [cardinal] value that represents the cardinal of the set.  *)
  module type CARDINAL = sig type n val n : n cardinal end

  (** Create a new type for a set with a determined cardinal. *)
  module Const(X : sig val cardinal : int end) : CARDINAL =
  struct
    type n
    let () = assert (X.cardinal >= 0)
    let n = lazy X.cardinal
  end

  module Empty : CARDINAL = struct
    type n
    let n = lazy 0
  end

  let const c : (module CARDINAL) =
    assert (c >= 0);
    (module struct type n let n = lazy c end)

  (** "Gensym", for sets whose cardinality is not yet known.
      Creates a new set to which elements can be added as long as its cardinal
      has not been observed. *)
  module Gensym() : sig
    include CARDINAL

    (** Add a new element is the set if [cardinal] has not been forced yet.
        It is forbidden to call [fresh] after forcing the cardinal. *)
    val fresh : unit -> n index
  end = struct
    type n
    let counter = ref 0
    let n = lazy !counter

    let fresh () =
      assert (not (Lazy.is_val n));
      let result = !counter in
      incr counter;
      result
  end

  (** Sum of two sets.
      These definitions implements the disjoint union operator L + R. *)

  (** The type [either] is used to tell whether a value belongs to the left or
      the right set *)
  type ('l, 'r) either =
    | L of 'l
    | R of 'r

  (** The SUM module type.
      It defines a set [n] and exposes the isomorphism between [n] and [l + r].
  *)
  module type SUM = sig
    type l and r
    include CARDINAL
    val inj_l : l index -> n index
    val inj_r : r index -> n index
    val prj : n index -> (l index, r index) either
  end

  (** Introduce a new set that is the sum of [L] and [R].
      It is strict in [L.cardinal] but not [R.cardinal]: if [R] is an instance
      of [Gensym()] that has not been forced, new elements can still be added.
      Forcing the resulting cardinal forces [R.cardinal] too.
  *)
  module Sum(L : CARDINAL)(R : CARDINAL) =
  struct
    type n = unit

    type l = L.n
    type r = R.n

    let l_n = cardinal L.n
    let r_n = R.n

    let n =
      if Lazy.is_val r_n then
        let n = l_n + cardinal r_n in
        lazy n
      else
        lazy (l_n + cardinal r_n)

    let inj_l x = x
    let inj_r y = l_n + y
    let prj x = if x < l_n then L x else R (x - l_n)
  end

  let sum (type l r)
      (l : l cardinal)
      (r : r cardinal) =
    let module L = struct type n = l let n = l end in
    let module R = struct type n = r let n = r end in
    (module Sum(L)(R) : SUM with type l = l and type r = r)

  (** Manipulate elements from a finite set *)
  module Index : sig
    type 'n t = 'n index
    val of_int : 'n cardinal -> int -> 'n index
    val to_int : 'n index -> int

    exception End_of_set
    val enumerate : 'n cardinal -> (unit -> 'n index)

    val iter : 'n cardinal -> ('n index -> unit) -> unit
  end = struct
    type 'n t = 'n index

    let of_int (c : _ cardinal) i =
      let lazy c = c in
      assert (i >= 0 && i < c); i

    let to_int i = i

    exception End_of_set

    let enumerate (lazy c : 'n cardinal) =
      let k = ref 0 in
      (fun () ->
         let result = !k in
         if result >= c then raise End_of_set;
         incr k;
         result)

    let iter (c : 'n cardinal) f =
      let lazy c = c in
      for i = 0 to c - 1 do
        f i
      done
  end

  (** Manipulate fixed-size vectors, whose domain is a type-level [set] *)
  type ('n, 'a) vector = 'a array

  module Vector : sig
    type ('n, 'a) t = ('n, 'a) vector

    val get : ('n, 'a) t -> 'n index -> 'a
    val set : ('n, 'a) t -> 'n index -> 'a -> unit
    val set_cons : ('n, 'a list) t -> 'n index -> 'a -> unit

    val length : ('n, 'a) t -> 'n cardinal
    val empty : (Empty.n, _) t

    val make : 'n cardinal -> 'a -> ('n, 'a) t
    val make' : 'n cardinal -> (unit -> 'a) -> ('n, 'a) t
    val init : 'n cardinal -> ('n index -> 'a) -> ('n, 'a) t
    val map : ('a -> 'b) -> ('n, 'a) t -> ('n, 'b) t
  end = struct
    type ('n, 'a) t = ('n, 'a) vector

    (* Modular abstraction should guarantee that get and set calls are always
       safe. *)
    let get = Array.unsafe_get
    let set = Array.unsafe_set
    let set_cons t i x = set t i (x :: get t i)

    let length vec = let c = Array.length vec in lazy c

    let empty = [||]

    let make (n : _ cardinal) v =
      let lazy n = n in
      Array.make n v

    let make' (n : _ cardinal) f =
      let lazy n = n in
      if n = 0 then empty else Array.make n (f ())

    let init (n : _ cardinal) f = let lazy n = n in Array.init n f
    let map = Array.map
  end

  (** Syntactic sugar to manipulate finite vectors *)

  module Infix : sig

    (** [v.%(i)] is [Vector.get v i] *)
    val (.%())   : ('n, 'a) vector -> 'n index -> 'a

    (** [v.%(i) <- x] is [Vector.set v i x] *)
    val (.%()<-) : ('n, 'a) vector -> 'n index -> 'a -> unit

    (** A shortcut for consing an element in a vector of list.
        [v.%::(i) <- x] cons [x] to the list at index [i] in [v] *)
    val (.%::()<-) : ('n, 'a list) vector -> 'n index -> 'a -> unit
  end = struct
    let (.%()) = Array.unsafe_get
    let (.%()<-) = Array.unsafe_set
    let (.%::()<-) vec i x = vec.%(i) <- x :: vec.%(i)
  end

end
