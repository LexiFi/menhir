(* This module allows mapping ``things'' to atoms. The mapping is
   built on demand and is accessible in both directions. *)

open Print

module Make (T : sig

  (* The type of things. *)

  type thing

  (* Maps over things. *)

  module Map : sig
    type key = thing
    type 'a t
    val empty: 'a t
    val add: key -> 'a -> 'a t -> 'a t
    val mem: key -> 'a t -> bool
    val find: key -> 'a t -> 'a
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  end

  (* How to print things. This is exploited in determining the
     atoms' basenames. *)

  val print: thing printer

end) (A : AlphaLib.Signatures.Atom with type identifier = string)
= struct

  open T
  open A

  let direct : Atom.t Map.t ref =
    ref Map.empty

  let reverse : thing AtomMap.t ref =
    ref AtomMap.empty

  let image thing =
    try
      Map.find thing !direct
    with Not_found ->
      let basename = Print.ws (fun b -> print b thing) in
      let atom = Atom.freshb basename in
      direct := Map.add thing atom !direct;
      reverse := AtomMap.add atom thing !reverse;
      atom

  let preimage atom =
    try
      AtomMap.find atom !reverse
    with Not_found ->
      assert false

  let known thing =
    Map.mem thing !direct

  let print b atom =
    print b (preimage atom)

  let fold f accu =
    Map.fold f !direct accu

end
