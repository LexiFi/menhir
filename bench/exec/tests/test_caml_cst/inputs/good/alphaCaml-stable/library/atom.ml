open Signatures

(* An implementation of identifiers as strings.

   The subset of ``base'' identifiers is the set of all strings that
   do not end with ("__" [ '0'-'9' ]+). So, [basename] chops off the
   longest (repeated) suffix of this form, while [combine] appends
   "__", followed with a representation of the integer salt grain, to
   the basename. *)

module String = struct

  type t = string

  let compare = String.compare

  let mirror s =
    let n = String.length s in
    let t = String.create n in
    for i = 0 to n-1 do
      t.[i] <- s.[n-1-i]
    done;
    t

  let basename s =
    mirror (Chop.chop (Lexing.from_string (mirror s)))

  let combine s i =
    if i = 0 then
      s
    else
      s ^ "__" ^ (string_of_int i)

  module Map = Map.Make(String)

end

(* An implementation of atoms. *)

module Make (Identifier : Identifier) = struct

  type identifier = Identifier.t

  exception UnboundIdentifier of identifier

  type 'a identifier_map =
      'a Identifier.Map.t

  let find id m =
    try
      Identifier.Map.find id m
    with Not_found ->
      raise (UnboundIdentifier id)

  module Atom = struct

    (* An atom is implemented as a pair of an integer, the atom's
       identity, and an identifier, the atom's basename.

       We make identifiers part of atoms, instead of storing them in a
       separate table, so as to avoid the need for collecting garbage in
       such a table. *)

    type t = {
	identity: int;
	basename: Identifier.t
      }

    (* An integer counter holds the next available atom. *)

    let counter =
      ref 0

    let allocate () =
      let number = !counter in
      counter := number + 1;
      assert (number >= 0);
      number

    (* [freshb identifier] returns a fresh atom with basename [basename
       identifier]. *)

    let freshb identifier = {
      identity = allocate();
      basename = Identifier.basename identifier
    }

    (* [mfreshb identifiers m] extends the map [m] with a mapping from
       each identifier in the set [identifiers] to a fresh atom
       allocated via [freshb]. The set [identifiers] is represented as
       a map of identifiers to unit values. *)

    let mfreshb identifiers m =
      Identifier.Map.fold (fun identifier () m ->
	Identifier.Map.add identifier (freshb identifier) m
      ) identifiers m

    (* [fresha a] returns a fresh atom whose basename is that of the
       atom [a]. *)

    let fresha a = {
      identity = allocate();
      basename = a.basename
    }

    (* Comparison over atoms. *)

    let equal a b =
      a.identity = b.identity

    let compare a b =
      a.identity - b.identity

    let hash a =
      Hashtbl.hash a.identity

    (* [identity a] maps every atom to a distinct integer. This number
       can be viewed as the atom's identity and can be turned into a
       unique name for printing purposes, if desired. *)

    let identity a =
      a.identity

    let index =
      identity

    (* An atom can be queried about its base identifier. *)

    let basename a =
      a.basename

    exception Unknown of t

  end

  open Atom

  (* Maps and sets over atoms are derived out of Patricia trees, which
     implement efficient maps and sets over integers. *)

  module I = IndexMap.Make (Atom) (Patricia.Big)

  module AtomSet = I.Set

  module AtomMap = I.Map

  module AtomIdMap = struct

    type key = Atom.t

    (* A map of atoms to identifiers is implemented as a map of atoms
       to identifiers, together with a map of basenames to so-called
       salt grains (integer counters). *)

    type t = {
	mapping: Identifier.t AtomMap.t;
	salt: int Identifier.Map.t
      }

    (* The empty map. *)

    let empty = {
      mapping = AtomMap.empty;
      salt = Identifier.Map.empty
    }

    (* The map [add a m] extends [m] by mapping [a] to a unique
       identifier. [a]'s basename is used in the computation of
       this identifier. *)

    let add a m =
      let basename = a.basename in
      let salt = m.salt in
      let grain =
	try
	  Identifier.Map.find basename salt
	with Not_found ->
	  0
      in
      try
	{
	  mapping = AtomMap.strict_add a (Identifier.combine basename grain) m.mapping;
	  salt = Identifier.Map.add basename (grain + 1) salt
        }
      with AtomMap.Strict _ ->
	assert false (* [a] is already a member of the domain of [m]. *)

    (* [add_set s m] extends [m] with a mapping of [a] to a unique
       identifier for every atom [a] in the set [s]. *)

    let add_set s m =
      AtomSet.fold add s m

    (* [lookup a m] is the identifier associated with [a] in the
       map [m]. [Atom.Unknown] is raised if [a] is not a member of
       the domain of [m]. *)

    let lookup a m =
      try
	AtomMap.lookup a m.mapping
      with Not_found ->
	raise (Atom.Unknown a)

    let find =
      lookup

    let iter f m =
      AtomMap.iter f m.mapping

    let fold f m accu =
      AtomMap.fold f m.mapping accu

  end

  module Subst = struct

    type t = Atom.t AtomMap.t

    let id = AtomMap.empty
    let is_id = AtomMap.is_empty
    let singleton = AtomMap.singleton
    let add = AtomMap.add
    let union = AtomMap.union

    let lookup a subst =
      try
	AtomMap.lookup a subst
      with Not_found ->
	a

    let find =
      lookup

    let compose subst1 subst2 =

      (* If a mapping of [a] to [b] appears in [subst2], then a
	 mapping of [a] to [lookup b subst1] must appear in the
	 composition. This is the meaning of the second operand of
	 [union]. For atoms [a] that are not in the domain of
	 [subst2], we keep their image through [subst1]. This is the
	 meaning of the first operand. *)

      AtomMap.union subst1 (AtomMap.endo_map (fun b -> lookup b subst1) subst2)

    let freshen s subst =
      AtomSet.fold (fun a subst ->
	add a (Atom.fresha a) subst
      ) s subst

    (* If [a1] is in the domain of [subst1] and [a2] is in the domain
       of [subst2] and [subst1(a1)] equals [subst2(a2)], then
       [freshen2 a1 subst1 a2 subst2] returns [subst1] and [subst2]
       unchanged. If [a1] is not in the domain of [subst1] and [a2] is
       not in the domain of [subst2], then it returns [subst1] and
       [subst2] extended with new mappings of [x1] and [x2] to a
       single fresh atom [b]. Otherwise, it fails. *)

    let error =
      Invalid_argument "freshen2"

    let freshen2 x1 subst1 x2 subst2 =
      (* Generate a fresh atom, using the left-hand basename (arbitrary). *)
      let y = Atom.fresha x1 in
      match AtomMap.add_or_lookup x1 y subst1 with
      | AtomMap.Added subst1 ->
	  begin try
	    let subst2 = AtomMap.strict_add x2 y subst2 in
	    (* Atom in neither domain. *)
	    subst1, subst2
	  with AtomMap.Strict _ ->
	    (* Atom on right-hand side only. *)
	    raise error
	  end
      | AtomMap.LookedUp y1 ->
	  begin try
	    let y2 = AtomMap.lookup x2 subst2 in
	    if Atom.equal y1 y2 then
	      (* Atom in both domains; agreement on its image. *)
	      subst1, subst2
	    else
	      (* Atom in both domains; disagreement. *)
	      raise error  
	  with Not_found ->
  	    (* Atom on left-hand side only. *)
	    raise error
	  end

  end

end

