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

module Make (Identifier : Identifier) (Sorts : sig val n: int end) = struct

  module Atom = struct

    (* An atom is implemented as a pair of an integer, the atom's
       identity, and an identifier, the atom's basename.

       We make identifiers part of atoms, instead of storing them in a
       separate table, so as to avoid the need for collecting garbage in
       such a table.

       An atom's sort is encoded as part of its identity (see below). *)

    type atom = {
	identity: int;
	basename: Identifier.t
      }

    type t =
	atom

    type sort =
	int

    type identifier =
	Identifier.t

    (* An array of integer counters holds the next available atom at
       each sort. The sort of each atom is encoded in its integer
       code, so that the sort of an atom can be dynamically
       determined. *)

    let counters =
      assert (0 <= Sorts.n);
      Array.init Sorts.n (fun i -> i)

    let allocate sort =
      assert (0 <= sort && sort < Sorts.n);
      let number = counters.(sort) in
      counters.(sort) <- number + Sorts.n;
      assert (number >= 0);
      number

    (* [sortof a] is the sort of the atom [a]. *)

    let sortof a =
      a.identity mod Sorts.n

    (* [freshb (sort, identifier)] returns a fresh atom of sort [sort]
       with basename [basename identifier]. *)

    let freshb (sort, identifier) = {
      identity = allocate sort;
      basename = Identifier.basename identifier
    }

    (* [fresha a] returns a fresh atom whose sort and basename are
       those of the atom [a]. *)

    let fresha a = {
      identity = allocate (sortof a);
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

  end

  open Atom

  (* Maps and sets over atoms are derived out of Patricia trees, which
     implement efficient maps and sets over integers. *)

  module I = IndexMap.Make (Atom) (Patricia.Big)

  module AtomSet = struct
    include I.Set
    type atom =
	element
    type atoms =
	t
    let insert s a =
      add a s
  end

  module AtomMap = struct
    include I.Map
    type atom =
	key
    type atoms =
	AtomSet.atoms
    type 'a map =
	'a t
  end

  (* This module implements maps whose keys are pairs of a sort and an
     identifier. *)

  module SortIdentifierMap = Map.Make (struct

    type t =
	Atom.sort * Identifier.t

    let compare (sort1, id1) (sort2, id2) =
      let c = sort1 - sort2 in
      if c = 0 then
	Identifier.compare id1 id2
      else
	c

  end)

  module Import = struct

    type identifier =
	Identifier.t

    type atom =
	Atom.atom

    type sort =
	Atom.sort

    type env =
	atom SortIdentifierMap.t

    let empty =
      SortIdentifierMap.empty

    let find sid env =
      SortIdentifierMap.find sid env

    let freshen env sid =
      try
	let _ = SortIdentifierMap.find sid env in
	env
      with Not_found ->
	SortIdentifierMap.add sid (Atom.freshb sid) env

    let union env1 env2 =
      SortIdentifierMap.fold SortIdentifierMap.add env2 env1

  end

  (* This module implements maps whose keys are atoms, viewed as pairs
     of a sort and a basename. Identities are ignored. *)

  module AtomAsSortIdentifierMap = Map.Make (struct

    type t =
	Atom.t

    let compare a1 a2 =
      let c = sortof a1 - sortof a2 in
      if c = 0 then
	Identifier.compare a1.basename a2.basename
      else
	c

  end)

  module Export = struct

    type atom =
	Atom.t

    type key =
	atom

    type atoms =
	AtomSet.t

    type identifier =
	Identifier.t

    (* A map of atoms to identifiers is implemented as a map of atoms
       to identifiers, together with a map of atoms, viewed as (sort,
       basename) pairs, to so-called salt grains (integer counters).
       In other words, all atoms that have the same sort and basename
       share a salt grain. *)

    type env = {
	mapping: Identifier.t AtomMap.t;
	salt: int AtomAsSortIdentifierMap.t
      }

    type t =
	env

    (* The empty map. *)

    let empty = {
      mapping = AtomMap.empty;
      salt = AtomAsSortIdentifierMap.empty
    }

    (* The map [add a m] extends [m] by mapping [a] to a unique
       identifier. [a]'s basename is used in the computation of
       this identifier. *)

    let add a m =
      let salt = m.salt in
      let grain =
	try
	  AtomAsSortIdentifierMap.find a salt
	with Not_found ->
	  0
      in
      try
	{
	  mapping = AtomMap.strict_add a (Identifier.combine a.basename grain) m.mapping;
	  salt = AtomAsSortIdentifierMap.add a (grain + 1) salt
        }
      with AtomMap.Strict _ ->
	assert false (* [a] is already a member of the domain of [m]. *)

    (* [add_set s m] extends [m] with a mapping of [a] to a unique
       identifier for every atom [a] in the set [s]. *)

    let add_set s m =
      AtomSet.fold add s m

    (* [lookup a m] is the identifier associated with [a] in the
       map [m]. [TEMPORARY] is raised if [a] is not a member of
       the domain of [m]. *)

    let lookup a m =
      try
	AtomMap.lookup a m.mapping
      with Not_found ->
	raise Not_found (* TEMPORARY *)

    let find =
      lookup

    let iter f m =
      AtomMap.iter f m.mapping

    let fold f m accu =
      AtomMap.fold f m.mapping accu

  end

  module Subst = struct

    type atom = Atom.t
    type subst = Atom.t AtomMap.t

    let id = AtomMap.empty
    let is_id = AtomMap.is_empty
    let singleton = AtomMap.singleton
    let add = AtomMap.add
    let remove = AtomMap.remove
    let restrict subst a = remove a subst
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

    let freshen subst a =
      add a (Atom.fresha a) subst

    open Asynchrony

    let freshen2 (subst1, subst2) x1 x2 =
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
	    raise asynchrony
	  end
      | AtomMap.LookedUp y1 ->
	  begin try
	    let y2 = AtomMap.lookup x2 subst2 in
	    if Atom.equal y1 y2 then
	      (* Atom in both domains; agreement on its image. *)
	      subst1, subst2
	    else
	      (* Atom in both domains; disagreement. *)
	      raise asynchrony
	  with Not_found ->
  	    (* Atom on left-hand side only. *)
	    raise asynchrony
	  end

  end

  module Abstraction = struct

    type atom =
	Atom.atom

    type subst =
	Subst.subst

    open Subst

    (* This type definition documents the operations which the pattern
       type ['a] must provide in order support opaque abstractions. *)

    type 'a abstractable = {

	(* [bv] folds over the bound atoms of its argument, a pattern of
	   type ['a]. *)

	bv: 'accu . ('accu, atom) fold -> ('accu, 'a) fold;

	(* [bv2] folds synchronously over the bound atoms of its two
	   arguments, which must have the same structure, otherwise
	   [Asynchrony] is raised. *)

	bv2: 'accu . ('accu, atom) fold2 -> ('accu, 'a) fold2;

	(* [apply] accepts two substitutions, applies one in outer scope and
	   the other within the pattern proper and in inner scope. *)

	apply: subst -> subst -> 'a map;

      }

    (* An abstraction consists of a value of type ['a], whose bound atoms are
       considered mute, together with a delayed substitution. *)

    type 'a abs = {
	mutable delayed: subst;
	mutable content: 'a;
      }

    (* [pack] packs up an abstraction. There is no delayed substitution. *)

    let pack (content : 'a) : 'a abs =
      {
	delayed = id;
	content = content;
      }

    (* [apply] applies a substitution to an abstraction. The substitution is
       delayed, that is, composed with the existing delayed substitution. *)

    let apply (subst : subst) (abs : 'a abs) : 'a abs =
      {
	abs with delayed = compose subst abs.delayed
      }

    (* [unpack] unpacks an abstraction, freshening its bound atoms.

       The unmodified delayed substitution is applied in outer
       scope. The delayed substitution, overridden with a freshening
       substitution of the bound atoms, is applied to the bound atoms
       and in inner scope.

       If the delayed substitution was nontrivial, the new content is
       memoized. Otherwise, memoizing is not useful -- on the contrary,
       it would lead to unnecessary GC activity. *)

    let unpack (d : 'a abstractable) (abs : 'a abs) : 'a =
      let delayed = abs.delayed
      and content = abs.content in
      let subst = d.bv freshen id content in
      let subst = union delayed subst in
      let content = d.apply delayed subst content in
      if not (is_id delayed) then begin
	abs.delayed <- id;
	abs.content <- content
      end;
      content

    (* [expose] unpacks an abstraction, possibly without freshening
       its bound atoms.

       Two cases arise. If there is no delayed substitution, then
       the abstraction is opened without freshening. If there is
       a delayed substitution, then we use [unpack], which performs
       freshening, because it is impossible in general to apply a
       substitution without freshening the bound atoms. One could
       wish to check whether the bound atoms appear in the image
       of the delayed substitution (after restricting its domain
       to not include the bound atoms). If they do not, then no
       freshening is required. But that might be a costly check,
       so it is not performed here.

       Note that, if there is a delayed substitution, [unpack]
       applies it, so the delayed substitution disappears, and
       the next call to [expose] will expose the bound atoms
       without freshening. *)

    let expose (d : 'a abstractable) (abs : 'a abs) : 'a =
      if is_id abs.delayed then
	abs.content
      else
	unpack d abs

    (* [unpack2] unpacks two abstractions at once, while making sure
       that the bound atoms on both sides coincide and are freshened
       synchronously. *)

    let unpack2 (d : 'a abstractable) (abs1 : 'a abs) (abs2 : 'a abs) : 'a * 'a =

      let delayed1 = abs1.delayed
      and content1 = abs1.content in
      let delayed2 = abs2.delayed
      and content2 = abs2.content in
      let subst1, subst2 = d.bv2 freshen2 (id, id) content1 content2 in
      let subst1 = union delayed1 subst1
      and subst2 = union delayed2 subst2 in
      let content1 = d.apply delayed1 subst1 content1
      and content2 = d.apply delayed2 subst2 content2 in
      if not (is_id delayed1) then begin
	abs1.delayed <- id;
	abs1.content <- content1
      end;
      if not (is_id delayed2) then begin
	abs2.delayed <- id;
	abs2.content <- content2
      end;
      content1, content2

    (* TEMPORARY one might think about adding to each abstraction a Boolean flag that tells whether its
       content is closed. The flag would be set on calling [fv], and would prevent substitutions
       from being delayed or applied -- they would just be killed. It would also speed up further
       calls to [fv]. *)

  end

end

