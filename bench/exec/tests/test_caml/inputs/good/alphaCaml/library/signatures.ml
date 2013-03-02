(* This module defines a number of types and signatures that specify
   the interaction between [AlphaLib] and user programs. *)

(*i ----------------------------------------------------------------------- i*)

(* Let us begin with a few convenient type abbreviations. *)

type 'a eq =
    'a -> 'a -> bool

type 'a map =
    'a -> 'a

type ('accu, 'a) fold =
    'accu -> 'a -> 'accu

type ('accu, 'a) fold2 =
    'accu -> 'a -> 'a -> 'accu

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{Identifier} *)

(* This signature defines the operations that an implementation of
   identifiers must provide in order to appear in an \kw{identifier
   module} declaration.

   Identifiers are usually human-readable. In fact, the default
   implementation of identifiers, which is automatically supplied by
   \ac when no \kw{identifier module} declaration is made, equates
   identifiers with strings.

   An implementation of atoms can be built on top of any implementation
   of identifiers via the internal functor [AlphaLib.Atom.Make].
   This functor application is automatically performed by \ac in
   order to produce each of the ``atom'' modules (one per sort).

   In order to implement this signature, one must isolate a strict
   subset of identifiers, which we refer to as ``base'' identifiers.
   There must exist a function, referred to as [basename], that maps
   arbitrary identifiers to base identifiers. This function does not
   have to (and usually cannot) be injective. (A function is injective
   when it maps distinct inputs to distinct outputs.) Nevertheless,
   the more information it preserves, the better. Conversely, there
   must exist an injective function, referred to as [combine], that
   maps a pair of a base identifier and an integer value to an
   identifier.

   Internally, every atom records the image through [basename] of the
   identifier that it originally stood for. This base identifier plays
   no role in determining the identity of the atom: that is, it is not
   used when comparing two atoms. It is kept around for use when the
   atom is converted back to an identifier. At this point, it is
   combined, via [combine], with a unique integer, in order to obtain
   suitably fresh identifier.

   If [basename] and [combine] are properly chosen, then the final
   identifier that is printed ``resembles'' the one that was
   originally found. More precisely, it is desirable that the
   next two laws be satisfied:
   \begin{center}
   [basename (combine identifier i) = identifier] \\
   [combine identifier 0 = identifier]
   \end{center}
   The first law states that the information added by combining an
   identifier with an integer [i] is exactly the information that
   is lost when applying [basename]. The second law states that
   combining the integer 0 with an identifier should have no effect.
   This is exploited to avoid needless renamings.

   The default implementation of identifiers as strings defines base
   identifiers as strings that do not end with two underscore
   characters and a number. A base identifier and an integer value are
   combined simply by appending [__], followed with a decimal
   representation of the latter, to the former. In practice, this
   means that the identifier [x] will be successively renamed into
   [x], [x__1], [x__2], [x__3], and so on. *)

module type Identifier = sig

  (* [t] is the type of identifiers. *)

  type t

  (* Identifiers must be comparable. As usual in Objective Caml,
     [compare id1 id2] must return a negative integer if [id1] is less
     than [id2], a positive integer if [id2] is less than [id1], and
     zero otherwise. *)

  val compare: t -> t -> int

  (* [basename] and [combine] are described above. *)

  val basename: t -> t
  val combine: t -> int -> t

  (* The sub-module [Map] provides maps whose keys are identifiers.
     It is usually produced by applying the standard library functor
     [Map.Make] to the type [t] and the function [compare] above.
     This sub-module is used by the functions that convert back and
     forth between raw and internal forms, that is, between atoms
     and identifiers. *)

  module Map : Map.S with type key = t

end

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{Atom} *)

(* Atoms are abstract entities that support two operations,
   namely creation of fresh atoms and comparison of two atoms.

   Each atom carries a unique integer, which can be viewed as its
   identity. This integer is used in comparisons. A global integer
   counter is maintained and incremented when fresh atoms are
   created.

   Every atom also carries a base name, that is, an identifier. This
   identifier is not in general unique, and is not part of the
   atom's identity. It is used when converting atoms back to
   identifiers: see, for instance, [AtomIdMap.add].

   When a fresh atom is created, its base name is taken either from
   an existing identifier or from an existing atom. Two functions,
   [freshb] and [fresha], are provided for this purpose. *)

module type Atom = sig

  (* Atoms. *)

  type atom

  type t =
      atom

  (* Identifiers. *)

  type identifier


  (* Sorts. *)

  type sort

  (* The call [freshb (sort, identifier)] produces a fresh atom of sort
     [sort] whose base name is [basename identifier]. *)

  val freshb: sort * identifier -> atom

  (* The call [fresha a] produces a fresh atom whose base name and
     sort are those of [a]. *)

  val fresha: atom -> atom

  (* [sortof a] is the sort of the atom [a]. *)

  val sortof: atom -> sort

  (* Atoms can be tested for equality, for ordering, and hashed.
     The user is warned against careless use of [compare] and
     [hash]. Atoms are renamed during \acon, which affects their
     relative ordering and their hash code. It is fine to use
     these operations as long as one guarantees that no renaming
     takes place. *)

  val equal: atom -> atom -> bool
  val compare: atom -> atom -> int
  val hash: atom -> int

  (* It is possible to retrieve an atom's identity and base name.
     There is in general no good reason of doing so, except for
     debugging purposes. *)

  val identity: atom -> int
  val basename: atom -> identifier

end

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{AtomSet} *)

(* Finite sets of atoms. *)

module type AtomSet = sig

  (* Atoms. *)

  type atom

  (* Sets of atoms. *)

  type atoms

  type t =
      atoms

  (* [empty] is the empty set. *)

  val empty: atoms

  (* [singleton a] is the singleton set [{a}]. *)

  val singleton: atom -> atoms

  (* [add x s] is ([{x}]\tcup[s]). *)

  val add: atom -> atoms -> atoms
  val insert: atoms -> atom -> atoms

  (* [remove x s] is ([s]\tminus[{x}]). *)

  val remove: atom -> atoms -> atoms

  (* [union s1 s2] is ([s1]\tcup[s2]). *)

  val union: atoms -> atoms -> atoms

  (* [inter s1 s2] is ([s1]\tcap[s2]). *)

  val inter: atoms -> atoms -> atoms

  (* [diff s1 s2] is ([s1]\tminus[s2]). *)

  val diff: atoms -> atoms -> atoms

  (* [mem a s] is [true] if and only if [a] is a member of [s]. *)

  val mem: atom -> atoms -> bool

  (* [is_empty s] is [true] if and only if [s] is the empty set. *)

  val is_empty: atoms -> bool

  (* [disjoint s1 s2] is [true] if and only if the sets [s1] and
     [s2] are disjoint, that is, if and only if their intersection
     is empty. *)

  val disjoint: atoms -> atoms -> bool

  (* [equal s1 s2] is [true] if and only if [s1] and [s2] are
     extensionally equal, that is, if and only if they have the same
     members. *)

  val equal: atoms -> atoms -> bool

  (* [subset s1 s2] is [true] if and only if [s1] is a subset of
     [s2], that is, if and only if every member of [s1] is also a
     member of [s2]. *)

  val subset: (atoms -> atoms -> bool)

  (* The call [iter f s] has the effect of applying the function
     [f] in turn to every member of [s]. *)

  val iter: (atom -> unit) -> atoms -> unit

  (* The call [fold f s accu] has the effect of applying the
     function [f] in turn to every member of [s] and to an
     accumulator whose value, threaded through the calls, is
     initially [accu]. Its result is the final value of the
     accumulator. *)

  val fold: (atom -> 'a -> 'a) -> atoms -> 'a -> 'a

  (* [iterator s] returns a stateful iterator over the set [s]. That
     is, if $s = \{ x_1, x_2, \ldots, x_n \}$, where $x_1 < x_2 <
     \ldots < x_n$, then [iterator s] is a function which, when
     invoked for the $k^{\text{th}}$ time, returns [Some ]$x_k$, if
     $k\leq n$, and [None] otherwise. *)

  val iterator: atoms -> (unit -> atom option)

  (* [cardinal s] is the cardinal of the set [s]. *)

  val cardinal: atoms -> int

  (* [choose s] returns an arbitrarily chosen element of [s], if [s]
     is nonempty, and raises [Not_found] otherwise. *)

  val choose: atoms -> atom

  (* [compare] is an ordering over sets. *)

  val compare: atoms -> atoms -> int

  (* [print] is a default printer for sets of atoms. It is
     parameterized with a printer for atoms. It displays sets as
     comma-separated sequences of atoms. *)

  val print: (Buffer.t -> atom -> unit) -> (Buffer.t -> atoms -> unit)

end

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{AtomMap} *)

(* The sub-module [AtomMap] offers a representation of finite maps
   whose keys are atoms and whose data can have arbitrary type
   ['a]. *)

module type AtomMap = sig

  (* Atoms. *)

  type atom

  type key =
      atom

  (* Sets of atoms. *)

  type atoms

  (* ['a map] is the type of maps of atoms to data of type ['a]. *)

  type 'a map

  type 'a t =
      'a map

  (* [empty] is the empty map. *)

  val empty: 'a map

  (* [singleton a d] is the singleton map that maps atom [a] to
     datum [d]. *)

  val singleton: atom -> 'a -> 'a map

  (* [add a d m] is the map that maps atom [a] to datum [d] and
     elsewhere behaves like [m]. *)

  val add: atom -> 'a -> 'a map -> 'a map

  (* [strict_add a d m] raises [Strict a] if [a] is in the domain of
     [m] and otherwise returns [add a d m]. *)

  exception Strict of atom

  val strict_add: atom -> 'a -> 'a map -> 'a map

  (* [remove a m] is the map that has no binding at [a] and
     elsewhere behaves like [m]. *)

  val remove: atom -> 'a map -> 'a map

  (* [union m1 m2] is the map that behaves like [m2] where [m2]
     is defined and elsewhere behaves like [m1]. In other words,
     the bindings in [m2] take precedence over those in [m1]. *)

  val union: 'a map -> 'a map -> 'a map

  (* [lookup a m] returns the datum associated with atom [a] in
     the map [m], if defined, and raises the exception [Not_found]
     otherwise. [lookup] is also known as [find]. *)

  val lookup: atom -> 'a map -> 'a
  val find: atom -> 'a map -> 'a

  (* [mem a m] tells whether the atom [a] appears in the domain of
     the map [m]. *)

  val mem: atom -> 'a map -> bool

  (* [is_empty m] is [true] if and only if [m] is the empty map. *)

  val is_empty: 'a map -> bool

  (* [map f m] is the map obtained by composing the function [f]
     with the map [m], that is, the map that maps an atom [a] to
     [(f d)] when [m] maps [a] to [d]. *)

  val map: ('a -> 'b) -> 'a map -> 'b map

  (* [mapi f m] is the map that maps an atom [a] to [(f a d)] when
     [m] maps [a] to [d]. *)

  val mapi: (atom -> 'a -> 'b) -> 'a map -> 'b map

  (* [iter f m] applies [f] in turn to each binding in the map [m]. *)

  val iter : (atom -> 'a -> unit) -> 'a map -> unit

  (* [fold f m accu] applies [f] in turn to each binding in the map
     [m], threading an accumulator through the sequence of calls. *)

  val fold : (atom -> 'a -> 'b -> 'b) -> 'a map -> 'b -> 'b

  (* [iterator m] returns a stateful iterator over the map [m]. That
     is, if $m = \{ x_1 \mapsto d_1, x_2 \mapsto d_2, \ldots, x_n
     \mapsto d_n \}$, where $x_1 < x_2 < \ldots < x_n$, then [iterator
     s] is a function which, when invoked for the $k^{\text{th}}$
     time, returns [Some ]$(x_k, d_k)$, if $k\leq n$, and [None]
     otherwise. *)

  val iterator: 'a map -> (unit -> (atom * 'a) option)

  (* [cardinal m] returns [m]'s cardinal, that is, the number of keys
     it binds, or, in other words, the cardinal of its domain. *)

  val cardinal: 'a map -> int

  (* [choose m] returns an arbitrarily chosen binding in [m], if [m]
     is nonempty, and raises [Not_found] otherwise. *)

  val choose: 'a map -> atom * 'a

  (* [domain m] is the domain of the map [m]. *)

  val domain: 'a map -> atoms

  (* [print] is a default printer for maps. It is parameterized with
     a printer for atoms and a printer for data. It displays maps as
     newline-terminated sequences of bindings. It displays bindings
     as a pair of an atom and a datum, separated with an arrow. *)

  val print: (Buffer.t -> atom -> unit) -> (Buffer.t -> 'a -> unit) -> (Buffer.t -> 'a map -> unit)

end

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{Subst} *)

(* Substitutions are are total mappings of atoms to atoms that behave
   as the identity outside of a finite set of atoms, known as their
   domain. *)

module type Subst = sig

  (* [atom] is a type of atoms. *)

  type atom

  (* [subst] is a type of substitutions. *)

  type subst

  (* [id] is the identity substitution. *)

  val id: subst

  (* [is_id subst] is [true] if and only if [subst] is the identity
     substitution. *)

  val is_id: subst -> bool

  (* [singleton a b] is the singleton substitution that maps atom
     [a] to atom [b]. *)

  val singleton: atom -> atom -> subst

  (* [add a b subst] is the substitution that maps atom [a] to atom
     [b] and elsewhere behaves like [subst]. *)

  val add: atom -> atom -> subst -> subst

  (* [lookup a subst] is the image of [a] through [subst]. It is
     never undefined, since substitutions are viewed as total
     mappings. [lookup] is also known as [find]. *)

  val lookup: atom -> subst -> atom
  val find: atom -> subst -> atom

  (* [remove a subst] and [restrict subst a] are [subst] deprived
     of any mapping for [a]. *)

  val remove: atom -> subst -> subst
  val restrict: subst -> atom -> subst

  (* [union subst1 subst2] is the substitution that behaves like
     [subst2] on its domain and elsewhere behaves like [subst1]. In
     other words, the bindings in [subst2] take precedence over
     those in [subst1]. *)

  val union: subst -> subst -> subst

  (* [compose subst1 subst2] is the composition of [subst1] with
     [subst2], that is, the substitution that maps every atom [a] to
     [subst1 (subst2 (a))]. *)

  val compose: subst -> subst -> subst

  (* [freshen subst a] is either [subst], if [a] is in the domain of
     [subst], or a substitution that extends [subst] with a mapping of
     [a] to a fresh atom. *)

  val freshen: subst -> atom -> subst

  (* If [a1] is in the domain of [subst1] and [a2] is in the domain of
     [subst2] and [subst1(a1)] equals [subst2(a2)], then [freshen2
     (subst1, subst2) a1 a2] returns [(subst1, subst2)] unchanged. If
     [a1] is not in the domain of [subst1] and [a2] is not in the
     domain of [subst2], then it returns [subst1] and [subst2]
     extended with new mappings of [x1] and [x2] to a single fresh
     atom [b]. Otherwise, it fails and raises [Asynchrony]. *)

  val freshen2: subst * subst -> atom -> atom -> subst * subst

end

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{Import} *)

(* Import environments are finite mappings of identifiers to atoms. *)

module type Import = sig

  (* Identifiers. *)

  type identifier

  (* Atoms. *)

  type atom

  (* Sorts. *)

  type sort

  (* Environments. *)

  type env

  (* [empty] is the environment whose domain is empty. *)

  val empty: env

  (* [find sid env] is the image of the sorted identifier [sid] in the
     environment [env]. If [sid] is not in the domain of [env],
     [Not_found] is raised. *)

  val find: sort * identifier -> env -> atom

  (* [freshen env sid] is either [env], if [sid] is in the domain of
     [env], or an environment that extends [env] with a mapping of
     [sid] to a fresh atom of appropriate sort. *)

  val freshen: env -> sort * identifier -> env

  (* [union env1 env2] is the environment that behaves like [env2] on
     its domain and elsewhere behaves like [env1]. In other words, the
     bindings in [env2] take precedence over those in [env1]. *)

  val union: env -> env -> env

end

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{Export} *)

(* An export environment is a finite map of atoms to distinct
   identifiers: that is, every atom is mapped to a distinct
   identifier. This invariant is enforced by having the library pick a
   unique identifier when a new atom is added to the domain of the
   map. That is, the client does not control which identifiers are
   picked. *)

module type Export = sig

  (* Atoms. *)

  type atom

  type key =
      atom

  (* Sets of atoms. *)

  type atoms

  (* Identifiers. *)

  type identifier

  (* Environments. *)

  type env

  type t =
      env

  (* [empty] is the empty environment. *)

  val empty: env

  (* [add a env] maps the atom [a] to a unique identifier (that is, an
     identifier not in the codomain of [env]) and elsewhere behaves
     like [env]. The base name of [a] is used when picking this
     identifier. The atom [a] must not be a member of the domain of
     [env]. *)

  val add: atom -> env -> env

  (* [add_set s env] is the environment obtained by successively
     [add]ing every member of the atom set [s] to the environment
     [env]. *)

  val add_set: atoms -> env -> env

  (* [lookup a env] returns the identifier associated with the atom
     [a] in the environment [env], if defined, and raises the
     exception [TEMPORARY] otherwise. [lookup] is also known as
     [find]. *)

  val lookup: atom -> env -> identifier
  val find: atom -> env -> identifier

  (* [iter f env] applies [f] in turn to each binding in the
     environment [env]. *)

  val iter : (atom -> identifier -> unit) -> env -> unit

  (* [fold f env accu] applies [f] in turn to each binding in the
     environment [env], threading an accumulator through the sequence
     of calls. *)

  val fold : (atom -> identifier -> 'b -> 'b) -> env -> 'b -> 'b

end

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{Abstraction} *)

(* This signature specifies the operations that an implementation of
   abstractions should provide. *)

module type Abstraction = sig

  (* Atoms. *)

  type atom

  (* A type of substitutions of atoms for atoms. *)

  type subst

  (* A parameterized type of abstractions. A value of type ['a abs]
     represents an abstraction whose body has type ['a], that is, a
     value of type ['a] in which some atoms are considered bound. *)

  type 'a abs

  (* A parameterized type of evidence. A value of type ['a
     abstractable] represents evidence that values of type ['a] are
     suitable for use as the body of an abstraction.

     It is in fact a dictionary containing methods for applying
     substitutions to values of type ['a] and computing the set of
     their bound names. *)

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

  (* [pack] packs up an abstraction. *)

  val pack: 'a -> 'a abs

  (* [apply] applies a substitution to an abstraction. *)

  val apply: subst -> 'a abs -> 'a abs

  (* [unpack] unpacks an abstraction, freshening its bound atoms. *)

  val unpack: 'a abstractable -> 'a abs -> 'a

  (* [expose] unpacks an abstraction, possibly without freshening
     its bound atoms. *)

  val expose: 'a abstractable -> 'a abs -> 'a

  (* [unpack2] unpacks two abstractions at once, while making sure
     that the bound atoms on both sides coincide and are freshened
     synchronously. It raises [Asynchrony] if the two abstractions
     have mismatching structures. *)

  val unpack2: 'a abstractable -> 'a abs -> 'a abs -> 'a * 'a

end

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{Toolbox} *)

(* This signature specifies the operations that every ``atom'' module
   provides. These operations are grouped into several sub-modules
   that provide:
   \begin{itemize}
   \item basic operations on atoms;
   \item sets of atoms;
   \item maps of atoms to arbitrary data;
   \item import environments;
   \item export environments;
   \item substitutions of atoms for atoms;
   \item abstractions over atoms.
   \end{itemize}
*)

(*i ----------------------------------------------------------------------- i*)
(* \ocwbar *)

module type Toolbox = sig

  module Atom : Atom

  module AtomSet : AtomSet with type atom = Atom.atom

  module AtomMap : AtomMap with type atom = Atom.atom
                            and type atoms = AtomSet.atoms

  module Import : Import with type identifier = Atom.identifier
                          and type atom = Atom.atom
                          and type sort = Atom.sort

  module Export : Export with type atom = Atom.atom
                          and type atoms = AtomSet.atoms
			  and type identifier = Atom.identifier

  module Subst : Subst with type atom = Atom.atom

  module Abstraction : Abstraction with type atom = Atom.atom
                                    and type subst = Subst.subst

end

(*i ----------------------------------------------------------------------- i*)
(* \ocwmoduletype{Container} *)

module type Container = sig

  type 'a t

  val map: ('a -> 'b) -> 'a t -> 'b t
  val self_map: ('a -> 'a) -> 'a t -> 'a t
  val fold: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  val fold2: ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a

end

