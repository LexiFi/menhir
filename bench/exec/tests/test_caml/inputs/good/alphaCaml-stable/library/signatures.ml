(* This module defines a couple of signatures that specify the
   interaction between [alphaLib] and user programs. *)

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

(* This signature specifies the operations that every ``atom'' module
   provides. These operations are grouped into several sub-modules
   that provide:
   \begin{itemize}
   \item basic operations on atoms;
   \item sets of atoms;
   \item maps of atoms to arbitrary data;
   \item maps of atoms to identifiers;
   \item substitutions of atoms for atoms.
   \end{itemize}
*)

(*i ----------------------------------------------------------------------- i*)
(* \ocwbar *)

module type Atom = sig

  (* The type [identifier] is the type of the identifiers on top of
     which this implementation of atoms is built. *)

  type identifier

  (* This exception is raised by the (automatically generated) functions
     that convert raw forms into internal forms when an unbound identifier
     is encountered. *)

  exception UnboundIdentifier of identifier

  (* This is a version of [Identifier.Map.find] that raises
     [UnboundIdentifier] when it fails. *)

  type 'a identifier_map
  val find: identifier -> 'a identifier_map -> 'a

  (* The sub-module [Atom] offers an abstract type of atoms. Atoms are
     abstract entities that support two (classes of) operations,
     namely creation of fresh atoms and comparison of two atoms.

     Every atom carries a unique integer, which can be viewed as its
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

  module Atom : sig

    (* [t] is the type of atoms. *)

    type t

    (* The call [freshb identifier] produces a fresh atom whose base
       name is that of [identifier]. *)

    val freshb: identifier -> t

    (* [mfreshb] is undocumented. *)

    val mfreshb: unit identifier_map -> t identifier_map -> t identifier_map

    (* The call [fresha a] produces a fresh atom whose base name is
       that of [a]. *)

    val fresha: t -> t

    (* Atoms can be tested for equality, for ordering, and hashed.
       The user is warned against careless use of [compare] and
       [hash]. Atoms are renamed during \acon, which affects their
       relative ordering and their hash code. It is fine to use
       these operations as long as one guarantees that no renaming
       takes place. *)

    val equal: t -> t -> bool
    val compare: t -> t -> int
    val hash: t -> int

    (* It is possible to retrieve an atom's identity and base name.
       There is in general no good reason of doing so, except for
       debugging purposes. *)

    val identity: t -> int
    val basename: t -> identifier

    (* The exception [Unknown] is raised by [AtomIdMap.lookup]. *)

    exception Unknown of t

  end

  (* The sub-module [AtomSet] offers a representation of finite sets
     of atoms. *)

  module AtomSet : sig

    (* [t] is the type of sets of atoms. *)

    type t
    type element = Atom.t

    (* [empty] is the empty set. *)

    val empty: t

    (* [singleton a] is the singleton set [{a}]. *)

    val singleton: element -> t

    (* [add x s] is ([{x}]\tcup[s]). *)

    val add: element -> t -> t

    (* [remove x s] is ([s]\tminus[{x}]). *)

    val remove: element -> t -> t

    (* [union s1 s2] is ([s1]\tcup[s2]). *)

    val union: t -> t -> t

    (* [inter s1 s2] is ([s1]\tcap[s2]). *)

    val inter: t -> t -> t

    (* [diff s1 s2] is ([s1]\tminus[s2]). *)

    val diff: t -> t -> t

    (* [mem a s] is [true] if and only if [a] is a member of [s]. *)

    val mem: element -> t -> bool

    (* [is_empty s] is [true] if and only if [s] is the empty set. *)

    val is_empty: t -> bool

    (* [disjoint s1 s2] is [true] if and only if the sets [s1] and
       [s2] are disjoint, that is, if and only if their intersection
       is empty. *)

    val disjoint: t -> t -> bool

    (* [equal s1 s2] is [true] if and only if [s1] and [s2] are
       extensionally equal, that is, if and only if they have the same
       members. *)

    val equal: t -> t -> bool

    (* [subset s1 s2] is [true] if and only if [s1] is a subset of
       [s2], that is, if and only if every member of [s1] is also a
       member of [s2]. *)

    val subset: (t -> t -> bool)

    (* The call [iter f s] has the effect of applying the function
       [f] in turn to every member of [s]. *)

    val iter: (element -> unit) -> t -> unit

    (* The call [fold f s accu] has the effect of applying the
       function [f] in turn to every member of [s] and to an
       accumulator whose value, threaded through the calls, is
       initially [accu]. Its result is the final value of the
       accumulator. *)

    val fold: (element -> 'a -> 'a) -> t -> 'a -> 'a

    (* [iterator s] returns a stateful iterator over the set [s]. That
       is, if $s = \{ x_1, x_2, \ldots, x_n \}$, where $x_1 < x_2 <
       \ldots < x_n$, then [iterator s] is a function which, when
       invoked for the $k^{\text{th}}$ time, returns [Some ]$x_k$, if
       $k\leq n$, and [None] otherwise. *)

    val iterator: t -> (unit -> element option)

    (* [cardinal s] is the cardinal of the set [s]. *)

    val cardinal: t -> int

    (* [choose s] returns an arbitrarily chosen element of [s], if [s]
       is nonempty, and raises [Not_found] otherwise. *)

    val choose: t -> element

    (* [compare] is an ordering over sets. *)

    val compare: t -> t -> int

    (* [print] is a default printer for sets of atoms. It is
       parameterized with a printer for atoms. It displays sets as
       comma-separated sequences of atoms. *)

    val print: (Buffer.t -> element -> unit) -> (Buffer.t -> t -> unit)

  end

  (* The sub-module [AtomMap] offers a representation of finite maps
     whose keys are atoms and whose data can have arbitrary type
     ['a]. *)

  module AtomMap : sig

    (* [key] is the type of atoms. *)

    type key =
	Atom.t

    (* ['a t] is the type of maps of atoms to data of type ['a]. *)

    type 'a t

    (* [empty] is the empty map. *)

    val empty: 'a t

    (* [singleton a d] is the singleton map that maps atom [a] to
       datum [d]. *)

    val singleton: key -> 'a -> 'a t

    (* [add a d m] is the map that maps atom [a] to datum [d] and
       elsewhere behaves like [m]. *)

    val add: key -> 'a -> 'a t -> 'a t

    (* [strict_add a d m] raises [Strict a] if [a] is in the domain of
       [m] and otherwise returns [add a d m]. *)

    exception Strict of key

    val strict_add: key -> 'a -> 'a t -> 'a t

    (* [remove a m] is the map that has no binding at [a] and
       elsewhere behaves like [m]. *)

    val remove: key -> 'a t -> 'a t

    (* [union m1 m2] is the map that behaves like [m2] where [m2]
       is defined and elsewhere behaves like [m1]. In other words,
       the bindings in [m2] take precedence over those in [m1]. *)

    val union: 'a t -> 'a t -> 'a t

    (* [lookup a m] returns the datum associated with atom [a] in
       the map [m], if defined, and raises the exception [Not_found]
       otherwise. [lookup] is also known as [find]. *)

    val lookup: key -> 'a t -> 'a
    val find: key -> 'a t -> 'a

    (* [mem a m] tells whether the atom [a] appears in the domain of
       the map [m]. *)

    val mem: key -> 'a t -> bool

    (* [is_empty m] is [true] if and only if [m] is the empty map. *)

    val is_empty: 'a t -> bool

    (* [map f m] is the map obtained by composing the function [f]
       with the map [m], that is, the map that maps an atom [a] to
       [(f d)] when [m] maps [a] to [d]. *)

    val map: ('a -> 'b) -> 'a t -> 'b t

    (* [mapi f m] is the map that maps an atom [a] to [(f a d)] when
       [m] maps [a] to [d]. *)

    val mapi: (key -> 'a -> 'b) -> 'a t -> 'b t

    (* [iter f m] applies [f] in turn to each binding in the map [m]. *)

    val iter : (key -> 'a -> unit) -> 'a t -> unit

    (* [fold f m accu] applies [f] in turn to each binding in the map
       [m], threading an accumulator through the sequence of calls. *)

    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b

    (* [iterator m] returns a stateful iterator over the map [m]. That
       is, if $m = \{ x_1 \mapsto d_1, x_2 \mapsto d_2, \ldots, x_n
       \mapsto d_n \}$, where $x_1 < x_2 < \ldots < x_n$, then [iterator
       s] is a function which, when invoked for the $k^{\text{th}}$
       time, returns [Some ]$(x_k, d_k)$, if $k\leq n$, and [None]
       otherwise. *)

    val iterator: 'a t -> (unit -> (key * 'a) option)

    (* [cardinal m] returns [m]'s cardinal, that is, the number of keys
       it binds, or, in other words, the cardinal of its domain. *)

    val cardinal: 'a t -> int

    (* [choose m] returns an arbitrarily chosen binding in [m], if [m]
       is nonempty, and raises [Not_found] otherwise. *)

    val choose: 'a t -> key * 'a

    (* [domain m] is the domain of the map [m]. *)

    val domain: 'a t -> AtomSet.t

    (* [print] is a default printer for maps. It is parameterized with
       a printer for atoms and a printer for data. It displays maps as
       newline-terminated sequences of bindings. It displays bindings
       as a pair of an atom and a datum, separated with an arrow. *)

    val print: (Buffer.t -> key -> unit) -> (Buffer.t -> 'a -> unit) -> (Buffer.t -> 'a t -> unit)

  end

  (* The sub-module [AtomIdMap] offers finite maps of atoms to
     identifiers, with the property that every atom is mapped to a
     distinct identifier. This invariant is enforced by having the
     library pick a unique identifier when a new atom is added to the
     domain of the map. That is, the client does not control which
     identifiers are picked. *)

  module AtomIdMap : sig

    (* [key] is the type of atoms. *)

    type key =
	Atom.t

    (* [t] is the type of maps. *)

    type t

    (* [empty] is the empty map. *)

    val empty: t

    (* [add a m] is a map that maps the atom [a] to a unique
       identifier (that is, an identifier not in the codomain of [m])
       and elsewhere behaves like [m]. The base name of [a] is used
       when picking this identifier. The atom [a] must not be a member
       of the domain of [m]. *)

    val add: key -> t -> t

    (* [add_set s m] is the map obtained by successively [add]ing
       every member of the atom set [s] to the map [m]. *)

    val add_set: AtomSet.t -> t -> t

    (* [lookup a m] returns the identifier associated with the atom
       [a] in the map [m], if defined, and raises the exception
       [Atom.Unknown] otherwise. [lookup] is also known as [find]. *)

    val lookup: key -> t -> identifier
    val find: key -> t -> identifier

    (* [iter f m] applies [f] in turn to each binding in the map [m]. *)

    val iter : (key -> identifier -> unit) -> t -> unit

    (* [fold f m accu] applies [f] in turn to each binding in the map
       [m], threading an accumulator through the sequence of calls. *)

    val fold : (key -> identifier -> 'b -> 'b) -> t -> 'b -> 'b

  end

  (* The sub-module [Subst] offers substitutions of atoms for atoms.
     These are total mappings of atoms to atoms that behave as the
     identity outside of a finite set of atoms, known as their
     domain. *)

  module Subst : sig

    (* [t] is the type of substitutions. *)

    type t

    (* [id] is the identity substitution. *)

    val id: t

    (* [is_id subst] is [true] if and only if [subst] is the identity
       substitution. *)

    val is_id: t -> bool

    (* [singleton a b] is the singleton substitution that maps atom
       [a] to atom [b]. *)

    val singleton: Atom.t -> Atom.t -> t

    (* [add a b subst] is the substitution that maps atom [a] to atom
       [b] and elsewhere behaves like [subst]. *)

    val add: Atom.t -> Atom.t -> t -> t

    (* [union subst1 subst2] is the substitution that behaves like
       [subst2] on its domain and elsewhere behaves like [subst1]. In
       other words, the bindings in [subst2] take precedence over
       those in [subst1]. *)

    val union: t -> t -> t

    (* [compose subst1 subst2] is the composition of [subst1] with
       [subst2], that is, the substitution that maps every atom [a] to
       [subst1 (subst2 (a))]. *)

    val compose: t -> t -> t

    (* [freshen s subst] is a substitution that maps every atom [a]
       in the set [s] to a fresh atom (obtained via [Atom.fresha a])
       and elsewhere behaves like [subst]. *)

    val freshen: AtomSet.t -> t -> t

    (* [lookup a subst] is the image of [a] through [subst]. It is
       never undefined, since substitutions are viewed as total
       mappings. [lookup] is also known as [find]. *)

    val lookup: Atom.t -> t -> Atom.t
    val find: Atom.t -> t -> Atom.t

    (* [freshen2] is undocumented. *)

    val freshen2: Atom.t -> t -> Atom.t -> t -> t * t

  end

end

