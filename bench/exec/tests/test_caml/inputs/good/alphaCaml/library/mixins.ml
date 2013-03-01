open Signatures
open PrivateSignatures

(* ------------------------------------------------------------------------- *)

(* Utilities. *)

let vacuous_map : 'a map =
  fun x -> x

let vacuous_fold : ('accu, 'a) fold =
  fun accu _ -> accu

let vacuous_fold2 : ('accu, 'a) fold2 =
  fun accu _ _ -> accu

(* ------------------------------------------------------------------------- *)

(* These are virtual base classes. *)

module Declare
    (F : Flavor)
= struct

  open F

  class virtual cmap = object (self : 'self)
    method private virtual free_atom: atom map
    method private virtual bound_atom: atom map
    method virtual abstraction: 'a . ('self -> 'a abstractable) ->
                                     ('self -> 'a map) -> 'a abs map
    method virtual outer: 'a . ('self -> 'a map) -> 'a map
    method virtual inner: 'a . ('self -> 'a map) -> 'a map
  end

  class virtual ['expaccu, 'pataccu] cfold = object (self : 'self)
    method private virtual free_atom: ('expaccu, atom) fold
    method private virtual bound_atom: ('pataccu, atom) fold
    method virtual abstraction: 'a . ('self -> 'a abstractable) ->
                                     ('self -> ('pataccu, 'a) fold) -> ('expaccu, 'a abs) fold
    method virtual outer: 'a . ('self -> ('expaccu, 'a) fold) -> ('pataccu, 'a) fold
    method virtual inner: 'a . ('self -> ('expaccu, 'a) fold) -> ('pataccu, 'a) fold
  end

  class virtual ['expaccu, 'pataccu] cfold2 = object (self : 'self)
    method private virtual free_atom: ('expaccu, atom) fold2
    method private virtual bound_atom: ('pataccu, atom) fold2
    method virtual abstraction: 'a . ('self -> 'a abstractable) ->
                                     ('self -> ('pataccu, 'a) fold2) -> ('expaccu, 'a abs) fold2
    method virtual outer: 'a . ('self -> ('expaccu, 'a) fold2) -> ('pataccu, 'a) fold2
    method virtual inner: 'a . ('self -> ('expaccu, 'a) fold2) -> ('pataccu, 'a) fold2
  end

end

(* ------------------------------------------------------------------------- *)

module Make
    (T : Toolbox)
= struct

type atom =
    T.Atom.atom

type 'a abs =
    'a T.Abstraction.abs

type 'a abstractable =
    'a T.Abstraction.abstractable

(* ------------------------------------------------------------------------- *)

(* Applying a substitution to a data structure made up of opaque
   atoms and abstractions. *)

class substitution outer inner = object (self : 'self)

  (* Store the parameters within instance variables, hiding the previous
     bindings. The values of these instance variables will be overridden
     at the boundary between patterns and expressions. *)

  val outer = outer
  val inner = inner

  (* When finding an atom within an expression, look it up in [outer]. *)

  method private free_atom a =
    T.Subst.find a outer

  (* When finding an atom within a pattern, look it up in [inner]. *)

  method private bound_atom a =
    T.Subst.find a inner

  (* At abstractions, apply the [outer] substitution to the
     abstraction using [T.Abstraction.apply]. The dictionary [d] is ignored,
     because [T.Abstraction.apply] does not require a dictionary (the
     substitution is suspended, and the abstraction's body is not
     examined). The continuation [f], which allows traversing the
     abstraction's body after updating [self], is also unused. *)

  method abstraction: 'a . ('self -> 'a abstractable) -> ('self -> 'a map) -> 'a abs map
  = fun d f abs ->
    T.Abstraction.apply outer abs

  (* At an [outer] keyword, switch from pattern mode to expression
     mode. The outer substitution becomes the sole current
     substitution.  Because the latter is, by convention, stored
     within the [outer] field, there is in fact nothing to do --
     [self] is not updated. *)

  method outer: 'a . ('self -> 'a map) -> 'a map
  = fun f ->
    f self

  (* At an [inner] keyword, the inner substitution becomes the sole
     current substitution. We reflect this by updating [self], that
     is, by passing an updated [self] to the continuation. *)

  method inner: 'a . ('self -> 'a map) -> 'a map
  = fun f ->
    f {< outer = inner; inner = T.Subst.id >}

end

(* ------------------------------------------------------------------------- *)

(* Iterating over the bound atoms of a pattern. *)

class ['accu] bv (f : ('accu, atom) fold) = object (self : 'self)

  method private free_atom: ('accu, atom) fold
  = assert false (* never invoked *)

  (* When finding an atom within a pattern, invoke [f]. *)

  method private bound_atom: ('accu, atom) fold =
    f

  method abstraction: 'a . ('self -> 'a abstractable) ->
                           ('self -> ('accu, 'a) fold) -> ('accu, 'a abs) fold
  = assert false (* never invoked *)

  (* At an [outer] or [inner] keyword, stop. We fold over patterns
     only. *)

  method outer: 'a . ('self -> ('accu, 'a) fold) -> ('accu, 'a) fold
  = fun f ->
    vacuous_fold

  method inner: 'a . ('self -> ('accu, 'a) fold) -> ('accu, 'a) fold
  = fun f ->
    vacuous_fold

end

(* ------------------------------------------------------------------------- *)

(* Iterating synchronously over the bound atoms of two patterns. *)

class ['accu] bv2 (f : ('accu, atom) fold2) = object (self : 'self)

  method private free_atom: ('accu, atom) fold2
  = assert false (* never invoked *)

  (* When finding atoms within a pattern, invoke [f]. *)

  method private bound_atom =
    f

  method abstraction: 'a . ('self -> 'a abstractable) ->
                           ('self -> ('accu, 'a) fold2) -> ('accu, 'a abs) fold2
  = assert false (* never invoked *)

  (* At an [outer] or [inner] keyword, stop. We fold over patterns
     only. *)

  method outer: 'a . ('self -> ('accu, 'a) fold2) -> ('accu, 'a) fold2
  = fun f ->
    vacuous_fold2

  method inner: 'a . ('self -> ('accu, 'a) fold2) -> ('accu, 'a) fold2
  = fun f ->
    vacuous_fold2

end

(* ------------------------------------------------------------------------- *)

(* Traversal and iteration over opaque data structures. The behavior
   at abstractions is defined in terms of [unpack], [unpack2], and
   [pack], via dictionaries. *)

class cmap = object (self : 'self)

  method private free_atom: atom map
  = vacuous_map

  method private bound_atom: atom map
  = vacuous_map

  (* At an abstraction, we unpack the abstraction, apply [f] to its
     (freshened) content, and rebuild a new abstraction. If the
     output of [f] is physically identical to its input, then the
     original abstraction is returned unchanged (although possibly
     modified in place by [unpack], but that is transparent). *)

  method abstraction: 'a . ('self -> 'a abstractable) -> ('self -> 'a map) -> 'a abs map
  = fun d f abs ->
    let input = T.Abstraction.unpack (d self) abs in
    let output = f self input in
    if input == output then
      abs
    else
      T.Abstraction.pack output

  method outer: 'a . ('self -> 'a map) -> 'a map
  = fun f -> f self

  method inner: 'a . ('self -> 'a map) -> 'a map
  = fun f -> f self

end

class ['accu] cfold = object (self : 'self)

  method private free_atom: ('accu, atom) fold
  = vacuous_fold

  method private bound_atom: ('accu, atom) fold
  = vacuous_fold

  (* At an abstraction, we unpack the abstraction and fold over its
     (freshened) content. *)

  method abstraction: 'a . ('self -> 'a abstractable) ->
                           ('self -> ('accu, 'a) fold) -> ('accu, 'a abs) fold
  = fun d f accu abs ->
    let content = T.Abstraction.unpack (d self) abs in
    f self accu content

  method outer: 'a . ('self -> ('accu, 'a) fold) -> ('accu, 'a) fold
  = fun f -> f self

  method inner: 'a . ('self -> ('accu, 'a) fold) -> ('accu, 'a) fold
  = fun f -> f self

end

class ['accu] cfold2 = object (self : 'self)

  method private free_atom: ('accu, atom) fold2
  = vacuous_fold2

  method private bound_atom: ('accu, atom) fold2
  = vacuous_fold2

  (* At abstractions, we unpack both abstractions and fold over their
     (synchronously freshened) contents. *)

  method abstraction: 'a . ('self -> 'a abstractable) ->
                           ('self -> ('accu, 'a) fold2) -> ('accu, 'a abs) fold2
  = fun d f accu abs1 abs2 ->
    let content1, content2 = T.Abstraction.unpack2 (d self) abs1 abs2 in
    f self accu content1 content2

  method outer: 'a . ('self -> ('accu, 'a) fold2) -> ('accu, 'a) fold2
  = fun f -> f self

  method inner: 'a . ('self -> ('accu, 'a) fold2) -> ('accu, 'a) fold2
  = fun f -> f self

end

(* ------------------------------------------------------------------------- *)

(* Accumulating the atoms that appear free within an expression, or
   that appear in bound/inner/outer position within a pattern. *)

type fbio_e =
    T.AtomSet.atoms

type fbio_p =
    T.AtomSet.atoms * T.AtomSet.atoms * T.AtomSet.atoms

let fbio_seed_e =
  T.AtomSet.empty

let fbio_seed_p =
  (T.AtomSet.empty, T.AtomSet.empty, T.AtomSet.empty)

class cfbio = object (self : 'self)

  method private free_atom free a =
    T.AtomSet.insert free a

  method private bound_atom ((bound, inner, outer) : fbio_p) a =
    T.AtomSet.insert bound a, inner, outer

  (* At an abstraction, no freshening is required, so we use [expose]
     instead of [unpack]. The accumulator is converted to pattern mode,
     and the abstraction's body is traversed. Upon exit, the accumulator
     is converted back to expression mode, and the bound atoms are
     subtracted from the atoms that appear free in inner position. *)

  method abstraction: 'a . ('self -> 'a abstractable) ->
                           ('self -> (fbio_p, 'a) fold) -> (fbio_e, 'a abs) fold
  = fun d f accu abs ->
    let content = T.Abstraction.expose (d self) abs in
    let bound, inner, outer = f self (T.AtomSet.empty, T.AtomSet.empty, accu) content in
    T.AtomSet.union (T.AtomSet.diff inner bound) outer

  (* At an [outer] keyword, the outer component of the accumulator is
     used to build a new expression-mode accumulator. After traversal,
     the updated expression-mode accumulator is used to update the
     original pattern-mode accumulator. *)

  method outer: 'a . ('self -> (fbio_e, 'a) fold) -> (fbio_p, 'a) fold
  = fun f (bound, inner, outer) content ->
    bound, inner, f self outer content

  (* At an [inner] keyword, the inner component of the accumulator
     is extracted and updated. *)

  method inner: 'a . ('self -> (fbio_e, 'a) fold) -> (fbio_p, 'a) fold
  = fun f (bound, inner, outer) content ->
    bound, f self inner content, outer

end

(* ------------------------------------------------------------------------- *)

(* Comparing two data structures for alpha-equality. *)

open Asynchrony

class caeq = object

  inherit [unit] cfold2

  (* It is sufficient to override [fold2] at occurrences of atoms.
     Everywhere else, the inherited behavior is appropriate. *)

  method private free_atom () a1 a2 =
    if not (T.Atom.equal a1 a2) then
      raise asynchrony

  method private bound_atom () a1 a2 =
    if not (T.Atom.equal a1 a2) then
      raise asynchrony

end

(* ------------------------------------------------------------------------- *)

(* Exporting a data structure, that is, converting opaque atoms and
   abstractions back into transparent ones. *)

class export outer inner = object (self : 'self)

  (* Store the parameters within instance variables, hiding the previous
     bindings. The values of these instance variables will be overridden
     at the boundary between patterns and expressions. *)

  val outer = outer
  val inner = inner

  (* When finding an atom within an expression, look it up in [outer]. *)

  method private free_atom a =
    try
      T.Export.find a outer
    with Not_found ->
      assert false (* TEMPORARY do something intelligent here? *) (* raise a sort-specific exception? *)

  (* When finding an atom within a pattern, look it up in [inner]. *)

  method private bound_atom a =
    try
      T.Export.find a inner
    with Not_found ->
      assert false

  (* At abstractions, construct a new [inner] environment by extending the
     [outer] environment with bindings of the pattern's bound atoms to fresh
     identifiers. Note that the abstraction's bound atoms need not be
     freshened. *)

  (* We compute a set of bound atoms and use [add_set], instead of using
     [freshen] and [union] as above. This is because [T.Export.add] picks
     locally fresh identifiers, not globally fresh atoms, so we cannot take a
     union of two export environments. *)

  method abstraction: 'a 'b . ('self -> 'a abstractable) -> ('self -> 'a -> 'b) -> 'a abs -> 'b
  = fun d f abs ->
    let d = d self in
    let content = T.Abstraction.expose d abs in
    let bv = d.T.Abstraction.bv T.AtomSet.insert T.AtomSet.empty content in
    f {< inner = T.Export.add_set bv outer >} content

  method outer: 'a 'b . ('self -> 'a -> 'b) -> 'a -> 'b
  = fun f ->
    f self

  method inner: 'a 'b . ('self -> 'a -> 'b) -> 'a -> 'b
  = fun f ->
    f {< outer = inner; inner = T.Export.empty >}

end

(* ------------------------------------------------------------------------- *)

(* Flattening a data structure, that is, converting opaque abstractions into
   transparent ones. *)

(* At atoms, the transformation is the identity. At abstractions, we simply
   call [unpack]. *)

class flatten = object (self : 'self)

  method private free_atom: atom map =
    vacuous_map

  method private bound_atom: atom map =
    vacuous_map

  method abstraction: 'a 'b . ('self -> 'a abstractable) ->
                              ('self -> 'a -> 'b) -> 'a abs -> 'b
  = fun d f abs ->
    let content = T.Abstraction.unpack (d self) abs in
    f self content

  method outer: 'a 'b . ('self -> 'a -> 'b) -> 'a -> 'b
  = fun f ->
    f self

  method inner: 'a 'b . ('self -> 'a -> 'b) -> 'a -> 'b
  = fun f ->
    f self

end

(* ------------------------------------------------------------------------- *)

(* Converting transparent abstractions back into opaque ones. *)

(* At atoms, the transformation is the identity. At abstractions, we
   simply call [pack]. *)

class unflatten = object (self : 'self)

  method private free_atom: atom map =
    vacuous_map

  method private bound_atom: atom map =
    vacuous_map

  method abstraction: 'a 'b . ('self -> unit) ->
                              ('self -> 'a -> 'b) -> 'a -> 'b abs
  = fun d f content ->
    T.Abstraction.pack (f self content)

  method outer: 'a 'b . ('self -> 'a -> 'b) -> 'a -> 'b
  = fun f ->
    f self

  method inner: 'a 'b . ('self -> 'a -> 'b) -> 'a -> 'b
  = fun f ->
    f self

end

(* ------------------------------------------------------------------------- *)

end

