open AlphaLib.Signatures
open AlphaLib.PrivateSignatures

(* ------------------------------------------------------------------------- *)

(* A module type describes the structure of our data. The types of atoms,
   abstractions, and evidence are left unspecified, so this module
   type serves as a common template for our raw, internal, and flat forms. *)

module type Data = sig

  type var

  type 'a abs

  type 'a abstractable (* TEMPORARY supprimer? *)

  type expression =
    | EVar of var
    | ELambda of binding abs
    | EApp of expression * expression

  and binding =
      var * expression

end

(* Internally, all atoms have the same type, regardless of their sort.
   This information is not revealed to the client, who is presented
   with a distinct abstract type of atoms for each sort. *)

module type PrivateData = sig

  type atom

  include Data with type var = atom

end

(* ------------------------------------------------------------------------- *)

(* Once the types of atoms, abstractions, and evidence are specified, we
   can build an implementation of [PrivateData]. This functor is used to
   generate the definitions of the raw, internal, and flat forms. *)

module Data (F : Flavor) = struct

  include F

  type var =
      atom

  type expression =
    | EVar of var
    | ELambda of binding abs
    | EApp of expression * expression

  and binding =
      var * expression

end

(* ------------------------------------------------------------------------- *)

(* A bunch of method selectors are declared at toplevel in order to avoid
   making them local functions (which would require memory allocation). *)

module Select = struct

  let expression self = self#expression

  let binding self = self#binding

  let binding_dictionary self = self#binding_dictionary

end

(* ------------------------------------------------------------------------- *)

(* A template for dictionary lookup methods. *)

module Dictionaries (D : Data) = struct

  open D

  class dictionaries = object

    method binding_dictionary: binding abstractable =
      assert false

  end

end

(* ------------------------------------------------------------------------- *)

(* A functor, parameterized over a flavor of data, defines generic
   code for [map], [fold], and [fold2]. At abstractions, this code
   requires dictionaries that cannot yet be defined, because their
   type is unspecified. For this reason, these classes are virtual. *)

module UnaryDataClasses (Data : PrivateData) = struct

  open Data

  (* Create default dictionary methods. These methods should never be
     called. Either they should be overridden, or the [abstraction]
     method should be defined so as to never invoke a dictionary
     method. Both of these styles are exploited further on. *)

  module D =
    Dictionaries(Data)

  (* Create virtual method declarations for the five methods that
     deal with free atoms, bound atoms, inner and outer keywords. *)

  module B =
    AlphaLib.Mixins.Declare(Data)

  (* Generic homogeneous map. *)

  class virtual cmap = object(self)

    inherit D.dictionaries
    inherit B.cmap

    method free_var x =
      self#free_atom x

    method bound_var x =
      self#bound_atom x

    method expression e =
      match e with
      | EVar v ->
	  self#evar e v
      | ELambda abs ->
	  self#elambda e abs
      | EApp (e1, e2) ->
	  self#eapp e e1 e2

    method evar e v =
      let v' = self#free_var v in
      if v == v' then e else EVar v'

    method elambda e abs =
      let abs' = self#abstraction Select.binding_dictionary Select.binding abs in
      if abs == abs' then e else ELambda abs'

    method eapp e e1 e2 =
      let e1' = self#expression e1 in
      let e2' = self#expression e2 in
      if e1 == e1' && e2 == e2' then e else EApp (e1', e2')

    method binding b =
      match b with
      | (v, e) ->
	  let v' = self#bound_var v in
	  let e' = self#inner Select.expression e in
	  if v == v' && e == e' then b else (v', e')

  end

  (* Generic fold. *)

  class virtual ['expaccu, 'pataccu] cfold = object(self)

    inherit D.dictionaries
    inherit ['expaccu, 'pataccu] B.cfold

    method free_var (accu : 'expaccu) a =
      self#free_atom accu a

    method bound_var (accu : 'pataccu) a =
      self#bound_atom accu a

    method expression (accu : 'expaccu) e =
      match e with
      | EVar v ->
	  self#evar accu v
      | ELambda abs ->
	  self#elambda accu abs
      | EApp (e1, e2) ->
	  self#eapp accu e1 e2

    method evar (accu : 'expaccu) v =
      self#free_var accu v

    method elambda (accu : 'expaccu) abs =
      self#abstraction Select.binding_dictionary Select.binding accu abs

    method eapp (accu : 'expaccu) e1 e2 =
      let accu = self#expression accu e1 in
      let accu = self#expression accu e2 in
      accu

    method binding (accu : 'pataccu) b =
      match b with
      | (v, e) ->
	  let accu = self#bound_var accu v in
	  let accu = self#inner Select.expression accu e in
	  accu

  end

  (* Generic fold2. *)

  class virtual ['expaccu, 'pataccu] cfold2 = object (self)

    inherit D.dictionaries
    inherit ['expaccu, 'pataccu] B.cfold2

    method free_var (accu : 'expaccu) a1 a2 =
      self#free_atom accu a1 a2

    method bound_var (accu : 'pataccu) a1 a2 =
      self#bound_atom accu a1 a2

    method expression (accu : 'expaccu) e1 e2 =
      match e1, e2 with
      | EVar v1, EVar v2 ->
	  self#evar accu v1 v2
      | ELambda abs1, ELambda abs2 ->
	  self#elambda accu abs1 abs2
      | EApp (e1, f1), EApp (e2, f2) ->
	  self#eapp accu e1 f1 e2 f2
      | _, _ ->
	  raise (Invalid_argument "fold2")

    method evar (accu : 'expaccu) v1 v2 =
      self#free_var accu v1 v2

    method elambda (accu : 'expaccu) abs1 abs2 =
      self#abstraction Select.binding_dictionary Select.binding accu abs1 abs2

    method eapp (accu : 'expaccu) e1 f1 e2 f2 =
      let accu = self#expression accu e1 e2 in
      let accu = self#expression accu f1 f2 in
      accu

    method binding (accu : 'pataccu) b1 b2 =
      match b1, b2 with
      | (v1, e1), (v2, e2) ->
	  let accu = self#bound_var accu v1 v2 in
	  let accu = self#inner Select.expression accu e1 e2 in
	  accu

  end

end

(* ------------------------------------------------------------------------- *)

(* A functor, parameterized over two flavors of data, defines generic
   code for heterogeneous [map]. *)

module BinaryDataClasses (Input : PrivateData) (Output : PrivateData) = struct

  (* Create default dictionary methods. *)

  module D =
    Dictionaries(Input)

  (* Generic heterogeneous map. *)

  class virtual cmap = object (self : 'self)

    inherit D.dictionaries

    method private virtual free_atom: Input.atom -> Output.atom

    method free_var (x : Input.var) : Output.var =
      self#free_atom x

    method private virtual bound_atom: Input.atom -> Output.atom

    method bound_var (x : Input.var) : Output.var =
      self#bound_atom x

    method expression e =
      match e with
      | Input.EVar v ->
	  self#evar v
      | Input.ELambda abs ->
	  self#elambda abs
      | Input.EApp (e1, e2) ->
	  self#eapp e1 e2

    method evar v =
      let v' = self#free_var v in
      Output.EVar v'

    method elambda abs =
      let abs' = self#abstraction Select.binding_dictionary Select.binding abs in
      Output.ELambda abs'

    method eapp e1 e2 =
      let e1' = self#expression e1 in
      let e2' = self#expression e2 in
      Output.EApp (e1', e2')

    method binding b =
      match b with
      | (v, e) ->
	  let v' = self#bound_var v in
	  let e' = self#inner Select.expression e in
	  (v', e')

    method virtual abstraction: 'a 'b .
      ('self -> 'a Input.abstractable) -> ('self -> 'a -> 'b) -> 'a Input.abs -> 'b Output.abs

    method virtual outer: 'a 'b . ('self -> 'a -> 'b) -> 'a -> 'b

    method virtual inner: 'a 'b . ('self -> 'a -> 'b) -> 'a -> 'b

  end

end

(* ------------------------------------------------------------------------- *)

(* Fix an implementation of identifiers. *)

module Identifier =
  AlphaLib.Toolbox.String

(* Create a toolbox. *)

module Toolbox =
  AlphaLib.Toolbox.Make(Identifier)(struct let n = 1 end) 

open Toolbox

(* Instantiate the mixin classes. *)

module Mixins =
  AlphaLib.Mixins.Make(Toolbox)

(* ------------------------------------------------------------------------- *)

(* Define internal form: atoms and abstractions are opaque. *)

module Internal = struct

  (* Define the internal flavor of data. *)

  module Data = Data (Abstraction)

  include Data

  (* Specialize the above generic code for [map], [fold], and [fold2]
     by providing appropriate dictionaries for opaque abstractions. *)

  module G =
    UnaryDataClasses(Data)

  class substitution outer inner = object
    inherit G.cmap
    inherit Mixins.substitution outer inner
  end

  class ['accu] bv f = object
    inherit ['accu, 'accu] G.cfold
    inherit ['accu] Mixins.bv f
  end

  class ['accu] bv2 f = object
    inherit ['accu, 'accu] G.cfold2
    inherit ['accu] Mixins.bv2 f
  end

  (* Dictionary implementations. *)

  class dictionaries = object

    method binding_dictionary = {
      Abstraction.bv = (fun f accu binding -> (new bv f)#binding accu binding);
      Abstraction.bv2 = (fun f accu binding1 binding2 -> (new bv2 f)#binding accu binding1 binding2);
      Abstraction.apply = (fun outer inner binding -> (new substitution outer inner)#binding binding);
    }

  end

  (* Wrappers for operations on abstractions. *)

  let dicts =
    new dictionaries

  let wrap op selector =
    op (selector dicts)

  let pack_binding =
    Abstraction.pack

  let unpack_binding =
    wrap Abstraction.unpack Select.binding_dictionary

  let expose_binding =
    wrap Abstraction.expose Select.binding_dictionary

  let unpack2_binding =
    wrap Abstraction.unpack2 Select.binding_dictionary

  (* Client-level classes. *)

  class cmap = object
    inherit G.cmap
    inherit dictionaries
    inherit Mixins.cmap
  end

  class virtual ['expaccu, 'pataccu] hcfold = object
    inherit ['expaccu, 'pataccu] G.cfold
    inherit dictionaries
  end

  class ['accu] cfold = object
    inherit ['accu, 'accu] hcfold
    inherit ['accu] Mixins.cfold
  end

  class virtual ['expaccu, 'pataccu] hcfold2 = object
    inherit ['expaccu, 'pataccu] G.cfold2
    inherit dictionaries
  end

  class ['accu] cfold2 = object
    inherit ['accu, 'accu] hcfold2
    inherit ['accu] Mixins.cfold2
  end

  class cfbio = object
    inherit [Mixins.fbio_e, Mixins.fbio_p] hcfold
    inherit dictionaries
    inherit Mixins.cfbio
  end

  class caeq = object
    inherit [unit] cfold2
    inherit dictionaries
    inherit Mixins.caeq
  end

  (* Client wrappers for [substitution]. *)

  let wrap_e selector subst x =
    (selector (new substitution subst Subst.id)) x

  let wrap_p selector outer inner x =
    (selector (new substitution outer inner)) x

  let subst_expression =
    wrap_e Select.expression

  let subst_binding =
    wrap_p Select.binding

  (* Client wrappers for [cfbio]. *)

  let cfbio = new cfbio

  let f_expression e =
    cfbio#expression Mixins.fbio_seed_e e

  let f_binding binding =
    cfbio#binding Mixins.fbio_seed_p binding

  (* Client wrappers for [caeq]. *)

  let caeq = new caeq

  let aeq_expression e1 e2 =
    AlphaLib.Asynchrony.wrap caeq#expression e1 e2

  let aeq_binding b1 b2 =
    AlphaLib.Asynchrony.wrap caeq#binding b1 b2

end

(* ------------------------------------------------------------------------- *)

(* Define raw form: atoms and abstractions are transparent. *)

module Raw = struct

  module Data = Data (struct
    type atom = Identifier.t
    type 'a abs = 'a
    type 'a abstractable = unit (* TEMPORARY supprimer? *)
  end)

  include Data

end

(* ------------------------------------------------------------------------- *)

(* Define a translation from internal form to raw form. *)

module GHE =
  BinaryDataClasses(Internal.Data)(Raw.Data)

class cexport outer inner = object
  inherit GHE.cmap
  inherit Internal.dictionaries
  inherit Mixins.export outer inner
end

let export_expression m e =
  (new cexport m Export.empty)#expression e

let export_binding outer inner b =
  (new cexport outer inner)#binding b

(* ------------------------------------------------------------------------- *)

(* Define flat form: atoms are opaque, abstractions are transparent. *)

module Flat = struct

  module Data = Data (struct
    include Toolbox.Atom (* defines [atom] *)
    type 'a abs = 'a
    type 'a abstractable = unit
  end)

  include Data

  class dictionaries = object

    method binding_dictionary: unit = ()

  end

end

(* ------------------------------------------------------------------------- *)

(* Define translations from internal form to flat form and back. *)

(* Flattening. *)

module CF =
  BinaryDataClasses(Internal.Data)(Flat.Data)

class cflatten = object
  inherit CF.cmap
  inherit Internal.dictionaries
  inherit Mixins.flatten
end

let flatten =
   new cflatten

let flatten_expression =
  flatten#expression

let flatten_binding =
  flatten#binding

(* Unflattening. *)

module CUF =
  BinaryDataClasses(Flat.Data)(Internal.Data)

class cunflatten = object
  inherit CUF.cmap
  inherit Flat.dictionaries
  inherit Mixins.unflatten
end

let unflatten =
   new cunflatten

let unflatten_expression =
  unflatten#expression

let unflatten_binding =
  unflatten#binding

(* ------------------------------------------------------------------------- *)

(* Export a few definitions. *)

type subst =
    Subst.subst

type atoms =
    AtomSet.atoms

type export =
    Export.env

(* ocamlfind ocamlc -package alphaLib -c manuel.mli; ocamlfind ocamlc -package alphaLib -c -w m -w x manuel.ml *)

