open Source
open SymbolTable

(* ------------------------------------------------------------------------- *)

(* We use lattices equipped with [product] and [sum] operators, which
   reflect the meaning of product and sum in algebraic data types. *)

module type LATTICE = sig
  include FixOnDemand.PROPERTY
  val product: property -> property -> property
  val sum: property -> property -> property
end

(* ------------------------------------------------------------------------- *)

(* The lattice of ``can appear'' information. The lattice elements
   are Booleans. *)

module CanAppear : LATTICE with type property = bool = struct
  type property = bool
  let bottom = false
  let maximal p = p
  let equal = (=)
  let product = (||)
  let sum = (||)
end

(* ------------------------------------------------------------------------- *)

(* The lattice of ``must appear'' information. The lattice elements
   are Booleans. *)

module MustAppear : LATTICE with type property = bool = struct
  type property = bool
  let bottom = false
  let maximal p = p
  let equal = (=)
  let product = (||)
  let sum = (&&)
end

(* ------------------------------------------------------------------------- *)

(* Pair lattices. *)

type ('a, 'b) canmust = {
    can: 'a;
    must: 'b;
  }

let same a =
  { can = a; must = a }

module Pair (A : LATTICE) (B : LATTICE)
: LATTICE with type property = (A.property, B.property) canmust = struct
  type property = (A.property, B.property) canmust
  let bottom = { can = A.bottom; must = B.bottom }
  let maximal _ = false
  let equal { can = a1; must = b1 } { can = a2; must = b2 } = A.equal a1 a2 && B.equal b1 b2
  let product { can = a1; must = b1 } { can = a2; must = b2 } = { can = A.product a1 a2; must = B.product b1 b2 }
  let sum { can = a1; must = b1 } { can = a2; must = b2 } = { can = A.sum a1 a2; must = B.sum b1 b2 }
end

module CanMust =
  Pair (CanAppear) (MustAppear)

(* ------------------------------------------------------------------------- *)

(* Homogeneous quadruple lattices. *)

type 'a quadruple = {
    support: 'a;
    inner: 'a;
    outer: 'a;
    bound: 'a
  }

let project sf q =
  match sf with
  | SFSupport ->
      q.support
  | SFInner ->
      q.inner
  | SFOuter ->
      q.outer
  | SFBound ->
      q.bound

let make x =
  { support = x; inner = x; outer = x; bound = x }

let map2 f q1 q2 = {
  support = f q1.support q2.support;
  inner = f q1.inner q2.inner;
  outer = f q1.outer q2.outer;
  bound = f q1.bound q2.bound;
}

let fold_map2 op f q1 q2 accu =
  let accu = op (f q1.support q2.support) accu in
  let accu = op (f q1.inner q2.inner) accu in
  let accu = op (f q1.outer q2.outer) accu in
  let accu = op (f q1.bound q2.bound) accu in
  accu

module HQuadruple (A : LATTICE) : LATTICE with type property = A.property quadruple = struct
  type property = A.property quadruple
  let bottom = make A.bottom
  let maximal _ = false
  let equal q1 q2 = fold_map2 (&&) A.equal q1 q2 true
  let product = map2 A.product
  let sum = map2 A.sum
end

(* ------------------------------------------------------------------------- *)

(* Our final data lattice is a lattice of quadruples of ``can/must''
   appear information. *)

type facts =
    (bool, bool) canmust quadruple

module Data : LATTICE with type property = facts =
   HQuadruple (CanMust)

(* ------------------------------------------------------------------------- *)

(* The transfer function. We focus on a single sort at a time, because
   this makes things simpler, obviating the need to manipulate sets of
   sorts. *)

module Transfer (F : sig val focus: sorte end) = struct

  open F
  open Data

  type key =
      datatype

  type property =
      facts

  let is_focus sort =
    Sorte.Atom.equal focus sort

  let has_focus sorts =
    List.exists is_focus sorts

  let eval_type get typ =
    match Annotation.content typ with
    | TData t' ->
	get t'
    | TAtom sort when is_focus sort ->
	{
	  support = same true;
	  inner = same false;
	  outer = same false;
	  bound = same true;
        }
    | TAtomSet sort when is_focus sort ->
	{
	  support = { can = true; must = false };
	  inner = same false;
	  outer = same false;
	  bound = { can = true; must = false };
        }
    | TAtom _
    | TAtomSet _
    | TBool ->
	make (same false)

  let rec eval_tuple get tuple =
    match Annotation.content tuple with
    | TComponent typ ->
	eval_type get typ
    | TInner (sorts, tuple) when has_focus sorts ->
	let q = eval_tuple get tuple in
	{
	  support = q.support;
	  inner = q.support;
	  outer = same false;
	  bound = same false;
        }
    | TOuter (sorts, tuple) when has_focus sorts ->
	let q = eval_tuple get tuple in
	{
	  support = q.support;
	  inner = same false;
	  outer = q.support;
	  bound = same false;
        }
    | TAbstraction (sorts, tuple) when has_focus sorts ->
	let q = eval_tuple get tuple in
	{
	  support = { can = q.support.can; must = false };
	  inner = same false;
	  outer = same false;
	  bound = same false;
        }
    | TInner (_, tuple)
    | TOuter (_, tuple)
    | TAbstraction (_, tuple)
    | TName (_, tuple) ->
	eval_tuple get tuple
    | TTuple tuples ->
	Misc.fold_map_left product (eval_tuple get) bottom tuples

  let transfer (get : key -> property) (t : key) : property =
    let def = DatatypeTable.def t in
    Misc.fold_map_left sum (fun (_, params) ->
      let tuple, _ = open_guarded_tuple params in
      eval_tuple get tuple
    ) bottom def.datatype_constructors

end

(* ------------------------------------------------------------------------- *)

(* The fixed point computation. *)

let fix sort =
  let module T =
    Transfer (struct let focus = sort end)
  in
  let module F =
    FixOnDemand.Make (Datatype.AtomMap) (Data) (T)
  in
  F.get, T.eval_type F.get, T.eval_tuple F.get

(* ------------------------------------------------------------------------- *)

(* Make sure that the fixed point is computed only once at each sort. *)

module M =
  Memoize.Make (Sorte.AtomMap)

let fix =
  M.memoize fix

(* ------------------------------------------------------------------------- *)

(* Public accessors. *)

let datatype sort t =
  let get, _, _ = fix sort in
  get t

let typ sort typ =
  let _, get, _ = fix sort in
  get typ

let tuple sort tuple =
  let _, _, get = fix sort in
  get tuple

