(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/multi/multi.ml,v 1.6 2001/03/28 16:12:36 fpottier Exp $ *)

(* This module instantiates HM(X) with a unification-based constraint system whose type language allows reasoning
   with sums-of-records. The programming language is extended with primitives which manipulate these objects. *)

module GroundSig = struct

  type symbol =
    | SPresent
    | SAbsent
    | SMaybe
    | SNever
    | SMutable
    | SImmutable
    | SUnit
    | SInt
    | SArrow
    | SDot
    | SMatrix

  type 'a term =
      symbol * 'a list

  type label =
      symbol * int

  let arity (s, _) =
    match s with
    | SPresent
    | SAbsent
    | SMaybe
    | SNever
    | SMutable
    | SImmutable
    | SUnit
    | SInt ->
	0
    | SMatrix ->
	1
    | SArrow
    | SDot ->
	2

  let map f (s, l) =
    (s, List.map f l)

  let fork f (s, l) =
    let ll = List.map f l in
    (s, List.map fst ll), (s, List.map snd ll)

  open Tree

  let print = function
    | SPresent, [] ->
	[ Token "1" ]
    | SAbsent, [] ->
	[ Token "0" ]
    | SMaybe, [] ->
	[ Token "M" ]
    | SNever, [] ->
	[ Token "N" ]
    | SMutable, [] ->
	[ Token "!" ]
    | SImmutable, [] ->
	[ Token "-" ]
    | SUnit, [] ->
	[ Token "unit" ]
    | SInt, [] ->
	[ Token "int" ]
    | SArrow, [ left; right ] ->
	[ Son ((SArrow, 1), left); Token " -> "; Son ((SArrow, 2), right) ]
    | SDot, [ f; t ] ->
	[ Son ((SDot, 1), f); Token "."; Son ((SDot, 2), t) ]
    | SMatrix, [ m ] ->
	[ Token "<< "; Son ((SMatrix, 1), m); Token " >>" ]
    | _ ->
	assert false

  let parenthesize label (s, _) =
    match label, s with
    | _, (SPresent | SAbsent | SMaybe | SNever | SMutable | SImmutable | SUnit | SInt | SMatrix)
    | (SArrow, 2), SArrow
    | (SMatrix, _), SArrow
    | (SDot, _), SArrow ->
	false
    | (SArrow, 1), SArrow ->
	true
    | _ ->
	assert false

  let safe = function
    | SArrow, _ ->
	false
    | SMatrix, _
    | SDot, _ ->
	true
    | _ ->
	assert false

  let iter action (_, l) =
    List.iter action l

  let fold action (_, l) accu =
    List.fold_right action l accu

  exception Iter2

  let iter2 action (s1, l1) (s2, l2) =
    if s1 <> s2 then
      raise Iter2;
    List.iter2 action l1 l2

  let matches symbol (s, _) =
    symbol = s

  let sprint = function
    | SPresent ->
	"1"
    | SAbsent ->
	"0"
    | SMaybe ->
	"M"
    | SNever ->
	"N"
    | SMutable ->
	"!"
    | SImmutable ->
	"-"
    | SUnit ->
	"unit"
    | SInt ->
	"int"
    | SArrow ->
	"(->)"
    | SDot ->
	"(.)"
    | SMatrix ->
	"(<<>>)"

end

module Dimension = struct

  type t =
    | DimField
    | DimTag

  let compare : t -> t -> int =
    compare

  let print = function
    | DimField ->
	"F"
    | DimTag ->
	"T"

end

module Label = struct

  (* The current implementation of [Herbrand] does not allow different types for labels which live along different
     dimensions. So, we mix them all together. *)

  type t =
    | FieldTag
    | Field of string
    | TagType
    | Tag of string

  let compare : t -> t -> int = compare

  let print = function
    | FieldTag ->
	"tag"
    | Field s ->
	s
    | TagType ->
	"type"
    | Tag s ->
	s

end

module System = Herbrand.Make(Label)(Dimension)(GroundSig)

module Primitives = struct

  type name =
    | PrimUnit
    | PrimInt of int
    | PrimEnsureUnit
    | PrimChoice
    | PrimConstructor of string
    | PrimAccess of string
    | PrimExtend of string
    | PrimRestrict of string
    | PrimExtendMutable of string
    | PrimMutate of string
    | PrimCaseNone
    | PrimCaseOne of string
    | PrimFix

  type node =
      System.node

  type scheme =
      System.scheme

  open GroundSig
  open System
  open Dimension
  open Label

  (* Define term construction helpers. Given the (arguably natural) way these helpers are written, constant type
     terms are shared, i.e. they are only allocated once at the lowest level, and never generalized. *)

  let present =
    term (SPresent, [])

  let absent =
    term (SAbsent, [])

  let maybe =
    term (SMaybe, [])

  let never =
    term (SNever, [])

  let _mutable =
    term (SMutable, [])

  let immutable =
    term (SImmutable, [])

  let unit =
    term (SUnit, [])

  let int =
    term (SInt, [])

  let arrow x y =
    term (SArrow, [x; y])

  let dot x y =
    term (SDot, [x; y])

  let matrix x =
    term (SMatrix, [x])

  (* Define typing rules for all primitive operations. *)

  let rule mono = function
    | PrimUnit ->
	[], unit
    | PrimInt _ ->
	[], int
    | PrimEnsureUnit ->
	[unit], unit
    | PrimConstructor tag ->
	let line = juxtapose DimField FieldTag maybe absent in
	[], matrix (juxtapose DimTag (Tag tag) line (fresh()))
    | PrimAccess x ->
	let alpha = fresh() in
	let column = juxtapose DimTag TagType (dot (fresh()) alpha) present in
	let mtrix = juxtapose DimField (Field x) column (fresh()) in
	[matrix mtrix], alpha
    | PrimExtend x ->
	let alpha = fresh()
	and phi = fresh() in
	let column1 = juxtapose DimTag TagType (fresh()) (fresh()) in
	let matrix1 = juxtapose DimField (Field x) column1 phi in
	let column2 = juxtapose DimTag TagType (dot immutable alpha) (fresh()) in
	let matrix2 = juxtapose DimField (Field x) column2 phi in
	[matrix matrix1; alpha], matrix matrix2
    | PrimExtendMutable x ->
	let alpha = fresh()
	and phi = fresh() in
	let column1 = juxtapose DimTag TagType (fresh()) (fresh()) in
	let matrix1 = juxtapose DimField (Field x) column1 phi in
	let column2 = juxtapose DimTag TagType (dot _mutable alpha) (fresh()) in
	mono column2; (* a fresh mutable cell must have monomorphic type. *)
	let matrix2 = juxtapose DimField (Field x) column2 phi in
	[matrix matrix1; alpha], matrix matrix2
    | PrimMutate x ->
	let alpha = fresh() in
	let column = juxtapose DimTag TagType (dot _mutable alpha) (fresh()) in
	let mtrix = juxtapose DimField (Field x) column (fresh()) in
	[matrix mtrix; alpha], unit
    | PrimRestrict x ->

        (* with conditional constraints, i.e. more precise:

	let phi = fresh()
	and tag = fresh()
	and presence = fresh() in

	unify phi (juxtapose DimField FieldTag (juxtapose DimTag TagType (fresh()) tag) (fresh()));
	conditionally tag SMaybe presence absent;

	let matrix1 = juxtapose DimField (Field x) (fresh()) phi in
	let column2 = juxtapose DimTag TagType (fresh()) presence in
	let matrix2 = juxtapose DimField (Field x) column2 phi in *)

	(* without conditional constraints: *)

	let phi = fresh() in
	let matrix1 = juxtapose DimField (Field x) (fresh()) phi in
	let column2 = juxtapose DimTag TagType (fresh()) absent in
	let matrix2 = juxtapose DimField (Field x) column2 phi in
	[matrix matrix1], matrix matrix2
    | PrimCaseOne tag ->
	let rho = fresh()
	and phi = fresh()
	and psi = fresh() in
	let matrix1 = juxtapose DimTag TagType rho (juxtapose DimTag (Tag tag) phi psi)
	and matrix2 = juxtapose DimTag TagType rho (juxtapose DimTag (Tag tag) phi (fresh()))
	and matrix3 = juxtapose DimTag TagType rho (juxtapose DimTag (Tag tag) (fresh()) psi) in
	let alpha = fresh() in
	let arrow2 = arrow (matrix matrix2) alpha
	and arrow3 = arrow (matrix matrix3) alpha in
	[arrow2; arrow3], arrow (matrix matrix1) alpha
    | PrimCaseNone ->
	let mtrix = juxtapose DimField FieldTag never (fresh()) in
	[], arrow (matrix mtrix) (fresh())
    | PrimChoice ->
	let alpha = fresh() in
	[alpha; alpha], alpha
    | PrimFix ->
	let alpha = fresh() in
	[arrow alpha alpha], alpha

end

module Hm = Hmx.Make(Primitives)(System)(Primitives)

