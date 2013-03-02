(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/sets/sets.ml,v 1.1.2.1 2002/12/26 11:08:34 fpottier Exp $ *)

(* This module instantiates HM(X) with a unification-based constraint system whose type language allows reasoning
   with sets. The programming language is extended with primitives which represent set-theoretic operations.

   The language is also extended with a unary data constructor [normal], a constant data constructor [exc], and a
   [match] construct which allows telling them apart. *)

module GroundSig = struct

  type 'a term =
    | TPresent
    | TAbsent
    | TUnit
    | TArrow of 'a * 'a
    | TSet of 'a
    | TPresent1 of 'a
    | TSum of 'a * 'a

  type label =
    | LArrowL
    | LArrowR
    | LSet
    | LPresent1
    | LNormal
    | LExc

  let arity = function
    | TPresent
    | TAbsent
    | TUnit ->
	0
    | TArrow _
    | TSum _ ->
	2
    | TSet _
    | TPresent1 _ ->
	1

  let map f = function
    | TPresent ->
	TPresent
    | TAbsent ->
	TAbsent
    | TUnit ->
	TUnit
    | TArrow (left, right) ->
	TArrow(f left, f right)
    | TSet arg ->
	TSet (f arg)
    | TPresent1 arg ->
	TPresent1 (f arg)
    | TSum (left, right) ->
	TSum (f left, f right)

  let fork f = function
    | TPresent ->
	TPresent, TPresent
    | TAbsent ->
	TAbsent, TAbsent
    | TUnit ->
	TUnit, TUnit
    | TArrow (left, right) ->
	let left1, left2 = f left
	and right1, right2 = f right in
	TArrow(left1, right1), TArrow(left2, right2)
    | TSet arg ->
	let arg1, arg2 = f arg in
	TSet arg1, TSet arg2
    | TPresent1 arg ->
	let arg1, arg2 = f arg in
	TPresent1 arg1, TPresent1 arg2
    | TSum (left, right) ->
	let left1, left2 = f left
	and right1, right2 = f right in
	TSum(left1, right1), TSum(left2, right2)

  open Tree

  let print = function
    | TPresent ->
	[ Token "Pre" ]
    | TAbsent ->
	[ Token "Abs" ]
    | TUnit ->
	[ Token "unit" ]
    | TArrow (left, right) ->
	[ Son (LArrowL, left); Token " -> "; Son (LArrowR, right) ]
    | TSet arg ->
	[ Token "{ "; Son (LSet, arg); Token " }" ]
    | TPresent1 arg ->
	[ Token "Pre "; Son (LPresent1, arg) ]
    | TSum (normal, exc) ->
	[ Token "[ normal: "; Son (LNormal, normal); Token "; exc: "; Son (LExc, exc); Token " ]" ]

  let parenthesize label term =
    match label, term with
    | (LSet | LNormal | LExc), _
    | _, (TPresent | TAbsent | TUnit | TSet _ | TSum _)
    | LArrowR, TArrow _
    | LNormal, TPresent1 _
    | LPresent1, _ ->
	false
    | LArrowL, TArrow _
    | (LArrowL | LArrowR), TPresent1 _ ->
	true

  let safe = function
    | LArrowL
    | LArrowR
    | LPresent1 ->
	false
    | LSet
    | LNormal
    | LExc ->
	true

  let iter action term =
    match term with
    | TPresent
    | TAbsent
    | TUnit ->
	()
    | TArrow (left, right) ->
	action left;
	action right
    | TSet arg ->
	action arg
    | TPresent1 arg ->
	action arg
    | TSum (left, right) ->
	action left;
	action right

  let fold action term accu =
    match term with
    | TPresent
    | TAbsent
    | TUnit ->
	accu
    | TArrow (left, right) ->
	action right (action left accu)
    | TSet arg ->
	action arg accu
    | TPresent1 arg ->
	action arg accu
    | TSum (left, right) ->
	action right (action left accu)

  exception Iter2

  let iter2 action term1 term2 =
    match term1, term2 with
    | TPresent, TPresent
    | TAbsent, TAbsent
    | TUnit, TUnit ->
	()
    | TArrow (left1, right1), TArrow (left2, right2) ->
	action left1 left2;
	action right1 right2
    | TSet arg1, TSet arg2 ->
	action arg1 arg2
    | TPresent1 arg1, TPresent1 arg2 ->
	action arg1 arg2
    | TSum (left1, right1), TSum (left2, right2) ->
	action left1 left2;
	action right1 right2
    | _, _ ->
	raise Iter2

  let arrow left right =
    TArrow(left, right)

  type symbol =
    | SPresent
    | SAbsent
    | SPresent1

  let matches symbol term =
    match symbol, term with
    | SPresent, TPresent
    | SAbsent, TAbsent
    | SPresent1, TPresent1 _ ->
	true
    | _, _ ->
	false

  let sprint = function
    | SPresent
    | SPresent1 ->
	"Pre"
    | SAbsent ->
	"Abs"

end

module Dimension = struct

  type t = unit
  let compare () () = 0
  let print () = ""

end

module Label = struct

  type t = string
  let compare : string -> string -> int = compare
  let print s = s

end

module System = Herbrand.Make(Label)(Dimension)(GroundSig)

module Primitives = struct

  type name =
    | PrimUnit
    | PrimEnsureUnit
    | PrimSetEmpty
    | PrimSetExtend of string
    | PrimSetRestrict of string
    | PrimSetMemberAssert of string
    | PrimSetMemberTest of string
    | PrimSetModify of string
    | PrimNormal
    | PrimExc
    | PrimMatch
    | PrimChoice

  type node =
      System.node

  open GroundSig
  open System

  (* Define term construction helpers. Given the (arguably natural) way these helpers are written, constant type
     terms are shared, i.e. they are only allocated once at the lowest level, and never generalized. *)

  let unit =
    term TUnit

  let absent =
    term TAbsent

  let present =
    term TPresent

  let set x =
    term (TSet x)

  let emptyset =
    set absent

  let arrow x y =
    term (TArrow (x, y))

  let sum x y =
    term (TSum (x, y))

  let present1 x =
    term (TPresent1 x)

  (* This flag determines how conditional constraints should be used in the type
     of the set membership test. *)

  type condflag =
    | None
    | InputOnly
    | InputOutput

  let flag =
    InputOutput

  (* Define typing rules for all primitive operations. *)

  let rule_set_touch label x =
    let rho = fresh() in
    let row1 = juxtapose () label (fresh()) rho
    and row2 = juxtapose () label x rho in
    [ set row1 ], set row2

  let rule mono = function
    | PrimUnit ->
	[], unit
    | PrimEnsureUnit ->
	[unit], unit
    | PrimSetEmpty ->
	[], emptyset
    | PrimSetExtend label ->
	rule_set_touch label present
    | PrimSetRestrict label ->
	rule_set_touch label absent
    | PrimSetMemberAssert label ->
	let row = juxtapose () label present (fresh()) in
	[set row], unit
    | PrimSetMemberTest label ->
	let rho = fresh()
	and phi = fresh() in
	let arg1 = set (juxtapose () label phi rho) in
	let rho_yes = fresh()
	and alpha_yes = fresh() in
	let arg2 = arrow (set (juxtapose () label present rho_yes)) alpha_yes in
	let rho_no = fresh()
	and alpha_no = fresh() in
	let arg3 = arrow (set (juxtapose () label absent  rho_no )) alpha_no  in
	let alpha = fresh() in
	begin
	  match flag with
	  | None
	  | InputOnly ->
	      unify alpha alpha_yes;
	      unify alpha alpha_no
	  | InputOutput ->
	      conditionally phi SPresent alpha alpha_yes;
	      conditionally phi SAbsent alpha alpha_no
	end;
	begin
	  match flag with
	  | None ->
	      unify rho rho_yes;
	      unify rho rho_no
	  | InputOnly 
	  | InputOutput ->
	      conditionally phi SPresent rho rho_yes;
	      conditionally phi SAbsent rho rho_no;
	end;
	[ arg1 ], arrow arg2 (arrow arg3 alpha)
    | PrimSetModify label ->
	let rho = fresh()
	and alpha = fresh() in
	let row1 = juxtapose () label (fresh()) rho in
	let row2 = juxtapose () label alpha (fresh()) in
	let row3 = juxtapose () label alpha rho in
	[ set row1 ], arrow (set row2) (set row3)
    | PrimNormal ->
	let alpha = fresh()
	and beta = fresh() in
	[], arrow alpha (sum (present1 alpha) beta)
    | PrimExc ->
	[], sum (fresh()) present
    | PrimMatch ->
	let phi = fresh()
	and psi = fresh()
	and alpha = fresh()
	and beta = fresh()
	and gamma = fresh()
	and delta = fresh() in
	let arg1 = sum phi psi
	and arg2 = arrow alpha beta
	and arg3 = arrow unit gamma in
	conditionally phi SPresent1 phi (present1 alpha);
	conditionally phi SPresent1 beta delta;
	conditionally psi SPresent gamma delta;
	[], arrow arg1 (arrow arg2 (arrow arg3 delta))
    | PrimChoice ->
	let alpha = fresh() in
	[ alpha; alpha ], alpha

end

module Hm = Hmx.Make(Primitives)(System)(Primitives)

