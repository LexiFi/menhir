(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/kindUnification.ml,v 1.4 2003/01/13 12:14:28 fpottier Exp $ *)

(* This module provides kind unification. Our kinds are the types of the
   simply-typed, label-selective $\lambda$-calculus, extended with row
   kinds. A row kind is parameterized by a set of row labels, represented here
   as an object of type [RowDomain.term]. *)

open Id
open UnionFind

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Data structures. *)

type row_label =
    id

module RowDomain =
  BasicSetEquations.Make (struct

    include IdSet

    let print s =
      try
	let label = choose s in
	fold (fun label accu ->
	  label ^ "+" ^ accu
        ) (remove label s) label
      with Not_found ->
	""

  end)

type channel =
    id

type kind =
    descriptor point

and descriptor =
  | KVariable
  | KStar
  | KArrow of parameters * kind
  | KRow of RowDomain.term

and parameters =
    queue list (* ordered by channel name *)

(* A \emph{queue} consists of a channel name, together with a non-empty
   list of kinds, whose order is relevant. *)

and queue =
    channel * kind * kind list

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Constructors. *)

let variable () =
  fresh KVariable

let star =
  fresh KStar

let arrow channel k1 k2 =
  fresh (KArrow ([ (channel, k1, []) ], k2))

let row domain =
  fresh (KRow domain)

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Unification. *)

(* [merge] merges two (ordered) lists of queues. When two queues are
   found with the same channel name, the contents of the right-hand
   queue are concatenated to (the right of) those of the left-hand
   queue. *)

let rec merge params1 params2 =
  match params1, params2 with
  | _, [] ->
      params1
  | [], _ ->
      params2
  | ((channel1, kf1, ks1) as queue1) :: rest1, ((channel2, kf2, ks2) as queue2) :: rest2 ->
      let c = compare channel1 channel2 in
      if c = 0 then
	(channel1, kf1, ks1 @ (kf2 :: ks2)) :: (merge rest1 rest2)
      else if c < 0 then
	queue1 :: (merge rest1 params2)
      else
	queue2 :: (merge rest2 params1)

(* [normalize k] ensures that, if [k] is an arrow kind, then its head is not
   itself an arrow. [k] is updated in place. Its (possibly new) descriptor is
   returned. *)

let rec normalize k = 
  match find k with
  | KArrow (params1, head1) as desc1 -> (
      match normalize head1 with
      |	KArrow (params2, head2) ->
	  change k (KArrow (merge params1 params2, head2));
      |	_ ->
	  desc1
    )
  | desc ->
      desc

(* [iter f k] applies [f] at least once to every sub-term of [k]. Note that
   this implementation is (exponentially) inefficient, since it may traverse
   shared sub-trees several times. *) (* TEMPORARY use marks *)

let rec iter f k =
  f k;
  match find k with
  | KVariable
  | KStar
  | KRow _ ->
      ()
  | KArrow (params, head) ->
      List.iter (fun (_, kf, ks) ->
	iter f kf;
	List.iter (iter f) ks
      ) params;
      iter f head

(* [assign k1 k2] calls [alias k1 k2], provided [k1] does not physically
   occur within [k2]. It raises [Wrong] otherwise. *)

exception Wrong

let assign k1 k2 = 
  iter (fun k ->
    if equivalent k1 k then
      raise Wrong
  ) k2;
  union k1 k2

(* [unify] unifies two kinds. *)

let rec unify k1 k2 =
  if not (equivalent k1 k2) then
    match normalize k1, normalize k2 with
    | KVariable, KVariable ->
	union k1 k2
    | KVariable, _ ->
	assign k1 k2
    | _, KVariable ->
	assign k2 k1
    | KStar, KStar ->
	union k1 k2
    | KArrow (params1, head1), KArrow (params2, head2) ->
	unify_arrows params1 head1 params2 head2;
	union k1 k2
    | KRow domain1, KRow domain2 -> (
	try
	  RowDomain.unify domain1 domain2;
	  union k1 k2
	with RowDomain.Error ->
	  raise Wrong
      )
    | KStar, KArrow _
    | KArrow _, KStar
    | (KStar | KArrow _), KRow _
    | KRow _, (KStar | KArrow _) ->
	raise Wrong

(* [unify_arrows params1 head1 params2 head2] unifies the normal arrow kinds
   [KArrow(params1, head1)] and [KArrow(params2, head2)]. Neither [head1] nor
   [head2] must be an arrow. *)

and unify_arrows params1 head1 params2 head2 =
  match params1, params2, head1, head2 with
  | [], [], _, _ ->
      unify head1 head2

  | (channel1, kf1, ks1) :: params1, (channel2, kf2, ks2) :: params2, _, _ when channel1 = channel2 ->

      (* Both arrows await parameters on the same channel. Eliminate them. *)

      unify kf1 kf2;

      let rec eliminate ks1 ks2 =
	match ks1, ks2 with
	| [], [] ->
	    unify_arrows params1 head1 params2 head2
	| kf1 :: ks1, [] ->
	    unify_arrows ((channel1, kf1, ks1) :: params1) head1 params2 head2
	| [], kf2 :: ks2 ->
	    unify_arrows params1 head1 ((channel2, kf2, ks2) :: params2) head2
	| kf1 :: ks1, kf2 :: ks2 ->
	    unify kf1 kf2;
	    eliminate ks1 ks2

      in
      eliminate ks1 ks2

  | ((channel1, kf1, ks1) as queue1) :: params1, params2, head1, head2
  | params2, ((channel1, kf1, ks1) as queue1) :: params1, head2, head1 ->

      (* The left-hand arrow type exhibits a channel which the right-hand
	 arrow does not offer. If both arrows have the same head, this is an
	 error --- this check is required for termination. Otherwise, force
	 that channel to also appear on the right-hand side, and continue.
	 Because the right-hand side cannot be an arrow, our first call to
	 [unify] below will either fail immediately or become a simple call
	 to [assign]. *)

      if equivalent head1 head2 then
	raise Wrong;

      let head2' = fresh KVariable in
      unify head2 (fresh (KArrow ([ queue1 ], head2')));
      unify_arrows params1 head1 params2 head2'

(* The external version of [unify] annotates the exception with the equation
   that was being solved. *)

exception Error of kind * kind

let unify k1 k2 =
  try
    unify k1 k2
  with Wrong ->
    raise (Error (k1, k2))

(* [default k] replaces any (unconstrained) kind variable within [k] with
   the kind [*], and every row domain variable with the full domain. *)

let default k =
  iter (fun k ->
    match find k with
    | KVariable ->
	union k star
    | KRow domain ->
	RowDomain.default domain
    | _ ->
	()
  ) k

(* ----------------------------------------------------------------------------------------------------------------- *)
(* Printing. *)

module T = struct

  open Tree

  type tree =
    | TVariable of id
    | TStar
    | TArrow of channel * tree * tree
    | TRow of string

  type label =
    | LArrowL
    | LArrowR

  let i2s =
    Name.i2s 'k' 'k'
 
  let name =
    let module N = Name.Make (struct
      type t = kind
      let equal = equivalent
    end) in
    N.name

  let arrow channel t1 t2 =
    TArrow (channel, t1, t2)

  let rec convert k =
    match find k with
    | KVariable ->
	TVariable (i2s (name k))
    | KStar ->
	TStar
    | KArrow (params, head) ->
	List.fold_left (fun head (channel, kf, ks) ->
	  arrow channel (convert kf) (List.fold_right (fun k head -> arrow channel (convert k) head) ks head)
        ) (convert head) params
    | KRow domain ->
	TRow (RowDomain.print domain)

  let describe = function
    | TVariable id ->
	[ Token id ]
    | TStar ->
	[ Token "*" ]
    | TArrow (channel, t1, t2) ->
	let tail =
	  [ Son (LArrowL, t1); Token " -> "; Son (LArrowR, t2) ] in
	if channel = "" then
	  tail
	else
	  (Token (channel ^ ":")) :: tail
    | TRow domain ->
	[ Token ("<" ^ domain ^ ">") ]

  let parenthesize label tree =
    match label, tree with
    | LArrowL, TArrow _ ->
	parentheses
    | _, _ ->
	nothing

end

module P = Tree.Make (T)

let print k =
  P.print (T.convert k)

