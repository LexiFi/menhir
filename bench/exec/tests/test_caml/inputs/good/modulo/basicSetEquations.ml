(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/basicSetEquations.ml,v 1.5 2003/01/13 12:14:28 fpottier Exp $ *)

(* This module provides a solver for equations involving set constants,
   variables, and disjoint sums (i.e. unions) thereof. *)

module type S = sig

  type set
  type term

  (* [variable ()] returns a fresh variable. *)

  val variable: unit -> term

  (* [empty] is a term which represents the empty set. *)

  val empty: term

  (* [sum s t] returns a term which represents the disjoint union of the
     constant set [s] and the term [t]. The fact that they should be disjoint
     is recorded and enforced. [Error] is raised if it is immediately apparent
     that the sum is not disjoint. *)

  exception Error

  val sum: set -> term -> term

  (* [unify] unifies its arguments, that is, forces them to have the same
     denotation. [Error] is raised if the equations become unsolvable as a
     result. *)

  val unify: term -> term -> unit

  (* [default t] replaces any variable which appears (unconstrained) within
     the term [t] with the empty set. *)

  val default: term -> unit

  (* [print t] provides a textual representation of the term [t]. *)

  val print: term -> string

end

module Make (Set : sig

  (* The type of sets. *)

  type t

  (* [empty] is the empty set. *)

  val empty: t

  (* [union] returns the union of two sets, which may safely be assumed
     disjoint. *)

  val union: t -> t -> t

  (* [inter] returns the intersection of two sets. *)

  val inter: t -> t -> t

  (* [diff] returns the difference of two sets. *)

  val diff: t -> t -> t

  (* [is_empty] tells whether a set is empty. *)

  val is_empty: t -> bool

  (* [equal] tells whether two sets are equal. *)

  val equal: t -> t -> bool

  (* [print s] provides a textual representation of the set [s]. *)

  val print: t -> string

end) = struct

  open UnionFind

  (* [disjoint] tells whether two sets are disjoint. *)

  let disjoint s1 s2 =
    Set.is_empty (Set.inter s1 s2)

  (* [subset] tells whether two sets are in a subset relationship. *)

  let subset s1 s2 =
    Set.is_empty (Set.diff s1 s2)

  (* A term is a variable $\alpha$, a constant set $L$, or a disjoint sum
     $L\oplus\alpha$, where $L$ is non-empty.

     Each variable carries a set of labels which it is not allowed to
     contain. In other words, if the variable $\alpha$ carries the set $L$,
     then we must satisfy $\alpha\subseteq\neg L$.

     We maintain the following invariant: whenever we form a disjoint sum
     $L\oplus\alpha$, we add $L$ to $\alpha$'s annotation. In other words,
     whenever a term $L\oplus\alpha$ exists, the constraint
     $\alpha\subseteq\neg L$ also exists. *)

  type set =
      Set.t

  type term =
      descriptor point

  and descriptor =
    | Variable of set
    | Constant of set
    | DisjointSum of set * term

  (* [normalize node] normalizes [node] so that its descriptor is a variable,
     a constant, or a disjoint sum of a constant and a variable. It returns
     the descriptor. *)

  let rec normalize node =
    match find node with
    | DisjointSum (s1, node2) as desc -> (
	match normalize node2 with
	| Constant s2 ->
	    change node (Constant (Set.union s1 s2))
	| DisjointSum (s2, tail2) ->
	    change node (DisjointSum (Set.union s1 s2, tail2))
	| Variable _ ->
	    desc
      )
    | desc ->
	desc

  (* [impose restriction node] makes sure that the (denotation of the) term
     [node] and the constant set [restriction] are disjoint, by imposing an
     appropriate constraint on [node]'s free variable, if necessary. If this
     requirement is violated, then [Error] is raised. *)

  exception Error

  let rec impose restriction node =
    match normalize node with
    | Constant s ->
	if not (disjoint s restriction) then
	  raise Error
    | Variable forbidden ->
	let forbidden' = Set.union forbidden restriction in
	if not (Set.equal forbidden forbidden') then
	  ignore (change node (Variable forbidden'))
    | DisjointSum (s, node) ->
	if not (disjoint s restriction) then
	  raise Error;
	impose restriction node

  (* [check x node] checks that the variable [x] does not occur within the
     term [node], and raises [Error] otherwise. *)

  let check x node =
    match normalize node with
    | Constant _ ->
	()
    | Variable _ ->
	if equivalent x node then
	  raise Error
    | DisjointSum (_, node) ->
	if equivalent x node then
	  raise Error

  (* Constructors. *)

  let variable forbidden =
    fresh (Variable forbidden)

  let constant s =
    fresh (Constant s)

  let empty =
    constant Set.empty

  let _sum s node =
    impose s node;
    fresh (DisjointSum (s, node))

  let sum s node =
    if Set.is_empty s then
      node
    else
      _sum s node

  (* [unify] unifies its arguments, that is, forces them to have the same
     denotation. [Error] is raised if the equations become unsolvable as a
     result. *)

  let rec unify node1 node2 =
    if not (equivalent node1 node2) then
      match normalize node1, normalize node2, node1, node2 with
      | Variable forbidden1, _, node1, node2
      |	_, Variable forbidden1, node2, node1 ->
	  impose forbidden1 node2;
	  check node1 node2;
	  union node1 node2
      | Constant s1, Constant s2, _, _ ->
	  if not (Set.equal s1 s2) then
	    raise Error;
	  union node1 node2
      | Constant s1, DisjointSum (s2, tail2), node1, node2
      |	DisjointSum (s2, tail2), Constant s1, node2, node1 ->
	  if not (subset s2 s1) then
	    raise Error;
	  unify tail2 (constant (Set.diff s1 s2));
	  union node2 node1
      | DisjointSum (s1, tail1), DisjointSum (s2, tail2), _, _ ->
	  let s1 = Set.diff s1 s2
	  and s2 = Set.diff s2 s1 in
	  begin
	    match Set.is_empty s1, Set.is_empty s2 with
	    | true, true ->
		unify tail1 tail2
	    | false, true ->
		unify (_sum s1 tail1) tail2
	    | true, false ->
		unify tail1 (_sum s2 tail2)
	    | false, false ->
		let tail = variable (Set.union s1 s2) in
		unify tail1 (_sum s2 tail);
		unify tail2 (_sum s1 tail)
	  end;
	  union node1 node2

  (* [default t] replaces any variable which appears (unconstrained) within
     the term [t] with the empty set. *)

  let rec default node =
    match normalize node with
    | Variable _ ->
	union node empty
    | DisjointSum (_, node) ->
	union node empty
    | Constant _ ->
	()

  (* A simplified version of [variable]. *)

  let variable () =
    variable Set.empty

  (* A pretty-printer. *)

  let i2s =
    Name.i2s 'L' 'L'

  let name =
    let module N = Name.Make (struct
      type t = term
      let equal = equivalent
    end) in
    N.name

  let print node =
    match normalize node with
    | Variable _ ->
	i2s (name node)
    | Constant set ->
	Set.print set
    | DisjointSum (set, node) ->
	Printf.sprintf "(%s+%s)" (Set.print set) (i2s (name node))

end

