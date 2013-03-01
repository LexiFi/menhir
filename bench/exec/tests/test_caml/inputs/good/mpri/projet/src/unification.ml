open Format
open AbstractSyntax

(* ------------------------------------------------------------------------- *)

(* Unification variables are represented as integers. *)

type variable =
    int

module Var = struct
  type t = variable
  let equal = (=)
  module Map = Map.Make (struct
    type t = variable
    let compare = compare
  end)
  let print pp x = fprintf pp "'a%d" x
end

(* Our first-order terms are types with variables at the leaves. *)

type term =
    variable ptyp

(* A substitution maps variables to terms. *)

type substitution =
    term Var.Map.t

(* A unification problem is a pair of a domain (a set of variables) and
   a conjunction of term equations. Unification problems must be closed,
   that is, every variable that appears within an equation must appear
   in the domain as well. *)

type equations =
    (term * term) list

type problem =
  | Problem of variable list * equations

(* ------------------------------------------------------------------------- *)

(* A term printer. *)

let print pp t =
  Type.print Var.print pp t

(* ------------------------------------------------------------------------- *)

(* [Build] helps construct unification problems in a pseudo-imperative style. *)

module Build (X : sig end) = struct

  let vs =
    ref []

  let eqs =
    ref []

  let c =
    ref 0

  let fresh () =
    let v = !c in
    c := v + 1;
    vs := v :: !vs;
    v

  let (=?=) t1 t2 =
    eqs := (t1, t2) :: !eqs

  let finished =
    ref false

  let finished () =
    assert (not !finished);
    finished := true;
    Problem (!vs, !eqs)

end

(* ------------------------------------------------------------------------- *)

(* First-order unification is performed by grouping variables into
   equivalence classes, and associating with each equivalence class a
   descriptor, where a descriptor is an optional, non-variable
   first-order term.

   When two equivalence classes are merged, the [union] of their
   descriptors is computed, giving rise to zero or one equations
   between terms. For this reason, we define the type [accumulator] as
   a conjunction of equations between terms. *)

module Desc = struct

  type descriptor =
      (* non-variable *) term option

  let default =
    None

  type accumulator =
      equations

  let union desc1 desc2 eqs =
    match desc1, desc2 with
    | None, desc
    | desc, None ->
	desc, eqs
    | Some t1, Some t2 ->
	desc1, (t1, t2) :: eqs

end

(* ------------------------------------------------------------------------- *)

(* The heart of unification. *)

module U =
  UnionFind.Make (Var) (Desc)

exception Mismatch

let rec unify (state : U.state) (eqs : equations) =
  match eqs with
  | [] ->
      state
  | (t1, t2) :: eqs ->
      match t1, t2 with
      | TVar v1, TVar v2 ->
	  let state, eqs = U.union v1 v2 state eqs in
	  unify state eqs
      | TVar v1, term2
      | term2, TVar v1 ->
	  let descriptor, eqs = Desc.union (U.descriptor v1 state) (Some term2) eqs in
	  let state = U.set v1 descriptor state in
	  unify state eqs
      | TArrow (t11, t12), TArrow (t21, t22) ->
	  unify state ((t11, t21) :: (t12, t22) :: eqs)
      | TConApp (tc1, ts1), TConApp (tc2, ts2) when tc1 = tc2 ->
	  unify state (List.combine ts1 ts2 @ eqs)
      | _, _ ->
	  raise Mismatch

(* ------------------------------------------------------------------------- *)

(* The read back phase turns the state of the union-find algorithm
   back into a most general unifier, that is, a mapping of variables
   to terms. This is essentially a graph traversal process,
   implemented using depth-first search. An acyclicity check is
   performed on the fly. *)

exception OccurCheck

let read_back (vs : variable list) (state : U.state) : substitution =

  (* [visited] is the set of vertices that have been or are being
     visited. [mgu] is a mapping of the vertices that have been
     visited to terms. *)

  let visited : unit Var.Map.t ref =
    ref Var.Map.empty
  and mgu : substitution ref = 
    ref Var.Map.empty
  in

  let rec dfs v =
    if Var.Map.mem v !visited then begin
      try
	Var.Map.find v !mgu
      with Not_found ->
	raise OccurCheck
    end
    else begin
      visited := Var.Map.add v () !visited;
      let t : term =
	match U.descriptor v state with
	| None ->
	    TVar (U.representative v state)
	| Some t ->
	    Type.lift dfs t
      in
      mgu := Var.Map.add v t !mgu;
      t
    end
  in

  List.iter (fun v ->
    let _ = dfs v in
    ()
  ) vs;

  !mgu

(* ------------------------------------------------------------------------- *)

(* Solving a unification problem involves: (i) dealing with each equation in
   turn, building a graph of type [U.state]; and (ii) applying the read-back
   procedure to obtain a most general unifier. *)

let mgu (Problem (vs, eqs)) : substitution =
  read_back vs (unify U.initial eqs)

