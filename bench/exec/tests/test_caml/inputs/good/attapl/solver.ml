(* $Header: /home/pauillac/cristal5/remy/repository/home/tex/mlrow/code/solver.ml,v 1.23 2004/04/13 16:21:25 fpottier Exp $ *)

(* TEMPORARY clarifier terminologie `multi-equation' et `variable' *)

(** This module provides a constraint solver. It is parameterized by
    a row algebra. *)

open Misc
open Sig

module Make
    (RowAlgebra : RowAlgebra)
= struct

  (** The algebra associated with this solver. *)
  module A = RowAlgebra

  (** This module defines integer ranks.

      The constraint solver assigns a nonnegative integer rank to
      every type variable that is currently in scope, that is, every
      type variable in [dtv(S)], where [S] is the current stack. The
      rank of a variable bound at the outermost level is 0; the rank
      of a variable bound at the kth [let] frame down the stack is k.
      Type variables that are not currently in scope include the
      universal quantifiers of type schemes that appear in environment
      frames, which by convention have rank [none], as well as type
      variables that are bound inside an external constraint (that is,
      a constraint that has not yet been examined by the solver),
      whose rank is irrelevant. *)
  module IntRank = struct

    type t = int

    (** [compare] is the usual total ordering on integers. *)
    let compare = (-)

    (** [none] is the special value used to identify the universal
        quantifiers of a type scheme in an environment frame. Such
        variables are not part of [dtv(S)], that is, the type variables
        currently in scope, which explains why they do not carry a
        nonnegative integer rank. *)
    let none = -1

    (** [outermost] is the rank assigned to variables that are
        existentially bound at the outermost level. *)
    let outermost = 0

  end

  (* Create a unifier. *)

  module U = Unifier.Make(RowAlgebra)(IntRank)

  open U

  type variable =
      U.variable

  type name =
      string

  type crterm =
      variable A.arterm

  type pool = {
      number: int; (** The present pool's rank. *)
      mutable inhabitants: variable list
    } 
  (* TEMPORARY ne pas oublier d'expliquer que les rangs et les pools ne sont
     pas toujours d'accord entre eux *)

  type environment =
    | EEmpty
    | EEnvFrame of environment * name * variable
  (* TEMPORARY expliquer qu' on emploie la pile native pour les let et conj. frames,
     plus des pools s'epar'es pour les let frames *)

  (* Printing support. *)

  module P = Print.Make(U)
    (* TEMPORARY P.reset becomes inaccessible *)

  let rec print_env = function
    | EEmpty ->
	""
    | EEnvFrame (env, name, v) ->
	(print_env env) ^ "val " ^ name ^ ": " ^ (P.print true v) ^ "\n"

  let rec print_crterm = function
    | A.TVariable v ->
	None, P.print false v
    | A.TTerm term ->
	let symbol, string =
	  A.print (fun label son ->
	    match print_crterm son with
	    | Some symbol, string when A.parenthesize label symbol ->
		"(" ^ string ^ ")"
	    | _, string ->
		string
	  ) term
	in
	Some symbol, string

  let print_crterm t =
    snd (print_crterm t)

  let rec print_constraint = function
    | CTrue
    | CConjunction [] ->
	"true"
    | CDump ->
	"dump"
    | CEquation (t1, t2) ->
	print_crterm t1 ^ " =?= " ^ print_crterm t2
    | CConjunction (c :: cs) ->
	List.fold_left (fun s c -> s ^ " and " ^ print_constraint c) (print_constraint c) cs
    | CLet ([ Scheme ([], fqs, c, _) ], CTrue) ->
	List.fold_left (fun s q -> s ^ " " ^ (P.print false q)) "exists" fqs ^ " . (" ^ print_constraint c ^ ")"
    | CLet (schemes, c) ->
	"let " ^
	print_schemes schemes ^
	" in (" ^
	print_constraint c ^ ")"
    | CInstance (name, t) ->
	name ^
	" < " ^
	print_crterm t

  and print_schemes schemes =
    print_separated_list "; " print_scheme schemes

  and print_scheme (Scheme (rqs, fqs, c, header)) =
    List.fold_left (fun s q -> s ^ " " ^ (P.print false q))
    (List.fold_left (fun s q -> s ^ " [" ^ (P.print false q) ^ "]") "forall" rqs) fqs ^
    "[" ^ print_constraint c ^ "](" ^
    StringMap.fold (fun name t s ->
      s ^ (if s = "" then "" else " and ") ^ name ^ " : " ^ print_crterm t
    ) header "" ^
    ")"

  (** This exception is raised when a constraint is found to be unsatisfiable. *)
  exception Inconsistency =
    U.Inconsistency

  (** [new_pool pool] returns a new empty pool, whose number is the successor
      of [pool]'s. *)
  let new_pool pool = {
    number = pool.number + 1;
    inhabitants = []
  } 

  (** [register pool v] adds [v] to the pool [pool]. It is assumed that [v]'s
      rank is already set, so it is not modified. *)
  let register pool v =
    pool.inhabitants <- v :: pool.inhabitants

  (** [introduce pool v] adds [v] to the pool [pool]. [v]'s rank is set to the
      pool's number. It is assumed that it was previously uninitialized. *)
  let introduce pool v =
    let desc = UnionFind.find v in
    assert (desc.rank = IntRank.none);
    desc.rank <- pool.number;
    register pool v

  (** [chop pool term] adds appropriate fresh variables and
      multi-equations to the environment and returns a variable
      which, according to these multi-equations, is equal to the term
      [term]. In other words, it turns a term of arbitrary depth into
      a variable, together with a number of multi-equations of depth
      at most one. *)
  let rec chop pool = function
    | A.TVariable v ->
	v
    | A.TTerm term ->
	let v = UnionFind.fresh { (* TEMPORARY invoquer une fonction de c'reation dans Unifier? *)
	  structure = Some (A.map (chop pool) term);
	  rank = pool.number;
	  mark = Mark.none
	} in
	register pool v;
	v

  (* [generalize] *)

  let generalize old_pool young_pool =

    (* We examine the variables in the young pool and immediately drop those
       which have become aliases for other (old or young) variables, using
       [UnionFind.redundant]. The variables that remain are sorted by rank
       using a simple bucket sort mechanism. (Recall that every variable in
       the young pool must have rank less than or equal to the pool's number.)
       They are also marked as ``young'', so as to be identifiable in constant
       time. *)

    let young_number =
      young_pool.number in

    let sorted =
      Array.create (young_number + 1) [] in

    let young =
      Mark.fresh() in

    List.iter (fun v ->
      if not (UnionFind.redundant v) then
	let desc = UnionFind.find v in
	desc.mark <- young;
	let rank = desc.rank in
	sorted.(rank) <- v :: sorted.(rank)
    ) young_pool.inhabitants;

    (* Next, we update the ranks of the young variables that remain. One goal
       is to ensure that if [v1] is dominated by [v2], then the rank of [v1]
       is less than or equal to the rank of [v2], or, in other words, that
       ranks are nonincreasing along any path down the structure of terms.
       The second goal is to ensure that the rank of every young variable is
       exactly the maximum of the ranks of the variables that it dominates,
       if there are any.

       The process consists of several depth-first traversals of the forest
       whose entry points are the young variables. Traversals stop at old
       variables. Roughly speaking, the first goal is achieved on the way
       down, while the second goal is achieved on the way back up.

       During each traversal, every visited variable is marked as such, so as
       to avoid being visited again. To ensure that visiting every variable
       once is enough, traversals whose starting point have lower ranks must
       be performed first. In the absence of cycles, this enforces the
       following invariant: when performing a traversal whose starting point
       has rank [k], every variable marked as visited has rank [k] or less
       already. (In the presence of cycles, this algorithm is incomplete and
       may compute ranks that are slightly higher than necessary.) Conversely,
       every non-visited variable must have rank greater than or equal to
       [k]. This explains why [k] does not need to be updated while going
       down. *)

    let visited =
      Mark.fresh() in

    for k = 0 to young_number do

      let rec traverse v =
	let desc = UnionFind.find v in

	(* If the variable is young and was not visited before, we immediately
	   mark it as visited (which is important, since terms may be cyclic).
	   If the variable has no structure, we set its rank to [k]. If it has
	   some structure, we first traverse its sons, then set its rank to the
	   maximum of their ranks. *)

	if Mark.same desc.mark young then begin
	  desc.mark <- visited;
	  desc.rank <- match desc.structure with
	  | None ->
	      k
	  | Some term ->
	      A.fold (fun son accu ->
		max (traverse son) accu
	      ) term IntRank.outermost
	end

	(* If the variable isn't marked ``young'' or ``visited'', then it must
	   be old. Then, we update its rank, but do not pursue the computation
	   any further. *)

	else if not (Mark.same desc.mark visited) then begin
	  desc.mark <- visited;
	  if k < desc.rank then
	    desc.rank <- k
	end;

	(* If the variable was visited before, we do nothing. *)

	(* In either case, we return the variable's current (possibly updated)
	   rank to the caller, so as to allow the maximum computation above. *)

	desc.rank

      in
      Misc.iter traverse sorted.(k)

    done;

    (* The rank of every young variable has now been determined as precisely
       as possible.

       Every young variable whose rank has become strictly less than the
       current pool's number may be safely turned into an old variable. We do
       so by moving it into the previous pool. In fact, it would be safe to
       move it directly to the pool that corresponds to its rank. However, in
       the current implementation, we do not have all pools at hand, but only
       the previous pool.

       Every young variable whose rank has remained equal to the current
       pool's number becomes universally quantified in the type scheme that is
       being created. We set its rank to [none]. *)

    for k = 0 to young_number - 1 do
      List.iter (register old_pool) sorted.(k)
    done;

    List.iter (fun v ->
      let desc = UnionFind.find v in
      if desc.rank < young_number then
	register old_pool v
      else
	desc.rank <- IntRank.none
    ) sorted.(young_number)

    (* TEMPORARY time to perform the occur check on the variables which we
       just ranked [none]. On peut 'eventuellement integrer l'occur check
       a la derniere passe de realisation? *)

    (* Every variable that was initially a member of [young_pool] may now be
       viewed as the entry point of a type scheme. The body of the type scheme
       is the term obtained by traversing the structure below the entry
       point. The type scheme's universally quantified variables have rank
       [none], while its free variables have nonnegative ranks. 

       Note that, when considering several such variables, the type schemes
       that they represent may share some of their structure. No copying is
       involved. *)

  (* [instance] *)

  let instance pool v =

    (* [get], [set], and [setp] implement a constant-time mapping from
       descriptors of rank [none] to variables. [setp] allows determining
       whether a given descriptor belongs to the domain of the
       mapping. [set] and [get] respectively allow extending and
       looking up the mapping.

       In order to implement a constant-time mapping without wasting extra
       space, we re-use the descriptor's [rank] field, which is redundant at
       this point, since its value must be [none], and store a pointer in
       it. The field is to be viewed as containing a pointer if and only if
       the descriptor is marked with [m]. *)

    let m =
      Mark.fresh() in

    let setp desc =
      Mark.same desc.mark m

    and set desc v =
      desc.mark <- m;
      desc.rank <- Obj.magic (v : variable)

    and get desc =
      (Obj.magic desc.rank : variable) in

    (* If [v] has rank [none], then [copy v] returns a copy of the variable
       [v], and copies its descendants recursively. The copy is registered in
       the current pool and given the current rank. If [v] has nonnegative
       rank, then [copy v] returns [v]. Only one copy per variable is created,
       even if a variable is found twice during the traversal. *)

    let rec copy v =
      let desc = UnionFind.find v in

      (* If a copy has been created already for this variable, return it. We
	 must check this condition first, since we must not read [desc.rank]
	 unless we know that the variable hasn't been copied yet. *)

      if setp desc then
	get desc

      (* Otherwise, check the variable's rank. If it is other than [none],
	 then the variable must not be copied. *)

      else if desc.rank <> IntRank.none then
	v

      (* Otherwise, the variable must be copied. We create a new variable,
	 update the mapping, then update the new variable's descriptor. Note
	 that the mapping must be updated before making a recursive call to
	 [copy], so as to guarantee termination in the presence of cyclic
	 terms. *)

      else
	let desc' = {
	  structure = None;
	  rank = pool.number;
	  mark = Mark.none
	} in
	let v' = UnionFind.fresh desc' in
	register pool v';
	set desc v';
	match desc.structure with
	| None ->
	    v'
	| Some term ->
	    desc'.structure <- Some (A.map copy term);
	    v'

    (* If [v] was effectively copied by [copy], then [restore v] returns
       [v] to its original state (that is, restores its rank to [none])
       and restores its descendants recursively. If [v] was not copied,
       [restore v] has no effect. *)

    and restore v =
      let desc = UnionFind.find v in
      if setp desc then begin
	desc.mark <- Mark.none;
	desc.rank <- IntRank.none;
	match desc.structure with
	| None ->
	    ()
	| Some term ->
	    A.iter restore term
      end

    in

    (* We are now ready to take an instance of the type scheme whose
       entry point is [v]. It is simply a matter of copying [v] and
       its descendants, stopping at non-universally-quantified nodes.
       The copy process affects the type scheme, which must be restored
       afterwards. The whole process is linear in the size of the type
       scheme, that is, in the number of universally quantified nodes. *)

    let v' = copy v in
    restore v;
    v'

  (** [distinct_variables vl] checks that the variables in the list [vl]
      belong to distinct equivalence classes and that their structure is
      [None]. In other words, they do represent distinct (independent)
      variables (as opposed to nonvariable terms). *)
  let distinct_variables vl =
    let m = Mark.fresh() in
    List.iter (fun v ->
      let desc = UnionFind.find v in
      match desc.structure with
      |	None ->
	  if Mark.same desc.mark m then
	    raise Inconsistency;
	  desc.mark <- m
      |	Some _ ->
	  raise Inconsistency
    ) vl

  (** [generic_variables vl] checks that every variable in the list [vl]
      has rank [none]. *)
  let generic_variables vl =
    List.iter (fun v ->
      let desc = UnionFind.find v in
      if desc.rank <> IntRank.none then
	raise Inconsistency
    ) vl

  (** This exception is raised when an unbound identifier is found. *)
  exception UnboundIdentifier of name

  (** [lookup name env] looks for a definition of [name] within
      the environment [env]. *)
  let rec lookup name = function
    | EEnvFrame (env, name', scheme) ->
	if name = name' then scheme
	else lookup name env
    | EEmpty ->
	raise (UnboundIdentifier name)

  (* [solve] *)

  let rec solve env pool = function
    | CTrue ->
	()
    | CDump ->
	Printf.fprintf stdout "%s" (print_env env)
    | CEquation (term1, term2) ->
	unify (register pool) (chop pool term1) (chop pool term2)
    | CConjunction cl ->
	List.iter (solve env pool) cl
    | CLet ([ Scheme ([], fqs, c, _) ], CTrue) ->
	(* This encodes an existential constraint. In this restricted
	   case, there is no need to stop and generalize. The code
	   below is only an optimization of the general case. *)
        (* TEMPORARY traiter un cas plus general que celui-ci? *)
	List.iter (introduce pool) fqs;
	solve env pool c
    | CLet (schemes, c2) ->
	let env' = List.fold_left (fun env' scheme ->
	  concat env' (solve_scheme env pool scheme)
        ) env schemes in
	solve env' pool c2
    | CInstance (name, term) ->
	unify (register pool) (instance pool (lookup name env)) (chop pool term)

  and solve_scheme env pool = function

    | Scheme ([], [], c1, header) ->

	(* There are no quantifiers. In this restricted case, there is no need
	   to stop and generalize. This is only an optimization of the general
	   case. *)

	solve env pool c1;
	StringMap.map (chop pool) header

    | Scheme (rqs, fqs, c1, header) ->

	(* The general case. *)

	let pool' = new_pool pool in
	List.iter (introduce pool') rqs;
	List.iter (introduce pool') fqs;
	let header = StringMap.map (chop pool') header in
	solve env pool' c1;
	distinct_variables rqs;
	generalize pool pool';
	generic_variables rqs;
	header

  and concat env header =
    StringMap.fold (fun name v env ->
      EEnvFrame (env, name, v)
    ) header env

  (** [init] produces a fresh initial state. It consists of an empty
      environment and a fresh, empty pool. *)
  let init () =
    EEmpty, {
      number = IntRank.outermost;
      inhabitants = []
    }

  (** The public version of [solve] starts out with an initial state
      and produces no result, except possibly an exception. *)
  let solve c =
    let env, pool = init() in
    solve env pool c (* TEMPORARY integrer un occur check ici aussi *)

  (** [variable()] returns a new variable. *)
  let variable = U.variable

  (** [variable_list xs] allocates a fresh variable for every element in the list [xs],
      and returns both a list of these variables and an association list that maps
      elements to variables, viewed as types. *)
  let variable_list xs =
    List.fold_right (fun x (vs, xts) ->
      let v = variable() in
      v :: vs, (x, A.TVariable v) :: xts
    ) xs ([], [])

  (** [variable_set xs] allocates a fresh variable for every element in the
      set [xs], and returns both a list of these variables and a map of
      elements to variables, viewed as types. *)
  let variable_set xs =
    StringSet.fold (fun x (vs, xts) ->
      let v = variable() in
      v :: vs, StringMap.add x (A.TVariable v) xts
    ) xs ([], StringMap.empty)

  (** [t1 =?= t2] is an equality constraint. *)
  let (=?=) t1 t2 =
    CEquation (t1, t2)

  (** [c1 ^ c2] is a conjunction constraint. The implementation performs
      some simple (unnecessary) optimizations. *)
  let (^) c1 c2 =
    match c1, c2 with
    | CTrue, c
    | c, CTrue ->
	c
    | CConjunction cl1, CConjunction cl2 ->
	CConjunction (List.rev_append cl1 cl2)
    | CConjunction cl, c
    | c, CConjunction cl ->
	CConjunction (c :: cl)
    | _, _ ->
	CConjunction [c1; c2]

  (* [ex qs c] returns the constraint [exists qs.c]. We encode existential
     constraints in terms of [let] constraints, since the latter are more
     general. *)
  let ex qs c =
    CLet ([ Scheme ([], qs, c, StringMap.empty) ], CTrue)

  (* [fl qs c] returns the constraint [forall qs.c]. We encode universal
     constraints in terms of [let] constraints, since the latter are more
     general. *)
  let fl qs c =
    CLet ([ Scheme (qs, [], c, StringMap.empty) ], CTrue)

  (** [exists f] creates a fresh variable [v] and returns the constraint
      [exists v.(f v)]. *)
  let exists f =
    let v = variable() in
    ex [ v ] (f (A.TVariable v))

  (** [exists_list l f] associates a fresh variable with every element
      in the list [l], yielding an association list [m], and returns
      the constraint [exists m.(f m)]. *)
  let exists_list l f =
    let l, m = variable_list l in
    ex l (f m)

  (** [forall_list l f] associates a fresh variable with every element
      in the list [l], yielding an association list [m], and returns
      the constraint [forall m.(f m)]. *)
  let forall_list l f =
    let l, m = variable_list l in
    fl l (f m)

  (** [exists_set names f] associates a fresh variable with every name in
      the set [names], yielding a map [m] of names to variables, and returns
      the constraint [exists m.(f m)]. *)
  let exists_set names f =
    let l, m = variable_set names in
    ex l (f m)

  (** [monoscheme header] turns [header] into a monomorphic type scheme. *)
  let monoscheme header =
    Scheme ([], [], CTrue, header)

  (** [scheme rqs names f] associates a fresh variable with every name in
      the set [names], yielding a map [m] of names to variables, and returns
      the type scheme [forall rqs m [f m] m], where the variables in [rqs]
      are rigid and the variables in [m] are flexible. *)
  let scheme rqs names f =
    let l, m = variable_set names in
    Scheme (rqs, l, f m, m)

end

