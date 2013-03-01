(* $Id: solver.ml 50 2007-10-01 14:42:23Z yann.regisgianas $ *)
   

open Position
type position = Position.t
open Misc
open Constraint
open Unifier
open MultiEquation
open CoreAlgebra

module type SolverException =
sig

  (** [TypingError] is raised when an inconsistency is detected during
      constraint solving. *)
  exception TypingError of position

  (** [UnboundIdentifier] is raised when an identifier is undefined in
      a particular context. *)
  exception UnboundIdentifier of position * PIdentifier.t

  (** [CannotGeneralize] when the type of an expression cannot be
      generalized contrary to what is specified by the programmers
      using type annotations. *)
  exception CannotGeneralize of position * variable

  (** [NonDistinctVariables] is raised when two rigid type variables have
      been unified. *)
  exception NonDistinctVariables of position * (variable list)

end

exception TypingError of position
exception UnboundIdentifier of position * PIdentifier.t
exception CannotGeneralize of position * variable
exception NonDistinctVariables of position * (variable list)
exception Inconsistency

type 'syntax tconstraint = 'syntax Constraint.tconstraint

type 'syntax solving_step = 
  | Init of 'syntax tconstraint
  | Solve of 'syntax tconstraint
  | Solved of 'syntax tconstraint
  | UnifyTerms of crterm * crterm
  | UnifyVars of variable * variable
  | Generalize of int * variable list

type 'syntax environment =
  | EEmpty
  | EEnvFrame of 'syntax environment * PIdentifier.t * (variable * 'syntax answer)

and 'syntax answer = 
    | ATerm of 
	'syntax answer list * ('syntax list -> 'syntax) 

    | AType of 
	variable * int * (variable -> int -> 'syntax) 

    | AGraft of
	'syntax answer * variable * int * 
	  ('syntax -> variable -> int -> 'syntax)

    | ATrue

let environment_as_list e = 
  let rec conv acu = function
    | EEmpty -> 
	acu

    | EEnvFrame (env, name, v) -> 
	conv ((name, v)::acu) env
  in
    conv [] e

(** [lookup name env] looks for a definition of [name] within
    the environment [env]. *)
let rec lookup pos name = function
  | EEnvFrame (env, name', (scheme, _)) ->
      if PIdentifier.equal name name' then scheme
      else lookup pos name env

  | EEmpty ->
      raise (UnboundIdentifier (pos, name))

(** [lookup name env] looks for a definition of [name] within
    the environment [env]. *)
let rec lookup_answer pos name = function
  | EEnvFrame (env, name', (_, answer)) ->
      if PIdentifier.equal name name' then answer
      else lookup_answer pos name env

  | EEmpty ->
      raise (UnboundIdentifier (pos, name))

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
    number young_pool in

  let sorted =
    Array.create (young_number + 1) [] in

  let young =
    Mark.fresh() in

  List.iter 
    (fun v ->
       let desc = UnionFind.find v in
	 desc.mark <- young;
	 let rank = desc.rank in
	   try
	     sorted.(rank) <- 
	     if not (List.exists (UnionFind.equivalent v) sorted.(rank)) 
	     then v :: sorted.(rank)
	     else
	       sorted.(rank)
	   with Invalid_argument _ -> 
	     (* The invariant is broken. *)
	     failwith (Printf.sprintf "Out of bound when generalizing %s/%s"
			 (string_of_int rank)
			 (string_of_int (Array.length sorted)))
    ) (inhabitants young_pool);

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
	| Some term ->
	    fold (fun son accu ->
		      max (traverse son) accu
		 ) term IntRank.outermost
	| _ ->
	    k
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
      try
	Misc.iter traverse sorted.(k)
      with Invalid_argument _ -> 
	(* The invariant is broken. *)
	failwith "Out of bound in traverse"

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
    try
      List.iter (register old_pool) sorted.(k)
    with Invalid_argument _ -> 
      (* The invariant is broken. *)
      failwith "Out of bound in young refresh."
  done;

  List.iter (fun v ->
	       let desc = UnionFind.find v in
		 if desc.rank < young_number then
		   register old_pool v
		 else (
		   desc.rank <- IntRank.generalize_rank desc.rank;
		   if desc.kind = Flexible then desc.kind <- Rigid;
(*		   Printf.eprintf "GENERALIZE: %s\n" 
		     (try ConstraintPrinter.print_variable v with
			  _ -> "unprintable"); *)
		 )
	    ) sorted.(young_number)


(** [distinct_variables vl] checks that the variables in the list [vl]
    belong to distinct equivalence classes and that their structure is
    [None]. In other words, they do represent distinct (independent)
    variables (as opposed to nonvariable terms). *)
exception DuplicatedMark of Mark.t
let distinct_variables pos vl =
  let m = Mark.fresh() in
    try
      List.iter (fun v ->
		   let desc = UnionFind.find v in
		     match desc.structure with
		       | Some _ ->
			   raise (CannotGeneralize (pos, v))
		       | _ ->
			   if Mark.same desc.mark m then
			     raise (DuplicatedMark m);
			   desc.mark <- m
		) vl
    with DuplicatedMark m -> 
      let vl' = List.filter (fun v -> Mark.same (UnionFind.find v).mark m)
		 vl 
      in
	raise (NonDistinctVariables (pos, vl')) 

(** [generic_variables vl] checks that every variable in the list [vl]
    has rank [none]. *)
let generic_variables pos vl =
  List.iter (fun v ->
	       let desc = UnionFind.find v in
		 if not (IntRank.is_generalized_rank desc.rank) then (
		   raise (CannotGeneralize (pos, v)))
	    ) vl

(* [solve] *)

let rec compute_syntax = function
  | ATerm (answers, f) ->
      f (List.map compute_syntax answers)
  | AType (ty, k, f) ->
      f ty k
  | AGraft (t, v, k, f) ->
      f (compute_syntax t) v k
  | ATrue -> assert false

let solve tracer env pool (c : 'syntax tconstraint) =
  let final_env = ref env in
  let rec solve env pool c : 'syntax =
    let pos = cposition c in
      try
	compute_syntax (solve_constraint env pool c)
      with Inconsistency -> raise (TypingError pos)

  and solve_constraint env pool (c : 'syntax tconstraint) : 'syntax answer =
    tracer (Solve c);

(*    Printf.eprintf "SOLVE: %s\n" (ConstraintPrinter.print c);  *)

    (match c with

       | CTrue _ ->
	   ATrue

       | CTerm (pos, cs, f) ->
	   let answers = List.map (solve_constraint env pool) cs in
	     ATerm (answers, f)

       | CGraft (pos, x, to_syntax) ->
	   let v = lookup pos x env in
	     AGraft (lookup_answer pos x env, v, (UnionFind.find v).rank,
		     to_syntax)

       | CType (pos, v, f) ->
	   AType (v, (UnionFind.find v).rank, f)

       | CDump _ ->
	   final_env := env;
	   ATrue

       | CEquation (pos, term1, term2) ->
	   let t1, t2 = twice (chop pool) term1 term2 in
	     tracer (UnifyTerms (term1, term2));
	     unify_terms pos pool t1 t2;
	     ATrue

       | CConjunction cl ->
	   List.fold_left (fun t c -> 
		     match solve_constraint env pool c with
		       | ATrue -> t
		       | x -> x) ATrue cl

       | CLet ([ Scheme (_, [], fqs, c, _) ], CTrue _) ->
	   (* This encodes an existential constraint. In this restricted
	      case, there is no need to stop and generalize. The code
	      below is only an optimization of the general case. *)
	   (* TEMPORARY traiter un cas plus general que celui-ci? *)
	   List.iter (introduce pool) fqs;
	   solve_constraint env pool c

       | CLet ([ Scheme (_, rqs, [], c, _) ], CTrue _) ->
	   (* This encodes an existential constraint. In this restricted
	      case, there is no need to stop and generalize. The code
	      below is only an optimization of the general case. *)
	   (* TEMPORARY traiter un cas plus general que celui-ci? *)
	   List.iter (introduce pool) rqs;
	   solve_constraint env pool c

       | CLet (schemes, c2) ->
	   let env' = List.fold_left 
			(fun env' scheme -> (
			   concat env' (solve_scheme env pool scheme)
			 )) env schemes in 
	     (
	       solve_constraint env' pool c2
	     )

       | CInstance (pos, name, term) -> 
	   let t = lookup pos name env in
	   let instance = instance pool t in
	   let t' = chop pool term in
	     unify_terms pos pool instance t';
	     ATrue

       | CDisjunction cs -> 
	   assert false

    )

  and solve_scheme env pool = function

(*    | Scheme (_, [], [], c1, header) ->

	(* There are no quantifiers. In this restricted case, 
	   there is no need to stop and generalize. 
	   This is only an optimization of the general case. *)

	let a = solve_constraint env pool c1 in
	  assert (a = ATrue);
	  let h = 
	    PIdentifier.Map.map (fun (t, c, _) -> (chop pool t, c)) header
	  in
	      PIdentifier.Map.iter 
		(fun x (v, c) -> 
		   Printf.eprintf "GEN %s : %s\n" 
		     (PIdentifier.as_string x)
		     (ConstraintPrinter.print_variable v))
		header;

	    PIdentifier.Map.map 
	      (fun (v, c) -> (v, solve_constraint env pool c)) h
*)
    | Scheme (pos, rqs, fqs, c1, header) ->
	(
	  (* The general case. *)
	  let vars = rqs @ fqs in
	  let pool' = new_pool pool in
	    List.iter (introduce pool') rqs;  
	    List.iter (introduce pool') fqs;
	    let header = 
	      PIdentifier.Map.map (fun (t, c, _) -> (chop pool' t, c)) header
	    in
	    let header = 
	      PIdentifier.Map.map
		(fun (v, c) -> (v, solve_constraint env pool' c)) header
	    in
	    let a = solve_constraint env pool' c1 in
	      assert (a = ATrue);
	      tracer (Generalize (number pool', vars));
	      distinct_variables pos rqs;
	      generalize pool pool';
	      generic_variables pos rqs;
(*	      PIdentifier.Map.iter 
		(fun x (v, c) -> 
		   Printf.eprintf "GEN %s : %s\n" 
		     (PIdentifier.as_string x)
		     (ConstraintPrinter.print_variable v))
		header; *)

	      header
	)
  and concat env header =
    PIdentifier.Map.fold (fun name v env ->
		      EEnvFrame (env, name, v)
		   ) header env

  and unify_terms pos pool t1 t2 =
    unify 
      ~tracer:(fun v1 v2 -> tracer (UnifyVars (v1, v2)))
      pos (register pool) t1 t2
  in 
      solve env pool c
	

(** [init] produces a fresh initial state. It consists of an empty
    environment and a fresh, empty pool. *)
let init () =
  EEmpty, MultiEquation.init ()

(** The public version of [solve] starts out with an initial state
    and produces no result, except possibly an exception. *)
let solve ?tracer c =
  let env, pool = init() in
  let tracer = default ignore tracer in
    tracer (Init c);
    (* TEMPORARY integrer un occur check ici aussi *)
    try
      solve tracer env pool c 
    with UnboundIdentifier (pos, name) ->
      Error.error "during type checking" pos 
	(Printf.sprintf "IE: `%s' is unbound.\n" (PIdentifier.as_string name));
      

