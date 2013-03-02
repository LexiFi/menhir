(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 1998 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace-out-of-date/equivalence.ml,v 1.11.6.25 1999/04/08 12:43:45 francois Exp $ *)

open Errors
open Standard
open Types

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

We instantiate Paige and Tarjan's algorithm.

Because of the presence of guarded constraints, we need to introduce some points which stand for pairs of variables.
Thus, the type of points cannot coincide with the type of variables.

*)

module Point = struct

  type point = {
      mutable point_item: (* Unknown, abstract type; internal to PaigeTarjan *) point;
      mutable point_variable: type_variable option;
    } 

  (* These callbacks are provided to PaigeTarjan, so that it may associate internal data with each point in
     constant time. *)

  let store_item p item =
    p.point_item <- Obj.magic item

  let get_item p =
    Obj.magic p.point_item

  (* This callback allows PaigeTarjan to describe the final partition to us. *)

  let set_representative p1 p2 =
    match p1.point_variable, p2.point_variable with
      Some v1, Some v2 ->
	v1.representative <- v2
    | None, None ->
	()
    | _ ->
	raise (CantHappen "Trying to identify a variable with a pair in Equivalence.Refiner.set_representative.")

  (* We need some way of associating variables and pairs of variables to points while initializing the process.
     For variables, the "representative" field is unused during initialization, so we re-use it. This is ill-typed,
     but saves space. For pairs of variables, we use an explicit association structure. *)

  module PairMap = Map.Make (struct
    type t = type_variable * type_variable
    let compare (v1, w1) (v2, w2) =
      let result = compare_variables v1 v2 in
      if result <> 0 then result
      else compare_variables w1 w2
  end)

  let map = ref PairMap.empty

  let reset () =
    map := PairMap.empty

  (* This function creates a new point and associates it to a given variable. *)

  let of_variable v =
    let rec point = {
      point_item = point;
      point_variable = Some v
    } in
    v.representative <- Obj.magic point;
    point

  (* This one finds the point associated to the given variable. The point must already exist. *)

  let of_variable_again v =
    Obj.magic v.representative

  (* This function finds the point associated to the given pair of variables. The point is created if it doesn't
     exist yet. *)

  let of_pair pair =
    try
      PairMap.find pair !map
    with Not_found ->

      let rec point = {
	point_item = point;
	point_variable = None
      } in
      map := PairMap.add pair point !map;
      point

end

module Refiner = PaigeTarjan.Make (Point)

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

We assume the constraint graph to be in canonical form (i.e. no nested unions or intersections). Garbage collection
must have been run on the set, for several reasons: we don't need to recompute signs, and we don't need to take
signless variables into account.

The scheme is allowed to contain bipolar variables. This makes the code slightly more complex, because we have to
isolate them inside special one-element classes. Dropping them would not be correct, since all variables appearing
in the set must be part of a class.

The conditions for a congruence to be compatible with a type scheme are as follows:

  1. Two equivalent variables have the same sign, but variables signed + and - are isolated.
  2. Two equivalent variables have exactly the same upper bounds and lower bounds, as far as variables
     are concerned.
  3. If a variable has a constructed upper [resp. lower] bound, then any equivalent variable has an
     equivalent upper [resp. lower] bound.
  4. If a variable carries a guarded constraint, then any equivalent variables carries an equivalent guarded
     constraint.
  5. In the presence of row variables, two equivalent variables must have the same kind. Merging variables with
     different kinds does not make sense.

So, computing the coarsest compatible congruence is done as follows:

  1. Compute the signs, exactly as during garbage collection.
  2. Put each variable signed + and - into a class of its own.
  3. Sort the remaining variables according to lexicographic order on the tuple
       (sign, F, kind, V)
     where F is the head constructor of the canonical upper [resp. lower] bound and V is the set of upper [resp. lower]
     variables for negative [resp. positive] variables. The order of elements within the tuple does not matter, but
     elements with the cheapest comparison functions are placed first to make lexicographic order cheaper.
  4. Walk the sorted list. All variables which have the same tuple are put into the same class.
  5. Run the coarsest stable refinement algorithm, with the following data.

     First, to take care of constructed bounds:
       - an edge labeled 0 from each variable to the domain of its unique bound, if such a thing is defined;
       -                 1                           range
       - and so on, for other types (the two above cases cover arrow types).
     The relations thus defined are functions (i.e. deterministic).

     Moreover, to take care of guarded constraints, for each constraint of the form (H < 'b) ? ('a < 'c):
       - an edge labeled H from 'b to a special node, which we call ('a, 'c);
       - an edge labeled 0 from ('a, 'c) to 'a;
       - an edge labeled 1 from ('a, 'c) to 'b.
     The relations labeled with head constructors are (a priori) non-deterministic. The relations labeled 0 and 1
     are functions.
     Note that adding direct edges, labeled e.g. H0 (resp. H1) from 'b to 'a (resp. 'c) would not be correct; it
     would lose track of the correlation between 'a and 'c.

     All "pair" points are initially in a separate class. This allows us to safely re-use the labels 0 and 1, without
     fear of identifying a pair point with a regular one. Note that it is not correct to put each pair point inside
     its own class; this would prevent identifying points which correspond to equivalent pairs.

Note that the output scheme is garbage collected and carries correct signs.

*)
(* ----------------------------------------------------------------------------------------------------------------- *)
(*

PaigeTarjan exclusively allows integer labels on relations. Hence, we must translate head constructors to integers.

*)

module Head = struct

  module HeadMap = Map.Make (struct
    type t = head_constructor
    let compare = Pervasives.compare
  end)

  let count =
    ref 0

  let map =
    ref HeadMap.empty

  let reset () =
    map := HeadMap.empty

  let register head =
    try
      let _ = HeadMap.find head !map in
      ()
    with Not_found ->
      let index = !count in
      map := HeadMap.add head index !map;
      count := index + 1

  let find head =
    HeadMap.find head !map

end

(* ----------------------------------------------------------------------------------------------------------------- *)
(*

Let's do it!

*)

let minimize scheme =

  (* Loop over all variables. Besides building the tuple list, this loop has another purpose: it determines, ahead of
     time, how many functions will be needed when running the partition refinement algorithm. Third, it builds a list
     of all bipolar variables. Fourth, it registers all head constructors which serve as guards. Fifth, it builds all
     pair points.

     There is a slight trick regarding the treatment of record and variant types. For these types, the head
     constructor encodes no information about the set of explicit fields. The trick is that the remainder variable is
     always argument number 0, and the set of fields is contained in this variable's span. Since we cannot identify
     variables with different spans, we cannot identify record types with different sets of fields. *)

  let tuples, count, bipolar = Walk.fold_scheme (fun v (tuples, count, bipolar) ->

    match v.sign with
      Positive ->
	(v, true, head_constructor_of_term v.lo, v.span, v.loset) :: tuples, max (arity v.lo) count, bipolar
    | Negative ->
	Set7.iter (fun (head, v1, v2) ->
	  let _ = Point.of_pair (v1, v2) in
	  Head.register head
        ) v.guards;
	(v, false,head_constructor_of_term v.hi, v.span, v.hiset) :: tuples, max (arity v.hi) count, bipolar
    | Bipolar ->
	tuples, count, v :: bipolar
    | _ ->
	raise (CantHappen "Unexpected sign in Equivalence.minimize.")

  ) scheme ([], 0, []) in

  (* We now know how many functions and relations are required. *)

  let count =
    if !Head.count > 0 then max 2 count
    else count in

  Refiner.set_counts count !Head.count;

  (* Compute the initial partition, by sorting the tuple list. *)

  let compare_tuples (_, sign1, h1, span1, neighbors1) (_, sign2, h2, span2, neighbors2) =
    let result = Pervasives.compare sign1 sign2 in
    if result = 0 then
      let result = Pervasives.compare h1 h2 in
      if result = 0 then
        let result = compare_spans span1 span2 in
	if result = 0 then
	  Set7.compare neighbors1 neighbors2
	else result
      else result
    else result in

  let compare_tuple_options to1 to2 = match (to1, to2) with
    (Some tuple1), (Some tuple2) ->
      compare_tuples tuple1 tuple2
  | (Some _), None ->
      1
  | None, (Some _) ->
      -1
  | None, None ->
      0 in

  let smaller_tuple tuple1 tuple2 =
    compare_tuples tuple1 tuple2 <= 0 in

  let tuples = Sort.list smaller_tuple tuples in

  (* Let the refiner know about the initial partition. First, create a class containing all pair points. Second, add
     any bipolar variables as isolated points. Lastly, walk the tuple list to define regular classes. *)

  Point.PairMap.iter (fun _ pp -> Refiner.add_point false pp) !Point.map;

  List.iter (fun v -> Refiner.add_point true (Point.of_variable v)) bipolar;

  let _ = List.fold_right (fun ((v, _, _, _, _) as tuple) current_tuple_option ->

    let new_tuple_option = Some tuple in

    (* If two variables have different tuples, then they cannot be in the same class. *)

    Refiner.add_point (compare_tuple_options new_tuple_option current_tuple_option <> 0) (Point.of_variable v);
    new_tuple_option

  ) tuples None in

  (* Make one pass over the constraint graph and construct the edges. For instance, when a negative variable v has a
     canonical upper bound of v1 -> v2, the edge (v, v1) is added to the function of index 0 and the edge (v, v2) is
     added to the function of index 1. 

     We also add edges from each variable to the appropriate pair points. Edges from each pair point to its components
     shall be added later -- remember that each pair point may be visited several times by this loop. *)

  List.iter (fun (v, positive, _, _, _) ->

    let p = Point.of_variable_again v in

    for_each_leaf (fun index leaf ->
      Refiner.add_functional_link index p (Point.of_variable_again (leaf_to_variable leaf))
    ) (if positive then v.lo else v.hi);

    if v.sign == Negative then
      Set7.iter (fun (head, v1, v2) ->
	Refiner.add_relational_link (Head.find head) p (Point.of_pair (v1, v2))
      ) v.guards;

  ) tuples;

  (* Make one pass on the list of pair points and create the edges from each pair point to its components. *)

  Point.PairMap.iter (fun (v1, v2) pp ->
    Refiner.add_functional_link 0 pp (Point.of_variable_again v1);
    Refiner.add_functional_link 1 pp (Point.of_variable_again v2)
  ) !Point.map;

  (* We no longer need our association structures. *)

  Point.reset();
  Head.reset();

  (* We have now created all the necessary functions. The final partition shall be the coarsest refinement of the
     initial partition which is stable with respect to them. We provide a callback which will be called every time two
     variables can be identified, allowing us to build a substitution. *)

  Refiner.run();

  Subst.multi_type_scheme scheme

let minimize scheme =
  Errors.trace "Equivalence.minimize" (fun () ->
    minimize scheme
  )

