open Strings
open Mm
open Mm.Var

(* -------------------------------------------------------------------------- *)
(* A (strict) ordering on keys. *)

let rec kcompare k1 k2 =
  match k1, k2 with
  | KStructMember i1, KStructMember i2 ->
      Pervasives.compare i1 i2
  | KComposeLeft k1, KComposeLeft k2
  | KComposeRight k1, KComposeRight k2 ->
      kcompare k1 k2
  | KComposeLeft _, KComposeRight _ ->
      -1
  | KComposeRight _, KComposeLeft _ ->
      1
  | KStructMember _, (KComposeLeft _ | KComposeRight _)
  | (KComposeLeft _ | KComposeRight _), KStructMember _ ->
      assert false (* should not happen *)

(* -------------------------------------------------------------------------- *)
(* Graphs where vertices are variables. *)

module Graph = struct

  type t = {
      vertices: AtomSet.t;
      edges: AtomSet.t AtomMap.t
    } 

  let empty = {
    vertices = AtomSet.empty;
    edges = AtomMap.empty
  } 

  let add_edge g x y = {
    vertices =
      AtomSet.add x (AtomSet.add y g.vertices);
    edges = 
      AtomMap.add x (
	try
	  let successors = AtomMap.lookup x g.edges in
	  AtomSet.add y successors
	with Not_found ->
	  AtomSet.singleton y
      ) g.edges
  }

  let exists_edge g x y =
    try
      let successors = AtomMap.lookup x g.edges in
      AtomSet.mem y successors
    with Not_found ->
      false

  (* Iteration over vertices. *)

  let iter g f =
    AtomSet.iter f g.vertices

  (* Transitive closure. *)

  let close g =
    let g' = ref g in
    iter g (fun x ->
      iter g (fun y ->
	iter g (fun z ->
	  if exists_edge !g' x y && exists_edge !g' y z then
	    g' := add_edge !g' x z
        )
      )
    );
    !g'

end

(* -------------------------------------------------------------------------- *)
(* Mixin composition. *)

let composition s1 s2 =

  let input1, (fields1, anonymous1) = open_structure s1
  and (input2, (fields2, _)) as s2 = open_structure s2 in

  (* Check that the output components share no field name. *)

  begin
    try
      let field =
	StringSet.choose (StringSet.inter (StringMap.domain fields1) (StringMap.domain fields2))
      in
      failwith ("A field " ^ field ^ " appears in both structures.")
    with Not_found ->
      ()
  end;

  (* Force the two structures to map shared fields to the same variables. *)

  let strip (_, x, _, _) = x in
  let names1 = StringMap.union input1 (StringMap.map strip fields1)
  and names2 = StringMap.union input2 (StringMap.map strip fields2) in

  let shared =
    StringSet.inter (StringMap.domain names1) (StringMap.domain names2) in

  let subst = StringSet.fold (fun field subst ->
    let var1 = StringMap.find field names1
    and var2 = StringMap.find field names2 in
    Subst.add var2 var1 subst
  ) shared Subst.id in

  let input2, (fields2, anonymous2) = subst_structure subst s2 in

  (* We can now define the new input component as the union of the
     two input components, minus any fields defined in the output
     components. *)

  let input =
    StringMap.union (StringMap.diff input1 (StringMap.domain fields2))
		    (StringMap.diff input2 (StringMap.domain fields1))
  in

  (* The output components are simply concatenated. The keys that they
     contain are lifted so as to reflect the new syntactic ordering
     imposed by this composition operation. *)

  let liftl (xs, x, e, key) =
    (xs, x, e, KComposeLeft key)
  and liftr (xs, x, e, key) =
    (xs, x, e, KComposeRight key)
  in

  let fields =
    StringMap.union
      (StringMap.map liftl fields1)
      (StringMap.map liftr fields2)
  and anonymous =
    (@)
      (List.map liftl anonymous1)
      (List.map liftr anonymous2)
  in

  (* We are done. *)

  EStructure (create_structure (input, (fields, anonymous)))

(* -------------------------------------------------------------------------- *)
(* Closing a structure. *)

let close s =

  let input, (fields, anonymous) = open_structure s in

  (* Make sure that the input component is empty. *)

  if not (StringSet.is_empty (StringMap.domain input)) then
    failwith "Attempt to close a structure whose input component is nonempty.";

  (* Gather the variables associated with this structure's definitions. *)

  let def vars (_, x, _, _) =
    AtomSet.add x vars
  in
  let vars =
    StringMap.foldv def AtomSet.empty fields in
  let vars =
    List.fold_left def vars anonymous in

  (* We can now build a dependency graph where these variables are
     the vertices. There is an edge from y to x when the definition
     of x depends on that of y or when a fake dependency of x on y
     is found. *)

  let def g (ys, x, e, _) =

    let edge y g =
      Graph.add_edge g y x
    in

    (* Examine the definition of [x], that is, the expression [e].
       Find which variables appear free in it and are defined in
       this structure. Add an edge from each of these variables to
       [x]. *)

    let g = AtomSet.fold edge (AtomSet.inter (free_expression e) vars) g in

    (* Add edges that reflect the fake dependencies. *)

    List.fold_right edge ys g

  in

  let g =
    StringMap.foldv def Graph.empty fields
  in
  let g =
    List.fold_left def g anonymous
  in

  (* Compute the graph's transitive closure. *)

  let g = Graph.close g in

  (* Build a (strict) partial order among variables that respects
     dependencies. *)

  let dordered x y =
    (not (Atom.equal x y)) && (Graph.exists_edge g x y) && (not (Graph.exists_edge g y x))
  in

  (* Build a total ordering among definitions. The ordering is
     obtained by first determining whether the dependencies impose
     an ordering, then, if not, using the syntactic ordering given
     by the keys. *)

  let compare (x1, _, k1) (x2, _, k2) =
    if dordered x1 x2 then
      -1
    else if dordered x2 x1 then
      1
    else
      kcompare k1 k2
  in

  (* Gather the variables and keys associated with this
     structure's definitions. Sort them. *)

  let def vks (_, x, e, k) =
    (x, e, k) :: vks
  in
  let xeks =
    StringMap.foldv def [] fields in
  let xeks =
    List.fold_left def xeks anonymous in
  let xeks =
    List.sort compare xeks in

  (* Generate a list of recursive bindings, in order. *)

  let bindings =
    List.map (fun (x, e, _) ->
      x, e
    ) xeks
  in

  (* Generate a set of record fields. *)

  let fields =
    StringMap.map (fun (_, x, _, _) ->
      EVar x
    ) fields
  in

  (* We're done! *)

  ELetRec (create_letrec (bindings, ERecord fields))

(* -------------------------------------------------------------------------- *)
(* Deleting components in a structure. *)

let delete victims s =

  let input, (fields, anonymous) = open_structure s in

  (* Extract the victims from the output component, if they are in
     it, and move them to the input component. *)

  let input, fields =
    StringSet.fold (fun victim (input, fields) ->
      try
	let _, x, _, _ = StringMap.find victim fields in
	StringMap.add victim x input, StringMap.remove victim fields
      with Not_found ->
	(* Presumably the victim is already a member of the
	   input component. Is this an error? *)
	input, fields
    ) victims (input, fields)
  in

  EStructure (create_structure (input, (fields, anonymous)))

(* -------------------------------------------------------------------------- *)
(* Faking a dependency in a structure. *)

let fake fX fY s =

  let input, (fields, anonymous) = open_structure s in

  (* Find the two named fields. The second one might be a member
     of the input or output component. *)

  let (ys, x, e, k) = StringMap.find fX fields in

  let y =
    try
      let (_, y, _, _) = StringMap.find fY fields in
      y
    with Not_found ->
      StringMap.find fY input
  in

  (* Add [y] to the set of fake dependencies for [x].
     One might wish to look for duplicate dependencies,
     but we don't -- they don't affect the semantics. *)

  let fields =
    StringMap.add fX (y :: ys, x, e, k) fields
  in

  EStructure (create_structure (input, (fields, anonymous)))

(* -------------------------------------------------------------------------- *)
(* Evaluation: expressions. *)

exception Lift of binding list * expression
exception Value

let rec evalc env e context continuation =
  try
    context (evalx env e)
  with
  | Lift (bindings, e) ->
      ELetRec (create_letrec (bindings, context e))
  | Value ->
      continuation e

and val2struct env e context continuation =
  match e with
  | EVar x ->
      context (AtomMap.lookup x env)
  | EStructure s ->
      continuation s
  | _ ->
      failwith "Structure expected."

and val2record env e context continuation =
  match e with
  | EVar x ->
      context (AtomMap.lookup x env)
  | ERecord fields ->
      continuation fields
  | _ ->
      failwith "Record expected."

and evalc2struct env e context continuation =
  evalc env e
    context
    (fun v ->
      val2struct env v
	context
	continuation
    )

and evalc2record env e context continuation =
  evalc env e
    context
    (fun v ->
      val2record env v
	context
	continuation
    )

and evalx env = function

  | EVar _
  | EStructure _ ->
      raise Value

  | EComposition (e1, e2) ->
      evalc env e1
	(fun e1' -> EComposition (e1', e2))
	(fun v1 ->
	  evalc env e2
	    (fun e2' -> EComposition (v1, e2'))
	    (fun v2 ->
	      val2struct env v1
		(fun v1' -> EComposition (v1', v2))
		(fun s1 ->
		  val2struct env v2
		    (fun v2' -> EComposition (EStructure s1, v2'))
		    (fun s2 ->
		      composition s1 s2
		    )
                )
            )
        )

  | EClose e ->
      evalc2struct env e
	(fun e' -> EClose e')
	close

  | EDeletion (e, victims) ->
      evalc2struct env e
	(fun e' -> EDeletion (e', victims))
	(delete victims)

  | EFakeDependency (e, fX, fY) ->
      evalc2struct env e
	(fun e' -> EFakeDependency (e', fX, fY))
	(fake fX fY)

  | ERecordSelection (e, field) ->
      evalc2record env e
	(fun e' -> ERecordSelection (e', field))
	(StringMap.find field)

  | ERecord fields ->

      (* We evaluate record fields in an unspecified order. The user
	 can introduce an explicit "let rec" binding in front of the
	 record if she expects something else. *)

      let rec evalf = function
	| (field, e) :: remainder ->

	    (* try to reduce this field; if successful, update the
	       record and stop; otherwise, examine the fields that
	       follow. *)

	    evalc env e
	      (fun e' -> ERecord (StringMap.add field e' fields))
	      (fun _ -> evalf remainder)

	| [] ->

	    (* was not able to reduce any field *)

	    raise Value
      in

      evalf (StringMap.elements fields)
      
  | ELetRec letrec ->
      let bindings, e = open_letrec letrec in
      raise (Lift (bindings, e))

(* -------------------------------------------------------------------------- *)
(* Evaluation: toplevel bindings. *)

let eval = function

  | ELetRec letrec ->
      let bindings, body = open_letrec letrec in

      (* [bindings1] contains the bindings that have been seen so far,
	 in reverse order; all bindings are to values. [env] contains
	 the same data, as a map of atoms to values. *)

      let rec evalb bindings1 env = function

	| (x, e) :: bindings2 ->
	    begin
	      try

		(* reduction inside a binding *)

		let e' = evalx env e in
		ELetRec (create_letrec ((List.rev bindings1) @ (x, e') :: bindings2, body))

	      with
	      | Lift (bindings, e) ->
		  (* we have found a nested let rec binding; lift it out *)
		  ELetRec (create_letrec ((List.rev bindings1) @ bindings @ (x, e) :: bindings2, body))
	      | Value ->
		  (* this binding is to a value, move right *)
		  evalb ((x, e) :: bindings1) (AtomMap.add x e env) bindings2

	    end

	| [] ->

	    (* done reducing inside bindings; reduce right-hand side *)

	    try
	      let body' = evalx env body in
	      ELetRec (create_letrec (List.rev bindings1, body'))
	    with
	    | Lift (bindings, e) ->
		(* we have found a nested let rec binding; lift it out *)
		ELetRec (create_letrec ((List.rev bindings1) @ bindings, e))

      in
      evalb [] AtomMap.empty bindings

  | e ->
      evalx AtomMap.empty e

