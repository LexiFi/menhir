(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/verifier.ml,v 1.4 2000/01/20 14:25:08 fpottier Exp $ *)

(* This module performs type analysis à la Java on a method's bytecode, hence its name (it produces results which are
   similar to those computed by the standard bytecode verifier). It differs from a true bytecode verifier, however,
   because it assumes the code to be \emph{correct}, and only aims to recover some type information about it. For
   this reason, it will silently ignore many type inconsistencies.

   The analysis is implemented as a data flow analysis, rather than as a type inference process. The main reason is,
   a type inference approach would require rather complex conditional constraints in order to express the flow of
   exceptions with enough precision. Indeed, not only does the target of a \texttt{throw} depend on the exception's
   type, but there is a notion of hierarchy among handlers: an outermost handler will not be called if some innermost
   handler catches the exception. This, I think, would be rather difficult to express with types. I prefer to avoid
   the complexity altogether, and write simple code.

   For the same reason (simplicity), I do not attempt to compute the graph's SSA form. Performance should not be a
   problem here, I suppose. *)

module type Out = sig

  (* In this type theory, a (constant) type is either [Bottom], the empty type, [Short], representing any word-sized
     integer, [Null], representing the null pointer, [Object c], representing an object of class [c], or [Top],
     representing garbage. These types form an upper semi-lattice, which allows iterative data flow analysis.

     Arrays and interfaces are represented as objects of class [javacard.lang.Object]. The analysis does not try
     to keep track of any information about array contents. *)

  type jclass

  type constant =
    | Bottom
    | Short
    | Null
    | Object of jclass
    | Top
  
  (* [local node k] returns the type of local variable [k] at node [node]. [stack node k] returns the type of stack
     position [k] at node [node], provided [k] is a meaningful stack position at this node. *)

  type node

  val local: node -> int -> constant
  val stack: node -> int -> constant

end

module Run
    (P : Package.S)
    (M : JavaCard.Method
           with type jclass = P.jclass)
    (V : View.Method                         (* with \verb+ret+ edges available *)
           with type jclass = P.jclass
	    and type concrete_node = M.node)
    (Sub : Subroutine.Out
           with type node = V.node)
= struct

  (* In this type theory, a (constant) type is either [Bottom], the empty type, [Short], representing any word-sized
     integer, [Null], representing the null pointer, [Object c], representing an object of class [c], or [Top],
     representing garbage. [Bottom] is a subtype of every type. [Null] is a subtype of every [Object] type. [Object c]
     is a subtype of [Object d] if and only if [c] is a subclass of [d]. [Top] is a supertype of every type. These
     types form an upper semi-lattice.

     Arrays and interfaces are represented as objects of class [javacard.lang.Object]. The analysis does not try
     to keep track of any information about array contents. *)

  type jclass =
      P.jclass

  type constant =
    | Bottom
    | Short
    | Null
    | Object of jclass
    | Top

  (* This function computes the join (least upper bound) of two constants. *)

  let join c1 c2 =
    match (c1, c2) with
    | _, Bottom ->
	c1
    | Bottom, _ ->
	c2
    | Top, _
    | _, Top ->
	Top
    | Short, Short ->
	Short
    | Short, (Null | Object _)
    | (Null | Object _), Short ->
	Top
    | Null, Null ->
	Null
    | Null, Object _ ->
	c2
    | Object _, Null ->
	c1
    | Object jclass1, Object jclass2 ->
	Object (P.lcs jclass1 jclass2)

  (* Analyze the stack height. TEMPORARY request this info instead of computing it? *)

  module H = Height.Run(V)

  (* Allocate variables to represent local variables and valid stack positions at every program point. In data flow
     analysis, a ``variable'' is simply a mutable cell holding a constant and a reference to the associated program
     point. *)

  type cell = {
      mutable curr: constant;
      mutable node: V.node
    } 

  module Type = SSA.Naive(V)(H)(struct
    type node = V.node
    type variable = cell
    let fresh node = {
      curr = Bottom;
      node = node
    } 
  end)

  (* TEMPORARY seed initial values, mark these variables dirty *)

  (* Create a queue of ``dirty'' nodes. This queue may contain duplicates. In that case, we will perform some extra
     (constant-time) work; this shouldn't affect performance very much. *)

  let queue = Queue.create()

  (* This function records a flow between a constant and a variable. If the variable's value changes, then its node is
     marked dirty. *)

  let flow c1 v2 =
    let initial = v2.curr in
    let final = join c1 initial in
    if final <> initial then begin
      v2.curr <- final;
      Queue.add v2.node queue
    end

  (* This function records a flow of local variables between two nodes. The predicate [changed] accepts a local
     variable index [k]. It must return [Unchanged] if local [k] is unchanged, or [Written c], where [c] is a type
     constant, if a new piece of data is written into it, and [Ignore] if no data flow should be recorded.

     [keep_locals] performs the same task, but assumes no locals are changed. *)

  type change =
    | Unchanged
    | Written of constant
    | Ignore

  let locals node changed target =
    for k = 0 to V.locals - 1 do
      match changed k with
      |	Unchanged ->
	  flow (Type.local node k).curr (Type.local target k)
      |	Written data ->
	  flow data (Type.local target k)
      |	Ignore ->
	  ()
    done

  let keep_locals node target =
    locals node (fun _ -> Unchanged) target

  (* This function records a stack flow between two nodes. The integer [down] tells how many stack words are
     consumed, and the list [data] (a list of type constants) tells how many new words are pushed, as well as
     their nature. *)

  let stack node down data target =
    let h = H.height node - down in

    for k = 0 to h - 1 do
      flow (Type.stack node k).curr (Type.stack target k)
    done;

    let _ = List.fold_left (fun k data ->
      flow data (Type.stack target k);
      k + 1
    ) h data in

    ()

  (* This function records the locals' and the stack's evolution when an exception is thrown. [data] is the type
     constant corresponding to the exception being thrown. *)

  let throw node data target =
    stack node (H.height node) [ data ] target;
    keep_locals node target

  (* [portion m n] returns a portion of the stack which would be obtained by popping [m] words, then taking the [n]
     top words. [top n] returns a list of the top [n] stack words. In both cases, the head of the list is the deepest
     element. *)

  let portion m n node =
    let h = H.height node - m in
    let rec portion n =
      if n = 0 then
	[]
      else
	(Type.stack node (h - n)).curr :: (portion (n - 1)) in
    portion n

  let top n node =
    portion 0 n node

  (* This function records the locals' and the stack's evolution along an [Imaginary] edge. *)

  let imaginary node label target =
    match label with
    | Flow.Imaginary m ->
	stack node (H.height node) (top m node) target;
	keep_locals node target
    | _ ->
	assert false

  (* This function returns a list of [n] [Short] constants. *)

  let rec short n =
    if n = 0 then []
    else Short :: (short (n - 1))

  (* Perform the analysis. *)

  open Abstract

  let () =
    try
      while true do
	let node = Queue.take queue in
	match node with
	| V.NodeConcrete cnode -> (

	    (* [normalch] records the fact that an operation consumes a certain number of stack words (which it
	       determines automatically) and pushes a list of new typed stack words [data] along all of its outgoing
	       [Normal] edges. It also records changes to local variables. [normal] is a variant which assumes all
	       locals are unchanged. *)

	    let normalch changed data =
	      let down, _ = V.stack_usage node in
	      V.successors (fun label target ->
		if label = Flow.Normal then begin
		  locals node changed target;
		  stack node down data target
		end
	      ) node in

	    let normal data =
	      normalch (fun _ -> Unchanged) data in

	    (* This auxiliary function records the fact that an operation throws an exception of the given type. It
	       also records the fact that all local variables are unchanged along all [Throw] edges of interest. It
	       assumes the object is known to be non-null, and accepts its class as an argument. (The object may of
	       course belong to a subclass of the given class.) *)

	    let throws jclass =
	      V.throws (throw node (Object jclass)) (Some jclass) cnode in

	    (* This auxiliary function calls [normal] with appropriate parameters after a (static or instance) field
	       [cpx] has been fetched by \verb+getfield+ or \verb+getstatic+. *)

	    let fetch cpx =
	      match P.fieldref cpx with
		| P.Prim (JavaCard.PrimBoolean
		        | JavaCard.PrimByte | JavaCard.PrimShort) ->
		    normal [ Short ]
		| P.Prim JavaCard.PrimInt ->
		    normal [ Short; Short ]
		| P.Intf _
		| P.Array _ ->
		    normal [ Object P.root ]
		| P.Class jclass ->
		    normal [ Object jclass ] in

	    (* Here comes the analyzer's heart. Obtain the current node's opcode. *)

	    let opcode = V.opcode cnode in

	    (* Deal with all exceptions which are statically known to be potentially thrown at this node. *)

	    V.statically_throws throws opcode;

	    (* Deal with all other possible executions. *)

	    match V.opcode cnode with
	    | Arraylength
	    | Arrayload (JavaCard.AbsiB | JavaCard.AbsiS)
	    | Instanceof _
	    | Load (JavaCard.AsiS, _)
	    | Sdivrem ->
		normal [ Short ]
	    | Arrayload JavaCard.AbsiA ->
		normal [ Object P.root ]
	    | Arrayload JavaCard.AbsiI
	    | Idivrem
	    | Load (JavaCard.AsiI, _) ->
		normal [ Short; Short ]
	    | Arraystore _
	    | If _
	    | Inc _
	    | Putfield _
	    | Putstatic _ ->
		normal []
	    | Athrow -> (
		match (Type.stack node (H.height node - 1)).curr with
		| Bottom ->
		    assert false (* All code must be reachable. *)
		| Short
		| Top ->
		    assert false (* The bytecode must be correct. *)
		| Null ->
		    ()
		| Object jclass ->
		    throws jclass
	      )
	    | Checkcast (JavaCard.CheckArray _)
	    | Newarray _ ->
		normal [ Object P.root ]
	    | Checkcast (JavaCard.CheckClass cpx) ->
		normal [ Object (P.classref cpx) ]
	    | Dup_x (m, 0) ->
		let data = top m node in
		normal (data @ data)
	    | Dup_x (m, n) ->
		let datam = top m node
		and datan = top n node in
		normal (datam @ datan)
	    | Dup2 ->
		let data = top 2 node in
		normal (data @ data)
	    | Getfield (_, _, cpx) ->
		fetch cpx
	    | Getstatic (_, cpx) ->
		fetch cpx
	    | Invokeinterface _
	    | Invokespecial _
	    | Invokestatic _
	    | Invokevirtual _ ->
		assert false (* TEMPORARY *)
	    | Jsr ->
		normal [ Top ];
		V.successors (fun label target ->
		  if label = Flow.Pseudo then
		    let affected = Sub.affected (Sub.target node) in
		    locals node (fun k ->
		      if Sub.IntSet.mem k affected then Ignore else Unchanged
		    ) target
		) node
	    | Load (JavaCard.AsiA, index) ->
		normal [ (Type.local node index).curr ]
	    | New cpx ->
		normal [ Object (P.classref cpx) ]
	    | Ret _ ->
		let affected = Sub.affected (Sub.owner node) in
		normalch (fun k ->
		  if Sub.IntSet.mem k affected then Unchanged else Ignore
		) []
	    | Return _ ->
		V.successors (imaginary node) node
	    | StackOp (_, up) -> (
		match M.opcode cnode with
		| JavaCard.Aconst_null ->
		    normal [ Null ]
		| JavaCard.Dup ->
		    let data = top 1 node in
		    normal (data @ data)
		| _ ->
		    normal (short up)
	      )
	    | Store (asi, index) ->
		normalch (fun k ->
		  match asi with
		  | JavaCard.AsiA
		  | JavaCard.AsiS ->
		      if k = index then
			Written (Type.stack node (H.height node - 1)).curr
		      else
			Unchanged
		  | JavaCard.AsiI ->
		      if (k = index) or (k = index + 1) then
			Written Short
		      else
			Unchanged
		) []
	    | Swap_x (m, n) ->
		let datam = top m node
		and datan = portion m n node in
		normal (datam @ datan)

	  )
	| V.NodeNormalCompletion
	| V.NodeExceptionalCompletion _
	| V.NodeEnd ->
	    V.successors (imaginary node) node

      done
    with Queue.Empty ->
      ()

  (* Publish functions which allow querying the analyzer about its results. *)

  type node = V.node

  let local node k =
    (Type.local node k).curr

  let stack node k =
    (Type.stack node k).curr

end


