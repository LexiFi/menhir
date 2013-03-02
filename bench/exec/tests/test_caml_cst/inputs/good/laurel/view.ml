(* $Header: /home/yquem/cristal/fpottier/cvs/laurel/view.ml,v 1.8 2000/02/29 08:39:42 fpottier Exp $ *)

(* This module performs the conversion from concrete JavaCard bytecode to the abstract instruction set, and builds
   an abstract representation of the method's control flow graph. *)

(* This module defines extra parameters required by our abstraction functor. *)

module type Extra = sig

  (* This function will be passed a concrete node which contains an \verb+athrow+ instruction. It must then return the
     class of the exception(s) being thrown at the specified node. It is allowed to return [None] if doesn't have such
     knowledge. *)

  type node
  type jclass

  val exceptions: node -> jclass option

  (* This function will be passed an action function, as well as a concrete node which contains a \verb+ret+
     instruction. It must then enumerate the nodes to which that \verb+ret+ instruction may return. Enumeration
     is done by invoking the action function. *)

  val ret: (node -> unit) -> node -> unit

  (* This function may be called if the method may throw an exception which does not appear (modulo superclassing) in
     its list of declared exceptions. The call to [undeclared] is not necessarily made at functor application time,
     but may occur only later. *)

  val undeclared: jclass -> 'a

end

(* This signature represents an abstract JavaCard method. It is our functor's return signature. *)

module type Method = sig

  (* Nodes which appear in the original graph are considered concrete. The abstract view has more nodes, since it
     adds ``imaginary'' nodes which represent normal and abnormal completions. *)

  type concrete_node

  type jclass

  type abstract_node =
    | NodeConcrete of concrete_node
    | NodeNormalCompletion
    | NodeExceptionalCompletion of jclass
    | NodeEnd

  include Graph.LabelledFlowIter with type node = abstract_node
                                  and type label = Flow.label

  (* This function maps concrete nodes to opcodes. *)

  val opcode: concrete_node -> Abstract.opcode

  (* This function allows determining which local variables (if any) are directly affected (i.e. written to) at a
     given node. \texttt{jsr} instructions are considered as affecting no variable. So are imaginary nodes. *)

  val affected: (int -> unit) -> node -> unit

  (* This function allows querying the instruction contained in a given node about its stack usage. The function
     returns a pair of integers. The first (resp. second) integer represents the number of stack words which are
     consumed (resp. produced) by the instruction. Consumption is assumed to take place only along [Normal] and
     [Throw] edges, so the first integer is meaningless at a node which has no such outgoing edges. Production is
     assumed to take place only along [Normal] edges, so the second integer is meaningless at a node which has no
     such outgoing edges. *)

  val stack_usage: node -> int * int

  (* The number of local variables used by the method. Locals are numbered from 0 to [locals]$ - 1$. *)

  val locals: int

  (* This function assumes given a non-null pointer whose actual class is either unknown (if [exc] equals [None]) or a
     sub-class of [given] (if [exc] equals [Some given]). It enumerates all handlers which may be called, by
     repeatedly calling [action]. *)

  val throws: (node -> unit) -> jclass option -> concrete_node -> unit

  (* This function tells which exceptions are statically known to be potentially thrown by a given instruction. This
     accounts for all exceptions, except those dynamically thrown by \texttt{athrow}. *)

  val statically_throws: (jclass -> unit) -> Abstract.opcode -> unit

end

(* This module defines the instruction abstraction function. It is defined outside of the functor, because both
   datatypes being dealt with are globally defined. *)

module Internal = struct

  open JavaCard

  let convert opcode =
    match opcode with

    (* Identify ``stack operators''. By merging \verb+aconst_null+ with, say, \verb+sspush 0+, we implicitly assume
       that the information flow analysis assigns the same type to the null pointer and to any integer. *)

    | Nop ->
	Abstract.StackOp (0, 0)
    | Aconst_null
    | Spush _ ->
	Abstract.StackOp (0, 1)
    | Ipush _ ->
	Abstract.StackOp (0, 2)
    | Pop ->
	Abstract.StackOp (1, 0)
    | S2b
    | Sneg ->
	Abstract.StackOp (1, 1)
    | Dup
    | S2i ->
	Abstract.StackOp (1, 2)
    | Pop2 ->
	Abstract.StackOp (2, 0)
    | I2b
    | I2s
    | Sadd
    | Sand
    | Smul
    | Sor
    | Sshl
    | Sshr
    | Ssub
    | Sushr
    | Sxor ->
	Abstract.StackOp (2, 1)
    | Ineg ->
	Abstract.StackOp (2, 2)
    | Icmp ->
	Abstract.StackOp (4, 1)
    | Iadd
    | Iand
    | Imul
    | Ior
    | Ishl
    | Ishr
    | Isub
    | Iushr
    | Ixor ->
	Abstract.StackOp (4, 2)

    (* Identify branching constructs. *)

    | Goto _ ->
	Abstract.If 0
    | Lookupswitch (SiS, _, _)
    | Tableswitch (SiS, _, _, _, _)
    | If ((Eq | Ne | Lt | Ge | Gt | Le | Nonnull | Null), _) ->
	Abstract.If 1
    | Lookupswitch (SiI, _, _)
    | Tableswitch (SiI, _, _, _, _)
    | If ((Acmpeq | Acmpne | Scmpeq | Scmpne | Scmplt | Scmpge | Scmpgt | Scmple), _) ->
	Abstract.If 2

    (* Identify short and integer division operations. *)

    | Sdiv
    | Srem ->
	Abstract.Sdivrem
    | Idiv
    | Irem ->
	Abstract.Idivrem

    (* All other instructions are left unchanged by the conversion. *)

    | Arraylength ->
	Abstract.Arraylength
    | Arrayload t ->
	Abstract.Arrayload t
    | Arraystore t ->
	Abstract.Arraystore t
    | Athrow ->
	Abstract.Athrow
    | Checkcast c ->
	Abstract.Checkcast c
    | Dup_x (m, n) ->
	Abstract.Dup_x (m, n)
    | Dup2 ->
	Abstract.Dup2
    | Getfield (t, this, cpx) ->
	Abstract.Getfield (t, this, cpx)
    | Getstatic (t, cpx) ->
	Abstract.Getstatic (t, cpx)
    | Iinc (index, i) ->
	Abstract.Inc (JavaCard.SiI, index)
    | Instanceof c ->
	Abstract.Instanceof c
    | Invokeinterface (nargs, cpx, token) ->
	Abstract.Invokeinterface (nargs, cpx, token)
    | Invokespecial cpx ->
	Abstract.Invokespecial cpx
    | Invokestatic cpx ->
	Abstract.Invokestatic cpx
    | Invokevirtual cpx ->
	Abstract.Invokevirtual cpx
    | Jsr _ ->
	Abstract.Jsr
    | Load (t, index) ->
	Abstract.Load (t, index)
    | New cpx ->
	Abstract.New cpx
    | Newarray ja ->
	Abstract.Newarray ja
    | Putfield (t, this, cpx) ->
	Abstract.Putfield (t, this, cpx)
    | Putstatic (t, cpx) ->
	Abstract.Putstatic (t, cpx)
    | Ret index ->
	Abstract.Ret index
    | Return t ->
	Abstract.Return t
    | Sinc (index, s) ->
	Abstract.Inc (JavaCard.SiS, index)
    | Store (t, index) ->
	Abstract.Store (t, index)
    | Swap_x (m, n) ->
	Abstract.Swap_x (m, n)

end

(* Here comes the main abstraction functor. It builds an abstract representation of the method's control flow graph.
   Some information, such as which handler is used when an exception is thrown, or which program points may be the
   target of a \verb+ret+ instruction, is not directly contained in the method's code. For this reason, the functor
   expects to be given an extra argument [X], capable of supplying this information upon demand. However, it is also
   possible for [X] to refuse giving information if it is not yet available. Of course, the more information is
   supplied by [X], the more precise the output graph.

   The design I have in mind is the following. In a first stage, no extra information is available, so a crude control
   flow graph is created, where \verb+athrow+ instructions may have too many successors and \verb+ret+ instructions
   have no successors. (The graph is still connected, thanks to [Pseudo] edges.) This graph can be fed to the
   [Subroutine] analysis module, which yields enough information to implement [X.ret] and build a more precise graph.
   This new graph can be used to perform conventional type analysis à la Java, which yields enough information to
   implement [X.exceptions] and build yet another, more precise graph. Lastly, this final graph may be used to perform
   more advanced analyses, such as information flow and/or control dependence analyses. *)

module Build
    (P : Package.S)
    (M : JavaCard.Method
           with type jclass = P.jclass)
    (X : Extra
           with type node = M.node
            and type jclass = M.jclass)
= struct

  type label = Flow.label

  (* In our abstract view of the method, nodes are either concrete nodes, corresponding to actual program points,
     or imaginary nodes. An imaginary node may be:
     \begin{itemize}
     \item the ``normal completion'' node, reached when invoking a \verb+return+ instruction;
     \item an ``exceptional completion'' node, labeled by an exception name, reached when throwing an exception
           for which no local handler exists;
     \item the control flow graph's end node, reachable from all of the above imaginary nodes.
     \end{itemize} *)

  type concrete_node = M.node

  type jclass = P.jclass

  type abstract_node =
    | NodeConcrete of concrete_node
    | NodeNormalCompletion
    | NodeExceptionalCompletion of jclass
    | NodeEnd

  type node = abstract_node

  (* Associate a number with each node in the graph. *)

  let table =
    Hashtbl.create 143

  let n =
    ref (M.n + 2)

  let () =
    List.iter (fun jclass ->
      let index = !n in
      Hashtbl.add table jclass index;
      n := index + 1
    ) M.exceptions

  let n =
    !n

  let index node =
    match node with
    | NodeConcrete node ->
	M.index node
    | NodeNormalCompletion ->
	M.n
    | NodeExceptionalCompletion jclass ->
	Hashtbl.find table jclass
    | NodeEnd ->
	M.n + 1

  (* Define iteration over all nodes. *)

  let iter action =
    M.iter (fun node ->
      action (NodeConcrete node)
    );
    action NodeNormalCompletion;
    List.iter (fun jclass ->
      action (NodeExceptionalCompletion jclass)
    ) M.exceptions;
    action NodeEnd

  (* Define our graph's start and end nodes. *)

  let start =
    NodeConcrete M.start

  let sink =
    NodeEnd

  (* Define a function which maps nodes to concrete nodes. *)

  let concrete = function
    | NodeConcrete node ->
	Some node
    | NodeNormalCompletion
    | NodeExceptionalCompletion _
    | NodeEnd ->
	None

  (* Define a function which maps concrete nodes to opcodes. *)

  let opcode node =
    Internal.convert (M.opcode node)

  (* This auxiliary function defines the behavior of exceptions. It is used when defining [successors] below. It is
     also published for direct use by other modules.

     The function assumes given a non-null pointer whose actual class is either unknown (if [exc] equals [None]) or a
     sub-class of [given] (if [exc] equals [Some given]). It enumerates all handlers which may be called, by
     repeatedly calling [action].

     To do so, we enumerate all handlers which are in scope at the current node, and determine whether the exception
     may reach each of them. The exception [DefinitelyGotHandler] is used to break out of the loop when we determine
     that some handler \emph{must} be reached. *)

  exception DefinitelyGotHandler

  let throws action exc node =
    try
      M.handlers (fun expected handler ->

	let throw definitely =
	  action (NodeConcrete handler);
	  if definitely then
	    raise DefinitelyGotHandler in

	match exc, expected with
	| Some given, Some cpx ->
	    let expected = P.classref cpx in

	    (* If the given class is a subclass of the handler's expected class, then the handler must be
	       invoked, so we may break out of the loop. If, on the other hand, the handler only catches
	       certain subclasses of the given class, then it may, or may not, be invoked. Lastly, if the
	       given and expected classes are unrelated, then the handler may not be invoked. *)

	    if P.subclass given expected then
	      throw true
	    else if P.subclass expected given then
	      throw false

	| _, None ->

	    (* If the handler catches all exceptions, then it must be invoked, and we may break out of the
	       loop. *)

	    throw true

	| None, Some _ ->

	    (* If the class of the exception being thrown is unknown, then the handler may, or may not, be
	       invoked. *)

	    throw false

      ) node;

      (* If we reach this point, then we haven't been able to find a local exception handler which is
	 definitely called -- so, the exception may escape the scope of this method. We must create a
	 [Throw] edge to an appropriate imaginary node. This requires knowing the class of the exception
	 which is being thrown, of course. If it is unknown, we create an edge to every exceptional
	 completion node. (This behavior is correct if the exception indeed belongs to the list of declared
	 exceptions, which we will check later.) If it is known, then some superclass of it must appear in
	 the list of declared exceptions; this determines which imaginary node becomes the destination of
	 the [Throw] edge. Otherwise, an error occurs. *)

      (* TEMPORARY prendre en compte le cas des interfaces: elle peuvent apparaître dans throw/throws *)

      match exc with
      | None ->
	  List.iter (fun jclass ->
	    action (NodeExceptionalCompletion jclass)
	  ) M.exceptions
      | Some given ->

	  let rec find jclass =
	    try
	      let _ = Hashtbl.find table jclass in
	      jclass
	    with Not_found ->
	      match P.superclass jclass with
	      | None ->
		  X.undeclared given
	      | Some jclass ->
		  find jclass in

	  action (NodeExceptionalCompletion (find given))

    with DefinitelyGotHandler ->
      ()

  open Abstract

  (* This function tells which exceptions are statically known to be potentially thrown by a given instruction. This
     accounts for all exceptions, except those dynamically thrown by \texttt{athrow}. *)

  (* TEMPORARY je suis surpris de voir dans la doc JCVM que new ne lance aucune exception. Il y a peut-être quelque
     part une série d'exceptions qui sont documentées comme pouvant être lancées à tout moment?
     oui: SystemException, SecurityException aussi (mais alors pourquoi certaines instructions sont-elles isolées?)
     Il faut faire attention à la façon dont est implémentée une véritable JCVM, car n'oublions pas que le fait
     de raffiner la spécification de la machine ne conserve pas la propriété de non-interférence! D'ailleurs il
     faudra documenter ce fait. *)

  let statically_throws action = function
    | Arraylength
    | Getfield _
    | Putfield _
    | Athrow ->
	action P.nullPointerException;
	action P.securityException
    | Arrayload _ ->
	action P.nullPointerException;
	action P.arrayIndexOutOfBoundsException;
	action P.securityException
    | Arraystore _ ->
	action P.nullPointerException;
	action P.arrayIndexOutOfBoundsException;
	action P.arrayStoreException;
	action P.securityException
    | Checkcast _ ->
	action P.classCastException;
	action P.securityException
    | Dup_x _
    | Dup2
    | Getstatic _
    | Inc _
    | Load _
    | New _
    | StackOp _
    | Store _
    | Swap_x _
    | If _
    | Jsr
    | Ret _
    | Return _ ->
	()
    | Idivrem
    | Sdivrem ->
	action P.arithmeticException
    | Instanceof _
    | Putstatic _ ->
	action P.securityException
    | Invokeinterface _
    | Invokevirtual _ ->
	(* TEMPORARY *)
	action P.nullPointerException;
	action P.securityException
    | Invokespecial _ ->
	(* TEMPORARY *)
	action P.nullPointerException
    | Invokestatic _ ->
	() (* TEMPORARY *)
    | Newarray _ ->
	action P.negativeArraySizeException

  (* This function defines every node's successors. *)

  let successors action = function
    | NodeConcrete node -> (

	(* We begin by defining a number of auxiliary functions, which help define flow edges. *)

	let edge label target =
	  action label (NodeConcrete target) in

	let jumps target =
	  edge Flow.Normal target

	and throws exc =
	  throws (action Flow.Throw) exc node

	and pseudo () =
	  M.successors (edge Flow.Pseudo) node in

	let next () =
	  M.successors jumps node in

	(* Now, look at the node's opcode. *)

	let opcode = opcode node in

	(* Deal with all statically known exceptions. *)

	statically_throws (fun jclass -> throws (Some jclass)) opcode;

	(* Deal with all other edges. *)

	match opcode with
	| Athrow ->
	    throws (X.exceptions node)
	| Arraylength
	| Getfield _
	| Putfield _
	| Arrayload _
	| Arraystore _
	| Checkcast _
	| Dup_x _
	| Dup2
	| Getstatic _
	| Inc _
	| Load _
	| New _
	| StackOp _
	| Store _
	| Swap_x _
	| Idivrem
	| Sdivrem
	| Instanceof _
	| Putstatic _
	| Newarray _ ->
	    next()
	| If _
	| Jsr -> (
	    match M.opcode node with
	    | JavaCard.Goto target ->
		jumps target
	    | JavaCard.If (_, target) ->
		next();
		jumps target
	    | JavaCard.Lookupswitch (_, default, cases) ->
		jumps default;
		List.iter (fun (_, target) -> jumps target) cases
	    | JavaCard.Tableswitch (_, default, _, _, table) ->
		jumps default;
		Array.iter jumps table
	    | JavaCard.Jsr target ->
		jumps target;
		pseudo()
	    | _ ->
		assert false
	  )
	| Invokeinterface _
	| Invokevirtual _
	| Invokespecial _
	| Invokestatic _ ->
	    next() (* TEMPORARY how do we signal the existence of a method call? possible loop *)
	| Ret _ ->
	    X.ret jumps node
	| Return asio ->

	    (* Instructions of the \verb+return+ family are linked by an imaginary edge to the normal completion
	       node. The numbers of stack words preserved by the edge is chosen so that only the value being returned
	       is preserved. This guarantees that the stack at the normal completion node is in a consistent state and
	       contains the value being returned (and nothing else). *)

	    let k = match asio with
	    | None ->
		0
	    | Some (JavaCard.AsiA | JavaCard.AsiS) ->
		1
	    | Some JavaCard.AsiI ->
		2 in
	    action (Flow.Imaginary k) NodeNormalCompletion

      )

    (* All completion nodes are linked by an imaginary edge to the graph's end node. Indeed, the definition of control
       dependence requires the graph to have an end node which is reachable from all nodes. The end node itself has
       no successors. TEMPORARY should it carry a self-loop? *)

    | NodeNormalCompletion
    | NodeExceptionalCompletion _ ->
	action (Flow.Imaginary 0) NodeEnd
    | NodeEnd ->
	()

  (* Having defined successors, let us define predecessors as well. *)

  let predecessors =
    let module This = struct
      type node = abstract_node
      type label = Flow.label
      let n = n
      let index = index
      let successors = successors
      let iter = iter
    end in
    let module Pred = Complete.LabelledPred(This) in
    Pred.predecessors

  (* This function allows determining which local variables (if any) are directly affected (i.e. written to) at a
     given node. \texttt{jsr} instructions are considered as affecting no variable. So are imaginary nodes. *)

  let affected action = function
    | NodeNormalCompletion
    | NodeExceptionalCompletion _
    | NodeEnd ->
	()
    | NodeConcrete node ->
	match opcode node with
	| Arraylength
	| Arrayload _
	| Arraystore _
	| Athrow
	| Checkcast _
	| Dup_x _
	| Dup2
	| Getfield _
	| Getstatic _
	| Idivrem
	| If _
	| Instanceof _
	| Invokeinterface _
	| Invokespecial _
	| Invokestatic _
	| Invokevirtual _
	| Jsr
	| Load _
	| New _
	| Newarray _
	| Putfield _
	| Putstatic _
	| Ret _
	| Return _
	| Sdivrem
	| StackOp _
	| Swap_x _ ->
	    ()
	| Inc (si, index) ->
	    action index;
	    if si = JavaCard.SiI then
	      action (index + 1)
	| Store (asi, index) ->
	    action index;
	    if asi = JavaCard.AsiI then
	      action (index + 1)

  (* This function allows querying the instruction contained in a given node about its stack usage. The function
     returns a pair of integers. The first (resp. second) integer represents the number of stack words which are
     consumed (resp. produced) by the instruction. Consumption is assumed to take place only along [Normal] and
     [Throw] edges, so the first integer is meaningless at a node which has no such outgoing edges. Production is
     assumed to take place only along [Normal] edges, so the second integer is meaningless at a node which has no
     such outgoing edges. *)

  let stack_usage = function
    | NodeEnd
    | NodeNormalCompletion
    | NodeExceptionalCompletion _ ->
	assert false
    | NodeConcrete node ->
	match opcode node with
	| Arrayload (JavaCard.AbsiA | JavaCard.AbsiB | JavaCard.AbsiS)
	| Sdivrem ->
	    2, 1
	| Arrayload JavaCard.AbsiI ->
	    2, 2
	| Arraystore (JavaCard.AbsiA | JavaCard.AbsiB | JavaCard.AbsiS)
	| Putfield (JavaCard.AbsiI, JavaCard.UseStack, _) ->
	    3, 0
	| Arraystore JavaCard.AbsiI ->
	    4, 0
	| Athrow
	| Putfield ((JavaCard.AbsiA | JavaCard.AbsiB | JavaCard.AbsiS),
		    JavaCard.UseThis, _)
	| Putstatic ((JavaCard.AbsiA | JavaCard.AbsiB | JavaCard.AbsiS), _)
	| Store ((JavaCard.AsiA | JavaCard.AsiS), _) ->
	    1, 0
	| Dup_x (m, 0) ->
	    m, 2 * m
	| Dup_x (m, n) ->
	    n, n + m
	| Dup2 ->
	    2, 4
	| Arraylength
	| Checkcast _
	| Instanceof _
	| Getfield ((JavaCard.AbsiA | JavaCard.AbsiB | JavaCard.AbsiS),
		    JavaCard.UseStack, _)
	| Newarray _ ->
	    1, 1
	| Getfield (JavaCard.AbsiI, JavaCard.UseStack, _) ->
	    1, 2
	| Getfield ((JavaCard.AbsiA | JavaCard.AbsiB | JavaCard.AbsiS),
		    JavaCard.UseThis, _)
	| Getstatic ((JavaCard.AbsiA | JavaCard.AbsiB | JavaCard.AbsiS), _)
	| Jsr
	| Load ((JavaCard.AsiA | JavaCard.AsiS), _)
	| New _ ->
	    0, 1
	| Getfield (JavaCard.AbsiI, JavaCard.UseThis, _)
	| Getstatic (JavaCard.AbsiI, _)
	| Load (JavaCard.AsiI, _) ->
	    0, 2
	| Idivrem ->
	    4, 2
	| If m ->
	    m, 0
	| Inc _
	| Ret _ ->
	    0, 0
	| Invokeinterface (nargs, cpx, token) ->
	    assert false (* TEMPORARY *)
	| Invokespecial cpx ->
	    assert false (* TEMPORARY *)
	| Invokestatic cpx ->
	    assert false (* TEMPORARY *)
	| Invokevirtual cpx ->
	    assert false (* TEMPORARY *)
	| Putfield ((JavaCard.AbsiA | JavaCard.AbsiB | JavaCard.AbsiS),
		    JavaCard.UseStack, _)
	| Putfield (JavaCard.AbsiI, JavaCard.UseThis, _)
	| Putstatic (JavaCard.AbsiI, _)
	| Store (JavaCard.AsiI, _) ->
	    2, 0
	| Return _ ->
	    assert false (* should have no outgoing [Normal] or [Throw] edges *)
	| StackOp (m, n) ->
	    m, n
	| Swap_x (m, n) ->
	    m + n, m + n

  (* Repeat the number of locals. *)

  let locals =
    M.locals

end

