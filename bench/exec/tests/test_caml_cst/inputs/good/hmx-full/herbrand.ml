(* $Header: /home/pauillac/formel1/fpottier/cvs/hmx/herbrand.ml,v 1.22.2.2 2001/12/19 18:00:24 fpottier Exp $ *)

(* This module implements a unification-based constraint system. The system supports (recursive) type terms taken from
   an arbitrary free term algebra, multi-dimensional row terms, unification and conditional unification. It is
   parameterized by the free term algebra, by the type of row labels and by the type of row dimensions. *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Parameterization}

   Our module is parameterized by the type of row labels, the type of row dimensions (typically an enumerated
   type, if there is a finite number of dimensions), and the type of (non-row) terms. If mono-dimensional rows
   are desired, simply use [unit] as dimension type. Row labels are typically strings, but integers allow more
   efficient unification operations. *)

module Make
  (Label : Term.OrderedPrintable)
  (Dimension : Term.OrderedPrintable)
  (T : Term.Algebra) = struct

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Basic type definitions} *)

(* A [node] is a cell which represents a point in the type structure graph. It has physical identity. Its contents
   simply consist of a mutable [link], which leads either directly to a [descriptor], or to another [node], in which
   case the first node is to be viewed as an alias for the second one.

   A type variable is a node. Every sub-term of a large term structure is also a node. *)

type node = {
    mutable link: link
  } 

and link =
  | Immediate of descriptor
  | Link of node

(* A [descriptor] describes the properties which are common to all nodes in a multi-equation. In a way, it can be
   thought of as representing the multi-equation itself. The structure that must be imposed on the members of this
   multi-equation is given in the descriptor's [structure] field. Furthermore, each multi-equation carries an
   integer [rank]. The [mark] field allows marking multi-equations when performing a traversal of the type graph. *)

and descriptor = {
    mutable structure: structure;
    mutable rank: rank;
    mutable mark: mark
  } 

and rank =
    int

and mark =
    unit ref

(* This data type describes all possible forms of structure that can be imposed on a node (or a multi-equation). The
   [Variable] case means that no structure is currently imposed, i.e. the node stands for a type variable. The
   [RowComponent] and [RowUniform] cases represent the two constructors which allow forming rows. They are
   distinguished because they must be unified modulo an equational theory, which is hard-coded into the unification
   algorithm. Other constructors are grouped in the [Term] case, and are unified modulo an empty theory.

   The module implements multi-dimensional rows. These are similar to classic (mono-dimensional) rows, except the
   two row constructors (namely juxtaposition and replication) are parameterized by a dimension.

   If the node is a variable, then it may carry one or more conditional constraints, whose resolution is deferred
   until more structure appears at this node. All of these constraints have a [subject] component equal to the current
   node. *)

and (* $n$ *) structure =
  | Variable of conditional list                   (* $n$-dimensional variable *)
  | Term of (* $n$ *) term                         (* $n$-dimensional term *)
  | Row of dimension * (* $n$ *) row_constructor   (* $n$-dimensional row constructor ($n>0$) *)

and dimension =
    Dimension.t

and (* $n$ *) row_constructor =                    (* $n>0$ *)
  | Juxtaposition of label * (* $n-1$ *) node * (* $n$ *) node
  | Replication of (* $n-1$ *) node

and label =
    Label.t

and term =
    node absterm

and 'a absterm =
    'a T.term

(* A conditional constraint is made up of a [subject], i.e. a type variable carrying the constraint, and a [symbol].
   The constraint will be \emph{triggered} if, and as soon as, the variable is unified with a term whose head
   constructor is the specified symbol. A conditional constraint bearing on a row variable is understood point-wise,
   i.e. as a family of constraints bearing on the variable's components. When a constraint is triggered, its left-hand
   and right-hand members are unified. *)

and conditional = {
    mutable subject: node;
    symbol: symbol;
    left: node;
    right: node
  } 

and symbol =
    T.symbol

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Core iterators} *)

module Structure = struct

  let iter f_node f_conditional = function
    | Variable conds ->
	List.iter f_conditional conds
    | Term term ->
	T.iter f_node term
    | Row (_, Juxtaposition (_, entry, rest)) ->
	f_node entry;
	f_node rest
    | Row (_, Replication entry) ->
	f_node entry

  let map f_node f_conditional = function
    | Variable conds ->
	Variable (List.map f_conditional conds)
    | Term term ->
	Term (T.map f_node term)
    | Row (dim, Juxtaposition (label, entry, rest)) ->
	Row (dim, Juxtaposition (label, f_node entry, f_node rest))
    | Row (dim, Replication entry) ->
	Row (dim, Replication (f_node entry))

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Core functions} *)

(* [repr node] returns a node's representative, that is, a canonical node for which [node] is an alias. It
   internally performs path compression, as in any union/find algorithm. *)

let rec repr node =
  match node.link with
  | Link node' ->
      let node'' = repr node' in
      if node'' != node' then

	(* If [node'] and [node''] differ, then [node'] is an alias for [node'']. Since we just invoked [repr] on
	   [node'], we know that its [link] field is [Link node'']. We copy this value to [node']'s [link] field,
	   thus performing path compression. Note that this function never performs memory allocation, contrary to
	   many naïve implementations. *)

	node.link <- node'.link;
      node''
  | Immediate _ ->
      node

(* [desc node] returns the descriptor associated with the node [node]. *)

let rec desc node =

  (* By not calling [repr] immediately, we optimize the common case where the node is not an alias, at the expense
     of the general case. *)

  match node.link with
  | Immediate desc ->
      desc
  | Link _ ->
      desc (repr node)

(* When invoked, [mark] returns a new mark. A mark is simply a pointer to a [unit] constant, which contains no
   information except its address. Marks may be physically compared.
   
   [no_mark] is a distinguished mark, which freshly created multi-equations carry by default. *)

let mark : unit -> mark =
  ref

let no_mark =
  mark()

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Support for levels} *)

module Level = struct

  (* At any time, there is a notion of \emph{current type level}. This level becomes the default rank of any freshly
     created multi-equations. *)

  let level =
    ref 0

  (* At any time, all existing multi-equations are stored in a global array, indexed by increasing ranks. It would be
     possible to have this array contain lists of \emph{weak} pointers; this would allow unused multi-equations to be
     automatically reclaimed by the garbage collector. However, these multi-equations will be explicitly thrown away
     at every \texttt{let} node (that is, whenever generalization is performed). As a result, it may not be worth the
     trouble to use O'Caml's [Weak] module; we choose to keep the code simple.

     The size of the array is always greater or equal to $[level]+1$, so each valid level is a valid index into the
     array. The array is grown when needed.

     When a multi-equation is created at level $n$, it is placed into this array at index $n$. However, a
     multi-equation's rank may decrease over time, because the unification algorithm updates ranks. Thus, in general,
     the $n$th entry in this array contains multi-equations of rank at most $n$. *)

  let world =
    ref ([| |] : (node list) array)

  (* Conditional constraints are stored in a similar way. The rank of a conditional constraint is the maximum rank
     of its three type components. As in the case of multi-equations, it may decrease over time. *)

  let cworld =
    ref ([| |] : (conditional list) array)

  (* [lowest] is the lowest level, e.g. the level attributed to constant terms. *)

  let lowest =
    1

  (* When invoked, [reset] resets the global state described above to its initial value. *)

  let reset () =
    level := lowest;
    world := Array.create (lowest + 1) [];
    cworld := Array.create (lowest + 1) []

  let () =
    reset()

  (* When invoked, [push] increments the current level. *)

  let enlarge length new_length arrayref =
    let array = Array.create new_length [] in
    Array.blit !arrayref 0 array 0 length;
    arrayref := array

  let push () =
    let length = Array.length !world in
    let new_level = !level + 1 in
    if length <= new_level then begin
      let new_length = 2 * new_level in
      enlarge length new_length world;
      enlarge length new_length cworld
    end;
    level := new_level

  (* When invoked, [pop] decrements the current level. No multi-equations or conditional constraints must remain at
     this level. *)

  let pop () =
    assert (!world.(!level) = []);
    assert (!cworld.(!level) = []);
    decr level

  (* [register rank node] adds the given [node] to the global array [world], at the index [rank]. *)

  let register rank node =
    !world.(rank) <- node :: !world.(rank)

  (* [fresh_at k structure] creates a fresh multi-equation, containing a single node, at level [k], and returns
     this node. The parameter [structure] is stored in the multi-equation descriptor's [structure] field.

     As an optimization, it would be possible to \emph{not} create a fresh node when the [structure] parameter
     describes a constant, e.g. a nullary type constructor. This is not currently done. *)

  let fresh_at k structure =
    
    (* Create a fresh descriptor and a fresh node. *)

    let descriptor = {
      structure = structure;
      rank = k;
      mark = no_mark
    } in
    let node = {
      link = Immediate descriptor
    } in

    (* Register the node. *)

    register k node;
    node

  (* [fresh] creates a node at the current level. *)

  let fresh structure =
    fresh_at !level structure

  (* [crank] computes the rank of a conditional constraint, according to the ranks of its members. *)

  let crank conditional =
    max (desc conditional.subject).rank (max
	(desc conditional.left).rank
	(desc conditional.right).rank)

  (* [cregister conditional] registers a conditional constraint, at an appropriate rank. It must be called whenever
     creating new conditional constraints. It returns its argument. *)

  let cregister_at rank conditional =
    !cworld.(rank) <- conditional :: !cworld.(rank)

  let cregister conditional =
    cregister_at (crank conditional) conditional;
    conditional

end

(* [scope action] executes the specified [action], with the side effect that all nodes freshly created during
   its scope are marked as such. *)

let scope action =
  Level.push();
  let result = action() in
  Level.pop();
  result

(* [capture] records the current generalization level and returns a function which, given a type node, lowers
   its level to the recorded level. *)

let capture () =
  let level = !Level.level in
  fun node ->
    let desc = desc node in
    desc.rank <- min level desc.rank

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Building terms}

   These functions are provided for use by clients. *)

let fresh () =
  Level.fresh (Variable [])

let term term =
  Level.fresh (Term term)

let juxtapose dim label node1 node2 =
  Level.fresh (Row (dim, Juxtaposition (label, node1, node2)))

let replicate dim node =
  Level.fresh (Row (dim, Replication node))

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Unification} *)

(* [unify node1 node2] equates two nodes. (This also involves equating their sons, if these nodes represent structured
   terms, and so on, recursively, until all consequences of this new equality constraint have been found.)

   When two multi-equations are merged, the rank of the new multi-equation is the minimum of the ranks the original
   multi-equations. Notice that this may cause certain multi-equations to have ranks lower than their sons, or greater
   than the maximum rank of their sons. In Rémy's words~\cite{remy-equational-92}, this means that the system may
   no longer be propagated or realized. *)

exception Unify

let rec unify node1 node2 =

  (* Replace the supplied nodes with their canonical representatives. *)

  let node1 = repr node1
  and node2 = repr node2 in

  (* If the two nodes are equal, there is nothing to do. This check is not just an optimization; it is essential
     in guaranteeing termination, since we are dealing with cyclic structures. *)

  if node1 != node2 then

    (* This code is invoked only when [node1] and [node2] are distinct nodes. Before it performs any recursive call,
       it makes [node1] an alias for [node2], or vice versa. Thus, the number of distinct nodes decreases with each
       recursive invocation, which guarantees termination.

       In fact, the argument above holds in the absence of rows. In the presence of row expansion, auxiliary
       variables may be created during unification. However, if sorting rules are preserved, then this process
       must terminate. *)

    let desc1 = desc node1
    and desc2 = desc node2 in

    try

      (* Our first step is to compare the ranks of the two descriptors, and compute their mininum. We define an
	 auxiliary function, [fresh], which creates fresh variables at this rank. We also create three auxiliary
	 functions, which allow making of the two nodes an alias for the other. [keep1] makes the second node an alias
	 for the first; [keep2] does the converse, and [keep_older] makes the newer node an alias for the older. In
	 the latter case, we do not have to update any ranks. *)

      let fresh, keep_older, keep1, keep2 =
	if desc1.rank < desc2.rank then
	  let rank =
	    desc1.rank in
	  let keep1 () =
	    node2.link <- Link node1
	  and keep2 () =
	    node1.link <- Link node2;
	    desc2.rank <- rank in
	  Level.fresh_at rank, keep1, keep1, keep2
	else
	  let rank = desc2.rank in
	  let keep1 () =
	    node2.link <- Link node1;
	    desc1.rank <- rank
	  and keep2 () =
	    node1.link <- Link node2 in
	  Level.fresh_at rank, keep2, keep1, keep2 in

      (* This auxiliary function is defined for convenience. *)

      let unifyt node structure =
	unify node (fresh structure) in

      (* Now, let us look at the structure of the two descriptors. *)

      match desc1.structure, desc2.structure with

      (* If at least one of the two descriptors is a variable, then we make it an alias for the other one; the
	 latter's rank is lowered, if necessary. Any conditional constraints bearing on the former are imposed on the
	 latter.

	 If both are variables, then we have a choice, because we may make either one an alias for the other; we solve
	 it arbitrarily, favoring code simplicity. *)

      | Variable conds1, _ ->
	  keep2();
	  impose conds1 node2
      | _, Variable conds2 ->
	  keep1();
	  impose conds2 node1

      (* If both descriptors have some term structure, then we again have a choice, as in the case of two variables;
	 we keep the older term. In addition, we must propagate consequences over their sub-terms. *)

      | Term term1, Term term2 -> (
	  keep_older();
	  try
	    T.iter2 unify term1 term2
	  with T.Iter2 ->
	    raise Unify
	)

      (* The remaining cases involve at least one row constructor. *)

      |	Row (dim1, cons1), Row (dim2, cons2) ->
	  let c = Dimension.compare dim1 dim2 in
	  if c = 0 then begin

            (* We are working within a single dimension. *)

	    match cons1, cons2 with
	    | Juxtaposition (l1, entry1, rest1), Juxtaposition (l2, entry2, rest2) ->
		let c = Label.compare l1 l2 in
		if c = 0 then begin

	          (* The labels coincide. This is the cheapest case. We perform the same actions as in the case of
		     structural unification above. *)

		  keep_older();
		  unify entry1 entry2;
		  unify rest1 rest2

		end
		else begin

		  (* The labels do not coincide. We again have a choice as to the direction of the link to be
		     created. We choose to view the node with the smallest label (according to the given, fixed
		     total order on labels) as canonical. This strategy will tend to organize rows in increasing
		     order, which will make the cheap case above more frequent, thus allowing rows to be unified in
		     quasi-linear time. *)

		  if c < 0 then keep1() else keep2();

		  (* Impose a common structure on both rows. We need to create an auxiliary row variable, as well as
		     two auxiliary row terms. These terms are directly created at level [rank]. This is an
		     interesting exception to the general rule according to which every newly created node is created
		     at the current level. *)

		  let rest = fresh (Variable []) in
		  unifyt rest1 (Row (dim1 (* say *), Juxtaposition (l2, entry2, rest)));
		  unifyt rest2 (Row (dim1 (* say *), Juxtaposition (l1, entry1, rest)))

		end
	    | Juxtaposition (l1, entry1, rest1), Replication entry2 ->
		
		(* The row is in fact uniform. Keep the second (more compact) representation. *)

		keep2();
		unify entry1 entry2;
		unifyt rest1 desc2.structure

	    | Replication entry1, Juxtaposition (l2, entry2, rest2) ->

		keep1();
		unify entry2 entry1;
		unifyt rest2 desc1.structure

	    | Replication entry1, Replication entry2 ->

		keep_older();
		unify entry1 entry2

	  end
	  else begin
	      
	    (* We are working with two orthogonal dimensions. *)

	    match cons1, cons2 with
	    | Juxtaposition (l1, entry1, rest1), Juxtaposition (l2, entry2, rest2) ->

		(* Recommend smaller dimensions first. *)

		if c < 0 then keep1() else keep2();

		(* Our two-dimensional plane must be divided into four regions. *)

		let l1_meet_l2 = fresh (Variable [])
		and entry1_minus_l2 = fresh (Variable [])
		and entry2_minus_l1 = fresh(Variable [])
		and rest = fresh (Variable []) in

		unifyt entry1 (Row (dim2, Juxtaposition (l2, l1_meet_l2, entry1_minus_l2)));
		unifyt rest1 (Row (dim2, Juxtaposition (l2, entry2_minus_l1, rest)));
		unifyt entry2 (Row (dim1, Juxtaposition (l1, l1_meet_l2, entry2_minus_l1)));
		unifyt rest2 (Row (dim1, Juxtaposition (l1, entry1_minus_l2, rest)))

	    | Juxtaposition (l1, entry1, rest1), Replication entry2 ->

		keep2();

		(* Our two-dimensional plane must be divided into two regions. *)

		let entry2_at_l1 = fresh (Variable [])
		and entry2_minus_l1 = fresh (Variable []) in
		unifyt entry2 (Row (dim1, Juxtaposition (l1, entry2_at_l1, entry2_minus_l1)));
		unifyt entry1 (Row (dim2, Replication entry2_at_l1));
		unifyt rest1 (Row (dim2, Replication entry2_minus_l1))

	    | Replication entry1, Juxtaposition (l2, entry2, rest2) ->

    	        (* This case is symmetric to the previous one. Duplicating the code buys us some extra efficiency. *)

		keep1();

		let entry1_at_l2 = fresh (Variable [])
		and entry1_minus_l2 = fresh (Variable []) in
		unifyt entry1 (Row (dim2, Juxtaposition (l2, entry1_at_l2, entry1_minus_l2)));
		unifyt entry2 (Row (dim1, Replication entry1_at_l2));
		unifyt rest2 (Row (dim1, Replication entry1_minus_l2))

	    | Replication entry1, Replication entry2 ->

		keep_older();

		let common = fresh (Variable []) in
		unifyt entry1 (Row (dim2, Replication common));
		unifyt entry2 (Row (dim1, Replication common))

	  end

      | Row (dim1, Juxtaposition (label1, entry1, rest1)), Term term2 ->

	  (* We have a choice between a representation as a row of terms, or as a term of rows. It is simple to see
	     that the former is more economical (i.e. involves fewer intermediate nodes) if the term's arity is
	     greater than 2, and the latter is more economical otherwise. Both have the same cost when the term's
	     arity is exactly 2. *)

	  if T.arity term2 >= 2 then keep1() else keep2();

	  (* Split every leaf of the term [term2] into a row where the label [label1] appears explicitly. This allows
	     creating two terms with the same head constructor as [term2], one of which describes the contents of the
	     entry at [label1], the other of which describes the remainder. *)

	  let entry, rest = T.fork (split dim1 label1) term2 in

	  (* Given these terms, we can perform unification. *)

	  unifyt entry1 (Term entry);
	  unifyt rest1 (Term rest)

      | Term term1, Row (dim2, Juxtaposition (label2, entry2, rest2)) ->

	  (* This case is symmetric to the previous one. Duplicating the code buys us some extra efficiency. *)

	  if T.arity term1 >= 2 then keep2() else keep1();

	  let entry, rest = T.fork (split dim2 label2) term1 in

	  unifyt entry2 (Term entry);
	  unifyt rest2 (Term rest)

      | Term term1, Row (dim2, Replication entry2) ->

	  (* We have a choice between a representation as a uniform row of terms, or as a term of uniform rows.
	     The former is more economical, unless the term's arity is 0. *)

	  if T.arity term1 > 0 then keep2() else keep1();

	  (* Impose a uniform row structure onto every son of [term1]. Create a term which describes every entry of
	     the uniform row [term1]. *)

	  let term2 = T.map (fun son1 ->
	    let entry = fresh (Variable []) in
	    unifyt son1 (Row (dim2, Replication entry));
	    entry
	  ) term1 in

	  (* Unify this term with [entry2]. *)

	  unifyt entry2 (Term term2)

      | Row (dim1, Replication entry1), Term term2 ->

	  (* This case is symmetric to the previous one. Duplicating the code buys us some extra efficiency. *)

	  if T.arity term2 > 0 then keep1() else keep2();

	  let term1 = T.map (fun son2 ->
	    let entry = fresh (Variable []) in
	    unifyt son2 (Row (dim1, Replication entry));
	    entry
	  ) term2 in

	  unifyt entry1 (Term term1)

    with Unify ->
      
      (* If an exception occurs, we attempt to undo these unification steps, in order to print a better error
	 message. We do not restore ranks, though, so the system will be left in an inconsistent state. This
	 will not be a problem, provided the user drops all of the current type structure after printing the
	 error message. *)

      node1.link <- Immediate desc1;
      node2.link <- Immediate desc2;
      raise Unify

(* [impose conditionals node] imposes the specified conditional constraints (whose [subject] field is considered
   irrelevant, and may be modified in place) onto the specified node, i.e. it makes the node the new subject of the
   constraints. *)

and impose conditionals node =
  if conditionals != [] then begin

    let node = repr node in
    let desc = desc node in
    match desc.structure with
    | Variable conds ->

	(* If the node is a variable, we simply add the new conditional constraints to it, and we modify their
	   [subject] field in place.

	   The list concatenation operation used here is not very efficient asymptotically, but we do not expect
	   a single type variable to carry a large number of conditional constraints. *)

	desc.structure <- Variable (conditionals @ conds);
	List.iter (fun conditional ->
	  conditional.subject <- node
	) conditionals

    | Row (dim, Replication entry) ->

	(* Imposing a conditional constraint onto a uniform row simply amounts to imposing it onto its unique
	   component. *)

	impose conditionals entry

    | Row (dim, Juxtaposition (label, entry, rest)) ->

	(* Imposing a conditional constraint onto a split row requires the constraint's left-hand and right-hand
	   members to be split as well. *)

	let conds_entry, conds_rest = Standard.fork (fun conditional ->

	  (* Split the conditional constraint's conclusion. *)

	  let left_entry, left_rest = split dim label conditional.left
	  and right_entry, right_rest = split dim label conditional.right in

	  (* Emit two new conditional constraints, one of which bears on the entry labeled [label], the other on the
	     rest of the row. *)

	  Level.cregister {
	    subject = conditional.subject; (* for instance *)
	    symbol = conditional.symbol;
	    left = left_entry;
	    right = right_entry
	  },
	  Level.cregister {
	    subject = conditional.subject; (* for instance *)
	    symbol = conditional.symbol;
	    left = left_rest;
	    right = right_rest
	  }

	) conditionals in

	impose conds_entry entry;
	impose conds_rest rest

    | Term term ->

	(* Imposing a conditional constraint on a term either triggers the constraint or has no effect. In any case,
	   the constraint is dropped. *)

	List.iter (fun conditional ->
	  if T.matches conditional.symbol term then
	    unify conditional.left conditional.right
	) conditionals

  end

(* [split label node] unifies the specified [node] with a row which explicitly carries the specified [label] along the
   specified [dimension]. It returns a pair consisting of the row's entry at this label and of its remainder. *)

and split dim label node =
  let fresh = Level.fresh_at (desc node).rank in
  let entry = fresh (Variable [])
  and rest = fresh (Variable []) in
  unify node (fresh (Row (dim, Juxtaposition (label, entry, rest))));
  entry, rest

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Copying utilities} *)

module Copy = struct

  (* The following utility allows preparing a copying pass over (part of) the type structure. It accepts three integer
     arguments. [n] identifies which portion of the type structure is to be copied; by convention, a node will be
     copied if and only if its rank is [n]. [new_rank] specifies the rank of the newly created nodes. They will be
     registered with module [Level] if it is not $-1$. [n'] is the rank to be given to the original nodes after the
     copying pass is over. The last argument, [hook], is called by [restore_conditional] (see below) whenever it
     visits a conditional constraint.

     [prepare] returns five functions. [mapped] allows determining whether a node has been copied. The functions
     [copy] and [copy_conditional] implement the copying pass. The functions [restore] and [restore_conditional]
     restore the original nodes' [rank] field to [n']; this field would otherwise remain invalid, since it is
     destroyed during the copying pass. *)

  let prepare n new_rank n' hook =

    (* The newly created nodes and conditional constraints must be registered unless they are generic (i.e. part of
       a type scheme being built). *)

    let register, cregister =
      if new_rank = -1 then
	let nop _ = () in
	nop, nop
      else
	Level.register new_rank, Level.cregister_at new_rank in

    (* These three functions implement a constant-time mapping from descriptors to nodes. [mapped] allows determining
       whether a given descriptor belongs to the domain of the mapping. [associate] and [retrieve] respectively allow
       extending and looking up the mapping.

       In order to implement a constant-time mapping without wasting space, we use the descriptor's [rank] field,
       which is redundant at this point, since its value must be [n], and store a pointer in it. The field is to be
       viewed as containing a pointer if and only if the descriptor is marked as ``mapped''. *)

    let is_mapped =
      mark() in

    let mapped desc =
      desc.mark == is_mapped

    and associate desc data =
      desc.mark <- is_mapped;
      desc.rank <- Obj.magic data

    and retrieve desc =
      ((Obj.magic desc.rank) : node) in

    (* Let us now define the node copying functions. *)

    let rec copy node =
      let node = repr node in
      let desc = desc node in

      (* If a copy has been created already for this node, return it. Note that we must check this condition first;
	 we must not read [desc.rank] unless we know that the node hasn't been copied. *)

      if mapped desc then
	retrieve desc

      (* Otherwise, check the node's rank. If it is not [n], then we do not need to copy the node. *)

      else if desc.rank <> n then
	node

      (* Otherwise, the node must be copied. Create a new node, update the mapping, and create the node's
	 descriptor. Note that the mapping must be updated before making the recursive call to [copy], so as to
	 guarantee termination. *)

      else begin

	let rec node' = {
	  link = Link node' (* dummy *)
	} in
	register node';
	associate desc node';
	node'.link <- Immediate {
	  structure = Structure.map copy copy_conditional desc.structure;
	  rank = new_rank;
	  mark = no_mark
	};
	node'

      end

    and copy_conditional conditional =
      let conditional = {
        subject = copy conditional.subject;
        symbol = conditional.symbol;
        left = copy conditional.left;
        right = copy conditional.right
      } in
      cregister conditional;
      conditional in

    (* Let us define the second pass. *)

    let rec restore node =
      let node = repr node in
      let desc = desc node in
      if mapped desc then begin
	desc.mark <- no_mark;
	desc.rank <- n';
	Structure.iter restore restore_conditional desc.structure
      end

    and restore_conditional conditional =
      restore conditional.subject;
      restore conditional.left;
      restore conditional.right;
      hook conditional in

    (* Return these functions. Their use is up to our caller. *)

    mapped, copy, copy_conditional, restore, restore_conditional

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Generalization and instantiation} *)

(* A type scheme consists of a node, which represents its body, and a list of conditional constraints, all of which
   bear on old (non-generic) variables, but involve some generic variables as part of their conclusion. *)

type scheme =
    Scheme of node * conditional list

(* [inject] turns a type node into a (trivial) type scheme. *)

let inject node =
  Scheme (node, [])

(* [iter] is similar to [List.iter], but does not require [f] to return [unit] results. Use with caution. *)

let rec iter f = function
  | [] ->
      ()
  | a :: l ->
      let _ = f a in
      iter f l

(* [generalize node] performs generalization, assuming [node] is the (single) entry point into the type structure
   being generalized. The effect of the call is to turn [node] into a type scheme, i.e. a suitable argument for
   [instantiate]. Only those variables which are found to have a rank equal to the current level are generalized.
   All multi-equations stored at the current level in the [world] array are dismissed; they either become generic
   or are re-located at lower ranks in the array. *)

let generalize node =

  (* Walk the list of all multi-equations initially created at the current level. Their current rank must be less
     than, or equal to the current level. Let us sort them by increasing rank, using a simple bucket sort mechanism.
     At the same time, we mark them as ``fresh''. *)

  let sorted =
    Array.create (!Level.level + 1) [] in

  let fresh =
    mark() in

  List.iter (function
    | { link = Link _ } ->

	(* Any multi-equation (i.e. [descriptor] structure) has a node directly pointing to it (otherwise it has
	   become unreachable, and we no longer need to care about it). This node must have been created at a level
	   greater than, or equal to, the multi-equation's current rank, since a multi-equation's rank can only
	   decrease during its lifetime. Hence, if the multi-equation is fresh, then the node must be fresh too. In
	   other words, any reachable fresh multi-equation has a node directly pointing to it in this list. As a
	   result, we can skip any nodes whose [link] field is not of the form [Immediate _]. They would point either
	   to a node associated with an old multi-equation, or to a node which we will find somewhere else in this
	   list. *)

	()

    | { link = Immediate desc } as node ->

	(* Upon finding a fresh multi-equation, mark it as such, and insert it into the proper bucket. *)

	desc.mark <- fresh;
	sorted.(desc.rank) <- node :: sorted.(desc.rank)

  ) !Level.world.(!Level.level);

  (* We may now remove these multi-equations from the global array [world]. Some of them will be generalized, others
     will be re-located at lower ranks. *)

  !Level.world.(!Level.level) <- [];

  (* We will now perform propagation and realization~\cite{remy-equational-92} in the fresh portion of the type
     structure.

     During this phase, fresh multi-equations are marked [fresh], while visited multi-equations are marked [visited].
     Other multi-equations may carry old, unknown marks, because we do not reset marks at the end of this phase.

     The call [explore node] explores the type structure below [node]. It stops at nodes that have already been
     visited. It stops at fresh variable nodes and at nodes that aren't fresh, setting their rank to [k] if it is
     higher. When coming back up, it ensures that the rank of every node is exactly the maximum of the ranks of its
     sons.

     Propagation/realization runs begin with fresh multi-equations of lower ranks, and continue with multi-equations
     of higher ranks. As a result, the level of a node that has been previously visited must be less than or equal to
     [k]. This explains why such nodes need not be visited a second time: no further progress would be made. (If the
     node is currently being visited, however, i.e.  if we have found a cycle in the type structure, this isn't true,
     so our algorithm may generalize more nodes than strictly necessary.) Conversely, it is also the case that the
     level of every non-visited node must be greater than or equal to [k]. This explains why [k] does not need to be
     updated while going down. *)

  let visited =
    mark() in

  for k = 0 to !Level.level do

    let rec explore node =
      let desc = desc node in

      if desc.mark == fresh then begin

	desc.mark <- visited;
	desc.rank <- match desc.structure with
	| Variable _ ->
	    k
	| Term term ->
	    T.fold (fun son accu ->
	      max (explore son) accu
	    ) term Level.lowest
	| Row (dim, Juxtaposition (label, entry, rest)) ->
	    max (explore entry) (explore rest)
	| Row (dim, Replication entry) ->
	    explore entry

      end
      else if desc.mark != visited then begin

	(* The current node is old. Update its rank, but do not pursue the computation any further. Indeed, we want to
	   deal only with the fresh portion of the type structure. The computation will be resumed when the next level
	   is popped. *)

	desc.mark <- visited;
	if k < desc.rank then
	  desc.rank <- k

      end;

      desc.rank

    in
    iter explore sorted.(k)

  done;

  (* Look at conditional constraints of the freshest level. We first discard all those whose subject is not a
     variable; these are garbage, because they must have been resolved or split into more elementary constraints.
     Then, we look at those which remain. Given the updated ranks computed above, some of them will turn out to be old
     and are relocated at lower levels. Among the others, we distinguish those whose subject is old (which we call the
     ``entry points'', because they are new constraints bearing on old variables), which we collect into a list,
     [entry_conditionals]. *)

  let entry_conditionals =
    List.fold_left (fun entry_conditionals conditional ->

      match (desc conditional.subject).structure with
      |	Variable _ ->

	  let rank = Level.crank conditional in
	  if rank < !Level.level then begin

	    (* The constraint involves old nodes only; relocate it. *)

	    !Level.cworld.(rank) <- conditional :: !Level.cworld.(rank);
	    entry_conditionals
	  end
	  else if (desc conditional.subject).rank < !Level.level then

	    (* The constraint is an entry point from the old into the new type structure. *)

	    conditional :: entry_conditionals

	  else

	    (* The constraint is part of the new type structure; ignore it for now. *)

	    entry_conditionals

      |	Row _
      |	Term _ ->

	  (* The constraint has been resolved already; drop it. *)

	  entry_conditionals

    ) [] !Level.cworld.(!Level.level) in

  !Level.cworld.(!Level.level) <- [];

  (* We now perform generalization, i.e. we copy all nodes of highest rank which are reachable from the entry
     points. These entry points are the node being generalized, on the one hand, and all conditional constraints
     bearing on old nodes, on the other hand. Note that this process can be viewed as a form of garbage collection,
     since any unreachable fresh nodes are dropped. 

     Generic nodes (i.e. nodes created during the copying process) are given rank $-1$, so they can be identified
     by the instantiation function. They are not registered, since they have no existence in the global constraint
     set.

     After copying, we restore \emph{only} those nodes which are reachable from the entry point conditionals. Indeed,
     these nodes may express constraints on old variables, thus they must be kept in the global constraint set. Their
     rank is restored to $n-1$, rather than $n$, because no variables must remain at rank $n$ after we're done.

     Any nodes which are not restored (because they are not reachable from the entry point conditionals) will be
     dropped below. Note that, in the particular case where conditional constraints are unused, all copied variables
     are dropped. (This means that the copying operation is superfluous; however, in the general case, it is
     necessary. Since it has linear cost, this is acceptable.) *)

  let copied, copy, ccopy, _, crestore =
    Copy.prepare
      !Level.level                                (* copy the variables with highest rank *)
      (-1)                                        (* give rank -1 to the copies *)
      (!Level.level - 1)                          (* after copying, decrement the rank of the original variables *)
      (Level.cregister_at (!Level.level - 1)) in  (* re-register any conditional constraints which are kept *)

  let scheme =
    Scheme(copy node, List.map ccopy entry_conditionals) in

  List.iter crestore entry_conditionals;

  (* The code above has already re-registered any conditional constraints which must be kept. There remains to
     re-register any freshly created multi-equation which has been found not to remain fresh. *)

  Array.iter (
    List.iter (function node ->
      let desc = desc node in
      if (not (copied desc)) & (desc.rank < !Level.level) then
	Level.register desc.rank node
    )
  ) sorted;

  (* We are done generalizing. *)

  scheme

(* [instantiate node] accepts a type scheme [node], and takes a fresh copy of it at the current level. *)

let instantiate (Scheme(body, entry_conditionals)) =

  (* Take a fresh copy of the type scheme's generic variables at the current level. *)

  let _, copy, ccopy, restore, crestore =
    Copy.prepare
      (-1)              (* copy generic variables *)
      !Level.level      (* create instance variables at the current level. *)
      (-1)              (* let the original variables remain generic *)
      (fun _ -> ()) in

  let body' = copy body
  and entry_conditionals' = List.map ccopy entry_conditionals in

  restore body;
  List.iter crestore entry_conditionals;

  (* Every entry point conditional must be added to the global constraint set via the function [impose], since its
     subject node might have received new information since it was created (e.g. it might have been unified with a
     term or with a row). *)

  List.iter (fun conditional ->
    impose [ conditional ] conditional.subject
  ) entry_conditionals';

  (* We are done. *)

  body'

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Printing types} *)

module Print = struct

  (* Implement a mapping from nodes to names. Because nodes have no identity other than physical, the only way of
     associating information with a node is to use physical equality. This makes the mapping linear time instead
     of constant time. We do not care, since this only affects user interaction, not batch computations. *)

  let alphabet =
    ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"; "k"; "l"; "m";
     "n"; "o"; "p"; "q"; "r"; "s"; "t"; "u"; "v"; "w"; "x"; "y"; "z"]

  let history =
    ref []

  let index =
    ref 0

  let available =
    ref []

  let reset () =
    history := [];
    index := 0;
    available := alphabet

  let rec next_name () =
    match !available with
    | name :: rest ->
	available := rest;
	name
    | [] ->
	incr index;
	let index = string_of_int !index in
	available := List.map (fun s -> s ^ index) alphabet;
	next_name()

  (* As a side effect, [name] stores the supplied conditional constraints into the global reference [conditionals]
     if the given [node] hasn't been encountered so far. *)

  let conditionals =
    ref []

  let name conds node =
    try
      List.assq node !history
    with Not_found ->
      conditionals := conds @ !conditionals;
      let name = next_name() in
      let rank = (desc node).rank in
      let name =
	if rank < 0 then
	  Printf.sprintf "'%s" name
	else
	  Printf.sprintf "'_%s" name in
      history := (node, name) :: !history;
      name

  (* Rather than directly turn a type graph into a string, we first produce intermediate trees, then turn the
     trees into strings. The main difficulty, during the first pass, is to deal with recursive structure, whereas,
     during the second pass, it is to deal with parentheses. *)

  module Intermediate = struct

    type tree =
      |	TVar of string
      |	TRowJuxtaposition of dimension * label * tree * tree
      |	TRowReplication of dimension * tree
      |	TAs of tree * string
      |	TTerm of tree absterm

    type label =
      |	LRowJuxL of dimension
      |	LRowJuxR of dimension
      |	LRowRepl
      |	LAs
      |	LTerm of T.label

    open Tree

    (* At the moment, dimensions are dropped when converting trees into streams. This allows much lighter notation,
       and is still (mostly) unambiguous if labels along different dimensions are taken from different universes. *)

    let describe = function
      |	TVar v ->
	  [ Token v ]
      |	TRowJuxtaposition (dim, label, entry, row) ->
	  [ Token ((Label.print label) ^ ": "); Son (LRowJuxL dim, entry); Token "; "; Son (LRowJuxR dim, row) ]
      |	TRowReplication (dim, row) ->
	  [ Token "\\"; Son (LRowRepl, row) ]
      |	TAs (t, v) ->
	  [ Son (LAs, t); Token " as "; Token v ]
      |	TTerm term ->
	  List.map (function
	    | Token token ->
		Token token
	    | Son (label, son) ->
		Son (LTerm label, son)
          ) (T.print term)

    let parenthesize label tree =
      match label, tree with
      |	LAs, (TRowJuxtaposition _ | TRowReplication _ | TTerm _) ->
	  true
      |	LAs, (TAs _ | TVar _) ->
	  assert false

      |	_, TVar _ ->
	  false

      |	(LRowJuxL _ | LRowRepl), TRowJuxtaposition _ ->
	  true
      |	LRowJuxR dim1, TRowJuxtaposition (dim2, _, _, _) ->
	  Dimension.compare dim1 dim2 <> 0

      |	(LRowJuxR _ | LRowRepl), TRowReplication _ ->
	  false
      |	LRowJuxL _, TRowReplication _ ->
	  true

      |	(LRowJuxL _ | LRowJuxR _), (TAs _ | TTerm _) ->
	  false
      |	LRowRepl, TAs _ ->
	  true
      |	LRowRepl, TTerm term ->
	  T.arity term > 0

      |	LTerm label, (TRowJuxtaposition _ | TRowReplication _ | TAs _) ->
	  not (T.safe label)
      |	LTerm label, TTerm term ->
	  T.parenthesize label term

    (* Turning types into intermediate trees. As a side effect, this function stores conditional constraints bearing
       on the variables it encounters. *)

    let convert node =

      let visiting = mark()
      and hit = mark() in

      let rec convert node =
	let node = repr node in
	let desc = desc node in

	(* Check whether this node was visited already. If so, then we mark it as ``hit again'', so as to record the
	   fact that we need to introduce a $\mu$ binder at this node when going back up. *)

	if (desc.mark == visiting) || (desc.mark == hit) then begin
	  desc.mark <- hit;
	  TVar (name [] node)
	end
	else begin

	  (* This node has never been visited. *)

	  (* We must detect cycles in the type structure. Before being processed, the current node is marked. Then,
	     processing is allowed to take place. When done, we check whether the node was hit recursively; if so, we
	     annotate it with a $\mu$ binder. When done, we remove the mark. *)

	  desc.mark <- visiting;

	  let result = 
	    match desc.structure with
	    | Variable conds ->
		TVar (name conds node)
	    | Row (dim, Juxtaposition (label, entry, rest)) ->
		TRowJuxtaposition (dim, label, convert entry, convert rest)
	    | Row (dim, Replication entry) ->
		TRowReplication (dim, convert entry)
	    | Term term ->
		TTerm (T.map convert term) in

	  let result =
	    if desc.mark == hit then
	      TAs (result, name [] node)
	    else
	      result in

	  desc.mark <- no_mark;
	  result

	end in

      convert node

  end

  (* Turning intermediate trees into strings. *)

  module Printer =
    Tree.Make(Intermediate)

  (* Composing the two transformations. *)

  let print node =
    Printer.print (Intermediate.convert node)

  (* Dealing with constraints. *)

  let print_conditional conditional =
    Printf.sprintf "\nif %s matches %s then %s = %s"
      (print conditional.subject)
      (T.sprint conditional.symbol)
      (print conditional.left)
      (print conditional.right)

  let print body =
    conditionals := [];
    let rec loop text =
      match !conditionals with
      |	[] ->
	  text
      |	conditional :: others ->
	  conditionals := others;
	  loop (text ^ (print_conditional conditional)) in
    loop (print body)

  (* External interface. *)

  let node =
    print

  let scheme = function
    | Scheme (body, []) ->
	print body
    | _ ->
	assert false (* unimplemented; should never (seldom?) happen with a toplevel type scheme *)

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Building constraints}

   These functions are provided for use by clients.

   We are now able to associate messages with unification errors. The message is not guaranteed to make sense,
   because we generate it \emph{after} destructive unification has been performed (and has failed). Even though
   the unification algorithm attempts to undo its effects when failing, not everything is undone (ranks and
   conditional constraints are not dealt with).

   One better (but slower) way of generating error messages would be to systematically generate the message
   \emph{before} attempting to perform unification. *)

exception Inconsistency of string

let unify node1 node2 =
  try
    unify node1 node2
  with Unify ->
    raise (Inconsistency (Printf.sprintf "\nCannot unify\n%s\nwith\n%s"
		     (Print.node node1)
		     (Print.node node2)))

let conditionally subject symbol left right =
  impose [ Level.cregister {
	     subject = subject;
	     symbol = symbol;
	     left = left;
	     right = right
           } ] subject

(*i --------------------------------------------------------------------------------------------------------------- i*)

end

