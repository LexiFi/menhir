(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/core.ml,v 1.39.2.3 2001/03/19 18:25:24 fpottier Exp $ *)

(* This module defines the library's fundamental data structures, used to represent type variables and subtyping
   constraints. *)

module type S = sig

  type symbol
  type kind
  type 'a expression
  type 'a coercion
  type 'a preterm
  type 'a set1
  type 'a set2
  type 'a set3

  (* To implement rows, we need maps from labels to types. The type of row labels is kept abstract in this module;
     the polymorphic type ['a row_map] is assumed to provide maps from labels to some type ['a]. We also need to
     represent ``spans'', which are sets of labels; we assume another abstract type, [span].

     A row consists of a set of labelled entries (i.e. a finite map from labels to content), plus one piece of
     remainder data. This definition of rows is polymorphic in the content's type. *)

  type 'a row_map
  type span

  type 'a row = {
      entries: 'a row_map;
      remainder: 'a
    } 

  (* A [variable] is a structure which carries constraints. *)

  type variable = {

      (* Each variable is assumed to appear within at most one constraint set. This allows us to store information
	 about the variable's lower and upper bounds within the variable itself. A variable's lower (resp. upper)
	 bounds consist of a single type term, together with a set of variables. *)

      mutable lo: term;
      mutable loset: leaf_set;
      mutable hiset: leaf_set;
      mutable hi: term;

      (* Additionally, each variable carries a list of conditional constraints, to be triggered if the variable's
	 lower bound outgrows a certain value. *)

      mutable conditionals: conditional_set

    } 

  (* A ``leaf'' is either a variable or a row. Leaves are our central data structure: they constitute the nodes in the
     constraint graph. Edges coming out of a given leaf are either subtyping constraints, if the leaf is a variable,
     or equality edges, if the leaf is a row. *)

  and leaf_link =
    | VarLink of variable
    | RowLink of leaf row

  and leaf = {

      (* Each leaf carries a unique integer stamp. This allows defining a total ordering on leaves, a property
	 required by certain data structures, mostly sets of leaves. *)

      stamp: int;

      (* A field allows marking leaves while traversing constraints. It should not be used directly; appropriate
	 operations are provided by the [Traverse] sub-module. *)

      mutable traversal: int;

      (* This field is used to speed up various substitution and copy operations. *)

      mutable representative: leaf;

      (* Because every row variable may be expanded into a row, a mutable field is used to keep track of the
	 relationship between the leaf and the entity it stands for. *)

      mutable link: leaf_link;

      (* According to the theory of rows, each leaf carries a sort, which tells whether it represents a regular type
	 or a row type. In the latter case, the sort also tells which labels \emph{cannot} be extracted out of this
	 row.

	 In practice, all closure operations preserve well-sortedness, so one may think that it is not necessary to
	 keep track of sorts at run-time. Does this intuition hold? Here's the answer:
	 \begin{itemize}
	 \item We choose not to represent the $\urow$ constructor within our data structures. Instead, its presence
	       shall be implicitly detected whenever a row mismatch is found. So, we must explicitly keep track of
               the distinction between regular variables and row variables.
         \item Minimization, in its default formulation, does not preserve well-sortedness. (For instance,
               unconstrained variables of different sorts will be merged.)
	 \end{itemize}
	 For these two reasons, we do keep track of sorts. A leaf's sort is either ``regular'', or ``row''.
	 In the latter case, the sort also includes a set of non-admissible labels, which we call the leaf's
	 ``span''. *)

      sort: sort;

      (* This field is used by the closure algorithm. Given a fresh leaf, it allows determining, in constant
	 time, which least upper bound (resp. greatest lower bound) expression it stands for.

	 Notice that we only store a set of original leaves, not the operator (join or meet) that was applied
	 to them. This is because it shall be always be known from the context. (More precisely, a fresh leaf
	 created by a LUB computation shall never be involved in a subsequent GLB computation, and vice-versa.)

	 This field is [None] when outside of the closure algorithm. While the algorithm is running, it is [None] in
	 an original leaf, and points to a set of original leaves in a fresh leaf. *)

      (* TEMPORARY could re-use representative (with some magic) field and save space (one field in this structure,
	 plus no need to use an option, just compare (==) with the leaf itself as usual). Dirty, though. *)

      mutable originals: original_set option;

      (* Each leaf may carry a sign (neutral, positive, negative or both). This piece of information is not
	 necessarily meaningful at all times; it is computed by the garbage collection algorithm. It would be
	 possible to store it only in variables, rather than in every leaf, but this is slightly simpler. *)

      mutable sign: sign;

      (* Each leaf carries an integer rank, which tells when it was created. This allows distinguishing a stack
	 of generations (entering a new \emph{let} binding causes a new generation to be created; it is turned
	 into a type scheme upon exit).

	 Because we perform no online unification, ranks are immutable. This situation may change if we choose
	 to perform online cycle elimination.

	 Note that all leaves which are part of a given row must have the same rank. Thus, it would be possible
	 to associate ranks to variables, rather than to leaves. However, this seems more convenient. *)

      rank: int

    } 

  and sort =
    | Regular
    | Row of span

  and sign = 
    | Neutral
    | Positive
    | Negative
    | Bipolar

  and conditional = symbol * leaf * leaf

  and term = leaf preterm
  and leaf_set = leaf set1
  and conditional_set = conditional set2
  and original_set = leaf set3

  (* A total ordering on leaves, used when storing leaves in sets, association maps, or other similar structures. *)

  val clv: leaf -> leaf -> int

  (* The following set of operations allow managing the library's notion of ``generations''. At all times, the library
     maintains an internal integer ``level''. Any freshly created type variables (see module [Fresh]) receive this
     level as their rank. Thus, by incrementing and decrementing the level at appropriate times, the caller can
     delimit a stack of generations, where freshest variables form the youngest generation. This allows delimiting the
     work performed by several algorithms (garbage collection, minimization, generalization) which only affect the
     fresh generation. Thus, [push] and [pop] are typically called when adding entries to (or taking entries from)
     the typing environment (i.e. at \texttt{let} and $\lambda$ nodes). *)

  module Level : sig

    val level: int ref (* TEMPORARY hide it *)

    (* [reset] resets the current level to the lowest level. [push] increments it, while [pop] decrements it. *)

    val reset: unit -> unit
    val push: unit -> unit
    val pop: unit -> unit

  end

  (* The following set of operations allows creating fresh variables. *)

  module Fresh : sig

    (* [lo s t] (resp. [hi s t]) creates a fresh variable leaf of sort [s] and makes [t] its lower (resp. upper)
       constructed bound. *)

    val lo: sort -> term -> leaf
    val hi: sort -> term -> leaf

    (* [pair s k] creates a pair of fresh variable leaves of sort [s] and kind [k], and links them with a subtyping
       constraint. *)

    val pair: sort -> kind -> leaf * leaf

  end

  module Traverse : sig

    (* The following operation allows applying a specified action to each fresh leaf currently in existence.
       The caller is expected to supply a list of entry points into the constraint graph. (Typically, there
       will be one entry point: the type of the current expression.)

       The action function is passed each leaf \emph{before} the constraints (or link) bearing upon it are
       examined. Thus, if the user modifies this information, the graph traversal \emph{will} be influenced. *)

    val fresh: (leaf -> unit) -> leaf list -> unit

  end

  module Row : sig

    (* [link lf] returns the contents of [lf]'s [link] field, with one additional guarantee: if the function returns
       [RowLink row], then [row] is guaranteed to be ``clean'', i.e. its remainder leaf is guaranteed to be a
       variable. The function internally performs path compression, so as to speed up further accesses. *)

    val link: leaf -> leaf_link

    (* [iter action row] applies the specified action to all elements of a row, including its remainder. The action
       function does not have access to each element's label. *)

    val iter: ('a -> unit) -> 'a row -> unit

  end

  (* The following operation allows taking a fresh copy of a type scheme, i.e. a new type scheme, which is identical,
     except each variable has been replaced with a fresh copy. This reflects the fact that, in our current theory,
     every variable which appears in a type scheme must be understood as universally quantified. *)
(* TEMPORARY
  module Copy : sig

    val scheme: 'a scheme_mapper -> 'a

  end
*)
  (* The following set of operations allow (incrementally) computing the closure of a constraint graph. *)

  module Closure : sig

    (* The exception [Inconsistent] is raised when the closure algorithm discovers an inconsistency. *)

    exception Inconsistent

    (* Calling [link lf1 lf2] adds a constraint from [lf1] to [lf2] in the constraint graph, and performs suitable
       closure operations. It raises [Inconsistent] if this operation makes the constraints unsatisfiable. [lf1] and
       [lf2] must have the same kind. *)

    val link: leaf -> leaf -> unit

    (* Calling [lo term lf] makes [term] a lower bound of [lf], and performs suitable closure operations. It raises
       [Inconsistent] if this operation makes the constraints unsatisfiable. [term] and [lf] must have the same
       kind. [lf] must be a variable (i.e. not a row). [hi lf term] behaves symmetrically. *)

    val lo: term -> leaf -> unit
    val hi: leaf -> term -> unit

    (* [conditional s lf0 lf1 lf2] creates a new conditional constraint $\cc{s}{[lf0]}{[lf1]}{[lf2]}$. It raises
       [Inconsistent] if this operation makes the constraints unsatisfiable. [lf0], [lf1] and [lf2] must have the same
       span (i.e. they may be regular or row variables, but all row variables involved must have the same span). [s]
       and [lf0] must have the same kind; [lf1] and [lf2] must have the same kind. *)

    val conditional: symbol -> leaf -> leaf -> leaf -> unit

    (* [meet] (resp. [join]) returns a fresh leaf which stands for the meet (resp. join) of its two arguments.
       It is simply a shortcut for two appropriate calls to [link]. Its two arguments must have the same kind. *)

    val meet: leaf -> leaf -> leaf
    val join: leaf -> leaf -> leaf

  end

  (* The following function performs in-place garbage collection on the fresh portion of the type graph. As a
     side-effect, every fresh variable is annotated with its correct sign. The function expects two parameters [neg]
     and [pos], which are considered lists of negative and positive entry points, respectively. (Typically, there will
     only be one positive entry point: the type of the current expression.) *)

  module Garbage : sig

    val collect: leaf list -> leaf list -> unit

  end

  (* The following function performs minimization of the specified type scheme. That is, it classifies leaves into
     equivalence classes, and chooses a representative in each class. It then proceeds to replace each leaf with its
     representative within the type scheme. *)
(* TEMPORARY
  module Minimize : sig

    val minimize: scheme -> 'a scheme_mapper -> 'a

  end
*)
  (* The following module allows translating type scheme expressions into actual (internal) type schemes. It
     accepts a type scheme expression, expressed in the form of
     \begin{itemize}
     \item a function which allows iterating over the scheme's entries (where each entry consists of a type
           expression), and provides their expected kind;
     \item a list of constraints, which are pairs of type expressions.
     \end{itemize}
     The function returns a translation function, which should be applied to the type scheme's entry points
     to produce the final (internal) type scheme. In other words, the translation function should be passed
     to the [smap] function over concrete type schemes.

     The [expression] function or the translation function will raise [Kind.Inconsistency] if the type scheme
     expression is ill-kinded. The latter will raise [Kind.UnderSpecified] if some type variable's kind cannot be
     determined. The exception carries the name of one such type variable. This may occur if some constraints are
     unreachable from the type scheme's entry points.

     Each of the scheme's entries is implicitly assumed to have sort [Regular], which is why the [entries]
     function is not required to provide each entry's sort.

     The [expression] function or the translation function will raise [Sort.Inconsistency] if the type scheme
     expression is ill-kinded. The latter will raise [Sort.UnderSpecified] if some
     expression's sort cannot be determined. This may occur if some constraints are unreachable from the type scheme's
     entry points.

     During translation, expressions are internally annotated with information of type [info], which remains
     abstract. *)

  module Translate : sig
    
    type info

    type entries =
	(kind -> info expression -> unit) -> unit

    and constraints =
	(info coercion) list

    module Kind : sig

      exception Inconsistency
      exception UnderSpecified of string

    end

    module Sort : sig

      exception Inconsistency
      exception UnderSpecified

    end

    type translation_function =
      |	TransFun of (bool -> info expression -> leaf)

    val expression: entries -> constraints -> translation_function

  end

end

(* This module is parameterized over:
   \begin{itemize}
   \item a ground signature, [G];
   \item an implementation of sets, for use with leaves, [LeafSet];
   \item an implementation of sets, for use with conditional constraints, [CondSet];
   \item an implementation of maps whose keys are sets, for use by the closure algorithm, [LeafSetMap];
   \item an implementation of maps whose keys are row labels, for use in rows, [RowMap].
   \item a way of turning strings into row labels, provided by module [Label].
   \end{itemize} *)

module Make 
    (G : Ground.Signature)
    (LeafSet : LeafSet.S)
    (CondSet : CondSet.S)
    (LeafSetMap : LeafSetMap.S)
    (RowMap : RowMap.S)
    (Label : Label.S with type t = RowMap.key)
= struct

  type 'a row_map = 'a RowMap.t
  type span = RowMap.Domain.t

  type symbol = G.Symbol.t
  and kind = G.Kind.t
  and 'a expression = 'a G.Abstract.expression
  and 'a coercion = 'a G.Abstract.coercion
  and 'a preterm = 'a G.Term.t
  and 'a set1 = 'a LeafSet.t
  and 'a set2 = 'a CondSet.t
  and 'a set3 = 'a LeafSetMap.Set.t

  type 'a row = {
      entries: 'a row_map;
      remainder: 'a
    } 

  type variable = {
      mutable lo: term;
      mutable loset: leaf_set;
      mutable hiset: leaf_set;
      mutable hi: term;
      mutable conditionals: conditional_set
    } 

  and leaf_link =
    | VarLink of variable
    | RowLink of leaf row

  and leaf = {
      stamp: int;
      mutable traversal: int;
      mutable representative: leaf;
      mutable link: leaf_link;
      sort: sort;
      mutable originals: original_set option;
      mutable sign: sign;
      rank: int
    } 

  and sort =
    | Regular
    | Row of span

  and sign = 
    | Neutral
    | Positive
    | Negative
    | Bipolar

  and conditional = symbol * leaf * leaf

  and term = leaf preterm
  and leaf_set = leaf set1
  and conditional_set = conditional set2
  and original_set = leaf set3

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Dealing with sets}

   Storing data into a set usually requires a total ordering function to be available. We define several such
   functions below. They receive very short names, to avoid visual clutter when invoking set operations. [clv]
   stands for ``compare leaves'', while [cc] stands for ``compare conditionals''. *)

  let clv lf1 lf2 =
    lf1.stamp - lf2.stamp

  let cc (head1, lf1, mf1) (head2, lf2, mf2) =
    let result = Pervasives.compare head1 head2 in
    if result <> 0 then result
    else let result = clv lf1 lf2 in
    if result <> 0 then result
    else clv mf1 mf2

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Creating fresh variables} *)

  (* This utility converts a leaf, which must represent a variable, into the latter. *)

  let leaf_to_variable lf =
    match lf.link with
    | VarLink v ->
	v
    | RowLink _ ->
	assert false

  (* These functions are used in debugging assertions. *)

  let is_var_leaf lf =
    match lf.link with
    | VarLink _ ->
	true
    | RowLink _ ->
	false

  let is_clean_row row =
    is_var_leaf row.remainder

  let is_row_sort = function
    | Regular ->
	false
    | Row _ ->
	true

  module Level = struct

    (* At any time, there is a notion of \emph{current type level}. This level becomes the default rank of any freshly
       created variables. *)

    let level =
      ref 0

    (* We maintain a data structure which records ``entry points'' from an older generation into a newer one; that is,
       leaves of rank $m$ carrying a constraint which involves a leaf of rank $n$, where $m<n$.

       The size of the array is always greater or equal to $[level]+1$, so each valid level is a valid index into the
       array. The array is grown when needed.

       The array may contain leaves of sort [Row _]. If such a leaf is expanded, any leaf which occurs in the row
       should be viewed as (implicitly) part of the frontier (because it has the same rank as the original leaf, and
       must also carry constraints to leaves of higher rank). Given this convention, new leaves are not explicitly
       added to the frontier when performing expansion. *)

    let frontier =
      ref ([| |] : leaf_set array)

    (* [lowest] is the lowest level. *)

    let lowest =
      1

    (* When invoked, [reset] resets the global state described above to its initial value. It is automatically
       invoked when the library starts up. *)

    let reset () =
      level := lowest;
      frontier := Array.create (lowest + 1) LeafSet.empty

    let () =
      reset()

    (* When invoked, [push] increments the current level. *)

    let enlarge length new_length arrayref =
      let array = Array.create new_length LeafSet.empty in
      Array.blit !arrayref 0 array 0 length;
      arrayref := array

    let push () =
      let length = Array.length !frontier in
      let new_level = !level + 1 in
      if length <= new_level then begin
	let new_length = 2 * new_level in
	enlarge length new_length frontier
      end;
      level := new_level

    (* When invoked, [pop] decrements the current level. Nothing must remain at this level. *)

    let pop () =
      assert (LeafSet.is_empty !frontier.(!level));
      decr level

  end

  module Fresh = struct

    (* An internal global counter is used to assign each leaf a unique stamp. Overflows are ignored, which may lead to
       problems if the library is used over an extended period of time. *)

    let current_stamp = ref min_int

    (* This internal function creates a fresh leaf with specified [rank], [sort] and [link]. *)

    let leaf rank sort link = 

      let new_stamp = !current_stamp + 1 in
      current_stamp := new_stamp;

      let rec leaf = {
        stamp = new_stamp;
	traversal = min_int;
	representative = leaf;
	link = link;
	sort = sort;
	originals = None;
	sign = Neutral;
	rank = rank
      }	in

      leaf

    (* This internal function creates a fresh leaf, standing for a fresh variable. The variable's rank and sort, as
       well as its lower and upper bounds, must be passed as parameters.

       If the leaf is created at a rank other than [!level], then the caller must be careful to add it to the
       frontier if [lo] and [hi] contain leaves of higher rank. *)

    let create rank sort lo hi =
      let v = {
	lo = lo;
	loset = LeafSet.empty;
	hiset = LeafSet.empty;
	hi = hi;
	conditionals = CondSet.empty
      } in
      leaf rank sort (VarLink v), v

    (* This auxiliary function determines a leaf's kind, by returning some unspecified term of the same kind. 

       This is made slightly tricky by the fact that leaves do not directly carry kind information. It is only encoded
       in variables' constructed bounds. *)

    let rec kind_of_leaf lf =
	match lf.link with
	| VarLink v ->
	    v.lo (* for instance *)
	| RowLink row ->
	    kind_of_leaf row.remainder

    (* [after lf] creates a fresh leaf, standing for a fresh variable, of rank [rank], and of the same sort and kind
       as [lf]. *)

    let after rank lf =
      let term = kind_of_leaf lf in
      let lf', _ = create rank lf.sort (G.Term.bottom term) (G.Term.top term) in
      lf'

    (* [lo s t] (resp. [hi s t]) creates a fresh variable leaf of sort [s] and makes [t] its lower (resp. upper)
       constructed bound. *)

    let lo sort lo =
      let lf, _ = create !Level.level sort lo (G.Term.top lo) in
      lf

    let hi sort hi =
      let lf, _ = create !Level.level sort (G.Term.bottom hi) hi in
      lf

    (* [pair s k] creates a pair of fresh variable leaves of sort [s] and kind [k], and links them with a subtyping
       constraint. It is quite efficient, since it does not call the closure algorithm. Besides, it shares calls to
       [G.Kind.bottom] and [G.Kind.top]. *)

    let pair sort kind =
      let bottom = G.Kind.bottom kind
      and top = G.Kind.top kind in
      let lf1, v1 = create !Level.level sort bottom top
      and lf2, v2 = create !Level.level sort bottom top in
      v1.hiset <- LeafSet.singleton lf2;
      v2.loset <- LeafSet.singleton lf1;
      lf1, lf2

  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Row operations} *)
      
  module Row = struct

    (* [link lf] returns the contents of [lf]'s [link] field, with one additional guarantee: if the function returns
       [RowLink row], then [row] is guaranteed to be ``clean'', i.e. its remainder leaf is guaranteed to be a
       variable. The function internally performs path compression, so as to speed up further accesses. *)

    let rec link lf =
      let link = lf.link in
      match link with
      |	RowLink row ->
	  let row' = normalize row in
	  if row != row' then
	    let link' = RowLink row' in
	    lf.link <- link';
	    link'
	  else
	    link
      |	VarLink _ ->
	  link

    (* [normalize row] expects a row [row], which may or may not be clean, and returns a normalized version of [row],
       i.e. a clean row with the same contents. The function internally performs path compression, so as to speed up
       further accesses. *)

    and normalize row =
      match link row.remainder with
      |	VarLink _ ->
	  row
      | RowLink remainder_row ->

	  (* Our remainder is some other row. Merge the two rows within a single one. Their domains are of course
	     disjoint, since our constraints are well-sorted. *)

	  {
	    entries = RowMap.fine_union
	                (fun _ _ -> assert false)
                        row.entries remainder_row.entries;
	    remainder = remainder_row.remainder
	  }

    (* The three following functions are iterators on rows.

       [iter action row] applies the specified action to all elements of a row, including its remainder. (It always
       begins with the remainder, because the minimization algorithm requires this feature.) The action function does
       not have access to each element's label. [iter2] performs a similar job, but expects two rows with identical
       structure (i.e. label sets). [map action row] returns a new row obtained by applying [action] to each element
       of [row]. No attempt at sharing is made. [endo_map] performs the same task as [map], but tries to return the
       same physical record, if possible. *)

    let iter action row =
      action row.remainder;
      RowMap.iter (fun key content -> action content) row.entries

    let iter2 action row1 row2 =
      RowMap.iter2 (fun key content1 content2 ->
	action content1 content2
      ) row1.entries row2.entries;
      action row1.remainder row2.remainder

    let map action row = {
      entries = RowMap.map action row.entries;
      remainder = action row.remainder
    } 

    let endo_map action ({ entries = entries; remainder = remainder } as row) =
      let entries' = RowMap.endo_map action entries
      and remainder' = action remainder in
      if (entries == entries') & (remainder == remainder') then
	row
      else {
	entries = entries';
	remainder = remainder'
      }	

    (* In the following, a ``domain'' means a set of labels, just like a ``span''. [domain row] returns the domain of
       the row [row]. *)

    let domain row =
      assert (is_clean_row row);
      RowMap.domain row.entries

    (* [expand_clean_row_variable dom lf] expects a domain [dom] and a leaf [lf] which stands for a row variable [v].
       [v]'s span must be also be specified. The function expands [lf], i.e. it equates it with a row whose explicit
       labels are exactly those in [dom], and returns this row. [dom] must be non-empty. *)

    let rec expand_clean_row_variable dom span lf =

      assert (is_row_sort lf.sort);
      assert (not (RowMap.Domain.is_empty dom));

      (* Extract the row variable [v] associated with the leaf [lf]. Extract [lf]'s rank. *)

      let v = leaf_to_variable lf in
      let rank = lf.rank in

      (* Build a fresh row, of domain [dom], containing fresh variables of the same kind as [v]. The rank of these
	 new variables is the same as that of [lf], since they conceptually belong to the same generation. *)

      let bot = G.Term.bottom v.lo
      and top = G.Term.top v.lo in

      let entries =
	RowMap.lift (fun _ -> fst (Fresh.create rank Regular bot top)) dom in
      let remainder_span =
	RowMap.Domain.fine_union (fun _ _ -> assert false) span dom in
      let remainder, remainderv =
	Fresh.create rank (Row remainder_span) bot top in
      let row = {
	entries = entries;
	remainder = remainder
      }	in

      (* Expand [lf]. *)

      lf.link <- RowLink row;

      (* [v] may be related, through constraints, to other leaves or terms. These need to be expanded as well.

	 Let us begin with constraints between leaves. Consider a lower bound [lf'] of [v]. Three cases arise:
	 \begin{itemize}
	 \item if [lf'] is a regular variable [v'], then the constraint must be read $\urow v'\leq v$. We must expand
               the constraint by making [v'] a lower bound of [v]'s components.
	 \item if [lf'] is a clean row variable [v'], then [v'] must be expanded as well, and the constraint must be
	       broken (decomposed) component-wise.
	 \item if [lf'] is a row, then it has been expanded by a previous recursive call to this
	       function, since this case cannot arise when outside of the row expansion machinery. Hence, [lf']
	       already carries the appropriate set of explicit labels, and all we have to do is break the
	       constraint component-wise.
	 \end{itemize}
	 We could code up these three cases explicitly. However, it is better to use a single call to
	 [break], which will perform exactly the desired action. We lose some performance, since a few
	 redundant checks will be performed, but code clarity is much more important.

	 None of these constraint creations require additions to the frontier. Indeed, if one of the new constraints
	 is an entry point from an old generation into a newer one, then so was the constraint which is being broken
	 up, and [lf] was already a member of the [frontier] array. *)

      LeafSet.iter (fun lf' ->
	break (fun mf1 mf2 ->
	  let w2 = leaf_to_variable mf2 in
	  w2.loset <- LeafSet.add clv mf1 w2.loset
        ) lf' lf
      ) v.loset;

      LeafSet.iter (fun lf' ->
	break (fun mf1 mf2 ->
	  let w1 = leaf_to_variable mf1 in
	  w1.hiset <- LeafSet.add clv mf2 w1.hiset
        ) lf lf'
      ) v.hiset;

      (* If this is violated, then [break] has expanded [lf], which is bad. Among other problems, [remainderv] would
	 then no longer be meaningful. *)

      assert (is_var_leaf remainder);

      (* We continue our task, this time considering the constraints which relate [v] to its constructed bounds.

	 We begin by expanding each constructed bound, so that it carries the expected set of explicit labels.
         Doing this in one time (by requesting all necessary labels), rather than in several times (requesting
	 labels one after another) is important, for two reasons. First, it should be faster. Second, this
	 maintains our recursive invariant, which is (unless I'm mistaken): expansion calls concerning leaves
	 of a given sort all carry the same set of requested labels.

	 We do not expect the leaves which appear inside our constructed bound to be variables; because expansion can
	 occur transparently during closure, a variable's constructed bound may contain expanded leaves.

         Expanding the constructed bound yields a term ([xbound] below) whose leaves are clean rows, represented as
	 pairs of an access function and a remainder. This is admittedly a little exotic.

	 If the constructed bound must not be expanded, then we just copy it.

	 Again, no additions to the frontier are required. *)

      let expand_bound set_bound bound =
	if G.Term.expands bound then begin

	  let xbound = G.Term.map (isolate dom remainder_span) bound in

	  RowMap.iter (fun label mf ->
	    set_bound (G.Term.map (fun (access, _) -> access label) xbound) mf
          ) entries;
	  set_bound (G.Term.map (fun (_, remainder) -> remainder) xbound) remainder

	end
	else
	  iter (set_bound bound) row in

      expand_bound (fun term mf -> (leaf_to_variable mf).lo <- term) v.lo;
      expand_bound (fun term mf -> (leaf_to_variable mf).hi <- term) v.hi;

      (* We finish our task, this time considering conditional constraints bearing upon [v]. Any such constraint is
	 of the form $\cc{s}{v}{lf_1}{lf_2}$, where $s$ is a symbol and $lf_1, lf_2$ are variables. The constraint must
	 be decomposed according to [v]'s components. Doing this requires expanding $lf_1$ and $lf_2$, unless they are
	 regular variables, in which case they are simply copied.

	 As in the case of constructed bounds, our code first expands each conditional constraint, then breaks it down
	 component-wise.

	 Again, no additions to the frontier are required. *)

      CondSet.iter (fun (s, lf1, lf2) ->
      
	let access1, remainder1 = isolate dom remainder_span lf1
	and access2, remainder2 = isolate dom remainder_span lf2 in

	RowMap.iter (fun label mf ->
	  let w = leaf_to_variable mf in
	  w.conditionals <- CondSet.add cc (s, access1 label, access2 label) w.conditionals
	) entries;
	remainderv.conditionals <- CondSet.add cc
	    (s, remainder1, remainder2) remainderv.conditionals

      ) v.conditionals;

      (* We're done, at last. Return the result row. Notice that, at this point, [v] is no longer known and will be
	 reclaimed by O'Caml's garbage collector. [lf] remains and is now linked to [row]. *)

      row

    (* [expand_clean_row dom row] expects a domain [dom] and a clean row [row]. It expands [row]'s remainder, so that
       [row]'s explicit labels (after normalization) are at least those in [dom]. It returns the new, normalized
       row.

       If the row's remainder variable is a regular variable, then we synthesize a copy of it for each of the missing
       labels. This may be costly, since the result row is not saved and may have to be computed again, but it should
       be a rare case in practice. *)

    and expand_clean_row dom row =
      assert (is_clean_row row);
      let missing = RowMap.Domain.diff dom (domain row) in
      if RowMap.Domain.is_empty missing then
	row
      else
	let remainder = row.remainder in
	match remainder.sort with
	| Regular -> 
	    let entries =
	      RowMap.union row.entries (RowMap.lift (fun _ -> remainder) missing) in
	    {
	      entries = entries;
  	      remainder = remainder
	    } 
	| Row span ->
	    let _ = expand_clean_row_variable missing span remainder in
	    normalize row

    (* [isolate] expects a non-empty domain [dom] and a leaf [lf]. It performs expansions as required to make [lf]
       explicitly carry \emph{exactly} all labels in [dom]. It returns a row, which is not necessarily clean: its
       remainder might be an expanded variable, if [lf] already carried some fields not requested by [dom].

       The result row is returned as a pair, whose first component is a function which allows accessing the row's
       entries by name (i.e.  it may be passed any label in [dom]), and whose second component is the row's
       remainder.

       In order to save some computations, the caller is expected to provide the span of the result row's remainder. *)

    and isolate dom remainder_span lf =
      match lf.sort, link lf with
      |	Regular, _ ->

	  (* Because the variable is regular, an implicit $\urow$ constructor is inserted in front of it. In other
	     words, expanding it simply involves copying it. This can be done very easily: the access function shall
	     associate [lf] with any label, and the remainder shall be [lf] as well. *)

	  let access _ = lf in
	  access, lf

      |	Row span, VarLink _ ->

	  (* The row is clean. After expansion, it carries exactly the set of expected labels. *)

	  let row = expand_clean_row_variable dom span lf in
	  let access label =
	    RowMap.lookup label row.entries in
	  access, row.remainder

      |	Row span, RowLink row ->

	  (* The row isn't clean. After expansion, it may carry more labels than required. Let us first expand it,
	     and perform path compression. *)

	  let row' = expand_clean_row dom row in
	  if row != row' then
	    lf.link <- RowLink row';

	  (* Because [row'] may carry more labels than [dom], this access function may be technically able to
	     successfully answer requests for labels not in [dom]. However, such requests would not make sense,
	     and are not allowed by [isolate]'s specification. *)

	  let access label =
	    RowMap.lookup label row'.entries in

	  (* We must build a remainder row. If [row'] happens to carry exactly the requested labels, then its
	     remainder fits the bill. Otherwise, we must group it with the extra entries to form a new row,
	     and create a fresh leaf which stands for this row. *)

	  let remainder =
	    let extra = RowMap.corestrict row'.entries dom in
	    if RowMap.is_empty extra then
	      row'.remainder
	    else
	      Fresh.leaf
		lf.rank
		(Row remainder_span)
		(RowLink { entries = extra; remainder = row'.remainder }) in

	  access, remainder

    (* [break action lf1 lf2] expands the leaves [lf1] and [lf2], if necessary, so that they carry the same set of
       labels. It then performs constraint propagation, i.e. it applies [action] to each pair of related entries,
       which are thus variable leaves. As explained above, ill-sorted constraints are ``fixed'' by mentally inserting
       $\urow$ constructors. *)

    (* TEMPORARY it would be possible to improve physical sharing of spans, by copying spans from one variable to
       another when they are found to be logically equal. The variable with (say) the lower stamp would be updated.
       This technique may be useful if space usage of span information becomes a concern. *)

    and break action lf1 lf2 =
      match lf1.sort, link lf1, lf2.sort, link lf2 with
      |	_, VarLink _, _, VarLink _ ->
	  action lf1 lf2
      |	Regular, VarLink _, _, RowLink row2 ->
	  iter (action lf1) row2
      |	Row span1, VarLink _, _, RowLink row2 ->
	  iter2 action (expand_clean_row_variable (domain row2) span1 lf1) row2
      |	_, RowLink row1, Regular, VarLink _ ->
	  iter (fun mf1 -> action mf1 lf2) row1
      |	_, RowLink row1, Row span2, VarLink _ ->
	  iter2 action row1 (expand_clean_row_variable (domain row1) span2 lf2)
      |	_, RowLink row1, _, RowLink row2 ->
	  let domain1 = domain row1
	  and domain2 = domain row2 in
	  iter2 action
	    (expand_clean_row domain2 row1)
	    (expand_clean_row domain1 row2)

    (* [arity] expects a clean [row], and returns its arity, i.e. the number of its entries, including its
       remainder. It is used by the minimization algorithm. *)

    let arity row =
      assert (is_clean_row row);
      (RowMap.cardinal row.entries) + 1

  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Traversing constraint graphs}
      
   To guarantee that each leaf is visited at most once during a graph traversal, we must mark leaves in some way.
   Furthermore, we have no way of re-initializing the marks at the beginning of each traversal, because leaves are
   not linked together. (The absence of these links allows O'Caml's garbage collector to transparently reclaim
   unreachable variables and constraints.)

   Because of this, we use unique integer marks, instead of Boolean marks. A new mark is generated at the beginning of
   each traversal. Overflows are ignored, which may lead to problems if the library is used over an extended period of
   time. 

   TEMPORARY to gain space, it would be possible to remove the [traversal] field and re-use some other field of the
   [leaf] structure. Doing so would require the other field to contain some valid pointer at all times, and would also
   require abandoning integer marks in favor of ``fresh pointer'' marks, obtained by invoking [ref ()]. The
   [representative] field cannot be re-used for this purpose, because its default value could not be restored
   afterwards. One should also note that this would leak to a minor memory leak, since old marks may remain in old
   type schemes. *)

  module Traverse = struct

    let current_traversal =
      ref min_int

    (* The following set of operations allows marking leaves while traversing constraint graphs. [is_marked lf]
       returns true iff [mark lf] was called since [start] was last called. It returns an undefined result if [start]
       was never called. *)

    let start () =
      incr current_traversal

    let mark lf =
      lf.traversal <- !current_traversal

    let is_marked lf =
      lf.traversal = !current_traversal

    (* This auxiliary function allows following the constraints carried by a leaf. It applies [action1] to leaves
       which are related to the current leaf via constraints, and [action2] to those which are its row components. *)

    let follow action1 action2 lf =
      match Row.link lf with
      | VarLink v ->

	  (* The leaf is a variable. Follow its constraints. *)

	  G.Term.iter action1 v.lo;
	  LeafSet.iter action1 v.loset;
	  LeafSet.iter action1 v.hiset;
	  G.Term.iter action1 v.hi;
	  CondSet.iter (fun (_, lf1, lf2) -> action1 lf1; action1 lf2) v.conditionals

      | RowLink row ->

          (* The leaf is a row. Iterate over its entries. *)

	  Row.iter action2 row

    (* Here comes the main traversal code. *)

    let fresh action points =
      start();

      (* This function traverses the constraint graph, marking leaves and signaling them to the caller (using
	 [action]) as it goes. The traversal stops at old leaves. *)

      let rec loop lf =
	if (lf.rank = !Level.level) & not (is_marked lf) then begin
	  mark lf;
	  action lf;
	  follow loop loop lf
	end in

      (* This function skips [lf] (as well as its components, if [lf] is a row), even if it is old, and starts
	 traversing its neighbors. *)

      let rec skip lf =
	follow loop skip lf in

      (* Traversing the fresh portion of the graph requires knowing its entry points. These are given by the
	 upper level of the [frontier], as well as by any additional entry points supplied by the caller. *)

      LeafSet.iter skip !Level.frontier.(!Level.level);
      List.iter loop points

  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Copying type schemes} *)
(* TEMPORARY
  (* The following operation allows taking a fresh copy of a type scheme, i.e. a new type scheme, which is identical,
     except each variable has been replaced with a fresh copy. This reflects the fact that, in our current theory,
     every variable which appears in a type scheme must be understood as universally quantified.

     Currently, copying a type scheme does not preserve polarity information. This may be fixed rather easily if
     required. *)

  module Copy = struct

    let scheme mapper =

      (* The [representative] field is used to maintain a constant-time mapping from each leaf to its copy. Leaves
	 which haven't been copied yet can be recognized by the fact that they are their own representative. Of
	 course, we have to reset these fields to their default values after the operation is complete; a list of all
	 copied leaves is used to speed up this process. This list is initially empty. *)

      let leaves = ref [] in

      (* This function defines the image of a leaf through the copy operation. If the leaf has already been
	 encountered, its copy is stored in its [representative] field. Otherwise, a fresh leaf must be created. In
	 the latter case, the constraints bearing on the original leaf are copied as well. This causes the whole
	 constraint graph to be traversed and copied.

	 No attempt at ``normalizing'' rows is made, so we obtain an exact copy of the type scheme. In most
	 applications, only normalized type schemes should be copied, so this should not affect performance;
	 on the contrary, we save a few tests. *)

      let rec leaf lf =
	if lf != lf.representative then
	  lf.representative
	else

	  (* Create a fresh leaf. In its [link] field, we store a (meaningless) placeholder. Then, we mark [lf]
	     as visited, and associate it with [lf']. Only then can we safely copy the contents of [lf]'s [link]
	     field, and write the copy into [lf']. *)

	  let lf' = Fresh.leaf lf.sort lf.link (* placeholder *) in
	  lf.representative <- lf';
	  leaves := lf :: !leaves;
	  lf'.link <- link lf.link;
	  lf'

      and link = function
	| VarLink v ->
	    VarLink {
	      lo = G.Term.map leaf v.lo;
	      loset = LeafSet.map clv leaf v.loset;
	      hiset = LeafSet.map clv leaf v.hiset;
	      hi = G.Term.map leaf v.hi;
	      conditionals = CondSet.map cc conditional v.conditionals;
	    } 
	| RowLink row ->
	    RowLink (Row.map leaf row)

      and conditional (symbol, lf1, lf2) =
	(symbol, leaf lf1, leaf lf2) in

      (* Let our client call [leaf] upon each of the type scheme's entry points, and build a new concrete
	 structure to express the result. *)

      let result = mapper leaf in

      (* Reset the [representative] fields of the original scheme to their default value, before returning the
	 result. *)

      List.iter (fun lf ->
	lf.representative <- lf
      ) !leaves;

      result

  end
*)
(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Closure} *)

  (* Let us give a general overview of the way row expansion is performed during closure.

     First, let us mention the fact that the $\urow$ constructor, which turns a regular variable $\alpha$ into a
     uniform row $\urow\alpha$ (i.e. a row whose entry at any label is $\alpha$), is not explicitly represented in our
     data structures. Instead, it is to be considered as present wherever required to make the constraints
     well-sorted. In other words, every constraint (including constraints between two variables, constraints between
     one variable and a term, and conditional constraints) has a sort; every component of that constraint must have
     this sort, except that a [Regular] variable is allowed to appear where a [Row] variable would be expected; in
     that case, a $\urow$ constructor is implicitly placed in front of the variable. This convention only makes sense
     because we work with well-sorted terms; of course, when the user types constraints into the system, the $\urow$
     must be explicitly specified, and well-sortedness must be explicitly checked for.

     While writing the closure algorithm, one must keep two things in mind:
     \begin{itemize}
     \item any leaf may be a variable or a row;
     \item any variable may be of sort [Regular] or [Row]; in the former case, it may have to be promoted
           to sort [Row] (by mentally inserting a $\urow$ constructor) if the context requires it.
     \end{itemize} *)

  module Closure = struct

    (* The exception [Inconsistent] is raised when the closure algorithm discovers an inconsistency. Given the
       system's current architecture (an explicit waiting queue is used to keep track of sub-goals, rather than
       O'Caml's implicit call stack), it is difficult to report detailed information about the steps which led
       to its discovery. *)

    exception Inconsistent

    (* We now define a data structure which holds the closure's algorithm ``global'' state. Such a structure is made
       necessary by the fact that the algorithm has three user-visible entry points ([link], [lo] and [hi]), rather
       than one. If it only had one, it would be best to define the internal, recursive functions inside the main
       entry point's closure, making a dedicated data-structure unnecessary. In this implementation, internal
       functions carry one more explicit parameter, i.e. the algorithm's state; however, if they had been defined
       inside the main function, they would carry one more implicit parameter, their environment. Unless I'm mistaken,
       the cost is the same.

       We maintain an explicit waiting queue of links to be added. This queue could be made implicit, by making the
       algorithm call itself recursively, instead of queuing new constraints. This might possibly improve performance
       slightly. However, it would also make the algorithm's invariant much more subtle and fragile. Indeed, the
       algorithm would have to interrupt itself to service recursive calls, instead of waiting until a stable state
       has been reached before dequeuing a request, as is done here. To sum up, using an explicit queue makes the code
       simpler, and allows it to be entirely faithful to its proof.

       Note that the order in which constraints are retrieved from the queue is irrelevant here. We use a LIFO queue,
       because we guess it is more GC-friendly (fewer writes to mutable cells, more short-lived objects) than a FIFO
       queue.

       While performing closure, we perform ``incremental canonization'', i.e. we create fresh leaves to stand
       for least upper bounds (resp. greatest lower bounds) of existing leaves. (The leaves which exist when
       the algorithm is first invoked are called ``original'' in the following.) A problematic aspect of this design
       is to ensure the algorithm's termination.

       The closure algorithm maintains two data structures which map each set of original leaves to the fresh
       leaf created to stand for its least upper bound (resp. greatest lower bound), if it exists. These
       structures are looked up when creating fresh leaves, so as to avoid creating two fresh leaves to
       stand for the same join (resp. meet) expression.

       Reciprocally, each fresh leaf carries a pointer to the set of original leaves which it stands for.
       This pointer is stored in the leaf's [originals] field. These fields need to be cleared when the
       closure algorithm exits; for this reason, the algorithm maintains a list of all freshly created leaves.

       Incremental canonization interacts very easily with row expansion. In fact, each of these two mechanisms is
       unaware of the other's existence. This might seem inefficient at first: indeed, if $\lambda$ is known to be,
       say, the join of $\alpha$ and $\beta$, and if all three variables are expanded to introduce some label $l$,
       then $\lambda.l$ can be safely said to be the join of $\alpha.l$ and $\beta.l$. However, because canonization
       works independently of row expansion, any request to compute the join of $\alpha.l$ and $\beta.l$ will yield a
       fresh variable. In practice, this should not be a problem, because minimization will eliminate these extra
       variables. Termination is still guaranteed, and code simplicity is optimal, which is essential here, given the
       rather complex nature of these algorithms.

       We note that any variables created during incremental canonization receive the (maximum) rank of the variables
       they stand for, to prevent them from being unnecessarily generalized. Also, we note that the closure algorithm
       must be careful to properly maintain the [frontier] array. *)

    type state = {
	queue: (leaf * leaf) Stack.t;
	mutable glb_map: (leaf, leaf) LeafSetMap.Map.t;
	mutable lub_map: (leaf, leaf) LeafSetMap.Map.t;
	mutable newbies: leaf list
      }	

    (* This internal function creates the algorithm's initial state. It is invoked once whenever one of the external
       entry points is used. The waiting queue is initially empty. The maps should initially be empty. It might make
       sense to re-use information from previous invocations of the algorithm; however, this would definitely be
       costly in terms of memory, and possibly also in terms of time. The list of freshly created leaves is
       initially empty. *)

    let initial () =
      {
	queue = Stack.create();
	glb_map = LeafSetMap.Map.empty;
	lub_map = LeafSetMap.Map.empty;
	newbies = []
      }	

    (* [schedule] adds a constraint to the waiting queue. In order to save some allocation and de-allocation time, we
       do not add reflexive constraints to the queue. *)

    let schedule state lf1 lf2 =
      if (lf1 != lf2) then
	Stack.push (lf1, lf2) state.queue

    (* The core of the closure algorithm follows. We begin with two auxiliary functions, [leaf_LUB] and
       [leaf_GLB], which compute the least upper bound (resp. greatest lower bound) of two leaves. *)

    let leaf_LUB state lf1 lf2 =

      (* As an optimization, we check whether the two leaves are equal. If so, no computation is necessary. *)

      if lf1 == lf2 then
	lf1 (* for instance *)
      else begin

	(* Determine the set of original leaves which these leaves stand for. (This is where each leaf's
	   [originals] field is used.) *)

	let originals =
	  match lf1.originals, lf2.originals with
	  | None, None ->

	      (* Notice that if [lf1] and [lf2] are two distinct, original leaves, then we always create a fresh leaf
		 to stand for their join, even if one is greater than the other. This allows us to save time by not
		 checking for this condition. Such a check would be incomplete anyway, because some useful constraints
		 might still be waiting in the queue, and because no complete entailment algorithm is currently
		 known. Creating a redundant fresh variable is not a problem; it shall later be eliminated during
		 minimization. *)

	      LeafSetMap.Set.make2 clv lf1 lf2

	  | Some originals1, None ->
	      LeafSetMap.Set.add clv lf2 originals1
	  | None, Some originals2 ->
	      LeafSetMap.Set.add clv lf1 originals2
	  | Some originals1, Some originals2 ->
	      LeafSetMap.Set.union clv originals1 originals2 in

	(* Determine whether a fresh leaf has already been created to stand for this set. (This is where our
	   map from sets to fresh leaves is used.) If so, return it. *)

	try
	  LeafSetMap.Map.find clv originals state.lub_map
	with Not_found ->

	  (* No leaf has been created. Make a new one. Add it to the list of freshly created leaves, record
	     which original leaves it stands for, and request appropriate constraints to be added. *)

	  let rank = max lf1.rank lf2.rank in
	  let lf = Fresh.after rank lf1 (* for instance *) in

	  state.newbies <- lf :: state.newbies;
	  lf.originals <- Some originals;
	  state.lub_map <- LeafSetMap.Map.add clv originals lf state.lub_map;
	  LeafSetMap.Set.iter (fun original ->
	    Stack.push (original, lf) state.queue
	  ) originals;

	  lf

      end

    and leaf_GLB state lf1 lf2 =
      if lf1 == lf2 then
	lf1
      else begin

	let originals =
	  match lf1.originals, lf2.originals with
	  | None, None ->
	      LeafSetMap.Set.make2 clv lf1 lf2
	  | Some originals1, None ->
	      LeafSetMap.Set.add clv lf2 originals1
	  | None, Some originals2 ->
	      LeafSetMap.Set.add clv lf1 originals2
	  | Some originals1, Some originals2 ->
	      LeafSetMap.Set.union clv originals1 originals2 in

	try
	  LeafSetMap.Map.find clv originals state.glb_map
	with Not_found ->

	  let rank = max lf1.rank lf2.rank in
	  let lf = Fresh.after rank lf1 in

	  state.newbies <- lf :: state.newbies;
	  lf.originals <- Some originals;
	  state.glb_map <- LeafSetMap.Map.add clv originals lf state.glb_map;
	  LeafSetMap.Set.iter (fun original ->
	    Stack.push (lf, original) state.queue
          ) originals;

	  lf

      end

    (* This auxiliary function adds [lf] to the frontier, if necessary. It is added at every rank [i] which appears
       in the supplied parameters [vset], [term] and [conds] and which is greater than its own rank. *)

    let update_frontier lf vset term conds =

      (* Optimize the common case where [lf] is fresh. *)

      if lf.rank < !Level.level then begin

	(* Create a local array to record the ranks [i] at which [lf] should appear in the frontier. *)

	let flag = Array.create (!Level.level + 1) false in
	let record vh2 =
	  flag.(vh2.rank) <- true in

	(* Look at the supplied parameters (which presumably appear in constraints carried by [lf]) and record
	   the ranks of the leaves they contain. *)

	LeafSet.iter record vset;
	G.Term.iter record term;
	CondSet.iter (fun (_, mf1, mf2) -> record mf1; record mf2) conds;

	(* Walk the upper portion of the array (i.e. all ranks greater than [lf]'s own rank) and add [lf] to
	   the frontier at every rank where some leaf has been found. *)

	for i = lf.rank + 1 to !Level.level do
	  if flag.(i) then
	    !Level.frontier.(i) <- LeafSet.add clv lf !Level.frontier.(i)
	done

      end

    (* This internal function is at the heart of the closure algorithm. It creates a constraint between two variables
       (be they regular or row variables). *)

    let link state lf1 lf2 =

      let v1 = leaf_to_variable lf1
      and v2 = leaf_to_variable lf2 in

      (* Because no reflexive constraints are ever added to the queue, we do not need to check whether [lf1] and [lf2]
	 are distinct leaves. However, we do need to check whether the constraint already appears in the set, even if
	 such a check has been made when queueing it, because it might have been added in between. Note that the
	 check's condition is dissymmetric; this is not a problem, since a constraint between two variables is
	 represented by two pointers, a forward one and a backward one. *)

      if not (LeafSet.mem clv lf1 v2.loset) then begin

	let vloset1 = LeafSet.remove clv lf2 (LeafSet.add clv lf1 v1.loset)
	and vhiset2 = LeafSet.remove clv lf1 (LeafSet.add clv lf2 v2.hiset) in

	(* Update the upper bounds, and the conditional constraints, bearing upon all variables below and including
	   [v1], excluding [v2]. In this loop, excluding [v2] saves some time, since it does need any updating.
	   Excluding [v1] from [vhiset2] ensures that no reflexive constraints are created here. *)

	LeafSet.iter (fun lf ->
	  let v = leaf_to_variable lf in
	  v.hiset <- LeafSet.union clv vhiset2 v.hiset;
	  v.hi <- G.Term.glb (leaf_GLB state) (leaf_LUB state) v2.hi v.hi;
	  v.conditionals <- CondSet.union cc v2.conditionals v.conditionals;

	  (* The constraints created above may cause it to become part of the frontier. *)

	  update_frontier lf vhiset2 v.hi v2.conditionals

	) vloset1;

	(* Symmetrically, update the lower bounds of all variables above and including [v2], excluding [v1]. *)

	LeafSet.iter (fun lf ->
	  let v = leaf_to_variable lf in
	  v.lo <- G.Term.lub (leaf_GLB state) (leaf_LUB state) v1.lo v.lo;
	  v.loset <- LeafSet.union clv vloset1 v.loset;
	  
	  update_frontier lf vloset1 v.lo CondSet.empty

	) vhiset2;

	(* Check whether any conditionals are triggered. *)

	CondSet.iter (fun (symbol, mf1, mf2) ->
	  if G.Symbol.ordered symbol v2.lo then
	    schedule state mf1 mf2
	) v2.conditionals;

	(* Decompose the new constraint obtained between [v1]'s constructed lower bound and [v2]'s constructed
	   upper bound. *)

	try
	  G.Term.break (schedule state) v1.lo v2.hi
	with G.Term.Clash ->
	  raise Inconsistent

      end

    (* This is the algorithm's internal entry point, to be called after the waiting queue has been appropriately
       initialized. *)

    let run state =

      (* As long as there are constraints in the queue, process them. The call to [Row.break] is the only spot
	 where the closure algorithm has to be aware of the existence of rows! *)

      begin
	try
	  while true do
	    let lf1, lf2 = Stack.pop state.queue in 
	    Row.break (link state) lf1 lf2
	  done
	with Stack.Empty ->
	  ()
      end;

      (* When the algorithm exits, we clear the canonization information contained in each fresh variable, making
	 it an original variable for the next run. *)

      List.iter (fun lf ->
	lf.originals <- None
      ) state.newbies

    (* [link] is the algorithm's main external entry point. It initializes the algorithm's ``global'' state, then runs
       it. *)

    let link lf1 lf2 =
      let state = initial () in
      schedule state lf1 lf2;
      run state

    (* [lo] (resp. [hi]) is another external entry point. It adds a new constructed lower (resp. upper) bound to a
       variable. It would have been possible not to supply these entry points, making our code somewhat simpler;
       but this would have required the user to create additional, dummy variables, which seemed inefficient.

       Here, [lf] is expected to be a variable, not a row. We do not want to deal with rows here; this would require
       duplicating even more code, and is likely not to be useful. *)

    let lo term lf =
      let v = leaf_to_variable lf in
      let state = initial() in

      (* Update the lower bounds of the variables above [v], including [v] itself; then, examine the consequences of
	 the new constraint which exists between [v]'s new lower bound and its existing upper bound. This code may
	 schedule new constraint additions, which are dealt with below, by running the algorithm's core. *)

      v.lo <- G.Term.lub (leaf_GLB state) (leaf_LUB state) term v.lo;
      update_frontier lf LeafSet.empty v.lo CondSet.empty;
      LeafSet.iter (fun lf ->
	let v = leaf_to_variable lf in
	v.lo <- G.Term.lub (leaf_GLB state) (leaf_LUB state) term v.lo;
	update_frontier lf LeafSet.empty v.lo CondSet.empty
      ) v.hiset;
      begin
	try
	  G.Term.break (schedule state) term v.hi
	with G.Term.Clash ->
	  raise Inconsistent
      end;

      (* Deal with the waiting queue. *)

      run state

    let hi lf term =
      let v = leaf_to_variable lf in
      let state = initial() in

      v.hi <- G.Term.glb (leaf_GLB state) (leaf_LUB state) term v.hi;
      update_frontier lf LeafSet.empty v.hi CondSet.empty;
      LeafSet.iter (fun lf ->
	let v = leaf_to_variable lf in
	v.hi <- G.Term.glb (leaf_GLB state) (leaf_LUB state) term v.hi;
	update_frontier lf LeafSet.empty v.hi CondSet.empty
      ) v.loset;
      begin
	try
	  G.Term.break (schedule state) v.lo term
	with G.Term.Clash ->
	  raise Inconsistent
      end;

      run state

    (* [conditional s lf0 lf1 lf2] creates a new conditional constraint $\cc{s}{[lf0]}{[lf1]}{[lf2]}$. It raises
       [Inconsistent] if this operation makes the constraints unsatisfiable. [lf0], [lf1] and [lf2] must have the same
       span (i.e. they may be regular or row variables, but all row variables involved must have the same span). [s]
       and [lf0] must have the same kind; [lf1] and [lf2] must have the same kind. *)

    let rec conditional symbol lf0 lf1 lf2 =

      (* Check whether [lf0] is a variable. *)

      match Row.link lf0 with
      |	VarLink v0 ->

	  (* Check whether the new conditional would be triggered immediately. If so, directly schedule its conclusion
	     for addition into the constraint set, and run the closure algorithm. Otherwise, add a new conditional to
	     [v0] and to all variables currently below it; no further steps are necessary. *)

	  if G.Symbol.ordered symbol v0.lo then begin

	    let state = initial () in
	    schedule state lf1 lf2;
	    run state

	  end
	  else

	    let conditional = (symbol, lf1, lf2)
	    and rank1 = lf1.rank
	    and rank2 = lf2.rank in

	    let add lf =
	      let v = leaf_to_variable lf in
	      v.conditionals <- CondSet.add cc conditional v.conditionals;

	      (* Update the frontier. This is painful. *)

	      if lf.rank < rank1 then
		!Level.frontier.(rank1) <- LeafSet.add clv lf !Level.frontier.(rank1);
	      if lf.rank < rank2 then
		!Level.frontier.(rank2) <- LeafSet.add clv lf !Level.frontier.(rank2)

	    in
	    add lf0;
	    LeafSet.iter add v0.loset

      |	RowLink _ ->

	  (* Because [lf0] is not a variable, we must in fact expand [lf1] and [lf2], and add an appropriate
	     conditional constraint to each of [lf0]'s components. To avoid duplicating the expansion code, we use a
	     fresh auxiliary variable [lf]. We first add the conditional to [lf], and then link [lf0] to [lf]. *)

	  let lf = Fresh.after lf0.rank lf0 in
	  conditional symbol lf lf1 lf2;
	  link lf0 lf

    (* [meet] (resp. [join]) returns a fresh leaf which stands for the meet (resp. join) of its two arguments.
       It is simply a shortcut for two appropriate calls to [link]. Its two arguments must have the same kind. *)

    (* TEMPORARY might want to run the closure algorithm once instead of twice, or to invoke [leaf_GLB] directly
       so as to avoid duplication *)

    let meet lf1 lf2 =
      let lf = Fresh.after !Level.level lf1 (* for instance *) in
      link lf lf1;
      link lf lf2;
      lf

    let join lf1 lf2 =
      let lf = Fresh.after !Level.level lf1 (* for instance *) in
      link lf1 lf;
      link lf2 lf;
      lf

  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Computing polarities and collecting garbage} *)

  module Garbage = struct

    (* Here comes the code in charge of computing polarities. Throughout this code, the following invariant is used:
       if a (fresh) leaf is marked, then its sign is valid, otherwise it is invalid and should be reset before
       proceeding. This invariant eliminates the need to reset all signs at the beginning of the computation, thus
       allowing garbage collection (as a whole) to be linear in the output graph's size, rather than in the input
       graph's size. Old leaves are implicitly considered bipolar, since they appear in the environment and may
       receive lower or upper bounds in the future.

       During the collection phase, we shall need to re-use the marks. At first glance, this is a problem, because we
       no longer can tell which leaves have valid signs. However, the collection phase walks the graph in such a way
       that only leaves with valid signs are considered -- so, we needn't worry about this. *)

    (* [initialize lf] makes sure that [lf]'s sign is meaningful. If [lf] has been visited already, there is nothing
       to be done. Otherwise, its sign is set to [Neutral], and the leaf is marked, so as to record that its sign is
       meaningful. (We could dispense with marking [lf], and with checking [lf] is fresh; it is not clear whether we
       would gain speed.) *)

    let initialize lf =
      if (lf.rank = !Level.level) & not (Traverse.is_marked lf) then begin
	Traverse.mark lf;
	lf.sign <- Neutral
      end

    (* [follow sign lf] is called after [lf] has been discovered to have sign [sign]. Its job is to propagate the
       consequences of this fact to [lf]'s neighbors.

       If [lf] is a variable [v], then two cases arise, depending on [sign].

       When [sign] is [true] (i.e. positive), the function first makes sure that all variables below [v] have valid
       signs, because the garbage collection algorithm will look at them to determine whether these links should be
       kept or discarded. Then, it gives appropriate signs to the variables which appear in [v]'s constructed lower
       bound.

       The negative case is symmetric to the positive one, except it also gives appropriate signs to the variables
       which appear in the conditional constraints bearing upon [v].

       If [lf] is a row, then its components must receive the same sign. *)

    let rec follow sign lf =
      match Row.link lf with
      |	VarLink v ->
	  if sign then begin
	    G.Term.siter give_sign true v.lo;
	    LeafSet.iter initialize v.loset
	  end
	  else begin
	    LeafSet.iter initialize v.hiset;
	    G.Term.siter give_sign false v.hi;
	    CondSet.iter (fun (_, lf1, lf2) ->
	      give_sign true lf1;
	      give_sign false lf2
	    ) v.conditionals
	  end
      |	RowLink row ->
	  Row.iter (give_sign sign) row

    (* [give_sign sign lf] gives sign [sign] to leaf [lf], if it is fresh, and propagates consequences to its
       neighbors, if necessary. If [lf] is not fresh, the traversal stops. *)

    and give_sign sign lf =
      if lf.rank = !Level.level then
      
	if Traverse.is_marked lf then begin

	  (* [lf] has been visited already, so its sign is valid. Update it, if necessary, then deal with [lf]'s
	     neighbor leaves. *)

	  match sign, lf.sign with
	  | true, Negative ->
	      lf.sign <- Bipolar;
	      follow true lf
	  | true, Neutral ->
	      lf.sign <- Positive;
	      follow true lf
	  | false, Positive ->
	      lf.sign <- Bipolar;
	      follow false lf
	  | false, Neutral ->
	      lf.sign <- Negative;
	      follow false lf
	  | _, _ ->
	      ()

	end
	else begin

	  (* [lf] has not been visited yet. Thus, it was to be considered as neutral, until now. Mark it as visited,
	     update its sign, then deal with its neighbors. *)

	  Traverse.mark lf;
	  lf.sign <- if sign then Positive else Negative;
	  follow sign lf

	end

    (* [skip lf] is used to initiate the traversal at frontier nodes. If [lf] is a row, [skip] recursively descends
       through its components. Otherwise, it starts traversing its neighbors, in both directions (because [lf] is
       considered bipolar). *)

    let rec skip lf =
      match Row.link lf with
      |	VarLink v ->
	  G.Term.siter give_sign true v.lo;
	  LeafSet.iter initialize v.loset;
	  LeafSet.iter initialize v.hiset;
	  G.Term.siter give_sign false v.hi;
	  CondSet.iter (fun (_, lf1, lf2) ->
	    give_sign true lf1;
	    give_sign false lf2
	  ) v.conditionals
      |	RowLink row ->
	  Row.iter skip row

    (* [polarize neg pos] computes polarities. [neg] and [pos] are negative and positive entry points supplied by
       the caller. The frontier is considered a list of bipolar entry points. *)

    let polarize neg pos =
      Traverse.start();
      LeafSet.iter skip !Level.frontier.(!Level.level);
      List.iter (give_sign false) neg;
      List.iter (give_sign true) pos

    (* Here comes the garbage collection algorithm. All constraints are discarded, except the following:
       \begin{itemize}
       \item each link from a negative variable to a positive one;
       \item each positive variable's constructed lower bound;
       \item each negative variable's constructed upper bound;
       \item each conditional constraint bearing on a negative variable.
       \end{itemize}
       Old variables are implicitly considered bipolar. *)

    let rec is_positive lf =
      (lf.rank < !Level.level) or
      match lf.sign with
      | Bipolar
      | Positive ->
	  collect lf;
	  true
      | Negative
      | Neutral ->
	  false

    and is_negative lf =
      (lf.rank < !Level.level) or
      match lf.sign with
      |	Bipolar
      | Negative ->
	  collect lf;
	  true
      | Positive
      | Neutral ->
	  false

    and collect lf =
      if lf.rank = !Level.level then
	match lf.link with

	| VarLink v ->

	    if not (Traverse.is_marked lf) then begin
	      Traverse.mark lf;

	      match lf.sign with
	      | Positive ->
		  G.Term.iter collect v.lo;
		  v.loset <- LeafSet.filter clv is_negative v.loset;
		  v.hiset <- LeafSet.empty;
		  v.hi <- G.Term.top v.hi;
		  v.conditionals <- CondSet.empty
	      | Negative ->
		  v.lo <- G.Term.bottom v.lo;
		  v.loset <- LeafSet.empty;
		  v.hiset <- LeafSet.filter clv is_positive v.hiset;
		  G.Term.iter collect v.hi;
		  CondSet.iter (fun (_, lf1, lf2) -> collect lf1; collect lf2) v.conditionals
	      | Bipolar ->
		  G.Term.iter collect v.lo;
		  v.loset <- LeafSet.filter clv is_negative v.loset;
		  v.hiset <- LeafSet.filter clv is_positive v.hiset;
		  G.Term.iter collect v.hi;
		  CondSet.iter (fun (_, lf1, lf2) -> collect lf1; collect lf2) v.conditionals
	      | Neutral ->
		  assert false

	    end

	| RowLink row ->
	    assert (lf.sign <> Neutral);
	    Row.iter collect row

    (* This additional code takes care of initiating collection at the frontier. *)

    let rec skip lf =
      match Row.link lf with
      |	VarLink v ->
	  G.Term.iter collect v.lo;
	  v.loset <- LeafSet.filter clv is_negative v.loset;
	  v.hiset <- LeafSet.filter clv is_positive v.hiset;
	  G.Term.iter collect v.hi;
	  CondSet.iter (fun (_, lf1, lf2) -> collect lf1; collect lf2) v.conditionals
      |	RowLink row ->
	  Row.iter skip row

    (* [collect neg pos] performs collection. *)

    let collect neg pos =
      Traverse.start();
      LeafSet.iter skip !Level.frontier.(!Level.level);
      List.iter collect neg;
      List.iter collect pos

   (* To the outside, we present a single entry point. *)

    let collect neg pos =
      polarize neg pos;
      collect neg pos

  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Minimization} *)
(* TEMPORARY
  module Minimize = struct

    (* We begin by defining an appropriate structure in order to instantiate Paige and Tarjan's partition refinement
       algorithm. *)

    module Point = struct

      (* Points are either leaves, or pairs thereof. The latter are made necessary by the existence of conditional
	 constraints. *)

      type point = {

          (* This field is used by our implementation of Paige and Tarjan's algorithm to store internal data. Because
	     the type of this data is unknown, we declare a dummy type; the algorithm shall need to break O'Caml's
	     type system to read and write this field. *)

	  mutable point_data: dummy;

	  (* If this point stands for a leaf (rather than a pair thereof), then a pointer to it is stored here. *)

	  mutable point_leaf: leaf option

	} 

      and dummy =
	  point (* for instance *)

      (* These functions allow the algorithm to read and write the field we have allocated for it. *)

      let store p data =
	p.point_data <- data

      let get p =
	p.point_data

      (* This function allows the algorithm to describe the final, refined partition to us. We store this information
	 in each leaf's [representative] field. *)

      let represents p1 p2 =
	match p1.point_leaf, p2.point_leaf with
	| Some lf1, Some lf2 ->
	    lf1.representative <- lf2
	| None, None ->
	    ()

	(* This safety check could be removed to gain some extra speed. *)

	| _ ->
	    assert false (* The algorithm has identified a variable with a pair. *)

      (* We need some way of mapping leaves and pairs of leaves to points while initializing. As for leaves, the
	 [representative] field is unused during initialization, so we re-use it. (This requires locally breaking
	 O'Caml's type system, but saves time or space, depending on how one looks at it.) As for pairs of leaves, we
	 use an explicit association structure. *)

      (* [of_leaf lf] creates a new point and associates it to the leaf [lf]. *)

      let of_leaf lf =
	let rec point = {
	  point_data = point; (* for instance *)
	  point_leaf = Some lf
	} in
	lf.representative <- Obj.magic point;
	point

      (* [of_leaf_again lf] finds the point associated to [lf]. It is essential that the point already exist,
	 otherwise chaos will ensue. *)

      let of_leaf_again lf =
	Obj.magic lf.representative

      (* Define an association map from pairs of leaves to points. *)

      module PairMap = Map.Make (struct
	type t = leaf * leaf
	let compare (lf1, mf1) (lf2, mf2) =
	  let result = clv lf1 lf2 in
	  if result <> 0 then result
	  else clv mf1 mf2
      end)

      let map =
	ref PairMap.empty

      let reset () =
	map := PairMap.empty

      (* [of_pair pair] finds the point associated to the given [pair] of leaves. The point is created if it doesn't
	 exist yet. *)

      let of_pair pair =
	try
	  PairMap.find pair !map
	with Not_found ->

	  let rec point = {
	    point_data = point; (* for instance *)
	    point_leaf = None
	  } in
	  map := PairMap.add pair point !map;
	  point

    end	    

    (* We may now instantiate Paige and Tarjan's partition refinement algorithm. *)

    module Refiner = PaigeTarjan.Make (Point)

    (* PaigeTarjan exclusively allows integer labels on relations, and expects them to be sequentially numbered from 0
       and up. Hence, we must translate head constructors to integers as we encounter them. *)

    module Head = struct

      module HeadMap = Map.Make (struct
	type t = G.Symbol.t
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

    (* We are now ready to implement minimization. This is done by computing the coarsest partition compatible with
       the specified type scheme. The conditions to be fulfilled by all such partitions are as follows:
       \begin{enumerate}
       \item Any two equivalent variables must have the same sign. Additionally, bipolar variables must be isolated.
       \item Any two equivalent variables must have the same predecessor and successor sets.
       \item Any two equivalent variables must have equivalent constructed lower (resp. upper) bounds. This implies,
             in particular, that they have the same kind.
       \item Any two equivalent variables must have equivalent conditional constraints.
       \item Any two equivalent variables must have the same sort. That is, they must either be two regular variables,
             or two row variables with the same set of non-admissible labels.
       \end{enumerate} 

       The type scheme must have been run through garbage collection. This ensures, in particular, that all signs
       are valid.

       The outline of our algorithm is as follows:
       \begin{itemize}
       \item Put each bipolar variable into a separate class.
       \item Put two positive (resp. negative) variables in the same class if they have the same set of predecessors
             (resp. successors), if their constructed lower (resp. upper) bounds have the same head constructor, and
             if they have the same sort.
       \item Put two rows in the same class if they have the same sort (i.e. the same span).
       \item Refine this initial partition, so that it becomes stable with respect to the following functions and
             relations:
             \begin{itemize}
             \item an edge labeled $i$ from each variable to the $i^{\text{th}}$ leaf of its constructed bound, where
                   $i$ ranges from $0$ (inclusive) to the constructed bound's arity (exclusive) (this defines
                   functions, because each variable only has one constructed bound);
             \item an edge labeled $0$ from each row to its remainder, and
                   an edge labeled $i$ to its $i^{\text{th}}$ entry, where entries are ordered according to an
                   arbitrary, but fixed, total order on row labels.
             \item for each conditional constraint of the form $\cc{s}{\beta}{\alpha}{\gamma}$, an edge labeled $s$
                   from $\beta$ to the pair $(\alpha, \gamma)$, an edge labeled $0$ from $(\alpha, \gamma)$ to
                   $\alpha$, and an edge labeled $1$ from $(\alpha, \gamma)$ to $\gamma$. Relations labeled with
                   symbols are (a priori) non-functional. Relations labeled $0$ and $1$ are functions.
             \end{itemize}
             Note that removing the pair point and adding direct edges from $\beta$ to $\alpha$ and $\gamma$ would
             not be correct; by doing so, we would lose track of the correlation which exists between $\alpha$ and
             $\gamma$.
             All pair points are initially in a separate class. This allows us to safely re-use the labels 0 and 1,
             without fear of identifying a pair point with a regular one.
       \end{itemize}

       Notice that rows are treated essentially as if they were ordered tuples. Labels do not matter; only their
       position within the row does. Does this mean that two rows which have the same number of labels, but different
       labels, may be identified? Of course, no: if they have different labels, then either their roots or their
       remainders must have different sorts, so they must be put in different classes at the start, preventing this
       undesirable identification. *)

    let minimize scheme mapper =

      (* First, traverse the type scheme. During this traversal,
	 \begin{itemize}
	 \item we build a list of all positive or negative variables, and record its length;
	 \item we build a list of all rows (called expanded variables below), and record its length;
	 \item we record the maximum arity of terms and rows;
	 \item we build a list of all bipolar variables, if any;
	 \item we register all symbols which appear in conditional constraints;
	 \item we build all necessary pair points.
         \end{itemize} *)

      let variables = ref []
      and nv = ref 0
      and xvariables = ref []
      and nxv = ref 0
      and rf = ref 0
      and bipolar = ref [] in

      Traverse.scheme (fun lf ->
	match Row.link lf with
	| VarLink v -> (
	    match lf.sign with
	    | Positive ->
		variables := lf :: !variables;
		incr nv;
		rf := max (G.Term.arity v.lo) !rf
	    | Negative ->
		variables := lf :: !variables;
		incr nv;
		rf := max (G.Term.arity v.hi) !rf;
		CondSet.iter (fun (symbol, lf1, lf2) ->
		  let _ = Point.of_pair (lf1, lf2) in
		  Head.register symbol
		) v.conditionals
	    | Bipolar ->
		bipolar := lf :: !bipolar
	    | Neutral ->
		assert false (* No neutral variables remain after garbage collection. *)
          )
	| RowLink row ->
	    xvariables := lf :: !xvariables;
	    incr nxv;
	    rf := max (Row.arity row) !rf
      ) scheme;

      (* Tell Paige and Tarjan how many functions and relations shall be used. If any conditionals are present, then
	 we need at least 2 function symbols, one for each variable appearing in a conditional's conclusion. *)

      Refiner.set_counts (if (!Head.count > 0) & (!rf < 2) then 2 else !rf) !Head.count;

      (* Determine, among the positive and negative variables, which may be equivalent. To do so, we sort the variable
	 list according to a special comparison function. We actually turn the list into an array first, so as to make
	 in-place sorting possible. Some tests show that sorting in place is about twice faster. *)

      let compare_sorts sort1 sort2 =
	match (sort1, sort2) with
	| Regular, Regular ->
	    0
	| Regular, Row _ ->
	    -1
	| Row _, Regular ->
	    1
	| Row span1, Row span2 ->
	    RowMap.Domain.compare span1 span2 in

      let compare lf1 lf2 =
	let result = Pervasives.compare lf1.sign lf2.sign in
	if result <> 0 then result
	else begin

	  let v1 = leaf_to_variable lf1
	  and v2 = leaf_to_variable lf2 in

	  match lf1.sign (* for instance *) with
	  | Positive ->
	      let result = Pervasives.compare
		  (G.Symbol.of_term v1.lo) (G.Symbol.of_term v2.lo) in
	      if result <> 0 then result
	      else let result = LeafSet.compare clv v1.loset v2.loset in
	      if result <> 0 then result
	      else compare_sorts lf1.sort lf2.sort
	  | Negative ->
	      let result = Pervasives.compare
		  (G.Symbol.of_term v1.hi) (G.Symbol.of_term v2.hi) in
	      if result <> 0 then result
	      else let result = LeafSet.compare clv v1.hiset v2.hiset in
	      if result <> 0 then result
	      else compare_sorts lf1.sort lf2.sort
	  | _ ->
	      assert false (* Bipolar variables have been put apart. *)

	end in

      let variables = Standard.array_of_measured_list !nv !variables in
      Sort.array (fun lf1 lf2 -> compare lf1 lf2 <= 0) variables;

      let xcompare lf1 lf2 =
	compare_sorts lf1.sort lf2.sort in

      let xvariables = Standard.array_of_measured_list !nxv !xvariables in
      Sort.array (fun lf1 lf2 -> xcompare lf1 lf2 <= 0) xvariables;

      (* Prepare the initial partition. First, create a class containing all pair points. Second, add any bipolar
	 variables as isolated points. Third, walk the array and define classes of variable leaves. Lastly, define
	 classes of row leaves. *)

      Point.PairMap.iter (fun _ pp ->
	Refiner.add_point false pp
      ) !Point.map;

      List.iter (fun lf ->
	Refiner.add_point true (Point.of_leaf lf)
      ) !bipolar;

      Array.iteri (fun index lf ->
	
	(* If the current leaf compares equal to the last one, according to our special-purpose comparison
	   function, then they may be put into the same class. *)

	Refiner.add_point
	  ((index = 0) or (compare variables.(index-1) lf <> 0))
	  (Point.of_leaf lf)

      ) variables;

      Array.iteri (fun index lf ->
	Refiner.add_point
	  ((index = 0) or (xcompare xvariables.(index-1) lf <> 0))
	  (Point.of_leaf lf)
      ) xvariables;

      (* Now, construct the edges which come out of variable leaves.

	 For instance, when a negative variable $v$ has a constructed upper bound of $v_1\rightarrow v_2$, the edge
	 $(v, v_1)$ is added to the function of index 0 and the edge $(v, v_2)$ is added to the function of index 1.

	 We also add edges from each variable to the appropriate pair points. Edges from each pair point to its
	 components shall be added later---remember that each pair point may be visited several times by this loop. *)

      Array.iter (fun lf ->
	let p = Point.of_leaf_again lf
	and v = leaf_to_variable lf in

	let bound = match lf.sign with
	| Positive ->
	    v.lo
	| Negative ->
	    CondSet.iter (fun (symbol, lf1, lf2) ->
	      Refiner.add_relational_link (Head.find symbol) p (Point.of_pair (lf1, lf2))
	    ) v.conditionals;
	    v.hi
	| _ ->
	    assert false in
	    
	let _ = G.Term.fold (fun lf index ->
	  Refiner.add_functional_link index p (Point.of_leaf_again lf);
	  index + 1
        ) bound 0 in

	()

      ) variables;

      (* Create edges from each row leaf to its components. Here, we use the fact that [Row.iter] always starts with
	 the row's remainder: thus, the remainder will always obtain index 0. *)

      Array.iter (fun lf ->

	let p = Point.of_leaf_again lf in

	match lf.link with
	| VarLink _ ->
	    assert false
	| RowLink row ->
	    let index = ref 0 in
	    Row.iter (fun mf ->
	      let i = !index in
	      Refiner.add_functional_link i p (Point.of_leaf_again mf);
	      index := i + 1
            ) row

      ) xvariables;

      (* Create edges from each pair point to its components. *)

      Point.PairMap.iter (fun (lf1, lf2) pp ->
	Refiner.add_functional_link 0 pp (Point.of_leaf_again lf1);
	Refiner.add_functional_link 1 pp (Point.of_leaf_again lf2)
      ) !Point.map;

      (* We no longer need our association structures. *)

      Point.reset();
      Head.reset();

      (* At last, we may run the refinement algorithm. *)

      Refiner.refine();

      (* Each leaf's [representative] field is now correctly set. Update the type scheme. We begin by updating its
	 entry points (this is done by applying [mapper] to [map].) Then, we traverse the constraints, starting at the
	 new, canonical entry points. During this traversal, whenever we reach a leaf, we rewrite the constraints it
	 carries \emph{before} following them. This guarantees that all leaves ever traversed are, in fact, their own
	 representative. *)

      let rec map lf =
	let lf = lf.representative in
	loop lf;
	lf

      and is_clean lf =
	if lf == lf.representative then begin
	  loop lf;
	  true
	end
	else
	  false

      and loop lf =
	if not (Traverse.is_marked lf) then begin
	  Traverse.mark lf;
	  match lf.link with
	  | VarLink v ->
	      v.lo <- G.Term.endo_map map v.lo;
	      v.loset <- LeafSet.filter clv is_clean v.loset;
	      v.hiset <- LeafSet.filter clv is_clean v.hiset;
	      v.hi <- G.Term.endo_map map v.hi;
	      v.conditionals <- CondSet.map cc
		(fun (symbol, lf1, lf2) -> (symbol, map lf1, map lf2))
		v.conditionals
	  | RowLink row ->
	      lf.link <- RowLink (Row.endo_map map row)
	end in

      Traverse.start();
      mapper map

  end
*)
(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Translating type expressions} *)

  module Translate = struct

    (* We need some data structure to associate information to type variable names. *)

    module StringMap = Map.Make (struct
      type t = string
      let compare = Pervasives.compare
    end)

    (* To do kind inference, we must define kind variables, as well as unification on kinds. This is routine. *)

    module Kind = struct

      type variable = {
	  mutable link: term option
	}	

      and term =
	| Constant of G.Kind.t
	| Variable of variable

      let rec repr term =
	match term with
	| Variable ({ link = Some term } as v) ->
	    let term = repr term in
	    v.link <- Some term;
	    term
	| _ ->
	    term

      exception Inconsistency

      let unify term1 term2 =
	let term1 = repr term1
	and term2 = repr term2 in
	match term1, term2 with
	| Variable v1, Variable v2 when v1 == v2 ->
	    ()
	| Variable v1, _ ->
	    v1.link <- Some term2
	| _, Variable v2 ->
	    v2.link <- Some term1
	| Constant kind1, Constant kind2 ->
	    if kind1 <> kind2 then
	      raise Inconsistency

      let fresh () =
	Variable { link = None }

      exception UnderSpecified of string

    end

    (* To do sort inference, we must define sort variables, as well as unification on sorts. This is slightly
       more complex. We begin with a solver for equations which involve set constants, variables, and disjoint
       sums (i.e. unions) thereof. *)

    module Span = struct

      (* [disjoint_union] returns the union of two domains, which must be disjoint sets. Otherwise, an exception is
	 raised. This is intended to help during debugging -- in a production system, [RowMap.Domain.union] can be
	 used instead. *)

    let disjoint_union span1 span2 =
      RowMap.Domain.fine_union (fun elem1 elem2 ->
	assert false
      ) span1 span2

      (* Equating a variable with a term is done, as usual, through a mutable [link] field. Furthermore, each
	 variable carries a [forbidden] field, which represents a set of labels which the variable's value is
	 not allowed to contain. In other words, if $\alpha$'s [forbidden] field is a set $L$, then we have
	 $\alpha\subseteq\neg L$. *)

      type variable = {
	  mutable link: term option;
	  mutable forbidden: span
	} 

      (* A term is either a constant set $L$, a variable $\alpha$, or a disjoint sum $L\oplus\alpha$, where $L$ is
	 non-empty. We maintain the following invariant: whenever we form a disjoint sum $L\oplus\alpha$, we add $L$
	 to $\alpha$'s [forbidden] set. In other words, whenever a term $L\oplus\alpha$ exists, the constraint
	 $\alpha\subseteq\neg L$ also exists. *)

      and term =
	| Constant of span
	| Variable of variable
	| DisjointSum of span * variable

      (* [repr] normalizes a term. It returns an equivalent term which is either a constant, or a clean variable,
	 or a disjoint sum of a constant and a clean variable. *)

      let rec repr term =
	match term with
	| Variable ({ link = Some term } as v) ->
	    let term = repr term in
	    v.link <- Some term;
	    term
	| DisjointSum(span1, v2) -> (
	    match repr (Variable v2) with
	    | Constant span2 ->
		Constant (disjoint_union span1 span2)
	    | Variable v2 ->
		DisjointSum(span1, v2)
	    | DisjointSum(span2, v2) ->
		DisjointSum (disjoint_union span1 span2, v2)
          )
	| _ ->
	    term

      (* [check] expects a constant set $L$ and a term $t$. It then enforces the constraint $t\subseteq\neg L$. In
	 doing so, it may add a new constraint on $t$'s free variable (if there is one), by enlarging its [forbidden]
	 set. If the constraint cannot be enforced, the function raises [Inconsistency]. *)

      exception Inconsistency

      let check forbidden term =
	match repr term with
	| Constant span ->
	    if not (RowMap.Domain.disjoint forbidden span) then
	      raise Inconsistency
	| Variable v ->
	    v.forbidden <- disjoint_union forbidden v.forbidden (* TEMPORARY why disjoint? *)
	| DisjointSum (span, v) ->
	    if not (RowMap.Domain.disjoint forbidden span) then
	      raise Inconsistency;
	    v.forbidden <- disjoint_union forbidden v.forbidden (* TEMPORARY why disjoint? *)

      let fresh forbidden =  {
	link = None;
	forbidden = forbidden
      }	

      (* [unify] expects two terms, and unifies them, i.e. enforces an equality between the two. It raises
	 [Inconsistency] if the constraints become unsolvable as a result. *)

      let rec unify term1 term2 =
	let term1 = repr term1
	and term2 = repr term2 in
	match term1, term2 with
	| Variable v1, Variable v2 when v1 == v2 ->
	    ()
	| Variable v1, _ ->
	    check v1.forbidden term2;
	    v1.link <- Some term2;
	    v1.forbidden <- RowMap.Domain.empty
	| _, Variable v2 ->
	    check v2.forbidden term1;
	    v2.link <- Some term1;
	    v2.forbidden <- RowMap.Domain.empty
	| Constant span1, Constant span2 ->
	    if not (RowMap.Domain.equal span1 span2) then
	      raise Inconsistency
	| Constant span1, DisjointSum (span2, v2) ->
	    if not (RowMap.Domain.subset span2 span1) then
	      raise Inconsistency;
	    unify (Constant (RowMap.Domain.diff span1 span2)) (Variable v2)
	| DisjointSum (span1, v1), Constant span2 ->
	    if not (RowMap.Domain.subset span1 span2) then
	      raise Inconsistency;
	    unify (Variable v1) (Constant (RowMap.Domain.diff span2 span1))
	| DisjointSum (span1, v1), DisjointSum (span2, v2) ->
	    let span1 = RowMap.Domain.diff span1 span2
	    and span2 = RowMap.Domain.diff span2 span1 in
	    match RowMap.Domain.is_empty span1,
	          RowMap.Domain.is_empty span2 with
	    | true, true ->
		unify (Variable v1) (Variable v2)
	    | false, true ->
		unify (DisjointSum(span1, v1)) (Variable v2)
	    | true, false ->
		unify (Variable v1) (DisjointSum(span2, v2))
	    | false, false ->
		let v = fresh (disjoint_union span1 span2) in
		unify (Variable v1) (DisjointSum (span2, v));
		unify (Variable v2) (DisjointSum (span1, v))

    end

    (* Here comes unification on sorts. *)

    module Sort = struct

      type variable = {
	  mutable link: term option
	}	

      and term =
	| Regular
	| Row of Span.term
	| Variable of variable

      let rec repr term =
	match term with
	| Variable ({ link = Some term } as v) ->
	    let term = repr term in
	    v.link <- Some term;
	    term
	| Row span ->
	    Row (Span.repr span)
	| _ ->
	    term

      exception Inconsistency

      let unify term1 term2 =
	let term1 = repr term1
	and term2 = repr term2 in
	match term1, term2 with
	| Variable v1, Variable v2 when v1 == v2 ->
	    ()
	| Variable v1, _ ->
	    v1.link <- Some term2
	| _, Variable v2 ->
	    v2.link <- Some term1
	| Regular, Regular ->
	    ()
	| Row span1, Row span2 -> (
	    try
	      Span.unify span1 span2
	    with Span.Inconsistency ->
	      raise Inconsistency
          )
	| _ ->
	    raise Inconsistency

      let fresh () =
	Variable { link = None }

      exception UnderSpecified

    end

    (* The function [expression] accepts a type scheme expression, and translates it to an internal type scheme.  It
       consists of three passes. First, it performs kind inference, so as to determine the kind of every type variable
       which appears in the expression. Second, it performs sort inference. Lastly, it performs the translation
       itself, which is driven by kind and sort information. *)

    type info =
	Sort.term

    type entries =
	(G.Kind.t -> info G.Abstract.expression -> unit) -> unit

    and constraints =
	info G.Abstract.coercion list

    type translation_function =
      |	TransFun of (bool -> info expression -> leaf)

    let expression entries constraints =

      (* These data structures map type variable names to their kind and to their sort. Whenever a new type variable
	 is discovered, new bindings are added. *)

      let kmap = ref StringMap.empty
      and smap = ref StringMap.empty in

      (* [infer term] creates kind constraints corresponding to the term [term], and returns its inferred kind. Fresh
	 kind variables are created if unknown type variables are found. [infer_expected] works similarly, but accepts
	 an expected kind, and does not return any result. It simply fails if the inferred kind doesn't match the
	 expected one. *)

      let rec infer_expected expected_kind term =
	Kind.unify (infer term) (Kind.Constant expected_kind)

      and infer term =
	match term.G.Abstract.actual with
	| G.Abstract.Variable name -> (
	    try
	      StringMap.find name !kmap
	    with Not_found ->
	      let kind = Kind.fresh() in
	      kmap := StringMap.add name kind !kmap;
	      kind
	  )
	| G.Abstract.Term term ->
	    G.Kind.iter infer_expected term;
	    Kind.Constant (G.Kind.of_term term)
	| G.Abstract.RowExtension (_, field, remainder) ->
	    let kind1 = infer field
	    and kind2 = infer remainder in
	    Kind.unify kind1 kind2;
	    kind1 (* for instance *)
	| G.Abstract.RowUniform term ->
	    infer term in

      (* Make one pass over the whole type scheme expression. *)

      entries infer_expected;
      List.iter (function
	| G.Abstract.Coercion (term1, term2) ->
	    Kind.unify (infer term1) (infer term2)
	| G.Abstract.Conditional (symbol, term0, term1, term2) ->
	    infer_expected (G.Kind.of_symbol symbol) term0;
	    Kind.unify (infer term1) (infer term2)
      ) constraints;

      (* [infer term] creates sort constraints corresponding to the term [term], and returns its inferred sort. At
	 the same time, it decorates [term] with sort annotations, so that they can be easily retrieved later. (This
	 is not a mere matter of speed; if terms were not annotated, sort information would be difficult to recover
	 during the translation phase.) *)

      let rec infer term =
	let sort = match term.G.Abstract.actual with

	| G.Abstract.Variable name -> (
	    try
	      StringMap.find name !smap
	    with Not_found ->
	      let sort = Sort.fresh() in
	      smap := StringMap.add name sort !smap;
	      sort
	  )
	| G.Abstract.Term term ->
	    let sort = Sort.fresh() in
	    G.Sort.iter (fun row_expected leaf ->

	      let leaf_sort = infer leaf in
	      if row_expected then begin

  	        (* If at least one leaf is expected to be a row, then this constructor must be a term constructor;
		   it cannot be a row constructor. This is the case, for instance, of the record constructor. *)

		Sort.unify sort (Sort.Regular);

		(* This leaf is expected to be a row, which contains information about all labels. *)

		Sort.unify leaf_sort (Sort.Row (Span.Constant RowMap.Domain.empty))

	      end
	      else

		(* ``Normal'' leaves may turn out to be rows or regular terms; but they must have the same sort as the
		   whole term itself. *)

		Sort.unify leaf_sort sort

            ) term;
	    sort
	| G.Abstract.RowExtension (label, term, remainder) ->

	    (* The term must be a regular term. *)

	    Sort.unify (infer term) Sort.Regular;

	    (* If the remainder is a row with span $\{ l \} \oplus\alpha$, then the whole term is a row of span
	       $\alpha$. *)

	    let singleton = RowMap.Domain.singleton (Label.get label) in
	    let span = Span.fresh singleton in
	    Sort.unify (infer remainder) (Sort.Row (Span.DisjointSum (singleton, span)));

	    Sort.Row (Span.Variable span)

	| G.Abstract.RowUniform term ->

	    (* Arguments to a $\urow$ constructor must be regular. The whole term is then a row, whose span is
	       unspecified. *)

	    Sort.unify (infer term) Sort.Regular;
	    let span = Span.fresh RowMap.Domain.empty in
	    Sort.Row (Span.Variable span) in

	(* After inferring the term's sort, record it for easy access during the next phase. *)

	term.G.Abstract.info <- Some sort;
	sort in

      (* Make one pass over the whole type scheme expression. We assume all of the type scheme's entry points to have
	 sort [Regular]. *)

      entries (fun _ term ->
	Sort.unify (infer term) Sort.Regular
      );
      List.iter (function
	| G.Abstract.Coercion (term1, term2) ->
	    Sort.unify (infer term1) (infer term2)
	| G.Abstract.Conditional (symbol, term0, term1, term2) ->
	    let sort = infer term0 in
	    Sort.unify sort (infer term1);
	    Sort.unify sort (infer term2)
      ) constraints;

      (* We should now know each variable's kind and sort. If this is not the case, then some variables are
	 under-constrained (because they are not reachable from the type scheme's entry points), which we consider an
	 error.

	 [kind_of] expects a variable name, and returns its inferred kind, by looking up the table which was built
	 during kind inference. Concerning sorts, things are different, because we need sort information not only for
	 variables, but for all terms. We shall extract this information out of each term's [info] field, which was
	 written by the previous phase. *)

      let kind_of name =
	match Kind.repr (StringMap.find name !kmap) with
	| Kind.Variable _ ->
	    raise (Kind.UnderSpecified name)
	| Kind.Constant kind ->
	    kind in

      (* Define how type expressions are mapped to terms. This time, we need another map, which maps type variable
	 names to actual (internal) type variables. More precisely, each type variable name is mapped to \emph{two}
	 internal type variables. One of them represents its positive part, and one represents its negative part. This
	 allows ensuring that no bipolar variables are created during the process. *)

      (* TEMPORARY is it worth holding on to this invariant? *)

      let vmap = ref StringMap.empty in

      let rec translate sign expression =

	(* Retrieve the expression's inferred sort, and turn it into a [sort], suitable for storing in a leaf's [sort]
	   field. *)

	let sort =
	  match expression.G.Abstract.info with
	  | None ->
	      assert false
	  | Some sort ->
	      match Sort.repr sort with
	      | Sort.Regular ->
		  Regular
	      | Sort.Row (Span.Constant span) ->
		  Row span
	      | Sort.Variable _
	      | Sort.Row (Span.Variable _ | Span.DisjointSum _) ->
		  raise Sort.UnderSpecified in

        (* This is the core of the translation function. *)
	
	match expression.G.Abstract.actual with
	| G.Abstract.Variable name ->
	    let neg, pos =
	      try
		StringMap.find name !vmap
	      with Not_found ->
		let pair = Fresh.pair sort (kind_of name) in
		vmap := StringMap.add name pair !vmap;
		pair in
	    if sign then pos else neg
	| G.Abstract.Term term ->
	    (if sign then Fresh.lo else Fresh.hi)
	      sort
	      (G.Term.smap translate sign term)
	| G.Abstract.RowExtension (label, term, remainder) ->
	    let entries =
	      RowMap.add (Label.get label) (translate sign term) RowMap.empty
	    and remainder =
	      translate sign remainder in
	    let row = Row.normalize {
	      entries = entries;
	      remainder = remainder
            } in
	    Fresh.leaf !Level.level sort (RowLink row)
	| G.Abstract.RowUniform term ->
	    translate sign term in

      (* Create the constraint graph. *)

      List.iter (function
	| G.Abstract.Coercion (term1, term2) ->
	    Closure.link (translate true term1) (translate false term2)
	| G.Abstract.Conditional (symbol, term0, term1, term2) ->
	    Closure.conditional symbol (translate true term0)
	      (translate true term1) (translate false term2)
      ) constraints;

      (* Provide the user with the translation function. It is up to him to apply it to the type scheme's entry
	 points, because we do not know about their concrete structure. *)

      TransFun translate

  end

end

