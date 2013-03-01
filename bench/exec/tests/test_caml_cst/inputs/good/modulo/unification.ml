(* $Header: /home/yquem/cristal/fpottier/cvs/modulo/unification.ml,v 1.60 2007/06/30 08:38:13 fpottier Exp $ *)

(* This module implements unification for an extension of the
   $\lambda\sigma$-calculus with Rémy-style \emph{rows} and
   Aït-Kaci/Garrigue-style \emph{label-selective} abstractions and
   applications.

   We employ the algorithm proposed by Dowek, Hardin, Kirchner and
   Pfenning~\cite{dowek-al-98}, with extensions to deal with rows and
   channeled abstractions and applications. The treatment of rows is currently
   correct but somewhat incomplete (see below), while that of channels is, I
   believe, correct and complete. *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Indices} *)

(* It is difficult, here, to give much detail about gap variables. However,
   most of the code is unaffected by their existence. To emphasize this fact,
   we define an abstract type of so-called \emph{indices}. An index is a pair
   of two non-negative integers, which represent a De Bruijn index and a gap
   variable count, respectively. The type of indices is not exactly abstract,
   since [inject] and [project] operations are provided. Still, this
   architecture offers us a degree of modularity: in the absence of gap
   variables, the type [Index.t] would be implemented by [int], and the code
   in the following sections would be essentially unchanged. The reader can
   imagine, upon first reading, that an index is an integer. *)

module Index : sig

  (* The type of indices. *)

  type t

  (* Converting to and from indices. *)

  val inject: int * int -> t
  val project: t -> int * int

  (* The \emph{height} of an index is its second component, i.e. the number of
     gap variables which it contains. This and the following operation would
     be undefined if indices were mere integers. *)

  val height: t -> int

  (* [instantiate i1 i2] returns the index [i2], where the last gap variable
     has been removed and replaced with the index [i1]. *)
  
  val instantiate: int * int -> t -> t

  (* The constant indices 0, 1, and 2, together with functions that allow
     recognizing them. *)

  val null: t
  val is_null: t -> bool
  val one: t
  val is_one: t -> bool
  val two: t

  (* [equal] compares two indices for equality. Note that it is sufficient to
     compare the lengths of the sequences of gap variables; well-kindedness
     then ensures that the sequences are identical. *)

  val equal: t -> t -> bool

  (* [comparable] tells whether two indices are comparable, i.e. whether, when
     viewed as strings of 1's and gap variables, one is a substring of the
     other. This function is used only for sanity checking: well-kindedness
     otherwise guarantees that two indices that appear in the same environment
     are comparable. *)

  val comparable: t -> t -> bool

  (* [less x y] tells whether $x\leq y$ holds, assuming [x] and [y] are
     comparable. More specifically, $x\leq y$ holds if [x], viewed as a
     string of 1's and gap variables, is a substring of [y]. *)

  val less: t -> t -> bool

  (* [max x y] returns the maximum of [x] and [y], which must be
     comparable. *)

  val max: t -> t -> t

  (* [compare x y] compares [x] and [y], which must be comparable.
     [Equal] is raised if they are equal. Otherwise, a negative or
     positive integer is returned, telling whether [x] is smaller
     or greater, respectively, than [y]. *)

  exception Equal
  val compare: t -> t -> int

  (* [add x y] concatenates (adds) the indices [x] and [y]. *)

  val add: t -> t -> t

  (* Assuming $x\geq y$ holds, [sub x y] subtracts [y] from [x]. *)

  val sub: t -> t -> t

  (* [decrement x] is [sub x one]. *)

  val decrement: t -> t

  (* [increment x] is [add x one]. *)

  val increment: t -> t

  (* [shift k x] adds the constant integer index [k] to [x]. *)

  val shift: int -> t -> t

  (* [invert x y] subtracts [y] from [x], assuming they are comparable.
     [Invert] is raised if the \emph{variable} [x] isn't in the image of
     the \emph{substitution} [y]. (Variables count from 1 and up.) *)

  exception Invert
  val invert: t -> t -> t

end = struct

  type t =
      int * int

  let inject x = x
  let project x = x

  let height (k, gamma) =
    gamma

  let instantiate (k', gamma') (k, gamma) =
    assert (gamma > 0);
    (k + k', gamma + gamma' - 1)

  let null =
    (0, 0)

  let is_null = function
    | (0, 0) ->
	true
    | _ ->
	false

  let one =
    (1, 0)

  (* Note: there is only one call site for [is_one], where it is sufficient to
     check that [k] is 1. If it is, then well-kindedness requires that [gamma]
     be 0. *)

  let is_one = function
    | (1, gamma) ->
	assert (gamma = 0);
	true
    | _ ->
	false

  let two =
    (2, 0)

  let equal ((k1, gamma1) : t) (k2, gamma2) =
    (k1 = k2) & (gamma1 = gamma2)

  let less ((k1, gamma1) : t) (k2, gamma2) =
    (k1 <= k2) & (gamma1 <= gamma2)

  let max (((k1, gamma1) as index1) : t) ((k2, gamma2) as index2) =
    if (k1 > k2) or (gamma1 > gamma2) then
      index1
    else
      index2

  let comparable ((k1, gamma1) : t) (k2, gamma2) =
    if (k1 < k2) then
      gamma1 <= gamma2
    else if (k1 > k2) then
      gamma1 >= gamma2
    else (* k1 = k2 *)
      true

  exception Equal

  let compare ((k1, gamma1) : t) (k2, gamma2) =
    if k1 = k2 then
      if gamma1 = gamma2 then
	raise Equal
      else
	gamma1 - gamma2
    else
      k1 - k2

  let add (k1, gamma1) (k2, gamma2) =
    (k1 + k2, gamma1 + gamma2)

  let sub (k1, gamma1) (k2, gamma2) =
    assert ((k1 >= k2) & (gamma1 >= gamma2));
    (k1 - k2, gamma1 - gamma2)

  let decrement (k, gamma) =
    assert (k > 0);
    (k - 1, gamma)

  let increment (k, gamma) =
    (k + 1, gamma)

  let shift ofs (k, gamma) =
    (ofs + k, gamma)

  (* Inversion is subtraction. The resulting index must be a well-formed
     \emph{variable} index, i.e. it must be greater than or equal to [one],
     otherwise inversion is impossible. *)

  exception Invert

  let invert ((k1, gamma1) as i1) ((k2, gamma2) as i2) =
    assert (comparable i1 i2);
    if (k1 > k2) & (gamma1 >= gamma2) then
      k1 - k2, gamma1 - gamma2
    else
      raise Invert

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Kind environments} *)

(* Below, we will annotate every term with the kind environment within
   which it is well-formed. There are two reasons why we keep track of
   kind environments.

   First, kind environments allow mapping indices back to names in a
   consistent way. This is crucial in order to print, say, unsolved
   equations.

   Second, the \emph{rank} of an equation, that is, the number of gap
   variables which appear in its kind environment, is needed to
   implement generalization, where all equations of highest rank must
   be generalized, and others may be left alone. *)

open Id

module KEnv = struct

  (* Our kind environments support unification. Whenever two terms are in a
     structural relationship, we unify (suitable parts of) their kind
     environments. This ensures consistency when these terms are converted to
     a name-based representation.

     Each [KECons] entry contains a pointer to a mutable cell that contains an
     (optional) name. The name is absent if it has not been chosen yet, and
     the cell is mutable to allow choosing a name at some point.

     Notice that we only keep track of the shape of the environment (i.e. the
     nature of its entries and their names), but we do not actually record
     kinds in it.

     The \emph{rank} of a kind environment is the number of gaps in it. It is
     stored explicitly for efficiency, but is otherwise redundant. *)

  (* TEMPORARY should perhaps allow dummy (but named) entries in kind environments
     to reflect user `let type' declarations? *)

  type kenv =
      descriptor UnionFind.point

  and descriptor = {
      rank: int;
      shape: shape;
      mutable (* transient *) copy: kenv option
    } 

  and shape =
    | KENil
    | KECons of id option ref * kenv
    | KEGap of kenv

  (* Accessors. *)

  let rank kenv =
    (UnionFind.find kenv).rank

  let shape kenv =
    (UnionFind.find kenv).shape

  (* Constructors. *)

  let nil =
    UnionFind.fresh {
      rank = 0;
      shape = KENil;
      copy = None
    }

  let cons cell kenv =
    UnionFind.fresh {
      rank = rank kenv;
      shape = KECons (cell, kenv);
      copy = None
    }

  let gap kenv =
    UnionFind.fresh {
      rank = rank kenv + 1;
      shape = KEGap kenv;
      copy = None
    }

  (* [resolve kenv] ensures that every [KECons] cell within [kenv] contains a
     name, by picking new ones if necessary. Names are chosen outside the name
     space of user type names, so as to never hide the name of a type
     introduced by the user. (Doing so would be ``safe'' in a sense, because
     the user type would then be printed with a star to indicate it was captured,
     but the user may still be confused.) New names are chosen so as to never
     capture previously generated names within [kenv]. (Capturing generated
     names in \emph{other} environments is still possible, though, since name
     cells may be shared between environments.) *)

  let was_generated id =
    id.[0] = '%'

  let choose ids =
    let rec choose i =
      let id = "%" ^ (Name.lowercase i) in
      if IdSet.mem id ids then choose (i+1) else id
    in
    choose 0

  let rec resolve kenv =
    match shape kenv with
    | KENil ->
	IdSet.empty
    | KECons ({ contents = Some id }, kenv) ->
	let ids = resolve kenv in
	if was_generated id then IdSet.add id ids else ids
    | KECons ({ contents = None } as cell, kenv) ->
	let ids = resolve kenv in
	let id = choose ids in
	cell := Some id;
	IdSet.add id ids
    | KEGap kenv ->
	resolve kenv

  (* Name lookup.

     [lookup choose k gamma kenv] looks up the index [(k, gamma)] in the
     kind environment [kenv] and returns a name, together with a capture
     count, which tells how many times this type name has been re-bound, or
     tells how far it has fallen off the end. (Falling off the end is allowed,
     because some built-in environment might be implicit.) Note that if the
     capture count is non-zero, then the type name no longer refers to the
     intended index.

     The internal version of [lookup] assumes that [kenv] has been resolved,
     i.e. every name has been chosen. The external version of [lookup]
     satisfies this assumption by first calling [resolve]. *)

  (* TEMPORARY there is linear cost for every lookup, even if [kenv] was
     resolved before. Can be sped up using marks, if deemed necessary. *)

  exception Off of int

  let protect id (id', c) =
    id', if id = id' then c + 1 else c

  let rec lookup k gamma kenv =
    match shape kenv with
    | KENil ->
	assert (gamma = 0);
	raise (Off k)
    | KEGap kenv ->
	assert (gamma > 0);
	lookup k (gamma-1) kenv
    | KECons ({ contents = Some id }, kenv) ->
	if k = 1 then begin
	  assert (gamma = 0);
	  id, 0
	end
	else
	  protect id (lookup (k-1) gamma kenv)
    | KECons ({ contents = None }, _) ->
	assert false

  let lookup k gamma kenv =
    let _ = resolve kenv in
    lookup k gamma kenv

  let lookup1 kenv =
    let id, c = lookup 1 0 kenv in
    assert (c = 0);
    id

  (* [print kenv] prints a list of the names which appear within
     [kenv]. *)

  let concat s1 s2 =
    if s1 = "" then s2 else s1 ^ "; " ^ s2

  let rec print kenv =
    match shape kenv with
    | KENil ->
	""
    | KECons ({ contents = None }, kenv) ->
	assert false
    | KECons ({ contents = Some id }, kenv) ->
	concat (print kenv) id
    | KEGap kenv ->
	print kenv

  let print kenv =
    let _ = resolve kenv in
    print kenv

  (* Unification. *)

  let save cell1 cell2 =
    match !cell1, !cell2 with
    | None, _ ->
	()
    | (Some id1) as sid1, _ ->
	cell2 := sid1
	(* The name in [cell2], if there was one, is lost. That is, when
	   two names are proposed for a variable, we make an arbitrary
	   choice. *)

  let rec unify kenv1 kenv2 =
    if not (UnionFind.equivalent kenv1 kenv2) then begin
      begin
	match shape kenv1, shape kenv2 with
	| KENil, KENil ->
	    ()
	| KECons (cell1, kenv1), KECons (cell2, kenv2) ->
	    if cell1 != cell2 then
	      save cell1 cell2;
	    unify kenv1 kenv2
	| KEGap kenv1, KEGap kenv2 ->
	    unify kenv1 kenv2
	| _, _ ->
	    assert false
      end;
      UnionFind.union kenv1 kenv2
    end

  (* Extracting sub-environments. *)

  let rec cut k gamma kenv =
    if (k = 0) & (gamma = 0) then
      kenv
    else
      match shape kenv with
      |	KENil ->
	  assert false
      |	KECons (_, kenv) ->
	  assert (k > 0);
	  cut (k-1) gamma kenv
      |	KEGap kenv ->
	  assert (gamma > 0);
	  cut k (gamma-1) kenv

  let cut1 kenv =
    cut 1 0 kenv

  (* Extending a kind environment with a chunk of another kind
     environment. *)

  let rec mimic model k gamma kenv =
    if (k = 0) & (gamma = 0) then
      kenv
    else
      match shape model with
      |	KENil ->
	  assert false
      |	KECons (cell, model) ->
	  assert (k > 0);
	  cons cell (mimic model (k-1) gamma kenv)
      |	KEGap model ->
	  assert (gamma > 0);
	   gap (mimic model k (gamma-1) kenv)

  (* Elementary extension functions.

     [intro] introduces a fresh, anonymous variable. [mimic1] also introduces a
     fresh variable, but re-uses an existing name cell, reflecting the fact
     that there is a natural choice for the name that is being introduced. *)

  let intro kenv =
    cons (ref None) kenv

  let mimic1 model kenv =
    mimic model 1 0 kenv

  (* [instantiate] copies a kind environment, replacing the nearest gap in it
     with a chunk of another kind environment. [restore] resets the transient
     fields in the original structure. [instantiate] combines the two. *)

  let rec instantiate model index kenv =
    let desc = UnionFind.find kenv in
    match desc.copy with
    | Some kenv' ->
	kenv'
    | None ->
	let kenv' = match shape kenv with
	| KENil ->
	    assert false
	| KECons (cell, kenv) ->
	    cons cell (instantiate model index kenv)
	| KEGap kenv ->
	    let k, gamma = index in
	    mimic model k gamma kenv
	in
	desc.copy <- Some kenv';
	kenv'

  let rec restore kenv =
    let desc = UnionFind.find kenv in
    match desc.copy with
    | None ->
	()
    | Some _ ->
	desc.copy <- None;
	match shape kenv with
	| KENil ->
	    assert false
	| KECons (_, kenv) ->
	    restore kenv
	| KEGap kenv ->
	    ()

  (* The external version of [cons] always creates a new cell. *)

  let cons ido kenv =
    cons (ref ido) kenv

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Internal data structures} *)

(* Following common practice, we use an imperative union-find algorithm to
   support the efficient assignment of a term to a meta-variable. (See
   [UnionFind].) Every term is represented in memory as a ``point'', whose
   associated ``descriptor'' gives the term's structure. Two terms, that is,
   two points, are unified by merging their associated equivalence classes.

   The descriptor associated with meta-variables carries no information.  In
   other words, a meta-variable does not carry a name; its identity is that of
   the [point] object that represents it.

   More precisely, the descriptor associated with a meta-variable is a
   self-referential [Closure] descriptor: we represent a meta-variable as a
   closure which carries the identity substitution and whose contents is
   itself. In other words, a meta-variable $X$ is represented as the closure
   $\clo{X}{\id}$, where $\id$ is $\shift{0}$. For this reason, there is no
   [MetaVariable] descriptor.  This trick allows treating meta-variables and
   closures alike during unification, leading to a reduction in code size. Of
   course, a distinction between meta-variables and actual closures must still
   be made in some cases, e.g.\ during normalization. This is achieved by
   checking whether the closure at hand is self-referential; true closures
   aren't.

   Variables and ``shift'' substitutions carry an index, numbered from 1 and
   up for variables, and from 0 and up for substitutions.

   [RigidApplication] nodes have the same meaning as [Application] nodes; the
   only difference is that their left son is guaranteed to be either a
   [Variable] node or another [RigidApplication] node. In other words, these
   nodes represent terms that are in weak head normal form. Such nodes are
   produced during (weak head) normalization. They are used to gain efficiency
   in situations where it is useful to quickly determine whether a term is in
   weak head normal form. They are otherwise redundant.

   Composition of substitutions is always eagerly, so composition does not
   appear in the abstract syntax. This choice is made mainly for the sake of
   simplicity.

   Every term carries its kind environment, for reasons described above.

   Descriptors carry a transient field, named [copy], which is used to
   copy terms efficiently, by establishing a constant-time mapping
   between every term and its copy. *)

type row_label =
    string

type channel =
    string

type term =
    descriptor UnionFind.point

and descriptor = {
    structure: structure;
    mutable environment: KEnv.kenv;
    mutable (* transient *) copy: term option
  } 

and structure =
  | Variable of Index.t
  | Abstraction of channel * term
  | Application of term * channel * term
  | RigidApplication of term * channel * term
  | Closure of term * subst
  | RowCons of row_label * term * term
  | RowUniform of term

and subst =
  | Shift of Index.t
  | Cons of term * subst

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Basic operations on terms} *)

(* These shortcuts are convenient. *)

let (===) =
  UnionFind.equivalent

let (=!=) x y =
  not (x === y)

let union =
  UnionFind.union

(* [struc] returns a term's structure. *)

let struc a =
  (UnionFind.find a).structure

(* [environment] returns a term's kind environment. *)

let environment a =
  (UnionFind.find a).environment

(* [kunify a b] requires [a] and [b] to have the same kind
   environment, and returns it. *)

let kunify a b =
  let kenv = environment a in
  KEnv.unify kenv (environment b);
  kenv

(* [create] creates a fresh node with specified (possibly self-referential)
   structure. *)

let create structure kenv =
  UnionFind.self (fun node -> {
    structure = structure node;
    environment = kenv;
    copy = None
  })

(* [fresh structure] returns a fresh node with specified
   (non-self-referential) structure. *)

let fresh structure kenv =
  create (fun _ -> structure) kenv

(* The identity substitution: building it and recognizing it.

   It is, in general, impossible to determine whether a substitution is the
   identity without reducing the terms which appear in it to head normal
   form. For this reason, in several places, we check for the identity merely
   by comparing the substitution with [id]; this check is incomplete, but is
   sufficient for simple speed optimizations. Where it is important that the
   check be complete, we use [simplify] (see below). *)

let id =
  Shift Index.null

let is_id = function
  | Shift index ->
      Index.is_null index
  | Cons _ ->
      false

(* Here are the term creation functions.

   For meta-variables, variables, and closures, the kind environment must be
   specified. In the case of closures, it is thus the substitution's outer
   kind environment. For other constructions, an appropriate kind environment
   is inferred from the sub-terms' own kind environments. Note that forming
   new terms may cause existing kind environments to be unified. *)

let metavariable kenv =
  create (fun node -> Closure (node, id)) kenv

let _variable i kenv =
  fresh (Variable i) kenv

let variable index kenv =
  _variable (Index.inject index) kenv

let abstraction p a =
  fresh (Abstraction (p, a)) (KEnv.cut1 (environment a))

let abs t =
  abstraction "" t

let application a1 p a2 =
  fresh (Application (a1, p, a2)) (kunify a1 a2)

let app t1 t2 =
  application t1 "" t2

let rigid_application a1 p a2 =
  fresh (RigidApplication (a1, p, a2)) (kunify a1 a2)

let rec check ikenv okenv = function
  | Shift i ->
      let k, gamma = Index.project i in
      KEnv.unify ikenv (KEnv.cut k gamma okenv)
  | Cons (a, s) ->
      KEnv.unify (environment a) okenv;
      check (KEnv.cut1 ikenv) okenv s

let _closure a s kenv =
  check (environment a) kenv s;
  fresh (Closure (a, s)) kenv

let closure a s kenv =
  if is_id s then a else _closure a s kenv

let row_cons label a1 a2 =
  fresh (RowCons (label, a1, a2)) (kunify a1 a2)

let row_uniform a =
  fresh (RowUniform a) (environment a)

let shift index =
  Shift (Index.inject index)

let shift1 =
  Shift Index.one

let cons a s =
  Cons (a, s)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Printing} *)

(* The external abstract syntax for our calculus. *)

type reference =
  | RefName of id
  | RefBuiltin of int

type tree =
  | TMetaVariable of string
  | TVariable of int * int * reference
  | TAbstraction of id * channel * tree
  | TApplication of tree * channel * tree
  | TClosure of tree * tree
  | TRowCons of row_label * tree * tree
  | TRowUniform of tree
  | TShift of int * int
  | TCons of id * tree * tree

(* [convert a] returns an abstract syntax tree for the term [a].

   When converting an index back to a name, if [KEnv.lookup] returns a
   non-zero capture count, then we append a number of stars to the type name,
   so as to indicate how far back it is really bound, disambiguating it. *)

let name =
  let module N = Name.Make (struct
    type t = term
    let equal = (===)
  end) in
  N.name

let rec convert a =
  match struc a with
  | Variable index ->
      let k, gamma = Index.project index in
      let reference = try
	let id, c = KEnv.lookup k gamma (environment a) in
	RefName (id ^ (String.make c '*'))
      with KEnv.Off k ->
	RefBuiltin k in
      TVariable (k, gamma, reference)
  | Abstraction (p, a) ->
      let id = KEnv.lookup1 (environment a) in
      TAbstraction (id, p, convert a)
  | Application (a1, p, a2)
  | RigidApplication (a1, p, a2) ->
      TApplication (convert a1, p, convert a2)
  | Closure (a1, s) ->
      if a === a1 then
	TMetaVariable (Name.uppercase (name a))
      else
	TClosure (convert a1, convert_subst (environment a1) s)
  | RowCons (label, a1, a2) ->
      TRowCons (label, convert a1, convert a2)
  | RowUniform a ->
      TRowUniform (convert a)

and convert_subst ikenv = function
  | Shift index ->
      let k, gamma = Index.project index in
      TShift (k, gamma)
  | Cons (a, s) ->
      TCons (KEnv.lookup1 ikenv, convert a, convert_subst (KEnv.cut1 ikenv) s)

(* [metavars] collects a list of all the meta-variables which occur within a
   term. *)

let rec metavars accu a =
  match struc a with
  | Variable _ ->
      accu
  | Abstraction (_, a)
  | RowUniform a ->
      metavars accu a
  | Application (a1, _, a2)
  | RigidApplication (a1, _, a2)
  | RowCons (_, a1, a2) ->
      metavars (metavars accu a1) a2
  | Closure (a1, s) ->
      if a === a1 then
	if List.exists ((===) a) accu then accu else a :: accu
      else
	metavars_subst (metavars accu a1) s

and metavars_subst accu = function
  | Shift _ ->
      accu
  | Cons (a, s) ->
      metavars_subst (metavars accu a) s

(* [info] returns human-readable information about an equation, namely
   the set of type names that are in scope at the toplevel, and the set
   of type names which each meta-variable is allowed to mention. *)

let info (a, b) =
  let s = Printf.sprintf "The types in scope at this point are:\n%s\n"
      (KEnv.print (environment a))
  in
  let mvs = metavars (metavars [] a) b in
  List.fold_left (fun s x ->
    Printf.sprintf "%s\nThe meta-variable %s may refer to the following types:\n%s\n"
      s
      (Name.uppercase (name x))
      (KEnv.print (environment x))
  ) s mvs

(* A pretty-printer for our abstract syntax.

   This pretty-printer prints numeric indices only; it does not use the
   name information contained in kind environments. *)

module P = Tree.Make (struct

  type dummy = tree
  type tree = dummy (* argh *)

  type label =
    | LAbstraction
    | LApplicationL
    | LApplicationR
    | LClosureL
    | LClosureR
    | LRowCompL
    | LRowCompR
    | LRowUniform
    | LConsL
    | LConsR

  open Tree

  let describe_index mark k gamma =
    if k = 0 then
      String.make gamma '!'
    else
      mark ^ (string_of_int k) ^ (String.make gamma '!')

  let describe = function
    | TMetaVariable x ->
	[ Token x ]
    | TVariable (k, gamma, reference) ->
	[ Token (describe_index "" k gamma) ]
    | TAbstraction (id, p, t) ->
	[ Token ("\\" ^ p ^ (if p = "" then "" else ":")); Son (LAbstraction, t) ]
    | TApplication (t1, p, t2) ->
	[ Son (LApplicationL, t1); Token (" " ^ p ^ (if p = "" then "" else ":")); Son (LApplicationR, t2) ]
    | TClosure (t1, t2) ->
	[ Son (LClosureL, t1); Token "["; Son (LClosureR, t2); Token "]" ]
    | TRowCons (label, t1, t2) ->
	[ Token label; Token ": "; Son (LRowCompL, t1); Token "; "; Son (LRowCompR, t2) ]
    | TRowUniform t ->
	[ Token "!"; Son (LRowUniform, t) ]
    | TShift (k, gamma) ->
	[ Token (describe_index "/" k gamma) ]
    | TCons (id, t1, t2) ->
	[ Son (LConsL, t1); Token "."; Son (LConsR, t2) ]

  let parenthesize label tree =
    match label, tree with
    | (LAbstraction | LApplicationR | LClosureL), (TRowCons _ | TRowUniform _) ->
	angle_brackets
    | LRowCompR, (TRowCons _ | TRowUniform _) ->
	nothing
    | (LApplicationL | LClosureR | LRowCompL | LRowUniform | LConsL | LConsR), (TRowCons _ | TRowUniform _) ->
	assert false

    | _, TMetaVariable _
    | _, TVariable _ ->
	nothing

    | LConsL, _ ->
	nothing
    | LRowUniform, _ ->
	parentheses
    | LRowCompL, _
    | LRowCompR, _ ->
	nothing

    | _, TShift _
    | _, TCons _ ->
	nothing

    | LAbstraction, _ ->
	nothing
    | _, TAbstraction _ ->
	parentheses

    | LApplicationL, TApplication _ ->
	nothing
    | (LApplicationR | LClosureL), TApplication _ ->
	parentheses

    | (LApplicationL | LApplicationR | LClosureL), TClosure _ ->
	nothing

    | (LClosureR | LConsR), (TApplication _ | TClosure _) ->
	assert false

end)

let print =
  P.print

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Weak head reduction} *)

(* A \emph{weak head normal form} is an abstraction, an application of a
   variable to an arbitrary number of operands, a closure whose body is a
   meta-variable, or a term formed with either of the two row constructors. In
   the second case, we further require that the application operands be sorted
   according to some fixed, arbitrary order on channels.

   The following functions allow reducing arbitrary terms to weak head normal
   form (defined below). The process is guaranteed to terminate if the terms
   at hand are well-kinded. *)

(* [drop i s] computes $\shift{i} \circ s$. *)

let rec drop i1 s2 =
  if Index.is_null i1 then s2
  else match s2 with
  | Shift i2 ->
      Shift (Index.add i1 i2)
  | Cons (_, s2tl) ->
      drop (Index.decrement i1) s2tl

(* [compose s1 s2] computes $s_1 \circ s_2$, i.e.\ the substitution which maps
   every De Bruijn index $n$ to $s_2(s_1(n))$.

   Every term [a] which appears explicitly in [s1] is wrapped in a closure
   $\clo{a}{s_2}$, so the computation stops at the boundary between substitutions
   and terms. The special case where [s2] is the identity substitution is
   optimized to avoid creating superfluous closures.

   [kenv] is [s2]'s outer kind environment. *)

let rec compose s1 s2 kenv =
  if is_id s2 then s1
  else match s1 with
  | Shift i1 ->
      drop i1 s2
  | Cons (a, s1tl) ->
      Cons (_closure a s2 kenv, compose s1tl s2 kenv)

(* Given a substitution [s], [protect] returns the substitution $1.(s \circ
   \shift{})$, i.e.\ a ``view'' of [s] under one more
   $\lambda$-abstraction. The common case where [s] is the identity
   substitution is optimized. [kenv] is the outer kind environment of
   the newly created substitution. *)

let _protect s kenv =
  Cons (_variable Index.one kenv, compose s shift1 kenv)

let protect s kenv =
  if is_id s then s else _protect s kenv

(* [apply] builds the application of a term to a \emph{spine}, that is, a list
   of pairs of a channel and a term). The spine is first sorted according to
   the ordering on channels. This ensures that the result is a weak head
   normal form. Of course, it is important that the sorting function be
   \emph{stable}, so that the order of arguments on a given channel is
   preserved: consecutive abstractions or applications on a \emph{single}
   channel do \emph{not} commute.

   The term [a] must be a [Variable] or [RigidApplication] node. This allows
   us to build [RigidApplication] nodes. *)

let apply a spine =
  List.fold_left (fun a (p, b) ->
    rigid_application a p b
  ) a (List.stable_sort (fun (p, _) (q, _) ->
         compare p q (* channel comparison *)
       ) spine)

(* [normalize a s spine kenv] computes the weak head normal form of the
   application of the term $\clo{a}{s}$ to the spine [spine]. If [s] is the
   identity and the spine is empty, then it also updates the term [a] in
   place, so that the same computation need not be performed again. [kenv]
   is the kind environment of the term $\clo{a}{s}$, that is, [s]'s outer
   kind environment.

   The motivation for making a substitution and a spine parameters of the
   normalization function is to make the [Application] and [Closure] cases
   very straightforward (see below). As a result, every call to [normalize]
   results in at most one recursive call to itself, which would not be the
   case with a more naïve approach. That call is usually a tail call, except
   in cases where some updating needs to be performed after normalization. *)

(* TEMPORARY normalizing explicit substitutions causes a loss of sharing; can
   it be helped? *)

let rec normalize a s spine kenv =
  match struc a, is_id s, spine with

  (* [a] is a weak head normal form; no spine, no substitution; return [a]
     unchanged. *)

  | (Abstraction _ | RigidApplication _), true, []
  | (RowCons _ | RowUniform _), true, _ ->
      assert (spine = []);
      a

  (* This is an optimization of the following case. *)

  | Variable _, true, _ ->
      apply a spine

  (* The term at hand is a variable; this is dealt with by a separate
     function, [normalize_variable]. *)

  | Variable i, _, _ ->
      normalize_variable i s spine kenv

  (* $\clo{(\abs{p}{a})}{s}$ is $\abs{p}{(\clo{a}{s'})}$, where $s'$ is
     [protect s]. *)

  | Abstraction (p, a), _, [] ->
      let kenv' = KEnv.mimic1 (environment a) kenv in
      abstraction p (_closure a (_protect s kenv') kenv')

  (* We have an abstraction on channel $p$, and an argument available
     on channel $q$. *)

  | Abstraction (p, a), _, (q, b) :: spine ->

      (* $\app{\clo{(\abs{p}{a})}{s}}{p}{b}$ is $\clo{a}{b.s}$. This is
	 $\beta$-reduction. Note that the channel name is dropped. *)

      if compare p q = 0 then (* channel comparison *)
        normalize a (Cons(b, s)) spine kenv

      (* $\app{\clo{(\abs{p}{a})}{s}}{q}{b}$ is
	 $\abs{p}{(\app{\clo{a}{s'}}{q}{\clo{b}{\shift{}}})}$, where $s'$ is
	 [protect s]. Abstraction and application on distinct channels
	 commute. *)

      else
	let kenv' = KEnv.mimic1 (environment a) kenv in
	let a' = closure a (protect s kenv') kenv' in
	normalize (abstraction p (application a' q (_closure b shift1 kenv')))
	          id spine kenv

  (* In the next two cases, [spine] must be empty, otherwise the term would be
     ill-kinded. Pushing the substitution [s] down into the term yields a weak
     head normal form. *)

  | RowCons (label, a1, a2), _, _ ->
      assert (spine = []);
      row_cons label (_closure a1 s kenv) (_closure a2 s kenv)

  | RowUniform a, _, _ ->
      assert (spine = []);
      row_uniform (_closure a s kenv)

  (* The term [a] is known to be in weak head normal form already, so we
     don't need to examine it further. *)

  | RigidApplication _, true, _ ->
      apply a spine

  (* This is identical to the general case below, except the result is
     retained to avoid further re-computations. *)

  | Application (a1, p, a2), true, [] ->
      let a' = normalize a1 s [ (p, a2) ] kenv in
      union a a';
      a'

  (* This is an optimization of the following case. *)

  | Application (a1, p, a2), true, _ ->
      normalize a1 s ((p, a2) :: spine) kenv

  (* This is an application node. The operand is pushed onto the spine,
     and we continue with [a1]. *)

  | Application (a1, p, a2), _, _
  | RigidApplication (a1, p, a2), _, _ ->
      normalize a1 s ((p, _closure a2 s kenv) :: spine) kenv

  (* Because of our representation convention, this might be either a
     meta-variable or an actual closure. We distinguish by checking
     whether it is self-referential. *)

  | Closure (a1, s1), flag, _ ->

      if a === a1 then
       	match flag, spine with

	(* [a] is a meta-variable $X$; no spine, no substitution; return [a]
	   unchanged. *)

	| true, [] ->
	    a

        (* [a] is a meta-variable $X$; return $X[s]$. *)

	| _, [] ->
	    _closure a s kenv

	(* [a] is a meta-variable $X$. The closure $X[s]$ appears on the
	   left-hand side of an application. Thus, $X$ must have functional
	   kind; we expand it, i.e.\ identify it with $\abs{p}{Y}$, where $Y$
	   is a fresh meta-variable. Then, $\beta$-reduction becomes
	   possible. Through several reduction steps,
	   $\app{\clo{(\abs{p}{Y})}{s}}{p}{b}$ reduces to $\clo{Y}{b.s}$.

	   Note that, in this case, we build a $\lambda$-abstraction not in
	   order to mimic an existing one, but because the presence of an
	   application dictates it. As a result, there is no natural way to
	   name the variable bound by this new $\lambda$-abstraction. This is
	   why we use [KEnv.intro] rather than [KEnv.mimic1]. *)

	| _, (p, b) :: spine ->
	    let y = metavariable (KEnv.intro (environment a)) in
	    union a (abstraction p y);
	    normalize y (Cons (b, s)) spine kenv

      else
	match flag, spine with

	(* [a] is a closure. This is identical to the general case below,
	   except the result is retained to avoid further re-computations. *)

	| true, [] ->
	    let a' = normalize a1 s1 spine kenv in
	    union a a';
	    a'

	(* [a] is a closure. The substitution is composed with the current
	   substitution, and we continue with [a1]. *)

	| _, _ ->
	    normalize a1 (compose s1 s kenv) spine kenv

(* [normalize_variable i s spine kenv] computes the weak head normal form of the
   application of the term $\clo{i}{s}$ to the spine [spine]. It is made a
   separate function only for the sake of efficiency: it does not allocate
   [Variable] nodes while walking down the substitution. *)

and normalize_variable i s spine kenv =
  match s with

  (* $\clo{i}{\shift{j}}$ is $i+j$. This yields a variable on the left-hand
     side of an application. We have reached a head normal form; there only
     remains to build it, which we do using [apply]. *)

  | Shift j ->
      apply (_variable (Index.add i j) kenv) spine

  (* $\clo{1}{a.s}$ is $a$. If $i>1$ holds, then $\clo{i}{a.s}$ is
     $\clo{(i-1)}{s}$. *)

  | Cons (a, s) ->
      if Index.is_one i then
	normalize a id spine kenv
      else
	normalize_variable (Index.decrement i) s spine kenv

(* [normalize a] reduces [a] to a weak head normal form. It simply invokes
   the above machinery with the identity substitution and an empty spine. *)

let normalize a =
  normalize a id [] (environment a)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Simplification and strong reduction} *)

(* [normalize_subst normalize s] ensures that all of the terms which appear in
   [s] are normal with respect to the notion of reduction defined by
   [normalize]. Furthermore, [s] is simplified by rewriting $i.\shift{i}$ to
   $\shift{i-1}$, for $i\geq 1$. As a result, if [s] is $\lambda\sigma$-equal
   to the identity, then the identity [id] will be returned, assuming
   [normalize] reduces at least to weak head normal form. *)

let rec normalize_subst normalize s =
  match s with
  | Shift _ ->
      s
  | Cons (a1, s1) ->
      let _ = normalize a1 in
      let s1' = normalize_subst normalize s1 in
      match struc a1, s1' with
      | Variable i1, Shift i2 when Index.equal i1 i2 ->
	  Shift (Index.decrement i1)
      | _, _ ->
	  if s1 == s1' then (* simple space optimization *)
	    s
	  else
	    Cons (a1, s1')

(* [simplify normalize a] reduces the term [a] -- which must be in weak head
   normal form -- so as to ensure that, if the reduct is of the form
   $\clo{X}{s}$, then [s] is not $\lambda\sigma$-equal to the identity and
   every term which appears in [s] is normal with respect to the notion of
   reduction defined by [normalize]. [a] is updated in place. *)

let simplify normalize a =
  match struc a with
  | Closure (x, s) ->
      if a =!= x then
	let s' = normalize_subst normalize s in
	if s != s' then
	  union a (closure x s' (environment a)) (* TEMPORARY avoid re-checking kenv *)
  | _ ->
      ()

(* [strong a] reduces the term [a] to normal form. [a] is updated in
   place. This function isn't used during solving, but can be used to simplify
   terms aggressively before copying or printing them.  [whnf_to_nf a] is
   similar, but assumes that [a] is already in weak head normal form. *)

let rec whnf_to_nf a =
  match struc a with
  | Variable _ ->
      ()
  | Abstraction (_, a1) ->
      strong a1
  | Application _ ->
      assert false
  | RigidApplication (a1, _, a2) ->
      strong a2;
      whnf_to_nf a1
  | Closure _ ->
      simplify strong a
  | RowCons (label, a1, a2) ->
      strong a1;
      strong a2
  | RowUniform a1 ->
      strong a1

and strong a =
  whnf_to_nf (normalize a)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Unification infra-structure} *)

(* An \emph{equation} consists of a pair of terms. *)

type equation =
    term * term

(* We use two queues to hold equations. This simple mechanism can be
   viewed as an implementation of a priority queue with two priority
   levels: scheduled and suspended. *)

let scheduled : equation FQueue.t =
  FQueue.create()

let suspended : equation FQueue.t =
  FQueue.create()

let schedule eq =
  FQueue.add eq scheduled

let suspend eq =
  FQueue.add eq suspended

let equations f accu =
  FQueue.fold f accu scheduled

(* [isolate rank] returns a list of all scheduled equations whose rank
   is [rank], and removes them from the queue. *)

(* TEMPORARY a more efficient implementation would be possible if one
   equation queue per rank was maintained *)

let isolate rank =
  let others = FQueue.create() in
  let isolated = ref [] in
  while FQueue.length scheduled > 0 do
    let (a, b) as eq = FQueue.take scheduled in
    if rank = KEnv.rank (environment a) then
      isolated := (a, b) :: !isolated
    else
      FQueue.add eq others
  done;
  FQueue.transfer others scheduled;
  !isolated

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Inverting pattern substitutions} *)

(* A substitution $a_1.a_2.\ldots.a_n.\shift{m}$ is a \emph{pattern
   substitution} if and only if every $a_i$ (where $1\leq i\leq n$) is a
   variable whose index is less than or equal to $m$ and these variables are
   pairwise distinct. Pattern substitutions represent an important special
   case, because they are injective, hence invertible. *)

(* [is_variable a] determines whether [a] is a variable. *)

let is_variable a =
  match struc a with
  | Variable _ ->
      true
  | _ ->
      false

(* If [a] is a variable, then [index a] returns its index. It raises
   the exception [NotAVariable] if [a] is in fact not a variable. *)

exception NotAVariable

let index a =
  match struc a with
  | Variable i ->
      i
  | _ ->
      raise NotAVariable

(* [is_pattern_subst s] accepts a normal substitution (i.e.\ one produced by
   [normalize_subst]) and tells whether it is a pattern substitution. The
   reason why [s] must be normal is that we must determine whether the terms
   that appear in it are variables. *)

(* TEMPORARY possible gain d'efficacité en cachant le résultat de ce test,
   lorsqu'il est positif, dans un noeud spécial [ClosurePat]. *)

module IndexSet = Set.Make (Index)

let rec check m indices = function
  | Shift i ->
      Index.less m i
  | Cons (a, s) ->
      let i = index a in

      (* [Index]'s ordering function raises an exception when its arguments
	 are equal. This kludge allows to perform an ``exclusive'' addition
	 operation, which fails if the new element already belongs to the
	 set. *)

      check (Index.max i m) (IndexSet.add i indices) s

let is_pattern_subst s =

  (* Every substitution must be of the form $a_1.a_2.\ldots.a_n.\shift{m}$. We
     must make sure that every $a_i$ (where $1\leq i\leq n$) is a variable
     whose index is less than or equal to $m$. Furthermore, we must check that
     these variables are pairwise distinct. *)

  try
    check Index.null IndexSet.empty s
  with
  | NotAVariable
  | Index.Equal ->
      false

(* Given two pattern substitutions $\xi$ and $\zeta$ with the same inner
   (resp. outer) kind environments (which must be the case, for instance, if
   they are applied to the same term in the same context), [cap kenv xi zeta]
   computes $\xi\cap\zeta$, that is, a pattern substitution which renders
   inaccessible all of the indices where $\xi$ and $\zeta$ disagree (and only
   those indices). The parameter [kenv] is $\xi$ and $\zeta$'s inner kind
   environment. In addition to $\xi\cap\zeta$ itself, [cap] returns
   $\xi\cap\zeta$'s inner kind environment. ($\xi\cap\zeta$'s outer kind
   environment is $\xi$ and $\zeta$'s inner kind environment.)

   Both $\xi$ and $\zeta$ must be normal. The substitution that is produced
   satisfies the invariants enforced by [normalize_subst].

   [cap_cons kenv i xi j zeta] computes $(i.\xi\cap j.\zeta)$.

   The second case in the definition of [cap_shift] expands a substitution of
   the form $\shift{j}$ into $j+1.\shift{j+1}$. Although this transformation
   is not legal in general (because 1 might be ill-sorted in the
   substitution's internal sorting context), it is legal here: indeed, 1 must
   be well-sorted within the substitution, since the other substitution is of
   the form $i.\xi$. *)

let rec cap kenv xi zeta =
  match xi, zeta with
  | Cons (i, xi), Cons (j, zeta) ->
      cap_cons kenv (index i) xi (index j) zeta
  | Cons (i, xi), Shift j
  | Shift j, Cons (i, xi) ->
      let j1 = Index.increment j in
      cap_cons kenv (index i) xi j1 (Shift j1)
  | Shift i, Shift j ->
      assert (Index.equal i j);
      kenv, id

and cap_cons kenv i xi j zeta =
  let itailenv, tail = cap (KEnv.cut1 kenv) xi zeta in
  let tail = compose tail shift1 kenv in

  (* If $\xi$ and $\zeta$ agree on index $1$, then include it in the result
     substitution, otherwise make it inaccessible. *)

  if Index.equal i j then
    KEnv.mimic1 kenv itailenv,
    Cons (_variable Index.one kenv, tail)
  else
    itailenv, tail

(* [invert_variable i xi] solves the equation $i=\clo{X}{\xi}$, where $\xi$ is
   a pattern substitution. The equation admits at most one solution, which
   must be a variable, whose index is returned. [NoSolution] is raised if
   there is no solution. *)

exception NoSolution

let rec invert_variable i = function

  (* If $\xi$ is $\shift{j}$, then inversion is essentially subtraction $i-j$.
     Of course, the result must be an appropriate index for a variable, i.e. a
     positive one. An exception is raised otherwise. *)

  | Shift j -> (
      try
	Index.invert i j
      with Index.Invert ->
	raise NoSolution
    )

  (* If $a$ is $i$, then $i$ is the image of $1$ through the substitution
     $a.\xi$. Otherwise, we look further; if $i$ is the image of some variable
     $b$ through $\xi$, then it is the image of $b+1$ through $a.\xi$. *)

  | Cons (a, xi) ->
      if Index.equal i (index a) then
	Index.one
      else
	Index.increment (invert_variable i xi)

(* We now come to the core (and the most complex part) of the pattern
   unification algorithm, that is, the function that attempts to apply the
   inverse of a pattern substitution $\xi$ to an arbitrary weak head normal
   form [a].

   The external function, namely [invert], is a wrapper for two internal
   functions, which are defined in a mutually recursive fashion, namely
   [invert] and [bar].

   In addition to the parameters $\xi$ and [a], described above, the internal
   function [invert] is parameterized by a meta-variable [x] and two Boolean
   flags [rigid] and [strict]. Indeed, every time [invert] is invoked, we are
   attempting to solve an equation of the form $\clo{X}{\xi}=C[a]$, where $C$
   is a certain term context. ($C[a]$ denotes the result of filling $C$'s hole
   with the term [a], \emph{not} a $\lambda\sigma$ closure.) Then, the
   parameter [x] represents the meta-variable $X$, while the parameters
   [rigid] and [strict] tell whether the context $C$ is, respectively, rigid
   and strict.

   This extra information is used in two ways. First, an occur check is
   performed on the fly; that is, an error is signaled if an occurrence of $X$
   is found within [a]. Second, if an error is signaled within a flexible
   context -- a situation which arises only when outside of the decidable
   ``pattern'' fragment, because no meta-variable can occur within a pattern
   substitution -- then the error is considered non-fatal and dealt
   with. Indeed, because the non-invertible term is in a flexible context, it
   may disappear altogether in the future, so the equation may still have
   solutions.

   An error is dealt with either by requiring that a certain meta-variable not
   mention certain De Bruijn indices (by equating it with a closure of a fresh
   meta-variable and of a certain substitution), or by suspending part of the
   inversion computation (by generating a fresh meta-variable, together with
   an appropriate equation, to stand for an intermediate result which could
   not be computed), or by suspending the whole equation.

   The internal function [invert] may raise either [NoSolution] or
   [Suspend]. When the parameter [rigid] was set, the former means that the
   equation has no solution, while the latter requests that the equation be
   suspended. Recursive calls to [invert], with [rigid] set to [false], may
   raise [NoSolution] to request that a sub-computation be suspended higher
   up.

   Fresh meta-variables and equations can be created and scheduled by [invert]
   only when [rigid] holds.

   [invert] and its auxiliary functions are careful to behave as the physical
   identity (that is, to preserve sharing) when the term [a] is its own image
   through the substitution $\xi$. This saves garbage collection time.

   The parameter [kenv] represents $\xi$'s internal kind environment. (Its
   outer kind environment is that of [a].) Thus, it is also the kind
   environment associated with the term which [invert] must return. It is used
   when creating that term.

   A perhaps interesting performance gain could be obtained by specializing
   [invert] and [bar] for the case where [rigid] is [false] and $\xi$ is the
   identity. Then, it is known beforehand that [invert] will return its
   argument unchanged, and [bar] will return a pair of the identity and of its
   first argument unchanged -- that is, provided they succeed. The only
   possible cause of failure, in that case, is to find an occurrence of
   $X$. In other words, both functions become simple ``occur checks'' in that
   case. This is not done at present, in order to keep the code smaller. *)

exception Suspend

let invert x kenv xi a =

  let rec invert rigid strict kenv xi a =
    simplify normalize a;
    match struc a with

    | Application _ ->
	assert false

    | RigidApplication (a1, p, a2) ->
	let a1' = invert rigid true kenv xi a1
	and a2' = invert rigid true kenv xi (normalize a2) in
	if (a1 === a1') & (a2 === a2') then a
	else
	  rigid_application a1' p a2'

    | Abstraction (p, a1) ->
	let kenv1 = environment a1 in
	let kenv' = KEnv.mimic1 kenv1 kenv in
	let a1' = invert rigid true kenv' (protect xi kenv1) (normalize a1) in
	if (a1 === a1') then a
	else
	  abstraction p a1'

    | Variable i ->

	(* First cause of errors during inversion: [invert_variable] may
	   fail. *)
	
	let i' = invert_variable i xi in
	if (Index.equal i i') & (UnionFind.equivalent (environment a) kenv) then a
	else
	  _variable i' kenv

    | RowCons (label, a1, a2) ->
	let a1' = invert rigid true kenv xi (normalize a1)
	and a2' = invert rigid true kenv xi (normalize a2) in
	if (a1 === a1') & (a2 === a2') then a
	else
	  row_cons label a1' a2'

    | RowUniform a1 ->
	let a1' = invert rigid true kenv xi a1 in
	if (a1 === a1') then a
	else
	  row_uniform a1'

    | Closure (y, zeta) ->

	(* TEMPORARY ne faut-il pas optimiser le cas où [zeta] est l'identité? *)

	(* Second cause of errors during inversion: [invert] fails if it finds
	   an occurrence of [x]. *)

	if y === x then
	  raise NoSolution
	else
	  try

	    (* We must compute the inverse of $\clo{Y}{\zeta}$ through
	       $\xi$. To do so, we compute a so-called \emph{weakening
	       substitution} $\zeta\mid\xi$, together with the composition
	       $(\zeta\mid\xi)\circ\zeta\circ\xi^{-1}$.  Note that $\zeta$ is
	       not necessarily a pattern substitution. *)

	    let izetabarxienv, zetabarxi, zetabarxizetaxim =
	      bar rigid strict (environment y) zeta kenv xi in

	    (* In the (simplest) case where the former is the identity, the
	       latter is $\zeta\circ\xi^{-1}$. Then, the desired result is
	       $\clo{Y}{\zeta\circ\xi^{-1}}$. *)

	    if is_id zetabarxi then
	      if zeta == zetabarxizetaxim then a
	      else
		closure y zetabarxizetaxim kenv

	    (* In general, the function [bar] may encounter errors while
	       attempting to compose $\zeta$ with $\xi^{-1}$. This indicates
	       that $Y$ should not mention certain De Bruijn indices. In that
	       case, $\zeta\mid\xi$ is not the identity. We equate $Y$ with
	       $\clo{Z}{\zeta\mid\xi}$, where $Z$ is a fresh meta-variable,
	       and we return
	       $\clo{Z}{(\zeta\mid\xi)\circ\zeta\circ\xi^{-1}}$. For more
	       details about this case, study [bar] below. Notice that,
	       because [zetabarxi] is not the identity, [rigid] must hold. In
	       other words, it is safe to specialize $Y$, because we are
	       working within a rigid context. *)

	    else begin

	      if not rigid then begin (* TEMPORARY *)
		print_endline (print (convert_subst izetabarxienv zetabarxi));
		flush stdout;
	      end;

	      assert rigid;
	      let z = metavariable izetabarxienv in
	      schedule (y, _closure z zetabarxi (environment y));
	      closure z zetabarxizetaxim kenv
	    end

	  with
	  | NoSolution when rigid ->

	      (* Our call to [bar] above has been unable to successfully
		 pursue the inversion process within $\zeta$. The equation we
		 are considering is $\clo{X}{\xi}=C[\clo{Y}{\zeta}]$, where
		 $C$ is a strict, rigid context. Because $C$ is strict, it is
		 safe to suspend this sub-computation, by returning a fresh
		 meta-variable which stands for its result. (If $C$ were not
		 strict, we would risk non-termination.)  That is, we return a
		 fresh meta-variable $Z$, together with a new equation
		 $Z[\xi]=Y[\zeta]$. As a result, the original equation will be
		 converted to $X=D[Z]$, where $D$ is the (strict, rigid)
		 context obtained by inverting $C$ through $\xi$. Even though
		 we suspend this sub-computation, the process still yields
		 some useful information about $X$. *)

	      if strict then
		let z = metavariable kenv in
		schedule (closure z xi (environment a), a);
		z

              (* Same situation, but $C$ is the empty context. Suspend the
		 whole equation. *)

	      else
		raise Suspend

(* [bar rigid strict zeta xi] returns $\zeta\mid\xi$ and
   $(\zeta\mid\xi)\circ\zeta\circ\xi^{-1}$, as described above and by Dowek
   \emph{et al.}~\cite{dowek-al-98}. It seems more efficient to compute these
   substitutions at once, rather than separately.

   $\zeta$ and $\xi$ must have the same outer kind environment. Their inner
   kind environments are passed as parameters to [bar]. The outer kind
   environment of the substitution returned by [bar] (namely $\zeta\mid\xi$)
   is $\zeta$'s inner kind environment. Its inner kind environment is returned
   by [bar].

   Roughly speaking, the idea is that the computation of $\zeta\circ\xi^{-1}$
   may fail, because the image of $\zeta$ is larger than that of $\xi$, or
   because an occurrence of $X$ is found. In the general case, when such a
   failure occurs, [bar] does nothing special, so [NoSolution] escapes out of
   it, and it is handled by [invert] above. However, in some cases, it is
   possible -- correct and complete -- to specialize $\zeta$ by composing it
   with a \emph{weakening substitution} $\zeta\mid\xi$, which is the most
   general substitution such that $(\zeta\mid\xi)\circ\zeta\circ\xi^{-1}$
   exists. This is done, in particular, when dealing with equations in the
   decidable ``pattern'' fragment.

   [bar] does not create fresh meta-variables or schedule new equations. (Note
   that its recursive call to [invert] sets the parameter [rigid] to [false].)
   When [rigid] is false, [bar]'s first result is always the identity.

   [bar] expects every term which appears in $\zeta$ to be in weak head normal
   form. This is initially established in [invert] above, where [simplify] is
   called before proceeding. *)

  and bar rigid strict izetaenv zeta0 ixienv xi0 =
    match zeta0, xi0 with
    | Cons (b, zeta), xi -> (

	(* We are now trying to compute $(b.\zeta)\circ\xi^{-1}$. *)

	let izetabarxienv, zetabarxi, zetabarxizetaxim =
	  bar rigid strict (KEnv.cut1 izetaenv) zeta ixienv xi in
	try

	  (* Try to invert $b$ through $\xi$. This instance of [invert] is
	     told that it lives within a flexible context, so it will not
	     create any fresh meta-variables or equations. This is important
	     for termination; if $\zeta$ is not a pattern substitution, we do
	     not want to make the unification problem any larger.

	     We do not normalize [b], because our assumption regarding $\zeta$
	     guarantees that it is a weak head normal form already. *)

	  let bxim = invert false strict ixienv xi b in

	  (* If we're still here, then [invert] succeeded, so $b$ is in the
	     image of $\xi$, so $\clo{b}{\xi^{-1}}$ exists, and the goal can
	     be written $\clo{b}{\xi^{-1}}.(\zeta\circ\xi^{-1})$. *)

	  KEnv.mimic1 izetaenv izetabarxienv,
	  protect zetabarxi izetaenv,
	  if (b === bxim) & (zeta == zetabarxizetaxim) then zeta0
	  else
	    Cons(bxim, zetabarxizetaxim)

	with NoSolution when rigid & (is_variable b) ->

	  (* The above call to [invert] failed, so [b] is not in the image of
	     $\xi$, or its inverse cannot be expressed.

	     If [b] is a variable, then we know [b] is not in the image of
	     $\xi$, otherwise its inverse would be expressible -- it would be
	     a variable as well. If, furthermore, the context is rigid, then
	     we know that the meta-variable $Y$ (see [invert]) must not refer
	     to the variable $1$. We enforce this restriction by appending
	     $\shift{}$ to the weakening substitution.

	     This special case is important, because it covers the case where
	     all equations are within the decidable ``pattern''
	     fragment. Indeed, in that case, [rigid] must be set, and
	     $\zeta_0$ must be a pattern substitution, which guarantees that
	     [b] is a variable.

	     The outer kind environment of $\zeta\mid\xi$ is $\zeta$'s inner
	     kind environment. *)

	  izetabarxienv,
	  compose zetabarxi shift1 izetaenv,
	  zetabarxizetaxim

      )
    | Shift i, Cons (a, xi) -> (

	(* We are now trying to compute $\shift{i}\circ(a.\xi)^{-1}$. If $a$
	   lies within the image of $\shift{i}$, then we expand $\shift{i}$
	   into $(i+1).\shift{i+1}$, which must eventually bring us to the
	   other case, namely the case where $a$ lies outside of the image of
	   $\shift{i}$. Then, we can disregard $a$ and continue by computing
	   $\shift{i}\circ\xi^{-1}$, provided we append $\shift{}$ to the
	   result to account for the offset.

	   Note that $\zeta_0$ and $\xi_0$'s outer kind environment is that of
	   [a]. Note also that $(\zeta\mid\xi)\circ\zeta\circ\xi^{-1}$'s outer
	   kind environment is $\xi$'s inner kind environment. *)

	try

	  let _ = Index.invert (index a) i in
	  (* [invert] suceeded, so [a] lies within the image of $\shift{i}$. *)
	  let i1 = Index.increment i in
	  bar rigid strict izetaenv (Cons (_variable i1 (environment a), Shift i1)) ixienv xi0

	with Index.Invert ->

	  let ishiftibarxienv, shiftibarxi, shiftibarxishiftixim =
	    bar rigid strict izetaenv zeta0 (KEnv.cut1 ixienv) xi in
	  ishiftibarxienv, shiftibarxi, compose shiftibarxishiftixim shift1 ixienv

      )

    (* If $i\geq j$ holds, then $\shift{i}\circ(\shift{j})^{-1}$ is naturally
       $\shift{i-j}$. Otherwise, the image of $\shift{i}$ is larger than that
       of $\shift{j}$, so a non-trivial pruning substitution, namely
       $\shift{j-i}$, must be used, and the composition modulo pruning is the
       identity. In the latter case, however, we require the context to be
       rigid, because we are not allowed to return a non-trivial weakening
       substitution otherwise. This is the third cause of errors during
       inversion: [bar] fails if $\xi_0$ is a ``greater shift'' than $\zeta_0$
       and the context isn't rigid.

       Note that if $\zeta\mid\xi$ is the identity, then its inner and outer
       kind environments coincide, so both are $\zeta$'s inner
       environment. If, on the other hand,
       $(\zeta\mid\xi)\circ\zeta\circ\xi^{-1}$ is the identity, then
       $\zeta\mid\xi$'s inner kind environment is $\xi$'s inner
       environment. *)

    | Shift i, Shift j ->
	assert (Index.comparable i j);
	if Index.less j i then
	  izetaenv, id, Shift (Index.sub i j)
	else if rigid then
	  if Index.is_null i then
	    ixienv, xi0, zeta0 (* Optimization of the next case. *)
	  else
	    ixienv, Shift (Index.sub j i), id
	else
	  raise NoSolution

  (* This is the body of the wrapper function [invert]. The root context is
     rigid and non-strict, so the parameters [rigid] and [strict] are
     initially [true] and [false]. *)

  in
  invert true false kenv xi a

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Unification} *)

(* This useful pattern substitution is predefined for convenience. *)

let swap12 kenv =
  Cons (_variable Index.two kenv, Cons (_variable Index.one kenv, Shift Index.two))

(* [unify a b] attempts to solve the equation $a = b$. It may unify existing
   terms, create new terms, and schedule new equations. (By unifying existing
   terms, it may cause some terms to cease being in (weak head) normal form.)
   It may also choose to suspend some equations which it cannot solve
   immediately. If [unify] manages to make any progress at all towards solving
   the equation $a = b$, then it sets the transient flag [progress]. Otherwise
   (that is, if [unify] does nothing but suspend the equation), the flag
   remains unchanged.

   [NoSolution] is raised if an inconsistent equation is found. [Suspend] can
   be viewed as an internal exception that is never raised outside of
   [unify].

   [unify_shift a i b] attempts to solve the equation
   $\clo{a}{\shift{i}}=b$. In order to gain some efficiency, it removes a
   ``shift'' substitution which appears on both sides, without reducing the
   terms to weak head normal form. Indeed, reduction would push the
   substitution inside, which is costly. Also, this may allow some equations
   to become of lower rank, which will avoid duplication.

   [unify_whnf a b] attempts to solve the equation $a = b$, assuming [a]
   and [b] are weak head normal forms. *)

let progress =
  ref false

let rec unify a b =

  (* Whenever we unify two terms, we also unify their kind environments.
     This step is \emph{not} required for correctness or for consistency
     of the name-based output -- it is in fact optional. Its effect is to
     propagate names more aggressively, thus increasing the likeliness
     that we come up with good names for type variables, rather than
     generating arbitrary ones. *)

  KEnv.unify (environment a) (environment b);

  (* Proceed. *)

  unify_shift a Index.null b

and unify_shift a i b =
  match struc a, struc b with

  (* If either side exhibits a ``shift'' substitution, remove it. If it
     appears on the left side, it adds to the accumulator [i]. If it appears
     on the right side, it is subtracted from the accumulator, and this is
     where the optimization lies. If the accumulator would become negative,
     then the sides are exchanged in the recursive call. *)

  | Closure (a1, Shift j), _ when a =!= a1 ->
      unify_shift a1 (Index.add i j) b
  | _, Closure (b1, Shift j) when b =!= b1 ->
      if Index.less i j then
	unify_shift b1 (Index.sub j i) a
      else
	unify_shift a (Index.sub i j) b1

  (* If both sides are weak head normal forms and there is no accumulator,
     we may call [unify_whnf] directly, without calling [normalize]. This
     small optimization helps recoup the cost of examining [a] and [b]'s
     structure. *)

  | (Variable _ | Abstraction _ | RigidApplication _ | RowCons _ | RowUniform _), 
    (Variable _ | Abstraction _ | RigidApplication _ | RowCons _ | RowUniform _) when Index.is_null i ->
      unify_whnf a b

  (* In the general case, normalization must be performed. *)

  | _, _ ->
      unify_whnf (normalize (closure a (Shift i) (environment b))) (normalize b)

and unify_whnf a b =
  simplify normalize a;
  simplify normalize b;

  if a =!= b then (* speed optimization *) try

  (* In (almost) all cases below, if we are able to successfully
     decompose the equation into sub-equations, then we make [a] and
     [b] aliases for one another. This increases sharing (decreasing
     memory usage) and allows the equation $a = b$ to be dismissed
     immediately if it is encountered again. We do so \emph{after}
     dealing with the sub-equations, because (i) this optimization
     cannot affect them, given that terms are non-recursive and (ii)
     if an error occurs while solving one of the sub-equations, then
     we must print [a] and [b], so of course they mustn't be aliases
     at this point. *)

    begin
      match struc a, struc b, a, b with

      | Application _, _, _, _
      | _, Application _, _, _ ->
	  assert false

      (* We begin with the rigid-rigid cases. *)

      | RowCons (label1, hd1, tl1), RowCons (label2, hd2, tl2), _, _ ->

	  let c = compare label1 label2 in (* row label comparison *)
	  if c = 0 then begin

	    (* The labels coincide. This is the cheapest case. *)

	    unify hd1 hd2;
	    unify tl1 tl2;
	    union a b

	  end
	  else begin

	    (* Impose a common structure on both rows. We need to create an
	       auxiliary row variable, as well as two auxiliary row
	       terms. *)

	    let tl = metavariable (environment tl1) in
	    unify_whnf (normalize tl1) (row_cons label2 hd2 tl);
	    unify_whnf (normalize tl2) (row_cons label1 hd1 tl);

	    (* The labels do not coincide. We have a choice as to the
	       direction of the link to be created. We choose to view the
	       node with the smallest label (according to an arbitrary,
	       fixed total order on labels) as canonical. This strategy will
	       tend to organize rows in increasing order, which will make
	       the cheap case above more frequent, thus allowing rows to be
	       unified in quasi-linear time. *)

	    if c < 0 then union b a else union a b

	  end

      | RowCons (label1, hd1, tl1), RowUniform hd2, a, b
      | RowUniform hd2, RowCons (label1, hd1, tl1), b, a ->

	  (* Force the two rows to have the same contents. This involves
	     unifying [hd1] and [hd2], and unifying [tl1] with a uniform row
	     whose contents is [hd2]. *)

	  unify hd1 hd2;
	  unify_whnf (normalize tl1) (row_uniform hd2);

	  (* Keep the uniform representation, which is more compact. *)

	  union a b

      | RowUniform hd1, RowUniform hd2, _, _ ->

	  unify hd1 hd2;
	  union a b

      | RigidApplication (a1, p, a2), RigidApplication (b1, q, b2), _, _ ->

	  (* If the equation has solutions, then both applications must have
	     the same head variable. Furthermore, because the equation must
	     be well-kinded, both sequences of arguments must be equal;
	     indeed, arguments are known to be ordered by channel name, so
	     commutation between channels is not an issue. As a result, the
	     equation can be decomposed piece-wise. Note that, after
	     checking $[a1] = [b1]$, the head variables are known to be
	     equal, so $[p] = [q]$ is guaranteed. Because [a2] and [b2] are
	     not necessarily in weak head normal form, we normalize them
	     before examining them.*)

	  unify_whnf a1 b1;
	  assert (compare p q = 0); (* ill-kinded equation otherwise *)
	  unify a2 b2;
	  union a b

      | Abstraction _, (RowCons _ | RowUniform _), _, _
      | (RowCons _ | RowUniform _), Abstraction _, _, _ ->

	  (* Ill-kinded equation. *)

	  assert false

      | Abstraction (p, a1), Abstraction (q, b1), _, _ ->

	  let c = compare p q in (* channel comparison *)
	  if c = 0 then begin

	    (* The channels coincide. This is the cheapest case. *)

	    unify a1 b1;
	    union a b

	  end
	  else begin

	    (* Impose a common structure on both terms. This reflects the
	       fact that abstractions on distinct channels commute. The
	       left-hand side becomes an abstraction of the form
	       $\abs{p}{\abs{q}{X}}$, while the right-hand side becomes
	       $\abs{q}{\abs{p}{X[2.1.\shift{2}]}}$. The substitution
	       reflects the fact that $X$ receives its parameters in a
	       different order. *)

	    let kenvp = environment a1
	    and kenvq = environment b1 in
	    let kenvpq = KEnv.mimic1 kenvq kenvp
	    and kenvqp = KEnv.mimic1 kenvp kenvq in
	    let x = metavariable kenvpq in
	    unify_whnf (normalize a1) (abstraction q x);
	    unify_whnf (normalize b1) (abstraction p (_closure x (swap12 kenvqp) kenvqp));

	    (* The channels do not coincide. As in the case of rows (see
	       above), we choose to view the node with the smallest channel
	       (according to an arbitrary, fixed total order on channels) as
	       canonical. *)

	    if c < 0 then union b a else union a b

	  end

      | Abstraction (p, a1), _, a, b
      | _, Abstraction (p, a1), b, a ->

	  (* One term is a $\lambda$-abstraction, but the other isn't. We
	     $\eta$-expand the latter, which brings us back to the previous
	     case. *)

	  let kenv1 = environment a1 in
	  unify a1 (application (_closure b shift1 kenv1) p (_variable Index.one kenv1))
	    (* TEMPORARY doit-on/peut-on faire union a b ou union b a? *)

      | Variable i1, Variable i2, _, _ ->

	  assert (Index.comparable i1 i2);
	  if not (Index.equal i1 i2) then
	    raise NoSolution;
	  union a b

      | Variable _, RigidApplication _, _, _
      | RigidApplication _, Variable _, _, _ ->

	  (* A ``rigid'' application must have a variable at its
	     head. Furthermore, matching a variable against an application
	     of a variable must be ill-kinded or insolvable. We currently
	     raise [NoSolution] in both cases, though it would be nice to
	     distinguish. *)

	  raise NoSolution

      | (Variable _ | RigidApplication _), (RowCons _ | RowUniform _), _, _
      | (RowCons _ | RowUniform _), (Variable _ | RigidApplication _), _, _ ->

	  (* We are matching a variable against a row. Although one may
	     think that this might succeed (because the variable, even
	     though it is unknown, must be a row too), it must indeed fail,
	     because expansion of the variable is made impossible by the
	     fact that we would not be able to \emph{name} its
	     components. TEMPORARY documenter plus precisement dans quels
	     cas (quel cadre?) ceci est complet *)

	  raise NoSolution

      (* The following case is flex-rigid: a meta-variable or a closure
	 appears against a rigid term. *)

      | Closure (a1, s1), (Variable _ | RigidApplication _
					    | RowCons _ | RowUniform _), a, b
      | (Variable _ | RigidApplication _ | RowCons _
				      | RowUniform _), Closure (a1, s1), b, a ->

	  if is_pattern_subst s1 then begin
	    unify_invert a1 s1 b;
	    if a =!= a1 then
	      union a b
	  end
	  else
	    raise Suspend

      (* This case is flex-flex, with a single meta-variable appearing on
	 both sides of the equation (possibly within a closure). *)

      | Closure (a1, xi), Closure (b1, zeta), _, _ when a1 === b1 ->

	  if (is_pattern_subst xi) & (is_pattern_subst zeta) then begin
	    union a b; (* TEMPORARY why not union b a, or nothing at all? *)

	    (* The equation is of the form $\clo{X}{\xi} = \clo{X}{\zeta}$,
	       where $\xi$ and $\zeta$ are pattern substitutions. This
	       admits a solution if and only if $X$ mentions only those De
	       Bruijn indices which are mapped to the same value by $\xi$
	       and $\zeta$. In other words, the equation can be simplified
	       to $X=\clo{Y}{\xi\cap\zeta}$, where $Y$ is a fresh
	       meta-variable. Of course, if $\xi\cap\zeta$ is the identity,
	       then the equation is a tautology, and can be dropped. *)

	    let kenv1 =
	      environment a1 in

	    let ixicapzetaenv, xicapzeta =
	      cap kenv1 xi zeta in

	    let xicapzeta =
	      normalize_subst normalize xicapzeta in

	    if not (is_id xicapzeta) then
	      union a1 (_closure (metavariable ixicapzetaenv) xicapzeta kenv1)

	  end
	  else

	    (* One of $\xi$ or $\zeta$ isn't a pattern substitution. Suspend
	       the equation. *)

	    raise Suspend

      (* The last case is flex-flex, with a distinct meta-variable on each
	 side (possibly within a closure). *)

      | Closure (a1, s1), Closure (b2, s2), _, _ ->

	  (* In the simple sub-cases where one side carries the identity
	     substitution, ``invert'' that one. (This is important not only
	     for speed, but also contributes to guaranteeing termination.)
	     Inversion is trivial; only the occur-check is performed. *)

	  if is_id s1 then
	    unify_invert a1 s1 b
	  else if is_id s2 then
	    unify_invert b2 s2 a

	    (* Otherwise, both substitutions are non-trivial. If one side
	       involves a pattern substitution, invert it. Otherwise,
	       suspend the equation. *)

	  else if is_pattern_subst s1 then
	    unify_invert a1 s1 b
	  else if is_pattern_subst s2 then
	    unify_invert b2 s2 a
	  else
	    raise Suspend

    (* If we reach this point, then we haven't raised [Suspend], so we have
       made some progress. If, on the other hand, we have raised [Suspend],
       then let us now move the equation $a = b$ to the queue of suspended
       equations. *)

    end;
    progress := true

  with Suspend ->
    suspend (a, b)

(* The equation is $\clo{X}{\xi}=b$, where $\xi$ is a pattern
   substitution. We attempt to equate $X$ with $\clo{b}{\xi^{-1}}$, that
   is, with the inverse image of $b$ through $\xi$. At the same time, we
   check that this term contains no occurrence of $X$.

   This computation may terminate in one of two ways. First, [invert] may
   raise an exception, namely [NoSolution] or [Suspend]. Second, [invert]
   may return a result; in that case, we have gained some information
   about $X$. *)

and unify_invert x xi b =
  union x (invert x (environment x) xi b)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Constraint solving} *)

(* We now define the main entry point to the solver, namely the function
   [solve]. We have all tools at hand now; there only remains to set up a
   toplevel loop, which repeatedly attempts to solve pending equations.

   We use two waiting queues. The first queue contains arbitrary pending
   equations. The second queue contains suspended equations, of the form
   $X[s]=a$, where $s$ is not a pattern substitution. (At least, they were of
   that form when they were suspended.) Such an equation need not be
   re-examined until new information about $X$ or $s$ becomes available.

   Currently, we use a simple mechanism, which consists in examining
   equations in the main queue as long as there are any, then moving all
   suspended equations back to the main queue, and starting over. Of course,
   we must avoid doing this \emph{ad infinitum}; the flag [progress]
   is used to prevent that. This is perhaps slightly inefficient, but
   pleasant. *)

(* TEMPORARY mesurer l'inefficacite causee par ce mecanisme naif, i.e. le
   temps passe a examiner des contraintes pour les re-suspendre aussitot.
   s'il est trop important, songer a un mecanisme plus efficace. *)

(* TEMPORARY might want to try solving newly scheduled equations
   immediately, so as to trigger an error while we're still at the
   correct program point *)

exception Inconsistency of equation

let solve () =
  let rec work () =
    try
      let (a, b) as eq = FQueue.take scheduled in
      begin
	try
	  unify a b
	with NoSolution ->
	  schedule eq;
	  FQueue.transfer suspended scheduled;
	  progress := false;
	  raise (Inconsistency eq)
      end;
      work()
    with FQueue.Empty ->
      FQueue.transfer suspended scheduled;
      if !progress then begin
	progress := false;
	work()
      end

  in
  assert ((FQueue.length suspended = 0) & (not !progress));
  work();
  assert ((FQueue.length suspended = 0) & (not !progress))

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Generalization and instantiation} *)

(* We now turn to ML-style generalization and instantiation. *)

(* A \emph{scheme} is either simply a term $t$, or a term $t$ that is
   quantified over a gap variable $\gamma$, a set of meta-variables $\bar{X}$
   and constrained by a set of equations $C$; we write
   $\forall\gamma.\forall\bar{X}[C].t$. The term $t$ is called the scheme's
   \emph{entry point}.

   The former case could, if desired, be encoded in terms of the latter, but
   it's perhaps simpler to keep it separate.

   The gap variable $\gamma$ is not explicit represented in the machine
   representation of a scheme; it is simply the most recently introduced gap
   variable, i.e. the first one encountered when examining the entry point or
   the terms that form the scheme's equations. Similarly, the meta-variables
   $\bar{X}$ are exactly the meta-variables encountered when examining these
   terms and stopping at the first gap variable. So, gap variables do indeed
   mark the ``gap'' between the polymorphic and monomorphic portions of the
   type structure.

   To sum up, a scheme is represented, in machine form, either as a term, or
   as a term together with a set of equations. In the latter case (and in that
   case only), the first gap variable reachable from these terms has special
   significance. *)

type scheme =
  | Mono of term
  | Poly of (term * term) list * term

(* [inject a] turns the term [a] into a scheme, without introducing any
   universal quantification. *)

let inject a =
  Mono a

(* [instantiate kenv offset a] returns a copy of the term [a], stopping at
   occurrences of the first gap variable and instantiating them with the index
   [offset]. [kenv] is the kind environment of the instance that is being
   created.

   [instantiate] performs a depth-first traversal of [a]. The transient field
   [copy] is used to establish a constant-time mapping between every node and
   its copy. *)

let instantiate kenv offset a =

  let rec instantiate node =
    let desc = UnionFind.find node in
    match desc.copy with

    (* If a copy has been created already for this node, return it. *)

    | Some node' ->
	node'

    (* Otherwise, create a new node, update the mapping, and create the node's
       descriptor. Note that the mapping must be updated before calling
       [instantiate] recursively, in order to preserve sharing. (It is also
       necessary to guarantee termination, because our [Closure] nodes may be
       -- artificially -- self-referential.) *)

    | None ->
	create (fun node' ->
	  desc.copy <- Some node';
	  instantiate_term desc.structure
	) (KEnv.instantiate kenv offset desc.environment)

  and instantiate_subst = function
    | (Shift i) as s ->

	(* If [i] has non-zero height, then this is an occurrence of (at
	   least) one gap variable. Then, we return a copy of the substitution
	   [s] where this variable is replaced with [offset], together with a
	   [true] signal, so as to let our caller know that we have hit the
	   gap. *)

	if Index.height i > 0
	then true, Shift (Index.instantiate offset i)
	else false, s
    | Cons (a, s) ->
	let signal, s = instantiate_subst s in
	signal, Cons (instantiate a, s)

  and instantiate_term = function
    | (Variable i) as structure ->
	if Index.height i > 0
	then Variable (Index.instantiate offset i)
	else structure
    | Abstraction (p, a1) ->
	Abstraction (p, instantiate a1)
    | RowUniform a1 ->
	RowUniform (instantiate a1)
    | Application (a1, p, a2) ->
	Application (instantiate a1, p, instantiate a2)
    | RigidApplication (a1, p, a2) ->
	RigidApplication (instantiate a1, p, instantiate a2)
    | RowCons (label, a1, a2) ->
	RowCons (label, instantiate a1, instantiate a2)
    | Closure (a1, s) ->

	(* Here, we first copy the substitution [s]. In doing so, we find out
	   whether [s] contains the gap variable we're looking for. If so, we
	   should stop copying, so we return [a1] unchanged. If not, we must
	   proceed with the copying process, so we return [instantiate a1]. *)

	let finished, s = instantiate_subst s in
	Closure ((if finished then a1 else instantiate a1), s)

  in
  instantiate a

(* [restore a] performs a depth-first traversal of the term [a]. At every
   sub-term, the transient field [copy] is restored to its default value.
   This function is used to undo [instantiate]'s side effect on the original
   term. *)

let rec restore node =
  let desc = UnionFind.find node in
  match desc.copy with
  | None ->
      ()
  | Some _ ->
      desc.copy <- None;
      KEnv.restore desc.environment;
      restore_term desc.structure

and restore_subst = function
  | Shift _ ->
      ()
  | Cons (a, s) ->
      restore a;
      restore_subst s

and restore_term = function
  | Variable _ ->
      ()
  | Abstraction (_, a1)
  | RowUniform a1 ->
      restore a1
  | Application (a1, _, a2)
  | RigidApplication (a1, _, a2)
  | RowCons (_, a1, a2) ->
      restore a1;
      restore a2
  | Closure (a1, s) ->
      restore a1;
      restore_subst s

(* This auxiliary function instantiates a list of equations (which must
   be part of a scheme). *)

let instantiate_equations kenv offset equations =
  List.iter (fun (a, b) ->
    schedule (instantiate kenv offset a, instantiate kenv offset b)
  ) equations;
  List.iter (fun (a, b) ->
    restore a; restore b
  ) equations

(* [instance kenv offset scheme] produces a fresh instance of the scheme
   [scheme], instantiating its universally quantified gap variable with
   the index [offset]. In doing so, it may create fresh meta-variables
   and schedule new equations. These new equations are added in the kind
   environment [kenv]. *)

let instance kenv offset = function
  | Mono a ->
      closure a (Shift (Index.inject offset)) kenv
  | Poly (equations, entry_point) ->
      let instance = instantiate kenv offset entry_point in
      instantiate_equations kenv offset equations;
      restore entry_point;
      instance

(* [generalize kenv action map] first runs [action], which is allowed to
   generate fresh meta-variables and schedule new equations (at a higher
   rank). Upon exit, [action] must return a set of terms that are of interest
   to it. (The particular data structure used to represent this set is left
   unspecified; we only require that a [map] function be available for it.)
   Then, [generalize] turns each of these terms into a scheme, where the
   freshly created meta-variables are universally quantified, subject to the
   new equations, and returns a set of schemes. The equations are simplified
   (as far as possible) so as to produce more compact schemes. [kenv] is the
   kind environment \emph{outside} this [let] node.

   We require that the client take at least one instance of one of the
   schemes. Otherwise, the equations produced in the scope of this call
   to [generalize] could be lost. *)

let generalize kenv action map =

  (* Run the [action] function. The function returns a set of (named) entry
     points, that is, terms of interest. Furthermore, a number of equations
     may be generated at this rank, which we first try to solve, then (if any
     remain) isolate. *)

  let entry_points = action () in
  solve();
  let equations = isolate (KEnv.rank kenv + 1) in

  (* Create one scheme per entry point. All of these schemes share
     the same structure; they differ only in their entry points. When taking
     an instance of such a scheme, only the portion of the structure that
     is reachable from that particular scheme's entry point (and
     equations) will be copied.

     Note that no copying is required here. As in the case of ML, the type
     structure of highest rank simply \emph{becomes} the scheme. *)

  let schemes = map (fun entry_point ->
    Poly (equations, entry_point)
  ) entry_points in

  (* Here, we require that our client take at least one instance of one of the
     schemes returned to him.

     Without this assumption, we would need to keep a copy of the equations
     around, instantiating the freshest gap variable with 0 -- which amounts
     to immediately taking an instance of one of the schemes thus
     created. Doing so would be necessary to ensure that these equations have
     a solution.

  instantiate_equations kenv (0, 0) equations; *)

  (* Return the schemes. *)

  schemes

(* TEMPORARY may want to solve only equations of current rank at generalization, not all equations *)

