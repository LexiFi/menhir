(*********************************************************************************************************************)
(*                                                                                                                   *)
(*                                                   Wallace                                                         *)
(*                                                                                                                   *)
(*                              François Pottier, projet Cristal, INRIA Rocquencourt                                 *)
(*                                                                                                                   *)
(*   Copyright 2000 Institut National de Recherche en Informatique et Automatique. Distributed only by permission.   *)
(*                                                                                                                   *)
(*********************************************************************************************************************)
(* $Header: /home/pauillac/formel1/fpottier/cvs/wallace/print.ml,v 1.9 2000/02/11 16:15:50 fpottier Exp $ *)

(* This is Wallace's abstract term pretty-printer. It is parameterized by two printing modules. The first one, the
   ``generic printer'', knows about the particular output format which is being used (e.g. plain text, \TeX, HTML,
   etc.), and nothing else. Several such modules are part of Wallace. The second one, the ``token printer'', also
   knows about the current output format. Furthermore, it knows about the set of tokens required by the current
   application, and is able to print them. No such module is bundled with Wallace; rather, one should be written for
   each application, since it depends on the type language.

   This module is also parameterized by a ground signature, as usual. It uses the ground signature's [Print]
   sub-module, which doesn't know about the output format (which is why abstract tokens must be used), but does know
   about the structure of terms.

   The pretty-printer provides logic for performing on-the-fly variable and term substitutions, for naming variables,
   for printing constraint graphs, and for glueing everything together. *)

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* This is the interface expected of a generic printer. It must provide a set of output primitives. *)

module type GenericPrinter = sig

  (* [variable sign n] prints a type variable, given its [sign] and its unique number [n]. Numbers range up starting
     from 1. *)

  type sign

  val variable: sign -> int -> unit

  (* [box p] invokes the printing function [p] and lets it print within a box (if the notion of box makes sense for
     this printer). *)

  val box: (unit -> unit) -> unit

  (* [parentheses p] invokes the printing function [p] and lets it print within parentheses within a box (if the
     notion of box makes sense for this printer). *)

  val parentheses: (unit -> unit) -> unit

  (* [angle p] invokes the printing function [p] and lets it print within angle brackets within a box (if the
     notion of box makes sense for this printer). *)

  val angle: (unit -> unit) -> unit

  (* [comma] (resp. [colon], [semi]) prints a comma (resp. colon, semicolon), followed by a breakable space.  *)

  val comma: unit -> unit
  val colon: unit -> unit
  val semi: unit -> unit

  (* [less] prints a $\leq$ symbol, surroundered by breakable spaces. *)

  val less: unit -> unit

  (* [conditional] prints a conditional constraint. Its condition and conclusion are supplied as arguments. *)

  val conditional: (unit -> unit) -> (unit -> unit) -> unit

  (* [newline] forces an end-of-line. *)

  val newline: unit -> unit

  (* [label] prints a row label. *)

  val label: string -> unit

  (* [urow] prints a $\urow$ constructor. *)

  val urow: unit -> unit

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* This is the interface expected of a token printer. It must provide a way of printing tokens. *)

module type TokenPrinter = sig

  (* Tokens are abstract values which stand for concrete elements such as commas, colons, arrows, etc. *)

  type token

  (* [token] prints a token. *)

  val token: token -> unit

end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* Here comes the core of the abstract pretty-printer. *)

module type S = sig

  type scheme

  (* [scheme substitute psigma sigma] prints a type scheme. The type scheme is specified by the two
     arguments [psigma] and [sigma]. Both are abstract type schemes, i.e. functions which allow iterating over the
     scheme's entry points. However, [psigma] is allowed to perform (printing) side-effects between two entry points,
     while [sigma] isn't. After applying [psigma] to display the type scheme's body, [scheme] prints its constraint
     graph.

     [substitute] is a Boolean flag, and tells whether on-the-fly variable substitution should be performed. If so,
     the type scheme must have been run through garbage collection. The pretty-printer will then transparently
     replace positive (resp. negative) variables with their unique lower (resp. upper) bound, if they have one,
     so as to make the type scheme more readable. *)

  val scheme: bool -> scheme -> scheme -> unit
    
end

module Make
    (G : Ground.Signature)
    (LeafSet : LeafSet.S)
    (CondSet : CondSet.S)
    (RowMap : RowMap.S)
    (Label : Label.S with type t = RowMap.key)
    (Core : Core.S with type 'a row_map = 'a RowMap.t
                    and type symbol = G.Symbol.t
	            and type kind = G.Kind.t
                    and type 'a preterm = 'a G.Term.t
		    and type 'a set1 = 'a LeafSet.t
		    and type 'a set2 = 'a CondSet.t)
    (GenericPrinter : GenericPrinter with type sign = Core.sign)
    (TokenPrinter : TokenPrinter with type token = G.Print.token)

= struct

  open Core
    
(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Substituting variables}

   Our pretty-printer has the ability of silently replacing some variables with their unique bound, if they have one.
   This yields more readable results, and is almost transparent to the rest of the library. (A side-effect is that the
   signs carried by some variables are modified.) Most of the necessary code is contained in the following
   sub-module. *)

  module LeafMap = Map.Make (struct
    type t = leaf
    let compare = clv
  end)

  module Mark = struct

    let variables =
      ref LeafMap.empty
    let terms =
      ref LeafMap.empty

    (* [reset] clears all substitution marks. *)

    let reset () =
      variables := LeafMap.empty;
      terms := LeafMap.empty

    (* [with_variable lf lf'] marks [lf] for replacement with [lf']. [lf] and [lf'] must be distinct variable
       leaves. [lf'] is made bipolar. *)

    let with_variable lf lf' =
      variables := LeafMap.add lf lf' !variables;
      lf'.sign <- Bipolar

    (* [with_term lf term] marks [lf] for replacement with a [term]. A check against cyclic abbreviations is performed,
       since such abbreviations would cause the pretty-printer to loop. If the check fails, the call has no effect.
       [lf] must be a variable leaf. [term] may contain row leaves. *)

    exception Occurs

    let with_term lf term =

      let rec check_variable lf' =
	if lf == lf' then
	  raise Occurs;
	try
	  check_term (LeafMap.find lf' !terms)
	with Not_found ->
	  try
	    check_leaf (LeafMap.find lf' !variables)
	  with Not_found ->
	    ()

      and check_leaf lf' =
	match Row.link lf' with
	| VarLink _ ->
	    check_variable lf'
	| RowLink row ->
	    Row.iter check_variable row

      and check_term term =
	G.Term.iter check_leaf term in

      try
	check_term term;
	terms := LeafMap.add lf term !terms
      with Occurs ->
	()

      (* [repr ift ifv lf] determines whether [lf] has been marked for replacement with a term or with a variable
	 (which may be [lf] itself). It then passes the result to [ift] or [ifv], as appropriate. [lf] must be a
	 variable leaf. *)

      let repr ift ifv lf =
	try
	  ift (LeafMap.find lf !terms)
	with Not_found ->
	  try
	    ifv (LeafMap.find lf !variables)
	  with Not_found ->
	    ifv lf

      (* [is_clean lf] tells whether [lf] is clean, i.e. has not been marked for replacement with some term or some
	 variable other than itself. [lf] must be a variable leaf. *)

      let is_clean lf =
	try
	  let _ = LeafMap.find lf !terms in
	  false
	with Not_found ->
	  try
	    let _ = LeafMap.find lf !variables in
	    false
	  with Not_found ->
	    true

  end

  (* The mechanism which allows marking variables is now available. There remains to explain how marks should be set
     up at the beginning of a pretty-printing session.

     [mark] expects its [scheme] argument to have been run through garbage collection. It checks whether each
     positive (resp. negative) variable has a unique lower (resp. upper) bound, and if so, replaces the variable
     with its bound.

     The criterion used to check whether a variable has a ``unique'' bound is incomplete. If a variable has several
     bounds, but its constructed bound is stronger than its other (variable) bounds, then the constructed bound will
     not be considered as the ``unique'' bound, even though this would be valid. Checking for this case would require
     performing costly entailment checks, for which no complete algorithm is known anyway. Here is an example of this
     situation: imagine $\texttt{int}, \alpha^-\leq\beta$ and $\alpha\leq\beta, \texttt{int}$. Here, \texttt{int} will
     not be recognized as $\beta$'s (resp. $\alpha$'s) unique lower (resp. upper) bound, even though this would be
     correct. *)

  let mark scheme =
    Traverse.scheme (fun lf ->
      match lf.link with
      |	VarLink v -> (

	  match lf.sign with
	  | Positive ->

	      (* If the variable carries no links to other variables, then its unique bound is its constructed
		 bound. *)

	      if LeafSet.is_empty v.loset then
		Mark.with_term lf v.lo

	      (* If the variable carries a single link, and if its constructed bound is trivial, then its unique bound
		 is the variable at the other end of the link. Here, [lf] and [lf'] are necessarily distinct, because
		 the type scheme has been run through garbage collection. *)

	      else begin
		try
		  let lf' = LeafSet.is_singleton v.loset in
		  if v.lo = G.Term.bottom v.lo then
		    Mark.with_variable lf lf'
		with LeafSet.NotSingleton ->
		  ()
	      end

	  | Negative ->

	      (* Variables carrying conditional constraints can never be hidden. *)	

	      if CondSet.is_empty v.conditionals then

		if LeafSet.is_empty v.hiset then
		  Mark.with_term lf v.hi

		else begin
		  try
		    let lf' = LeafSet.is_singleton v.hiset in
		    if v.hi = G.Term.top v.hi then
		      Mark.with_variable lf lf'
		  with LeafSet.NotSingleton ->
		    ()
		end

	  | Bipolar ->
	      ()
	  | Neutral ->
	      assert false

      )
      | RowLink _ ->
	  ()

    ) scheme

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Giving names to variables}

   ``Names'' are, in fact, unique integers numbered from 1 and up. The variables' final appearance depends on
   the chosen printer. *)

  module Name = struct

    let counter =
      ref 0

    let map =
      ref LeafMap.empty

    (* [reset] clears the association table. The next call to [variable] shall yield 1. *)

    let reset () =
      counter := 0;
      map := LeafMap.empty

    (* [variable lf] returns the number assigned to [lf], if one already exists; otherwise, it generates a new unique
       number, assigns it to [lf], and returns it. *)

    let variable lf =
      try
	LeafMap.find lf !map
      with Not_found ->
	incr counter;
	map := LeafMap.add lf !counter !map;
	!counter

  end

(*i --------------------------------------------------------------------------------------------------------------- i*)
(* \mysection{Detecting row variables} *)

let is_row_leaf lf =
  match lf.sort with
  | Regular ->
      false
  | Row _ ->
      true

let is_row_term term =
  let is_row = ref false in
  G.Sort.iter (fun leaf_must_be_row lf ->
    if (not leaf_must_be_row) & (is_row_leaf lf) then
      is_row := true
  ) term;
  !is_row

let contains_row lfset =
  LeafSet.exists is_row_leaf lfset

(*i --------------------------------------------------------------------------------------------------------------- i*)
(*s \mysection{Printing} *)

  (* Let us define how variables and terms should be printed. Most the necessary primitives are provided by our
     functor arguments, so this is very simple.

     Each of these printing functions expects the following arguments:
     \begin{itemize}
     \item a [shield] function, which expects a term and tells whether it should be parenthesized;
     \item a Boolean flag, which tells whether a row term ([true]) or a regular term ([false]) is expected;
     \item a component to be printed.
     \end{itemize} *)
 
  let rec variable shield is_row lf =

    (* If the variable has been equated with some term, then print that term. *)

    Mark.repr (term shield is_row) (fun lf ->

      (* If the variable is a regular variable, but a row variable was expected, print a $\urow$ constructor in
	 front of it. At the same time, perform some sanity checks. *)

      begin
	match lf.sort, is_row with
	| Regular, true ->
	    GenericPrinter.urow()
	| Regular, false
	| Row _, true ->
	    ()
	| Row _, false ->
	    assert false
      end;

      (* Print the variable itself. *)

      GenericPrinter.variable lf.sign (Name.variable lf)

    ) lf

  and leaf shield is_row lf =
    match Row.link lf with
    | VarLink _ ->
	variable shield is_row lf
    | RowLink row ->

	(* If the variable has been equated with a row, print the row, within angle brackets. The brackets will be
	   superfluous in some cases (e.g. if the row is an argument to a record constructor), but not all (e.g. if
	   the row is a regular term's leaf, or if it appears within a conditional constraint). We might fix this
	   later. TEMPORARY *)

	GenericPrinter.angle (fun () ->
	  RowMap.iter (fun label mf ->
	    GenericPrinter.label (Label.print label);
	    GenericPrinter.colon();
	    variable (fun _ -> false) false mf;
	    GenericPrinter.semi()
          ) row.entries;
	  variable (fun _ -> false) true row.remainder
        )

  and term shield is_row term =
    (if shield term then GenericPrinter.parentheses else GenericPrinter.box) (fun () ->

      (* If the term is expected to be a row term, we check whether it can be one. If not, we place a $\urow$
	 constructor in front of it. Otherwise, we do nothing special, which implicitly corresponds to pushing
	 the $\urow$ constructor into the term. *)
      
      let is_row = if is_row & (not (G.Term.expands term)) then begin
	GenericPrinter.urow();
	false
      end
      else
	is_row in

      G.Print.term TokenPrinter.token is_row leaf term
    )

  (* When called from the outside, the printing functions need not surround the term with parentheses. *)

  let leaf =
    leaf (fun _ -> false)

  let term =
    term (fun _ -> false)

  (* We may now define how type schemes are printed. *)

  type scheme = Core.scheme

  let scheme substitute pscheme scheme =
    
    (* If requested by the caller, prepare for on-the-fly substitutions. *)

    if substitute then
      mark scheme;

    (* Print the type scheme's entry points. The abstract function [pscheme] is allowed to perform side-effects, so
       as to print some matter between two consecutive entry points. We assume that the scheme's entry points are not
       rows. A similar assumption is made in [Core.Translate.expression]. *)

    pscheme (fun _ lf -> leaf false lf);

    (* Build a list of unmarked variables. Marked variables are ignored altogether, since they do not represent a
       visible node of the constraint graph.

       At the same time, force each variable to be given a name, and make sure the list is sorted with respect
       to names. This should yield more readable output. *)

    let list = ref [] in
    Traverse.scheme (fun lf ->
      match lf.link with
      |	VarLink v ->
	  if Mark.is_clean lf then
	    let _ = Name.variable lf in
	    list := (lf, v) :: !list
      |	RowLink _ ->
	  ()
    ) scheme;
    let list = List.rev !list in

    (* Print the constraint set. Each regular constraint has the form $L\leq v\leq U$, where $v$ is a variable, and
       $L$ and $U$ are sets of lower and upper bounds. In addition, [v] might carry an arbitrary number of
       conditional constraints. *)

    List.iter (fun (lf, v) ->
      assert (Mark.is_clean lf);

      (* We begin by determining whether this constraint involves regular variables or rows. If at least one row
	 is involved, be it the variable $v$ itself or one of its bounds, then the constraint is deemed to involve
	 rows. In that case, any term which is not a row term will be prefixed with a $\urow$ constructor. *)

      let is_row =
	(is_row_term v.lo) or (contains_row v.loset) or
	(is_row_term v.hi) or (contains_row v.hiset) in

      let set_to_clean_list =
	LeafSet.fold (fun lf list ->
	  if Mark.is_clean lf then
	    (lazy (leaf is_row lf)) :: list
	  else
	    list
	) in

      let lolist = set_to_clean_list v.loset
	  (if v.lo = G.Term.bottom v.lo then [] else [ lazy (term is_row v.lo) ])
      and hilist = set_to_clean_list v.hiset
	  (if v.hi = G.Term.top v.hi then [] else [ lazy (term is_row v.hi) ]) in

      let rec print_nonempty_list = function
	| [] ->
	    assert false
	| [ p ] ->
	    Lazy.force p
	| p :: rest ->
	    Lazy.force p;
	    GenericPrinter.comma();
	    print_nonempty_list rest in

      if (lolist <> []) or (hilist <> []) then begin
	GenericPrinter.box (fun () ->

	  if lolist <> [] then begin
	    print_nonempty_list (List.rev lolist);
	    GenericPrinter.less()
	  end;
	  leaf is_row lf;
	  if hilist <> [] then begin
	    GenericPrinter.less();
	    print_nonempty_list hilist
	  end

	);
	GenericPrinter.newline()
      end;

      CondSet.iter (fun (symbol, lf1, lf2) ->
	GenericPrinter.box (fun () ->
	  let is_row =
	    (is_row_leaf lf) or (is_row_leaf lf1) or (is_row_leaf lf2) in

	  GenericPrinter.conditional
	    (fun () ->
	      G.Print.symbol TokenPrinter.token symbol;
	      GenericPrinter.less();
	      leaf is_row lf)
	    (fun () ->
	      leaf is_row lf1;
	      GenericPrinter.less();
	      leaf is_row lf2)
	);
	GenericPrinter.newline()
      ) v.conditionals

      (* TEMPORARY add option to print links only once *)

    ) list;

    Mark.reset();
    Name.reset()
    
end

