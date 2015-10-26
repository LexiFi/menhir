 (* This module discovers information about the shape and content of the stack
    in each of the automaton's states. *)

open Grammar
module C = Conflict (* artificial dependency; ensures that [Conflict] runs first *)

(* ------------------------------------------------------------------------ *)
(* Compute a lower bound on the height of the stack at every state. At the
   same time, compute which symbols are held in this stack prefix. *)

(* In order to compute (a lower bound on) the height of the stack at a state
   [s], we examine the LR(0) items that compose [s]. For each item, if the
   bullet is at position [pos], then we can be assured that the height of the
   stack is at least [pos]. Thus, we compute the maximum of [pos] over all
   items (of which there is at least one). *)

(* The set of items that we use is not closed, but this does not matter; the
   items that would be added by the closure would not add any information
   regarding the height of the stack, since the bullet is at position 0 in
   these items. *)

(* Instead of computing just the stack height, we compute, in the same manner,
   which symbols are on the stack at a state [s]. This is an array of symbols
   whose length is the height of the stack at [s]. By convention, the top of
   the stack is the end of the array. *)

(* We first compute and tabulate this information at the level of the LR(0)
   automaton. *)

let stack_symbols : Lr0.node -> Symbol.t array =
  let dummy =
    Array.make 0 (Symbol.T Terminal.sharp)
  in
  Misc.tabulate Lr0.n (fun node ->
    Item.Set.fold (fun item accu ->
      let _prod, _nt, rhs, pos, _length = Item.def item in
      if pos > Array.length accu then Array.sub rhs 0 pos else accu
    ) (Lr0.items node) dummy
  )

(* Then, it is easy to extend it to the LR(1) automaton. *)

let stack_symbols (node : Lr1.node) : Symbol.t array =
  stack_symbols (Lr0.core (Lr1.state node))

let stack_height (node : Lr1.node) : int =
  Array.length (stack_symbols node)

(* ------------------------------------------------------------------------ *)
(* Above, we have computed a prefix of the stack at every state. We have
   computed the length of this prefix and the symbols that are held in
   this prefix of the stack. Now, compute which states may be held in this
   prefix. *)

(* In order to compute this information, we perform an analysis of the
   automaton, via a least fixed fixed point computation. *)

(* It is worth noting that it would be possible to use an analysis based on a
   least fixed point computation to discover at the same time the length of
   the stack prefix, the symbols that it contains, and the states that it may
   contain. This alternate approach, which was used until 2012/08/25, would
   lead us to discovering a richer invariant, that is, potentially longer
   prefixes. This extra information, however, was useless; computing it was a
   waste of time. Hence, as of 2012/08/25, the height of the stack prefix and
   the symbols that it contains are predicted (see above), and the least fixed
   computation is used only to populate these prefixes of predictable length
   with state information. *)

(* By the way, this least fixed point analysis remains the most costly
   computation throughout this module. *)

(* Vectors of sets of states. *)

module StateVector = struct

  type property =
    Lr1.NodeSet.t list

  let empty =
    []

  let rec equal v1 v2 =
    match v1, v2 with
    | [], [] ->
        true
    | states1 :: v1, states2 :: v2 ->
        Lr1.NodeSet.equal states1 states2 &&
        equal v1 v2
    | _, _ ->
        (* Because all heights are known ahead of time, we are able
	   to (and careful to) compare only vectors of equal length. *)
        assert false

  let rec join v1 v2 =
    match v1, v2 with
    | [], [] ->
	[]
    | states1 :: v1, states2 :: v2 ->
	Lr1.NodeSet.union states1 states2 ::
	join v1 v2
    | _, _ ->
        (* Because all heights are known ahead of time, we are able
	   to (and careful to) compare only vectors of equal length. *)
	assert false

  let push v x =
    x :: v

  let truncate =
    MenhirLib.General.take

end

(* In order to perform the fixed point computation, we must extend our type of
   vectors with a bottom element. This element will not appear in the least
   fixed point, provided every state of the automaton is reachable. *)

module StateLattice = struct

  type property =
    | Bottom
    | NonBottom of StateVector.property

  let bottom =
    Bottom

  let empty =
    NonBottom StateVector.empty

  let equal v1 v2 =
    match v1, v2 with
    | Bottom, Bottom ->
        true
    | NonBottom v1, NonBottom v2 ->
        StateVector.equal v1 v2
    | _, _ ->
      false

  let join v1 v2 =
    match v1, v2 with
    | Bottom, v
    | v, Bottom ->
        v
    | NonBottom v1, NonBottom v2 ->
        NonBottom (StateVector.join v1 v2)

  let push v x =
    match v with
    | Bottom ->
        Bottom 
    | NonBottom v ->
        NonBottom (StateVector.push v x)

  let truncate h v =
    match v with
    | Bottom ->
        Bottom
    | NonBottom v ->
	NonBottom (StateVector.truncate h v)

  let is_maximal _ =
    false

end

open StateLattice

(* Define the fixed point. *)

let stack_states : Lr1.node -> property =

  let module F =
    Fix.Make
      (Maps.PersistentMapsToImperativeMaps(Lr1.NodeMap))
      (StateLattice)
  in

  F.lfp (fun node (get : Lr1.node -> property) ->

    (* We use the fact that a state has incoming transitions if and only if
       it is not a start state. *)

    match Lr1.incoming_symbol node with

    | None ->
	assert (Lr1.predecessors node = []);
        assert (stack_height node = 0);

	(* If [node] is a start state, then the stack at [node] may be (in
	   fact, must be) the empty stack. *)

	empty

    | Some _symbol ->

	(* If [node] is not a start state, then include the contribution of
	   every incoming transition. We compute a join over all predecessors.
	   The contribution of one predecessor is the abstract value found at
	   this predecessor, extended with a new cell for this transition, and
	   truncated to the stack height at [node], so as to avoid obtaining a
	   vector that is longer than expected/necessary. *)

	let height = stack_height node in

	List.fold_left (fun v predecessor ->
	  join v
	    (truncate height
	      (push (get predecessor) (Lr1.NodeSet.singleton predecessor))
	    )
	) bottom (Lr1.predecessors node)

  )

(* If every state is reachable, then the least fixed point must be non-bottom
   everywhere, so we may view it as a function that produces a vector of sets
   of states. *)

let stack_states (node : Lr1.node) : StateVector.property =
  match stack_states node with
  | Bottom ->
      (* apparently this node is unreachable *)
      assert false
  | NonBottom v ->
      v

(* ------------------------------------------------------------------------ *)
(* For each production, compute where (that is, in which states) this
   production can be reduced. *)

let production_where : Lr1.NodeSet.t ProductionMap.t =
  Lr1.fold (fun accu node ->
    TerminalMap.fold (fun _ prods accu ->
      let prod = Misc.single prods in
      let nodes =
	try
	  ProductionMap.lookup prod accu
	with Not_found ->
	  Lr1.NodeSet.empty
      in
      ProductionMap.add prod (Lr1.NodeSet.add node nodes) accu
    ) (Lr1.reductions node) accu
  ) ProductionMap.empty

let production_where (prod : Production.index) : Lr1.NodeSet.t =
  try
    (* Production [prod] may be reduced at [nodes]. *)
    let nodes = ProductionMap.lookup prod production_where in
    assert (not (Lr1.NodeSet.is_empty nodes));
    nodes
  with Not_found ->
    (* The production [prod] is never reduced. *)
    Lr1.NodeSet.empty

let ever_reduced prod =
  not (Lr1.NodeSet.is_empty (production_where prod))

let fold_reduced f prod accu =
  Lr1.NodeSet.fold f (production_where prod) accu

(* ------------------------------------------------------------------------ *)
(* Warn about productions that are never reduced. *)

let () =
  let count = ref 0 in
  Production.iter (fun prod ->
    if Lr1.NodeSet.is_empty (production_where prod) then
      match Production.classify prod with
      | Some nt ->
	  incr count;
	  Error.grammar_warning
	    (Nonterminal.positions nt)
	    "symbol %s is never accepted." (Nonterminal.print false nt)
      | None ->
	  incr count;
	  Error.grammar_warning
	    (Production.positions prod)
	    "production %sis never reduced." (Production.print prod)
  );
  if !count > 0 then
    Error.grammar_warning []
      "in total, %d productions are never reduced." !count

(* ------------------------------------------------------------------------ *)
(* From the above information, deduce, for each production, the states that
   may appear in the stack when this production is reduced. *)

(* We are careful to produce a vector of states whose length is exactly that
   of the production [prod]. *)

let production_states : Production.index -> StateLattice.property =
  Production.tabulate (fun prod ->
    let nodes = production_where prod in
    let height = Production.length prod in
    Lr1.NodeSet.fold (fun node accu ->
      join accu
	(truncate height 
	   (NonBottom (stack_states node))
	)
    ) nodes bottom
  )

(* ------------------------------------------------------------------------ *)
(* We now determine which states must be represented, that is,
   explicitly pushed onto the stack. For simplicity, a state is either
   always represented or never represented. More fine-grained
   strategies, where a single state is sometimes pushed onto the stack
   and sometimes not pushed, depending on which outgoing transition is
   being taken, are conceivable, but quite tricky, and probably not
   worth the trouble.

   (1) If two states are liable to appear within a single stack cell,
   then one is represented if and only if the other is
   represented. This ensures that the structure of stacks is known
   everywhere and that we can propose types for stacks.

   (2) If a state [s] has an outgoing transition along nonterminal
   symbol [nt], and if the [goto] table for symbol [nt] has more than
   one target, then state [s] is represented.

   (3) If a stack cell contains more than one state and if at least
   one of these states is able to handle the [error] token, then these
   states are represented.

   (4) If the semantic action associated with a production mentions
   the [$syntaxerror] keyword, then the state that is being reduced to
   (that is, the state that initiated the recognition of this
   production) is represented. (Indeed, it will be passed as an
   argument to [errorcase].) *)

(* Data. *)

let rep : bool UnionFind.point array =
  Array.init Lr1.n (fun _ -> UnionFind.fresh false)

(* Getter. *)

let represented state =
  rep.(Lr1.number state)

(* Setters. *)

let represent state =
  UnionFind.change (represented state) true

let represents states =
  represent (Lr1.NodeSet.choose states)

(* Enforce condition (1) above. *)

let share (v : StateVector.property) =
  List.iter (fun states ->
    let dummy = UnionFind.fresh false in
    Lr1.NodeSet.iter (fun state ->
      UnionFind.eunion dummy (represented state)
    ) states
  ) v

let () =
  Lr1.iter (fun node ->
    share (stack_states node)
  );
  Production.iter (fun prod ->
    match production_states prod with
    | Bottom ->
	()
    | NonBottom v ->
	share v
  )

(* Enforce condition (2) above. *)

let () =
  Nonterminal.iter (fun nt ->
    let count = 
      Lr1.targets (fun count _ _ ->
	count + 1
      ) 0 (Symbol.N nt)
    in
    if count > 1 then
      Lr1.targets (fun () sources _ ->
	List.iter represent sources
      ) () (Symbol.N nt)
  )

(* Enforce condition (3) above. *)

let handler state =
  try
    let _ = SymbolMap.find (Symbol.T Terminal.error) (Lr1.transitions state) in
    true
  with Not_found ->
    try
      let _ = TerminalMap.lookup Terminal.error (Lr1.reductions state) in
      true
    with Not_found ->
      false

let handlers states =
  Lr1.NodeSet.exists handler states

let () =
  Lr1.iter (fun node ->
    let v = stack_states node in
    List.iter (fun states ->
      if Lr1.NodeSet.cardinal states >= 2 && handlers states then
	represents states
    ) v
  )

(* Enforce condition (4) above. *)

let () =
  Production.iterx (fun prod ->
    if Action.has_syntaxerror (Production.action prod) then
      match production_states prod with
      | Bottom ->
	  ()
      | NonBottom v ->
  	  let sites = production_where prod in
	  let length = Production.length prod in
	  if length = 0 then
	    Lr1.NodeSet.iter represent sites
	  else
	    let states = List.nth v (length - 1) in
	    represents states
  )

(* Define accessors. *)

let represented state =
  UnionFind.find (represented state)

let representeds states =
  if Lr1.NodeSet.is_empty states then
    assert false
  else
    represented (Lr1.NodeSet.choose states)

(* Statistics. *)

let () =
  Error.logC 1 (fun f ->
    let count =
      Lr1.fold (fun count node ->
        if represented node then count + 1 else count
      ) 0
    in
    Printf.fprintf f "%d out of %d states are represented.\n" count Lr1.n
  )

(* ------------------------------------------------------------------------ *)
(* Accessors for information about the stack. *)

(* We describe a stack prefix as a list of cells, where each cell is a pair
   of a symbol and a set of states. The top of the stack is the head of the
   list. *)

type cell =
    Symbol.t * Lr1.NodeSet.t

type word =
    cell list

(* This auxiliary function converts a stack-as-an-array (top of stack
   at the right end) to a stack-as-a-list (top of stack at list head). *)

let convert a =
  let n = Array.length a in
  let rec loop i accu =
    if i = n then accu else loop (i + 1) (a.(i) :: accu)
  in
  loop 0 []

(* [stack s] describes the stack when the automaton is in state [s]. *)

let stack node : word =
  List.combine
    (convert (stack_symbols node))
    (stack_states node)

(* [prodstack prod] describes the stack when production [prod] is about to be
   reduced. *)

let prodstack prod : word =
  match production_states prod with
  | Bottom ->
      (* This production is never reduced. *)
      assert false
  | NonBottom v ->
      List.combine
	(convert (Production.rhs prod))
	v

(* [gotostack nt] is the structure of the stack when a shift
   transition over nonterminal [nt] is about to be taken. It
   consists of just one cell. *)

let gotostack : Nonterminal.t -> word =
  Nonterminal.tabulate (fun nt ->
    let sources =
      Lr1.targets (fun accu sources _ ->
	List.fold_right Lr1.NodeSet.add sources accu
      ) Lr1.NodeSet.empty (Symbol.N nt)
    in
    [ Symbol.N nt, sources ]
  )

let fold f accu w =
  List.fold_right (fun (symbol, states) accu ->
    f accu (representeds states) symbol states
  ) w accu

let fold_top f accu w =
  match w with
  | [] ->
      accu
  | (symbol, states) :: _ ->
      f (representeds states) symbol

let print (w : word) =
  let b = Buffer.create 64 in
  fold (fun () _represented symbol _states ->
    Buffer.add_string b (Symbol.print symbol);
    Buffer.add_char b ' '
  ) () w;
  Buffer.contents b

(* ------------------------------------------------------------------------ *)
(* Explain how the stack should be deconstructed when an error is
   found.

   We sometimes have a choice as too how many stack cells should be
   popped. Indeed, several cells in the known suffix of the stack may
   physically hold a state. If neither of these states handles errors,
   then we could jump to either. (Indeed, if we jump to one that's
   nearer, it will in turn pop further stack cells and jump to one
   that's farther.) In the interests of code size, we should pop as
   few stack cells as possible. So, we jump to the topmost represented
   state in the known suffix. *)

type state =
  | Represented
  | UnRepresented of Lr1.node

type instruction =
  | Die
  | DownTo of word * state

let rewind node : instruction =
  let w = stack node in

  let rec rewind w =
    match w with
    | [] ->

	(* I believe that every stack description either is definite
	   (that is, ends with [TailEmpty]) or contains at least one
	   represented state. Thus, if we find an empty [w], this
	   means that the stack is definitely empty. *)

        Die

    | ((_, states) as cell) :: w ->

	if representeds states then

	  (* Here is a represented state. We will pop this
	     cell and no more. *)

	  DownTo ([ cell ], Represented)

	else if handlers states then begin

	  (* Here is an unrepresented state that can handle
	     errors. The cell must hold a singleton set of states, so
	     we know which state to jump to, even though it isn't
	     represented. *)

	  assert (Lr1.NodeSet.cardinal states = 1);
	  let state = Lr1.NodeSet.choose states in
	  DownTo ([ cell ], UnRepresented state)

	end
	else

	  (* Here is an unrepresented state that does not handle
	     errors. Pop this cell and look further. *)

	  match rewind w with
	  | Die ->
	      Die
	  | DownTo (w, st) ->
	      DownTo (cell :: w, st)

  in
  rewind w

(* ------------------------------------------------------------------------ *)
(* We now determine which positions must be kept track of. For
   simplicity, we do this on a per symbol basis. That is, for each
   symbol, either we never keep track of position information, or we
   always do. In fact, we do distinguish start and end positions.
   This leads to computing two sets of symbols -- those that keep
   track of their start position and those that keep track of their
   end position.

   A symbol on the right-hand side of a production must keep track of
   its (start or end) position if that position is explicitly
   requested by a semantic action.

   Furthermore, if the left-hand symbol of a production must keep
   track of its start (resp. end) position, then the first
   (resp. last) symbol of its right-hand side (if there is one) must
   do so as well. That is, unless the right-hand side is empty. *)

open Keyword

let startp =
  ref SymbolSet.empty

let endp =
  ref SymbolSet.empty

let rec require where symbol =
  let wherep =
    match where with
    | WhereStart ->
	startp
    | WhereEnd ->
	endp
  in
  if not (SymbolSet.mem symbol !wherep) then begin
    wherep := SymbolSet.add symbol !wherep;
    match symbol with
    | Symbol.T _ ->
	()
    | Symbol.N nt ->
	Production.iternt nt (require_aux where)
  end

and require_aux where prod =
  let _nt, rhs = Production.def prod in
  let length = Array.length rhs in
  if length > 0 then
    match where with
    | WhereStart ->
	require where rhs.(0)
    | WhereEnd ->
	require where rhs.(length - 1)

let () =
  Production.iterx (fun prod ->
    let rhs = Production.rhs prod
    and ids = Production.identifiers prod
    and action = Production.action prod in

    KeywordSet.iter (function
      | SyntaxError ->
	  ()
      | Position (Left, where, _) ->
	  require_aux where prod
      | Position (RightNamed id, where, _) ->
	  Array.iteri (fun i id' ->
	    if id = id' then
	      require where rhs.(i)
	  ) ids
    ) (Action.keywords action)
  )

let startp =
  !startp

let endp =
  !endp

let () =
  Error.logC 1 (fun f ->
    Printf.fprintf f
      "%d out of %d symbols keep track of their start position.\n\
       %d out of %d symbols keep track of their end position.\n"
        (SymbolSet.cardinal startp) (Terminal.n + Nonterminal.n)
        (SymbolSet.cardinal endp) (Terminal.n + Nonterminal.n))

let startp symbol =
  SymbolSet.mem symbol startp

let endp symbol =
  SymbolSet.mem symbol endp

(* ------------------------------------------------------------------------- *)
(* Miscellaneous. *)

let universal symbol =
  Lr1.fold (fun universal s ->
    universal && (if represented s then SymbolMap.mem symbol (Lr1.transitions s) else true)
  ) true

(* ------------------------------------------------------------------------ *)
(* Discover which states can peek at an error. These are the states
   where an error token may be on the stream. These are the states
   that are targets of a reduce action on [error]. *)

(* 2012/08/25 I am optimizing this code, whose original version I found had
   quadratic complexity. The problem is as follows. We can easily iterate over
   all states to find which states [s] have a reduce action on error. What we
   must find out, then, is into which state [t] this reduce action takes us.
   This is not easy to predict, as it depends on the contents of the stack.
   The original code used an overapproximation, as follows: if the reduction
   concerns a production whose head symbol is [nt], then all of the states
   that have an incoming transition labeled [nt] are potential targets. The
   new version of the code below relies on the same approximation, but uses
   two successive loops instead of two nested loops. *)

let errorpeekers =
  (* First compute a set of symbols [nt]... *)
  let nts : SymbolSet.t =
    Lr1.fold (fun nts node ->
      try
	let prods = TerminalMap.lookup Terminal.error (Lr1.reductions node) in
	let prod = Misc.single prods in
	let nt = Production.nt prod in
	SymbolSet.add (Symbol.N nt) nts
      with Not_found ->
	nts
    ) SymbolSet.empty
  in
  (* ... then compute the set of all target states of all transitions
     labeled by some symbol in the set [nt]. *)
  SymbolSet.fold (fun nt errorpeekers ->
    Lr1.targets (fun errorpeekers _ target ->
      Lr1.NodeSet.add target errorpeekers
    ) errorpeekers nt
  ) nts Lr1.NodeSet.empty

let errorpeeker node =
  Lr1.NodeSet.mem node errorpeekers

(* ------------------------------------------------------------------------ *)
(* Here is how we check whether state [s] should have a default
   reduction.

   We check whether [s] has no outgoing shift transitions and only has
   one possible reduction action. In that case, we produce a default
   reduction action, that is, we perform reduction without consulting
   the lookahead token. This saves code, but can alter the parser's
   behavior in the presence of errors.

   The check for default actions subsumes the check for the case where
   [s] admits a reduce action with lookahead symbol "#". In that case,
   it must be the only possible action -- see
   [Lr1.default_conflict_resolution]. That is, we have reached a point
   where we have recognized a well-formed input and are now expecting
   an end-of-stream. In that case, performing reduction without
   looking at the next token is the right thing to do, since there
   should in fact be none. The state that we reduce to will also have
   the same property, and so on, so we will in fact end up rewinding
   the entire stack and accepting the input when the stack becomes
   empty.

   (New as of 2012/01/23.) A state where a shift/reduce conflict was
   solved in favor of neither (due to a use of the %nonassoc
   directive) must not perform a default reduction. Indeed, this would
   effectively mean that the failure that was requested by the user is
   forgotten and replaced with a reduction. This surprising behavior
   is present in ocamlyacc and was present in earlier versions of
   Menhir. See e.g. http://caml.inria.fr/mantis/view.php?id=5462

   There is a chance that we might run into trouble if the ideas
   described in the above two paragraphs collide, that is, if we
   forbid a default reduction (due to a shift/reduce conflict solved
   by %nonassoc) in a node where we would like to have default
   reduction on "#". This situation seems unlikely to arise, so I will
   not do anything about it for the moment. (Furthermore, someone who
   uses precedence declarations is looking for trouble anyway.)

   Between 2012/05/25 and 2015/09/25, if [--canonical] has been specified,
   then we disallow default reductions on a normal token, because we do not
   want to introduce any spurious actions into the automaton. We do still
   allow default reductions on "#", since they are needed for the automaton to
   terminate properly. From 2015/09/25 on, we again always allow default
   reductions, as they seem to be beneficial when explaining syntax errors. *)

let (has_default_reduction : Lr1.node -> (Production.index * TerminalSet.t) option), hdrcount =
  Misc.tabulateo Lr1.number Lr1.fold Lr1.n (fun s ->

    if Lr1.forbid_default_reduction s then
      None
    else

      let reduction = ProductionMap.is_singleton (Lr1.invert (Lr1.reductions s)) in
      match reduction with
      | Some _ ->
	  if SymbolMap.purelynonterminal (Lr1.transitions s)
          then reduction
          else None
      | None ->
	  reduction

  )

let () =
  Error.logC 1 (fun f ->
    Printf.fprintf f
       "%d out of %d states have a default reduction.\n"
       hdrcount Lr1.n)

(* ------------------------------------------------------------------------ *)

let () =
  Time.tick "Constructing the invariant"

(* ------------------------------------------------------------------------ *)

(* If any fatal error was signaled up to this point, stop now. This may include
   errors signaled in the modules [lr1] and [invariant] by calling the function
   [Error.grammar_warning]. *)

let () =
  if Error.errors() then
    exit 1

