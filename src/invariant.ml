(******************************************************************************)
(*                                                                            *)
(*                                   Menhir                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*              Yann Régis-Gianas, PPS, Université Paris Diderot              *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the GNU General Public License version 2, as described in the    *)
(*  file LICENSE.                                                             *)
(*                                                                            *)
(******************************************************************************)

 (* This module discovers information about the shape and content of the stack
    in each of the automaton's states. *)

open Grammar
module C = Conflict (* artificial dependency; ensures that [Conflict] runs first *)

(* ------------------------------------------------------------------------ *)
(* Compute the known suffix of the stack, a sequence of symbols,
   at every state. This is the "short invariant". *)

module SSy =
  StackSymbols.Run()

open SSy

(* ------------------------------------------------------------------------ *)
(* Now, compute which states may be held in the known suffix of the stack. *)

module SSt =
  StackStates.Run(SSy)

open SSt

(* ------------------------------------------------------------------------ *)
(* If requested, print the information that has been computed above. *)

let () =
  Error.logC 3 (dump "short")

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
  UnionFind.set (represented state) true

let represents states =
  represent (Lr1.NodeSet.choose states)

(* Enforce condition (1) above. *)

let share (v : property) =
  Array.iter (fun states ->
    let dummy = UnionFind.fresh false in
    Lr1.NodeSet.iter (fun state ->
      UnionFind.union dummy (represented state)
    ) states
  ) v

let () =
  Lr1.iter (fun node ->
    share (stack_states node)
  );
  Production.iter (fun prod ->
    share (production_states prod)
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
    Array.iter (fun states ->
      if Lr1.NodeSet.cardinal states >= 2 && handlers states then
        represents states
    ) v
  )

(* Enforce condition (4) above. *)

let () =
  Production.iterx (fun prod ->
    if Action.has_syntaxerror (Production.action prod) then
      let sites = Lr1.production_where prod in
      let length = Production.length prod in
      if length = 0 then
        Lr1.NodeSet.iter represent sites
      else
        let states = (production_states prod).(0) in
        represents states
  )

(* Define accessors. *)

(* If [--represent-states] is passed on the command line, then every state is
   represented. The above computation is still performed. *)

let represented state =
  Settings.represent_states ||
  UnionFind.get (represented state)

let representeds states =
  Settings.represent_states ||
  if Lr1.NodeSet.is_empty states then
    false
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

(* If requested, show a detailed table of which states are represented. *)

let () =
  Error.logC 3 (fun f ->
    Lr1.iter (fun node ->
      Printf.fprintf f "represented(%s) = %b\n"
        (Lr1.print node) (represented node)
    )
  )

(* ------------------------------------------------------------------------ *)

(* Machinery for the computation of which symbols must keep track of their
   start or end positions. *)

open Keyword

type variable =
  Symbol.t * where (* WhereStart or WhereEnd *)

module M : Fix.IMPERATIVE_MAPS with type key = variable = struct
  type key = variable
  type 'data t = {
    mutable startp: 'data SymbolMap.t;
    mutable endp:   'data SymbolMap.t;
  }
  open SymbolMap
  let create() =
    { startp = empty; endp = empty }
  let clear m =
    m.startp <- empty; m.endp <- empty
  let add (sym, where) data m =
    match where with
    | WhereStart ->
        m.startp <- add sym data m.startp
    | WhereEnd ->
        m.endp <- add sym data m.endp
    | WhereSymbolStart ->
        assert false
  let find (sym, where) m =
    match where with
    | WhereStart ->
        find sym m.startp
    | WhereEnd ->
        find sym m.endp
    | WhereSymbolStart ->
        assert false
  let iter f m =
    iter (fun sym -> f (sym, WhereStart)) m.startp;
    iter (fun sym -> f (sym, WhereEnd)) m.endp
end

(* ------------------------------------------------------------------------ *)

(* We now determine which positions must be kept track of. For simplicity, we
   do this on a per-symbol basis. That is, for each symbol, either we never
   keep track of position information, or we always do. In fact, we do
   distinguish start and end positions. This leads to computing two sets of
   symbols -- those that keep track of their start position and those that
   keep track of their end position.

   A symbol on the right-hand side of a production must keep track of its
   (start or end) position if that position is explicitly requested by a
   semantic action.

   Furthermore, if the left-hand symbol of a production must keep track of its
   start (resp. end) position, then the first (resp. last) symbol of its
   right-hand side (if there is one) must do so as well. That is, unless the
   right-hand side is empty. *)

(* 2015/11/11. When a production [prod] is reduced, the top stack cell may be
   consulted for its end position. This implies that this cell must exist
   and must store an end position! Now, when does this happen?

   1- This happens if [prod] is an epsilon production and the left-hand symbol
      of the production, [nt prod], keeps track of its start or end position.
   2- This happens if the semantic action explicitly mentions the keyword
      [$endpos($0)].

   Now, if this happens, what should we do?

   a- If this happens in a state [s] whose incoming symbol is [sym], then [sym]
      must keep track of its end position.
   b- If this happens in an initial state, where the stack may be empty, then
      the sentinel cell at the bottom of the stack must contain an end position.

   Point (b) doesn't concern us here, but point (a) does. We must implement the
   constraint (1) \/ (2) -> (a). Point (b) is taken care of in the code back-end,
   where, for simplicity, we always create a sentinel cell. *)

(* I will say that this is a lot more sophisticated than I would like. The code
   back-end has been known for its efficiency and I am trying to maintain this
   property -- in particular, I would like to keep track of no positions at all,
   if the user doesn't use any position keyword. But I am suffering. *)

(* If [--represent-positions] is passed on the command line, then every position
   is stored. *)

module F =
  FixSolver.Make(M)(Fix.Prop.Boolean)

let () =

  (* We gather the constraints explained above in two loops. The first loop
     looks at every (non-start) production [prod]. The second loop looks at
     every (non-initial) state [s]. *)

  Production.iterx (fun prod ->

    let nt, rhs = Production.def prod
    and ids = Production.identifiers prod
    and action = Production.action prod in
    let length = Array.length rhs in

    if length > 0 then begin
      (* If [nt] keeps track of its start position, then the first symbol
         in the right-hand side must do so as well. *)
      F.record_VarVar (Symbol.N nt, WhereStart) (rhs.(0), WhereStart);
      (* If [nt] keeps track of its end position, then the last symbol
         in the right-hand side must do so as well. *)
      F.record_VarVar (Symbol.N nt, WhereEnd) (rhs.(length - 1), WhereEnd)
    end;

    KeywordSet.iter (function
      | SyntaxError ->
          ()
      | Position (Before, _, _) ->
          (* Doing nothing here because [$endpos($0)] is dealt with in
             the second loop. *)
          ()
      | Position (Left, _, _) ->
          (* [$startpos] and [$endpos] have been expanded away. *)
          assert false
      | Position (_, _, FlavorLocation) ->
          (* [$loc] and [$sloc] have been expanded away. *)
          assert false
      | Position (RightNamed _, WhereSymbolStart, _) ->
          (* [$symbolstartpos(x)] does not exist. *)
          assert false
      | Position (RightNamed id, where, _) ->
          (* If the semantic action mentions [$startpos($i)], then the
             [i]-th symbol in the right-hand side must keep track of
             its start position. Similarly for end positions. *)
          Array.iteri (fun i id' ->
            if id = id' then
              F.record_ConVar true (rhs.(i), where)
          ) ids
    ) (Action.keywords action)

  ); (* end of loop on productions *)

  Lr1.iterx (fun s ->
    (* Let [sym] be the incoming symbol of state [s]. *)
    let sym = Option.force (Lr1.incoming_symbol s) in

    (* Condition (1) in the long comment above (2015/11/11). If an epsilon
       production [prod] can be reduced in state [s], if its left-hand side
       [nt] keeps track of its start or end position, then [sym] must keep
       track of its end position. *)
    TerminalMap.iter (fun _ prods ->
      let prod = Misc.single prods in
      let nt, rhs = Production.def prod in
      let length = Array.length rhs in
      if length = 0 then begin
        F.record_VarVar (Symbol.N nt, WhereStart) (sym, WhereEnd);
        F.record_VarVar (Symbol.N nt, WhereEnd) (sym, WhereEnd)
      end
    ) (Lr1.reductions s);

    (* Condition (2) in the long comment above (2015/11/11). If a production
       can be reduced in state [s] and mentions [$endpos($0)], then [sym]
       must keep track of its end position. *)
    if Lr1.has_beforeend s then
      F.record_ConVar true (sym, WhereEnd)

  )

let track : variable -> bool option =
  let module S = F.Solve() in
  S.solution

let track : variable -> bool =
  fun x -> Option.value (track x) ~default:false

let startp symbol =
  Settings.represent_positions ||
  track (symbol, WhereStart)

let endp symbol =
  Settings.represent_positions ||
  track (symbol, WhereEnd)

let for_every_symbol (f : Symbol.t -> unit) : unit =
  Terminal.iter (fun t -> f (Symbol.T t));
  Nonterminal.iter (fun nt -> f (Symbol.N nt))

let sum_over_every_symbol (f : Symbol.t -> bool) : int =
  let c = ref 0 in
  for_every_symbol (fun sym -> if f sym then c := !c + 1);
  !c

let () =
  Error.logC 1 (fun f ->
    Printf.fprintf f
      "%d out of %d symbols keep track of their start position.\n\
       %d out of %d symbols keep track of their end position.\n"
        (sum_over_every_symbol startp) (Terminal.n + Nonterminal.n)
        (sum_over_every_symbol endp) (Terminal.n + Nonterminal.n))

(* ------------------------------------------------------------------------ *)
(* Constructors and accessors for information about the stack. *)

(* Types. *)

type cell = {
  symbol: Symbol.t;
  states: Lr1.NodeSet.t;
  holds_semv: bool;
  holds_state: bool;
  holds_startp: bool;
  holds_endp: bool;
}

type word =
  cell array

(* Constructors. *)

(* If [--represent-values] is passed on the command line, then every semantic
   value is stored. *)

let has_semv symbol =
  Settings.represent_values ||
  match symbol with
  | Symbol.N _nt ->
      true
  | Symbol.T tok ->
      match Terminal.ocamltype tok with
      | None ->
          (* Token has unit type and is omitted in stack cell. *)
          false
      | Some _ocamltype ->
          true

let cell symbol states =
  let holds_semv = has_semv symbol in
  let holds_state = representeds states in
  let holds_startp, holds_endp = startp symbol, endp symbol in
  { symbol; states; holds_semv; holds_state; holds_startp; holds_endp }

(* Accessors. *)

let similar cell1 cell2 =
  Symbol.equal cell1.symbol cell2.symbol &&
  cell1.holds_state = cell2.holds_state
    (* The fields [holds_semv], [holds_startp] and [holds_endp]
       do not need to be compared, because they are determined
       by the field [symbol]. The field [states] does not need
       to be compared because it does not influence the layout
       of the cell; comparing the field [holds_state] suffices. *)

let pop =
  MArray.pop

let fold_top f default w =
  let n = Array.length w in
  if n = 0 then
    default
  else
    f w.(n-1)

(* ------------------------------------------------------------------------ *)

(* Publish the short invariant. *)

module type STACK = sig

  (**[stack s] is the known suffix of the stack at state [s]. *)
  val stack: Lr1.node -> word

  (**[prodstack prod] is the known suffix of the stack at a state where
     production [prod] can be reduced. In the short invariant, the length of
     this suffix is [Production.length prod]. In the long invariant, its
     length can be greater. If there are no states where [prod] can be
     reduced, then every cell contains an empty set of states. *)
  val prodstack: Production.index -> word

  (**[gotostack nt] is the known suffix of the stack at a state where an
     edge labeled [nt] has just been followed. In the short invariant, the
     length of this suffix is [1]: indeed, it consists of just one cell,
     associated with the symbol [nt]. In the long invariant, its length can
     be greater. *)
  val gotostack: Nonterminal.t -> word

end

(* Suppose we have a function [foo] that maps things to vectors of foos and
   a function [bar] that maps things to vectors of bars. Suppose we have a
   function [cell] that builds a cell out of a foo and a bar. Then, we want
   to construct and tabulate a function that maps things to vectors of
   cells. This is done in a generic way as follows. *)

let publish tabulate foo bar cell =
  tabulate (fun thing ->
    let foos, bars = foo thing, bar thing in
    assert (Array.length foos >= Array.length bars);
    (* We allow [bars] to be shorter than [foos]. This is required in the
       computation of the long invariant, where [validate] can reject sets
       of states that are not equi-represented. In that case, we truncate
       [foos] to match [bars]. *)
    let k = Array.length bars in
    let foos = MArray.truncate k foos in
    Array.init k (fun i -> cell foos.(i) bars.(i))
  )

let stack : Lr1.node -> word =
  publish Lr1.tabulate stack_symbols stack_states cell

let prodstack : Production.index -> word =
  publish Production.tabulate production_symbols production_states cell

let gotostack : Nonterminal.t -> word =
  publish Nonterminal.tabulate goto_symbols goto_states cell

(* ------------------------------------------------------------------------ *)
(* Explain how the stack should be deconstructed when an error is found.

   We sometimes have a choice as to how many stack cells should be popped.
   Indeed, several cells in the known suffix of the stack may physically hold
   a state. If neither of these states handles errors, then we could jump to
   either. (Indeed, if we jump to one that's nearer, it will in turn pop
   further stack cells and jump to one that's farther.) In the interest of
   code size, we should pop as few stack cells as possible. So, we jump to the
   topmost represented state in the known suffix. *)

type state =
  | Represented
  | UnRepresented of Lr1.node

type instruction =
  | Die
  | DownTo of word * state

let rewind node : instruction =
  let w = stack node in

  let rec rewind w =
    if Array.length w = 0 then

      (* I believe that every stack description either is definite
         (that is, ends with [TailEmpty]) or contains at least one
         represented state. Thus, if we find an empty [w], this
         means that the stack is definitely empty. *)

      Die

    else
      let { states; _ } as cell = MArray.last w in
      let w = MArray.pop w in

      if representeds states then

        (* Here is a represented state. We will pop this
           cell and no more. *)

        DownTo ([| cell |], Represented)

      else if handlers states then begin

        (* Here is an unrepresented state that can handle
           errors. The cell must hold a singleton set of states, so
           we know which state to jump to, even though it isn't
           represented. *)

        assert (Lr1.NodeSet.cardinal states = 1);
        let state = Lr1.NodeSet.choose states in
        DownTo ([| cell |], UnRepresented state)

      end
      else

        (* Here is an unrepresented state that does not handle
           errors. Pop this cell and look further. *)

        match rewind w with
        | Die ->
            Die
        | DownTo (w, st) ->
            DownTo (MArray.push w cell, st)

  in
  rewind w

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
    Lr1.NodeSet.union errorpeekers (Lr1.all_targets nt)
  ) nts Lr1.NodeSet.empty

let errorpeeker node =
  Lr1.NodeSet.mem node errorpeekers

(* ------------------------------------------------------------------------ *)

let () =
  Time.tick "Constructing the invariant"

(* ------------------------------------------------------------------------ *)

(* Compute and publish the long invariant. *)

(* Fortunately, all of the building blocks are at hand, so this is easy. *)

(* A caveat: it is not obvious that the sets of states computed here are
   equi-represented. (A set is equi-represented if all of its elements are
   represented *or* all of its elements are unrepresented.) Yet, we need
   this property, otherwise the long invariant cannot be safely translated
   to an OCaml GADT.

   One might think that this property is likely true, because every set of
   states that appears somewhere in the long invariant must also appear
   somewhere in the short invariant, and we know that every set of states in
   the short invariant is equi-represented, because we have explicitly
   imposed this requirement. However, this is *incorrect*: testing shows
   that not every set of states in the long invariant is equi-represented.

   To work around this problem, we truncate the long invariant so as to
   forget about any stack cells that are not equi-represented. *)

module Long () = struct

  (* Compute. *)

  module SSy =
    StackSymbols.Long()

  module SSt =
    StackStates.Run(SSy)

  open SSy (* crucial! shadows the short invariant *)
  open SSt (* crucial! shadows the short invariant *)

  (* Validate. *)

  let unrepresented node =
    not (represented node)

  let equi_represented nodes =
    Lr1.NodeSet.for_all represented nodes ||
    Lr1.NodeSet.for_all unrepresented nodes

  let validate states =
    MArray.greatest_suffix_forall equi_represented states

  let stack_states s =
    validate @@ stack_states s

  let production_states prod =
    validate @@ production_states prod

  let goto_states nt =
    validate @@ goto_states nt

  (* Dump. *)

  let () =
    Error.logC 3 (dump "long")

  (* Publish. *)

  let stack : Lr1.node -> word =
    publish Lr1.tabulate stack_symbols stack_states cell

  let prodstack : Production.index -> word =
    publish Production.tabulate production_symbols production_states cell

  let gotostack : Nonterminal.t -> word =
    publish Nonterminal.tabulate goto_symbols goto_states cell

  let () =
    Time.tick "Constructing the long invariant"

end (* Long *)

(* ------------------------------------------------------------------------ *)

(* Compute which entry states can reach each [run], [reduce], and [goto]
   function. *)

(* This information is computed only on demand. *)

(* This information is used in the new code back-end to determine in which
   states we have static knowledge of the final result type of the parser,
   ['final]. This information can be built into the GADT that describes the
   states, and this in turn can be used to perform certain optimizations (such
   as removing case analyses that have only one branch) while preserving the
   well-typedness of the OCaml code. *)

(* This information is computed via a forward data flow analysis. *)

(* The join semi-lattice of properties is as follows. *)

module P = struct

  (* [SingleOrigin s] means that we are reachable via a single entry state
     [s]. [Top] means that we are reachable via multiple entry states. *)
  type property =
    | SingleOrigin of Nonterminal.t
    | Top

  let leq_join p1 p2 =
    match p1, p2 with
    | _, Top
    | Top, _ ->
        Top
    | SingleOrigin start1, SingleOrigin start2 ->
        if Nonterminal.equal start1 start2 then p2 else Top

end

(* The call graph of the [run], [reduce] and [goto] functions. *)

module G = struct

  include P

  type variable =
    | Run of Lr1.node
    | Reduce of Production.index
    | Goto of Nonterminal.t

  type t = variable

  let foreach_root yield =
    (* The entry points are the [run] functions associated with each of
       the entry states. *)
    Lr1.entry |> ProductionMap.iter (fun prod node ->
      let nt = Option.force (Production.classify prod) in
      yield (Run node) (SingleOrigin nt)
    )

  let foreach_successor v origin yield =
    match v with
    | Run node ->
        (* For each transition from [node] to [node'], the function [run node]
           calls the function [run node']. In the case of [goto] transitions,
           this is not a direct call (it goes through [reduce] and [goto]
           functions), but it is nevertheless accounted for here. *)
        Lr1.transitions node |> SymbolMap.iter begin fun _label node' ->
          yield (Run node') origin
        end;
        Lr1.reductions node |> TerminalMap.iter begin fun _tok prods ->
          let prod = Misc.single prods in
          yield (Reduce prod) origin
        end
    | Reduce prod ->
        (* A [reduce] function ends with a call to a [goto] function. *)
        let nt = Production.nt prod in
        yield (Goto nt) origin
    | Goto _nt ->
        (* A [goto] function appears to make no calls. The calls that it
           makes have already been accounted for above. *)
        ()

end

(* Run the analysis on demand. *)

let solution : (G.variable -> P.property option) Lazy.t =
  lazy (
    let module D = Fix.DataFlow.ForType(G)(P)(G) in
    D.solution
  )

(* Convert a [property option] to something clearer for the end user. *)

module Origin = struct

  type origin =
    | Dead
    | SingleOrigin of Nonterminal.t
    | MultipleOrigins

  let convert op =
    match op with
    | None ->
        Dead
    | Some (P.SingleOrigin nt) ->
        SingleOrigin nt
    | Some (P.Top) ->
        MultipleOrigins

  (* Publish the data. *)

  let run node =
    convert (Lazy.force solution (G.Run node))

  let reduce prod =
    convert (Lazy.force solution (G.Reduce prod))

  let goto nt =
    convert (Lazy.force solution (G.Goto nt))

end (* Origin *)
