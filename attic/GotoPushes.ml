(* This file contains fragments of EmitStackLang that reflect the choice
   between "goto pushes" and "run pushes". *)

let is_unit prod =
  Production.length prod = 1

let may_perform_unit_reduction s =
  match Default.has_default_reduction s with
  | Some (prod, _) ->
      is_unit prod
  | None ->
      TerminalMap.fold (fun _tok prods accu ->
        accu || List.exists is_unit prods
      ) (Lr1.reductions s) false

(* -------------------------------------------------------------------------- *)

(* When a "goto" transition (that is, a transition labeled with a nonterminal
   symbol) is taken, a new cell must be pushed onto the stack. Two possible
   placements come to mind for the PUSH instruction:

   - One convention, "goto pushes", is to place this instruction in the [goto]
     subroutine, prior to the case analysis whose branches contain jumps
     to [run] subroutines.

   - Another convention, "run pushes", is to place this instruction at the
     beginning of [run]. *)

(* With "goto pushes", we emit one PUSH instruction per nonterminal symbol,
   whereas with "run pushes", we emit one such instruction per state whose
   incoming symbol is a nonterminal symbol. For each terminal symbol [nt],
   there is usually more than one state whose incoming symbol is [nt]. Thus,
   "goto pushes" is more economical in terms of code size.

   "run pushes" could be viewed as the composition of "goto pushes" with an
   optimization phase where PUSH instructions are moved into [case]
   instructions and past [jump] instructions. However, this optimization could
   be somewhat painful to implement and costly, as it requires every
   predecessor of the jump target to exhibit a similar [push] instruction. So,
   if "run pushes" is what is desired, then we prefer to produce the desired
   code directly. *)

(* Even if one wishes to optimize for code size, "run pushes" remains
   interesting in the case where every [run] subroutine of interest contains a
   POP instruction. Indeed, in that case, the PUSH and POP instructions cancel
   out, so there is no cost in code size for choosing "run pushes", while there
   is a gain in speed. *)

(* [every_run_pops nt] tests whether every state whose incoming symbol is [nt]
   performs a default reduction of a non-epsilon production. This condition
   ensures that the [run] subroutine for every such state contains a POP
   instruction. *)

let every_run_pops nt =
  Lr1.targets (fun accu _ target ->
    accu &&
    match Default.has_default_reduction target with
    | Some (prod, _) ->
        Production.length prod > 0
    | None -> false
  ) true (Symbol.N nt)

(* As suggested above, if we are trying to optimize for code size, then we
   choose "goto pushes", except in those places where "run pushes" entails
   no penalty. Otherwise, we choose "run pushes" everywhere. *)

let gotopushes : Nonterminal.t -> bool =
  if Settings.optimization_level = 0 then
    Nonterminal.tabulate (fun nt -> not (every_run_pops nt))
  else
    fun _nt -> false

let runpushes s =
  match Lr1.incoming_symbol s with
  | Some (Symbol.T _) ->
      true
  | None ->
      false
  | Some (Symbol.N nt) ->
      (* There is a jump from a [goto] subroutine to a [run] subroutine.
         Exactly one of them must be in charge of pushing a new cell. Thus,
         [runpushes s] must be [true] if and only if [gotopushes nt] is
         [false]. *)
      not (gotopushes nt)

(* -------------------------------------------------------------------------- *)

(* Code for the [run] subroutine associated with the state [s]. *)

let run s =

  ...

  (* Determine whether a new cell must be pushed onto the stack. *)

  let must_push = runpushes s in

  (* If [run] is expected to push a new cell onto the stack, do so now. *)

  if must_push then begin
    let cell = top (Long.stack s) in
    if present cell then
      push (components cell state semv startp endp) cell
  end;

  (* If this [run] routine does not push a new cell onto the stack and
     may reduce a unit production, then it is desirable to inline or
     specialize it in a situation where the state stored in the top
     stack cell is known. *)

  if (not must_push) && may_perform_unit_reduction s then
    set_hint (IfCells 1);

  ...

(* -------------------------------------------------------------------------- *)

(* Code for the [goto] subroutine associated with nonterminal symbol [nt]. *)

let goto nt =

  (* If it is up to this [goto] subroutine to push a new cell onto the stack,
     then do so now. If not, then it will be done by the [run] subroutine to
     which we are about to jump. *)

  if gotopushes nt then begin
    let cell = top (Long.gotostack nt) in
    if present cell then
      push (components cell state semv startp endp) cell
  end;

  ...


(* -------------------------------------------------------------------------- *)

(* Code for all subroutines. *)

(* This is where we declare the type of each subroutine. *)

let code label =
  match label with

  | Run s ->
      (* If our convention is that [goto] pushes the new stack cell,
         then the shape of the stack at the beginning of [run] is
         given by [Long.stack]. If [run] pushes the new cell,
         then we must pop one cell off this shape. *)
      let stack = Long.stack s in
      let stack = if runpushes s then Invariant.pop stack else stack in
      set_block_type stack (Origin.run s);
      run s

  ...
