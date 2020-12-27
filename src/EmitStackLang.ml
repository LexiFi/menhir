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

open Printf
open Grammar
let prefix = CodeBits.prefix
let if1 = CodeBits.if1
let ifn = CodeBits.ifn
let number = Lr1.number
let numbers = List.map number
open StackLang
open StackLangBuilder

(* -------------------------------------------------------------------------- *)

(* [log] emits either an [ITrace] instruction or an [IComment]
   instruction, depending on [--trace]. *)

let log format =
  Printf.ksprintf
    (fun s -> if Settings.trace then trace s else comment s)
    format

(* -------------------------------------------------------------------------- *)

(* [must_read_positions_upon_entering s] determines whether the start and end
   positions of the current token should be read from [lexbuf] upon entering
   the state [s]. This is the case if and only if [s] is entered via a shift
   transition. *)

let must_read_positions_upon_entering s =
  match Lr1.incoming_symbol s with
  | None
  | Some (Symbol.N _) ->
      false
  | Some (Symbol.T _) ->
      true

(* -------------------------------------------------------------------------- *)

(* [must_query_lexer_upon_entering s] determines whether the lexer must be
   queried for the next token upon entering the state [s]. *)

let must_query_lexer_upon_entering s =
  match Lr1.incoming_symbol s with
  | Some (Symbol.N _) ->
      (* The state [s] is entered via a goto transition. No token must
         be consumed; the current token must be kept. *)
      false
  | None
  | Some (Symbol.T _) ->
      (* The state [s] either is an initial state or is entered via a shift
         transition. *)
      match Default.has_default_reduction s with
      | Some (_, toks)
        when TerminalSet.mem Terminal.sharp toks ->
          assert (TerminalSet.cardinal toks = 1);
          (* The state [s] has a default reduction on [#]. The next token must
             not be requested, as that would drive the lexer beyond the end of
             the input stream. *)
          false
      | _ ->
          (* The state [s] has either no default reduction or a default
             reduction on a set of tokens that does not contain [#].
             The lexer must be queried for the next token. *)
          true

(* -------------------------------------------------------------------------- *)

(* Conventional names. *)

let lexer, lexbuf =
  prefix "lexer", prefix "lexbuf"

let required =
  [lexer; lexbuf]

let token, state, semv, beforeendp, startp, endp, startpos, endpos =
  CodePieces.(token, state, semv, beforeendp, startp, endp, startpos, endpos)

let discard =
  prefix "discard"

(* -------------------------------------------------------------------------- *)

(* Code labels. *)

(* A code label is potentially the target of [jump] instructions. *)

(* We need three kinds of code labels: [run] (one such label per state),
   [reduce] (one such label per production), and [goto] (one such label
   per nonterminal symbol). *)

type state =
  Lr1.node

type prod =
  Production.index

type nt =
  Nonterminal.t

module L = struct

  type label =
    | Run of state
    | Reduce of prod
    | Goto of nt

  let print label =
    prefix (
      match label with
      | Run s ->
          sprintf "run_%s"
            (Misc.padded_index Lr1.n (Lr1.number s))
      | Reduce prod ->
          sprintf "reduce_%s"
            (Misc.padded_index Production.n (Production.p2i prod))
      | Goto nt ->
          sprintf "goto_%s"
            (Nonterminal.print true nt)
    )

  let jump label =
    jump (print label)

  let iter yield =
    Lr1.iter (fun s -> yield (Run s));
    Production.iter (fun prod -> yield (Reduce prod));
    Nonterminal.iterx (fun nt -> yield (Goto nt));
    ()

(* -------------------------------------------------------------------------- *)

(* Code for the [run] subroutine associated with the state [s]. *)

let run s =

  (* Determine whether this is an initial state. *)

  let is_start = Lr1.is_start s in

  (* Determine whether the start and end positions of the current token
     should be read from [lexbuf]. *)

  let must_read_positions = must_read_positions_upon_entering s in

  (* Determine whether the lexer should be queried for the next token. *)

  let must_query_lexer = must_query_lexer_upon_entering s in

  (* At this point, the registers [lexer] and [lexbuf] are definitely needed.
     The register [token] is needed unless we are about to query the lexer for
     a new token. The registers [state] and [semv] are needed unless this is
     an initial state. The registers [startp] and [endp] are needed unless
     this is an initial state or we are about to read these positions from
     [lexbuf]. *)

  (* Note that [state] does not contain the state [s]; instead, it contains a
     predecessor state. *)

  need_list (
    lexer :: lexbuf ::
    if1 (not must_query_lexer) token @
    ifn (not is_start) [state; semv] @
    ifn (not (is_start || must_read_positions)) [startp; endp] @
    []
  );

  (* Log that we are entering state [s]. *)

  log "State %d:\n" (number s);

  (* If necessary, read the positions of the current token from [lexbuf]. *)

  if must_read_positions then begin
    prim startp (PrimOCamlFieldAccess (lexbuf, "Lexing.lex_start_p"));
    prim endp (PrimOCamlFieldAccess (lexbuf, "Lexing.lex_curr_p"))
  end;

  (* If this is not an initial state, push a new cell onto the stack. *)

  if not is_start then
    push (VTuple (vregs [ state; semv; startp; endp ]));

  (* Define the current state to be [s]. *)

  def (PReg state) (VTag (number s));

  (* If necessary, query the lexer for the next token, and rebind [token].
     This is done by calling [discard], a global function that is defined
     directly in IL code, outside StackLang. *)

  if must_query_lexer then
    prim token (PrimOCamlCall (discard, [lexer; lexbuf]));

  (* If this is a start state, define [endp], in case it is needed later on.
     This is unlikely, yet it may be needed if the start state can reduce a
     production whose semantic action uses the keyword [$endpos($0)]. *)

  if is_start then
    prim endp PrimOCamlDummyPos;

  (* At this point, the registers [lexer], [lexbuf], and [token] are defined,
     and are needed in the future, except perhaps in the very special case
     where this state has a default reduction on [#]. The registers [state]
     and [endp] are defined, and are needed in the future if the code that
     lies ahead of us involves a jump to a [reduce] subroutine that needs
     these registers. The registers [semv] and [startp] are not needed. *)

  need_list [ lexer; lexbuf; token; state; endp ];

  (* If the state [s] has a default reduction of production [prod], then jump
     to the subroutine that reduces this production. *)

  match Default.has_default_reduction s with
  | Some (prod, _) ->
      jump (Reduce prod)
  | None ->

  (* If the state [s] has no default reduction, then perform a case analysis
     of the current token [token]. The branches involve (shift) transitions,
     reductions, and (possibly) a default branch that triggers an error. *)

  case_token token begin fun branch default ->

    (* Transitions. *)

    Lr1.transitions s
    |> SymbolMap.remove (Symbol.T Terminal.error)
    |> SymbolMap.iter begin fun symbol s' ->
      match symbol with
      | Symbol.T tok ->
          (* A transition of [s] along [tok] to [s']. *)
          assert (not (Terminal.pseudo tok));
          (* Use a pattern that recognizes the token [tok] and binds [semv] to
             its semantic value. *)
          branch (TokSingle (tok, semv)) begin fun () ->
            (* Log that we are shifting. *)
            log "Shifting (%s) to state %d\n"
              (Terminal.print tok) (number s');
            (* The subroutine [run s'] does not need the current token [token]
               and is responsible for obtaining its start and end positions
               [startp] and [endp] from [lexbuf], so, besides [lexer] and
               [lexbuf], we transmit only the registers [state] and [semv]. *)
            jump (Run s')
          end
      | Symbol.N _ ->
          ()
    end;

    (* Reductions. *)

    Lr1.reductions s
    |> TerminalMap.remove Terminal.error
    |> Lr0.invert
    |> ProductionMap.iter begin fun prod toks ->
      (* A reduction of [prod] on every token in the set [toks]. *)
      branch (TokMultiple toks) begin fun () ->
        jump (Reduce prod)
      end
    end;

    (* A default branch. *)

    default die

  end

(* -------------------------------------------------------------------------- *)

(* Code for the [reduce] subroutine associated with production [prod]. *)

let reduce prod =

  (* The array [ids] lists the identifiers that are bound by this production.
     These identifiers can be referred to by the semantic action. *)

  let ids = Production.identifiers prod
  and n = Production.length prod in
  assert (Array.length ids = n);

  (* The register [state] is defined by a [pop] instruction that follows,
     unless this is an epsilon production, in which case [state] is needed
     (i.e., it must be provided by the caller). *)

  let is_epsilon = (n = 0) in

  (* The register [endp] is also defined by the instructions that follow,
     unless this is an epsilon production or the semantic action refers to
     [beforeendp] -- in either of these cases, [endp] is needed (i.e., it must
     be provided by the caller). *)

  let has_beforeend =
    not (Production.is_start prod) &&
    let action = Production.action prod in
    Action.has_beforeend action
  in

  (* Thus, we initially need the following registers. *)

  need_list (
    lexer :: lexbuf :: token ::
    if1 is_epsilon state @
    if1 (is_epsilon || has_beforeend) endp @
    []
  );

  (* Pop [n] stack cells and store their content in suitable registers.
     The state stored in the bottom cell (the one that is popped last)
     is stored in the register [state] and thus becomes the new current
     state. *)

  for i = n - 1 downto 0 do
    pop (PTuple (
      (if i = 0 then PReg state else PWildcard) ::
      (PReg ids.(i)) ::
      (PReg (startpos ids i)) ::
      (PReg (endpos ids i)) ::
      []
    ))
  done;

  (* If this is a start production, then reducing this production means
     accepting. This is done via a [return] instruction, which ends the
     execution of the program. *)

  if Production.is_start prod then begin
    assert (n = 1);
    log "Accepting\n";
    return ids.(0)
  end

  (* If this is not a start production, then it has a semantic action. *)

  else begin
    let action = Production.action prod in

    (* Log that we are reducing production [prod]. *)

    log "Reducing production %s\n" (Production.print prod);

    (* Define [beforeendp], if needed by the semantic action. Define
       [startp] and [endp], which may be needed by the semantic action,
       and are later needed by [goto]. *)

    if Action.has_beforeend action then move beforeendp endp;
    move startp (if n = 0 then endp else startpos ids 0);
    move endp   (if n = 0 then endp else endpos ids (n - 1));

    (* We now need the registers [lexer], [lexbuf], [token], [state],
       [startp], [endp], plus whatever registers are needed by the
       semantic action. *)

    need (List.fold_right RegisterSet.add
      [ lexer; lexbuf; token; state; startp; endp ]
      (Action.vars action)
    );

    (* Execute the semantic action. Store its result in [semv]. *)

    prim semv (PrimOCamlAction action);

    (* Execute a goto transition. *)

    jump (Goto (Production.nt prod))

  end

(* -------------------------------------------------------------------------- *)

(* Code for the [goto] subroutine associated with nonterminal symbol [nt]. *)

let goto nt =

  (* The [run] functions that we call are reached via goto transitions,
     therefore do not query the lexer. This means that [token] is needed. *)

  need_list [ lexer; lexbuf; token; state; semv; startp; endp ];

  (* Perform a case analysis on the current state [state]. In each branch,
     jump to an appropriate new state. There is no default branch. Although a
     default branch may need to be later added in order to avoid a warning
     from the OCaml compiler, this default branch is dead. *)

  case_tag state begin fun branch ->

    Lr1.targets (fun () sources target ->
      (* If the current state is a member of [sources], jump to [target]. *)
      branch (TagMultiple (numbers sources)) begin fun () ->
        jump (Run target)
      end
    ) () (Symbol.N nt)

  end

(* -------------------------------------------------------------------------- *)

(* Code for all subroutines. *)

let code = function
  | Run s ->
      run s
  | Reduce prod ->
      reduce prod
  | Goto nt ->
      goto nt

(* The entry points. *)

let entry =
  ProductionMap.fold (fun _prod s accu ->
    Lr1.NodeMap.add s (Run s) accu
  ) Lr1.entry Lr1.NodeMap.empty

end (* L *)

(* -------------------------------------------------------------------------- *)

(* Build a StackLang program. *)

module Run () = struct

  (* The program. *)
  include Build(L)

  (* A mapping of the start states to the program's entry labels. *)
  let entry s =
    L.(print (Run s))

  let () =
    Time.tick "Producing StackLang code"

end
