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
open StackLang
open StackLangUtils
module Subst = Substitution
open NamingConventions

(* -------------------------------------------------------------------------- *)

let state_reg = state

(* Checking that a StackLang program contains no references to undefined
   registers. *)

let wf_regs cfg label block rs rs' =
  (* Check that [rs'] is a subset of [rs]. *)
  let stray = RegisterSet.diff rs' rs in
  if not (RegisterSet.is_empty stray) then (
    eprintf "StackLang: in block %s, reference to undefined register%s:\n  %s\n"
      label
      (if RegisterSet.cardinal stray > 1 then "s" else "")
      (RegisterSet.print stray) ;
    eprintf "StackLang: the following registers are defined:\n  %s\n"
      (RegisterSet.print rs) ;
    eprintf "Block :" ;
    StackLangPrinter.print_block stderr block ;
    eprintf "\nContext :" ;
    StackLangPrinter.print_block stderr (lookup label cfg).block ;
    exit 1 )

let wf_regs_jump cfg label label_jump rs rs' =
  (* Check that [rs'] is a subset of [rs]. *)
  let stray = RegisterSet.diff rs' rs in
  if not (RegisterSet.is_empty stray) then (
    eprintf "StackLang: in block %s, reference to undefined register%s:\n  %s\n"
      label
      (if RegisterSet.cardinal stray > 1 then "s" else "")
      (RegisterSet.print stray) ;
    eprintf "StackLang: the following registers are defined:\n  %s\n"
      (RegisterSet.print rs) ;
    eprintf "Block :" ;
    StackLangPrinter.print_block stderr (StringMap.find label cfg).block ;
    eprintf "\nJumping to %s :" label_jump ;
    StackLangPrinter.print_block stderr (StringMap.find label_jump cfg).block ;
    exit 1 )

let wf_reg cfg label block rs r =
  wf_regs cfg label block rs (RegisterSet.singleton r)

let rec wf_value cfg label block rs v =
  match v with
  | VTag _ ->
      ()
  | VReg r ->
      wf_reg cfg label block rs r
  | VTuple vs ->
      List.iter (wf_value cfg label block rs) vs
  | VUnit ->
      ()

let rec def rs p =
  match p with
  | PWildcard ->
      rs
  | PReg r ->
      (* Check that no name is bound twice by a pattern. *)
      assert (not (RegisterSet.mem r rs)) ;
      RegisterSet.add r rs
  | PTuple ps ->
      List.fold_left def rs ps

let def rs p =
  (* The newly defined registers are the previously defined registers
     plus the registers defined by the pattern [p]. *)
  RegisterSet.union rs (def RegisterSet.empty p)

let wf_prim cfg label block rs p =
  match p with
  | PrimOCamlCall (_, args) ->
      List.iter (wf_value cfg label block rs) args
  | PrimOCamlFieldAccess (r, _) ->
      wf_reg cfg label block rs r
  | PrimOCamlDummyPos ->
      ()
  | PrimOCamlAction _ ->
      ()
  | PrimSubstOcamlAction _ ->
      ()

(* [wf_block cfg label rs block] checks that the block [block] does not refer
   to an undefined register, under the assumption that the registers [rs] are
   initially defined. The control flow graph [cfg] is used to map labels to
   blocks. [label] is the label of the current block and is used only as part
   of error messages. *)

let rec wf_block cfg label rs block =
  match block with
  | INeed (rs', block) ->
      wf_regs cfg label (INeed (rs', block)) rs rs' ;
      (* A [need] instruction undefines the registers that it does not
         mention, so we continue with [rs']. *)
      let rs = rs' in
      wf_block cfg label rs block
  | IPush (v, _cell, block) ->
      (* TODO : check that the cell and v are in sync *)
      wf_value cfg label (IPush (v, _cell, block)) rs v ;
      wf_block cfg label rs block
  | IPop (p, block) ->
      let rs = def rs p in
      wf_block cfg label rs block
  | IDef (p, v, block) ->
      wf_value cfg label (IDef (p, v, block)) rs v ;
      let rs = def rs p in
      wf_block cfg label rs block
  | IPrim (r, p, block) ->
      wf_prim cfg label (IPrim (r, p, block)) rs p ;
      let rs = def rs (PReg r) in
      wf_block cfg label rs block
  | ITrace (_, block) | IComment (_, block) ->
      wf_block cfg label rs block
  | IDie ->
      ()
  | IReturn v ->
      wf_value cfg label (IReturn v) rs v
  | IJump label' ->
      (* Check that every register that is needed at the destination label
         is defined here. *)
      wf_regs_jump cfg label label' rs (needed (lookup label' cfg))
  | ISubstitutedJump (label', substitution) ->
      wf_regs_jump cfg label label' rs
        (Subst.apply_registers substitution (needed (lookup label' cfg)))
  | ICaseToken (r, branches, odefault) ->
      wf_reg cfg label (ICaseToken (r, branches, odefault)) rs r ;
      List.iter (wf_branch cfg label rs) branches ;
      Option.iter (wf_block cfg label rs) odefault
  | ICaseTag (r, branches) ->
      wf_reg cfg label (ICaseTag (r, branches)) rs r ;
      List.iter (branch_iter (wf_block cfg label rs)) branches
  | ITypedBlock ({block; needed_registers= rs'} as t_block) ->
      wf_regs cfg label (ITypedBlock t_block) rs rs' ;
      (* A [need] instruction undefines the registers that it does not
         mention, so we continue with [rs']. *)
      let rs = rs' in
      wf_block cfg label rs block

and wf_branch cfg label rs (tokpat, block) =
  let rs =
    match tokpat with
    | TokSingle (_, r) ->
        def rs (PReg r)
    | TokMultiple _ ->
        rs
  in
  wf_block cfg label rs block

(* [wf_block cfg label block] checks that the block [block] at address [label]
   does not refer to an undefined register. We assume that the block begins
   with an [INeed] instruction and use this instruction serves as a reference
   to find out which registers are initially defined. *)

let wf_t_block (cfg : cfg) label {block; needed_registers} =
  wf_block cfg label needed_registers block

(* [wf program] checks that the program [program] contains no references to
   undefined registers. *)

let wf program =
  Program.iter (wf_t_block program.cfg) program ;
  Time.tick "Checking the StackLang code for well-formedness"

(* -------------------------------------------------------------------------- *)

(* Measuring the size of a StackLang program. *)

type measure =
  { mutable push: int
  ; mutable pop: int
  ; mutable def: int
  ; mutable prim: int
  ; mutable trace: int
  ; mutable die: int
  ; mutable return: int
  ; mutable jump: int
  ; mutable casetoken: int
  ; mutable casetag: int
  ; mutable total: int }

let zero () =
  { push= 0
  ; pop= 0
  ; def= 0
  ; prim= 0
  ; trace= 0
  ; die= 0
  ; return= 0
  ; jump= 0
  ; casetoken= 0
  ; casetag= 0
  ; total= 0 }

let print m =
  let pad i = Misc.padded_index m.total i in
  printf "PUSH    %s\n" (pad m.push) ;
  printf "POP     %s\n" (pad m.pop) ;
  printf "DEF     %s\n" (pad m.def) ;
  printf "PRIM    %s\n" (pad m.prim) ;
  printf "TRCE    %s\n" (pad m.trace) ;
  printf "DIE     %s\n" (pad m.die) ;
  printf "RET     %s\n" (pad m.return) ;
  printf "JUMP    %s\n" (pad m.jump) ;
  printf "CASEtok %s\n" (pad m.casetoken) ;
  printf "CASEtag %s\n" (pad m.casetag) ;
  printf "total   %s\n" (pad m.total) ;
  ()

let rec measure_block m block =
  match block with
  | INeed (_, block) ->
      measure_block m block
  | IPush (_, _, block) ->
      m.total <- m.total + 1 ;
      m.push <- m.push + 1 ;
      measure_block m block
  | IPop (_, block) ->
      m.total <- m.total + 1 ;
      m.pop <- m.pop + 1 ;
      measure_block m block
  | IDef (_, _, block) ->
      m.total <- m.total + 1 ;
      m.def <- m.def + 1 ;
      measure_block m block
  | IPrim (_, _, block) ->
      m.total <- m.total + 1 ;
      m.prim <- m.prim + 1 ;
      measure_block m block
  | ITrace (_, block) ->
      m.total <- m.total + 1 ;
      m.trace <- m.trace + 1 ;
      measure_block m block
  | IComment (_, block) ->
      measure_block m block
  | IDie ->
      m.total <- m.total + 1 ;
      m.die <- m.die + 1
  | IReturn _ ->
      m.total <- m.total + 1 ;
      m.return <- m.return + 1
  | IJump _ | ISubstitutedJump _ ->
      m.total <- m.total + 1 ;
      m.jump <- m.jump + 1
  | ICaseToken (_, branches, odefault) ->
      m.total <- m.total + 1 ;
      m.casetoken <- m.casetoken + 1 ;
      List.iter (branch_iter (measure_block m)) branches ;
      Option.iter (measure_block m) odefault
  | ICaseTag (_, branches) ->
      m.total <- m.total + 1 ;
      m.casetag <- m.casetag + 1 ;
      List.iter (branch_iter (measure_block m)) branches
  | ITypedBlock {block} ->
      measure_block m block

let measure_t_block measure {block} = measure_block measure block

let measure program =
  let m = zero () in
  Program.iter (fun _ -> measure_t_block m) program ;
  m

let rec detect_def_state pattern value =
  match (pattern, value) with
  | PReg reg, VTag tag when reg = state ->
      Some tag
  | PTuple p_li, VTuple v_li ->
      Option.first_value (List.map2 detect_def_state p_li v_li)
  | _ ->
      None

let rec pattern_shadow_state = function
  | PReg reg when reg = state ->
      true
  | PTuple li ->
      List.exists pattern_shadow_state li
  | _ ->
      false

let get_args_map block_map =
  StringMap.map
    (function
      | INeed (registers, _block) ->
          StringSet.elements registers
      | _ ->
          assert false)
    block_map

let wt_t_block program label t_block =
  let cfg = program.cfg in
  let states = program.states in
  let wt_cells_arrays block reason known_cells needed_cells =
    let fail message =
      eprintf "\nWhile checking %s, in block %s : %s\n" reason label message ;
      eprintf "Known cells :" ;
      StackLangPrinter.print_known_cells stderr known_cells ;
      eprintf "\nNeeded cells :" ;
      StackLangPrinter.print_known_cells stderr needed_cells ;
      eprintf "\nBlock :" ;
      StackLangPrinter.print_block stderr block ;
      eprintf "\nContext :" ;
      StackLangPrinter.print_block stderr
        (ITypedBlock (lookup label program.cfg)) ;
      eprintf "\n" ;
      StackLangPrinter.print_states stderr program.states ;
      eprintf "\n" ;
      exit 1
    in
    let l1 = Array.length known_cells in
    let l2 = Array.length needed_cells in
    if l1 < l2 then
      fail @@ sprintf "Could discover %i known cells, but %i are needed." l1 l2 ;
    for i = 1 to l2 do
      if known_cells.(l1 - i) <> needed_cells.(l2 - i) then
        fail @@ sprintf "Enough cells are known, but their content differ."
    done
  in
  let wt_cells block reason known_cells needed_cells =
    wt_cells_arrays block reason (Array.rev_of_list known_cells) needed_cells
  in
  (* TODO : document and make this display *)
  let rec wt_block (known_cells : 'a) (extra_known_cells : 'a) state = function
    (* These blocks do not care about the state of the stack *)
    | INeed (_, block)
    | IPrim (_, _, block)
    | ITrace (_, block)
    | IComment (_, block) ->
        wt_block known_cells extra_known_cells state block
    (* Always well-typed *)
    | IDie | IReturn _ ->
        ()
    | IPush (_, cell, block) ->
        wt_block (cell :: known_cells) extra_known_cells state block
    | IPop (pattern, block) -> (
      match known_cells, extra_known_cells with
      | [], _ | _, [] ->
          assert false
      | _ :: known_cells, _ :: extra_known_cells ->
          (* If the state is shadowed, then it becomes unknown. *)
          let state = if pattern_shadow_state pattern then None else state in
          wt_block known_cells extra_known_cells state block )
    | IDef (pattern, value, block) ->
        let state =
          match detect_def_state pattern value with
          | None ->
              state
          | Some tag ->
              Some tag
        in
        wt_block known_cells extra_known_cells state block
    | IJump label as block ->
        let target = lookup label cfg in
        ( if RegisterSet.mem state_reg target.needed_registers then
          (* This check that the stack is compatible with the state we are passing
             to the routine. This is only needed if we are actually passing a
             state*)
          match state with
          | Some tag ->
              wt_cells_arrays block "state sync"
                (lookup_tag tag states).known_cells target.stack_type
          | None ->
              (* If the state is unknown, we cannot check that the stack is
                 compatible with it. *)
              () ) ;
        (* We check that the stack has at least the amount of cells the target
           routine expects *)
        wt_cells block "jump" known_cells target.stack_type
    | ISubstitutedJump (label, subst) as block ->
        (* Same as above, except that there are multiple ways for the state to
           have a value. *)
        let target = lookup label cfg in
        ( if RegisterSet.mem state_reg target.needed_registers then
          match (Subst.apply subst (VReg state_reg), state) with
          | VTag _, Some _ ->
              assert false
          | VTag tag, None | _, Some tag ->
              wt_cells_arrays block "subst state sync"
                (lookup_tag tag states).known_cells target.stack_type
          | _, None ->
              () ) ;
        wt_cells block "subst jump" known_cells target.stack_type
    | ICaseToken (_, branches, odefault) ->
        List.iter
          (branch_iter (wt_block known_cells extra_known_cells state))
          branches ;
        Option.iter (wt_block known_cells extra_known_cells state) odefault
    | ICaseTag (_, branches) ->
        aux_icase_tag known_cells extra_known_cells state branches
    | ITypedBlock {block; stack_type} as this_block ->
        (* We check that the stack has at least the number of known cells that
           the type annotation expect. *)
        wt_cells this_block "typed block" known_cells stack_type ;
        let known_cells = Array.rev_to_list stack_type in
        wt_block known_cells extra_known_cells state block
  and aux_icase_tag _known_cells extra_known_cells state branches =
    let branch_aux (TagMultiple taglist, block) =
      (* By matching on the state, we discover state information.
         We can enrich the known cells with theses. *)
      let state_known_cells =
        Array.rev_to_list (state_info_intersection states taglist).known_cells
      in
      let known_cells = extra_known_cells @ state_known_cells in
      (* We are matching on a state, therefore state is always needed,
         and we can discard these values. *)
      wt_block known_cells extra_known_cells state block
    in
    List.iter branch_aux branches
  in
  let {block; stack_type} = t_block in
  let known_cells = Array.rev_to_list stack_type in
  wt_block known_cells known_cells None block

let wt program = Program.iter (wt_t_block program) program
