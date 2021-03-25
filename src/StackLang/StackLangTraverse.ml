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
module Subst = Substitution

let fresh_int =
  let n = ref (-1) in
  fun () ->
    n := !n + 1 ;
    !n

let fstt (e, _, _) = e

let suffix name i = Printf.sprintf "%s_%i" name i

let fstate = "_menhir_s"

let branch_iter f (_pat, block) = f block

let branch_map f (pat, block) = (pat, f block)

let block_map f = function
  | INeed (regs, block) ->
      INeed (regs, f block)
  | IPush (value, cell, block) ->
      IPush (value, cell, f block)
  | IPop (reg, block) ->
      IPop (reg, f block)
  | IDef (pat, value, block) ->
      IDef (pat, value, f block)
  | IPrim (reg, prim, block) ->
      IPrim (reg, prim, f block)
  | ITrace (reg, block) ->
      ITrace (reg, f block)
  | IComment (comment, block) ->
      IComment (comment, f block)
  | (IDie | IReturn _ | IJump _ | ISubstitutedJump _) as end_block ->
      end_block
  | ICaseToken (reg, branches, odefault) ->
      ICaseToken (reg, List.map (branch_map f) branches, Option.map f odefault)
  | ICaseTag (reg, branches) ->
      ICaseTag (reg, List.map (branch_map f) branches)
  | ITypedBlock t_block ->
      ITypedBlock {t_block with block= f t_block.block}

(* -------------------------------------------------------------------------- *)

(* Checking that a StackLang program contains no references to undefined
   registers. *)

let wf_regs cfg label rs rs' =
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

let wf_reg cfg label rs r = wf_regs cfg label rs (RegisterSet.singleton r)

let rec wf_value cfg label rs v =
  match v with
  | VTag _ ->
      ()
  | VReg r ->
      wf_reg cfg label rs r
  | VTuple vs ->
      List.iter (wf_value cfg label rs) vs
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

let wf_prim cfg label rs p =
  match p with
  | PrimOCamlCall (_, args) ->
      List.iter (wf_value cfg label rs) args
  | PrimOCamlFieldAccess (r, _) ->
      wf_reg cfg label rs r
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
      wf_regs cfg label rs rs' ;
      (* A [need] instruction undefines the registers that it does not
         mention, so we continue with [rs']. *)
      let rs = rs' in
      wf_block cfg label rs block
  | IPush (v, _cell, block) ->
      (* TODO : check that the cell and v are in sync *)
      wf_value cfg label rs v ;
      wf_block cfg label rs block
  | IPop (p, block) ->
      let rs = def rs p in
      wf_block cfg label rs block
  | IDef (p, v, block) ->
      wf_value cfg label rs v ;
      let rs = def rs p in
      wf_block cfg label rs block
  | IPrim (r, p, block) ->
      wf_prim cfg label rs p ;
      let rs = def rs (PReg r) in
      wf_block cfg label rs block
  | ITrace (_, block) | IComment (_, block) ->
      wf_block cfg label rs block
  | IDie ->
      ()
  | IReturn v ->
      wf_value cfg label rs v
  | IJump label' ->
      (* Check that every register that is needed at the destination label
         is defined here. *)
      wf_regs_jump cfg label label' rs (needed (lookup label' cfg))
  | ISubstitutedJump (label', substitution) ->
      wf_regs cfg label rs
        (Subst.apply_registers substitution (needed (lookup label' cfg)))
  | ICaseToken (r, branches, odefault) ->
      wf_reg cfg label rs r ;
      List.iter (wf_branch cfg label rs) branches ;
      Option.iter (wf_block cfg label rs) odefault
  | ICaseTag (r, branches) ->
      wf_reg cfg label rs r ;
      List.iter (branch_iter (wf_block cfg label rs)) branches
  | ITypedBlock {block; stack_type= _; final_type= _; needed_registers= rs'} ->
      wf_regs cfg label rs rs' ;
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
  LabelMap.iter (wf_t_block program.cfg) program.cfg ;
  Time.tick "Checking the StackLang code for well-formedness"

(* -------------------------------------------------------------------------- *)

(* [successors yield block] applies the function [yield] in turn to every
   label that is the target of a [jump] instruction in the block [block]. *)

let rec successors yield block =
  match block with
  | INeed (_, block)
  | IPush (_, _, block)
  | IPop (_, block)
  | IDef (_, _, block)
  | IPrim (_, _, block)
  | ITrace (_, block)
  | IComment (_, block) ->
      successors yield block
  | IDie | IReturn _ ->
      ()
  | IJump label | ISubstitutedJump (label, _) ->
      yield label
  | ICaseToken (_, branches, oblock) ->
      List.iter (branch_iter (successors yield)) branches ;
      Option.iter (successors yield) oblock
  | ICaseTag (_, branches) ->
      List.iter (branch_iter (successors yield)) branches
  | ITypedBlock {block; stack_type= _; final_type= _} ->
      successors yield block

(* -------------------------------------------------------------------------- *)

(* [in_degree program] computes the in-degree of every label in the program
   [program]. It returns a table that maps every reachable label to its
   in-degree. Unreachable labels do not appear in the table. The entry labels
   artifically receive an in-degree of at least 2; this ensures that they
   cannot be inlined by the function [inline] that follows. *)

let in_degree program =
  (* Initialize a queue and a map of labels to degrees. *)
  let queue : label Queue.t = Queue.create ()
  and degree : int LabelMap.t ref = ref LabelMap.empty in
  (* [tick label] increments the degree associated with [label]. If its
     previous degree was zero, then [label] is enqueued for exploration. *)
  let tick label =
    let d =
      try LabelMap.find label !degree
      with Not_found -> Queue.add label queue ; 0
    in
    degree := LabelMap.add label (d + 1) !degree
  in
  (* [visit () label] examines the block at address [label]. *)
  let visit () label = successors tick (lookup label program.cfg).block in
  (* Initialize the queue with the entry labels. Process the queue until it
     becomes empty. Return the final table. *)
  StringMap.iter
    (fun _s label ->
      Queue.add label queue ;
      degree := LabelMap.add label 2 !degree)
    program.entry ;
  Misc.qfold visit () queue ;
  !degree

(* -------------------------------------------------------------------------- *)

(* [inline degree program] transforms the program [program] by removing every
   unreachable block and by inlining every block whose in-degree is 1. It is
   assumed that every entry label has an in-degree of at least 2. *)

let rec inline_block cfg degree block =
  match block with
  | IJump label ->
      (* If the target label's in-degree is 1, follow the indirection;
         otherwise, keep the [jump] instruction. *)
      if lookup label degree = 1 then
        let typed_block = lookup label cfg in
        ITypedBlock
          {typed_block with block= inline_block cfg degree typed_block.block}
      else IJump label
  | ISubstitutedJump (label, substitution) ->
      (* If the target label's in-degree is 1, follow the indirection;
         otherwise, keep the [jump] instruction. *)
      if lookup label degree = 1 then
        let typed_block = lookup label cfg in
        Subst.tight_restore_defs substitution (needed typed_block)
          (ITypedBlock
             {typed_block with block= inline_block cfg degree typed_block.block})
      else ISubstitutedJump (label, substitution)
  | block ->
      block_map (inline_block cfg degree) block

let inline_cfg degree (cfg : typed_block RegisterMap.t) : cfg =
  LabelMap.fold
    (fun label ({block} as t_block) accu ->
      match LabelMap.find label degree with
      | exception Not_found ->
          (* An unreachable label. *)
          accu
      | d ->
          assert (d > 0) ;
          if d = 1 then accu
          else
            LabelMap.add label
              {t_block with block= inline_block cfg degree block}
              accu)
    cfg LabelMap.empty

let inline degree ({cfg; entry; states} as program) : program =
  if Settings.code_inlining then {cfg= inline_cfg degree cfg; entry; states}
  else program

(* [inline program] transforms the program [program] by removing every
   unreachable block and by inlining away every (non-entry) label whose
   in-degree is 1. *)

let inline program =
  let program = inline (in_degree program) program in
  Time.tick "Inlining in StackLang" ;
  program

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
  | ITypedBlock {block; stack_type= _; final_type= _} ->
      measure_block m block

let measure program =
  let m = zero () in
  LabelMap.iter (fun _ block -> measure_block m block.block) program.cfg ;
  m

let get_args_map block_map =
  StringMap.map
    (function
      | INeed (registers, _block) ->
          StringSet.elements registers
      | _ ->
          assert false)
    block_map

let rec is_pattern_equivalent_to_value pattern value =
  match (pattern, value) with
  | PWildcard, _ ->
      true
  | PReg reg_pat, VReg reg_val when reg_pat = reg_val ->
      true
  | PTuple li_pat, VTuple li_val when List.length li_pat = List.length li_val ->
      List.for_all2 is_pattern_equivalent_to_value li_pat li_val
  | _, _ ->
      false

(** [intersection pattern value] he intersection of the registers defined by
    [pattern] with those refered by [value] *)
let rec intersection pattern value =
  match (pattern, value) with
  | PWildcard, _ ->
      RegisterSet.empty
  | PReg reg_pat, VReg reg_val when reg_pat = reg_val ->
      RegisterSet.singleton reg_pat
  | pattern, VTuple li_val ->
      List.fold_left RegisterSet.union RegisterSet.empty
        (List.map (intersection pattern) li_val)
  | PTuple li_pat, value ->
      List.fold_left RegisterSet.union RegisterSet.empty
        (List.map (fun pattern -> intersection pattern value) li_pat)
  | _, _ ->
      RegisterSet.empty

let rec value_refers_to_register register value =
  match value with
  | VReg register' when register = register' ->
      true
  | VTuple li ->
      List.exists (value_refers_to_register register) li
  | _ ->
      false

(** [pop_n_cells stack_type n] return a new stack_type, with [n] top cells
    removed. Asserts that every popped cell is not empty. *)
let pop_n_cells stack_type n =
  let length = Array.length stack_type in
  let new_length = max (length - n) 0 in
  let n = length - new_length in
  Array.iter
    (fun cell ->
      assert (
        cell.hold_semv || cell.hold_state || cell.hold_startpos
        || cell.hold_endpos ))
    (Array.sub stack_type new_length n) ;
  Array.sub stack_type 0 new_length

let _inline_tags program =
  let rec aux subst = function
    | IDef (PReg name, VTag tag, block) ->
        aux (Subst.extend name (VTag tag) subst) block
    | INeed (regs, block) ->
        INeed (Subst.apply_registers subst regs, aux subst block)
    | IDef (pattern, value, block) ->
        IDef
          ( pattern
          , Subst.apply subst value
          , aux (Subst.remove subst pattern) block )
    | IPush (value, cell, block) ->
        IPush (Subst.apply subst value, cell, aux subst block)
    | IPrim (reg, prim, block) ->
        IPrim (reg, prim, aux (Subst.remove subst (PReg reg)) block)
    | IPop (pattern, block) ->
        IPop (pattern, aux (Subst.remove subst pattern) block)
    | IJump label ->
        (*Subst.tight_restore_defs substitution
          (needed (StringMap.find reg program.cfg))
          (IJump reg)*)
        ISubstitutedJump (label, subst)
    | ISubstitutedJump (label, subst') ->
        ISubstitutedJump (label, Subst.compose subst subst')
    | ICaseTag (reg, branches) ->
        Subst.restore_defs subst
          (ICaseTag
             ( reg
             , List.map
                 (fun (tagpat, block) -> (tagpat, aux Subst.empty block))
                 branches ))
    | ITypedBlock ({block; needed_registers; stack_type} as t_block) ->
        let stack_type' =
          match Subst.apply subst (VReg fstate) with
          | VTag tag ->
              let tag = Lr1.node_of_number tag in
              Lr1.NodeMap.find tag program.states
          | VReg _reg ->
              stack_type
          | _ ->
              assert false
        in
        let stack_type =
          if Array.length stack_type > Array.length stack_type' then stack_type
          else stack_type'
        in
        ITypedBlock
          { t_block with
            stack_type
          ; needed_registers= Subst.apply_registers subst needed_registers
          ; block= aux subst block }
    | block ->
        block_map (aux subst) block
  in
  { program with
    cfg=
      RegisterMap.map
        (fun t_block -> {t_block with block= aux Subst.empty t_block.block})
        program.cfg }

let restore_pushes push_list block =
  List.fold_left
    (fun block (value, cell, id) ->
      IComment (sprintf "Restoring push_%i" id, IPush (value, cell, block)))
    block push_list

let rec string_of_value value =
  match value with
  | VTag t ->
      string_of_int t
  | VReg s ->
      s
  | VTuple li ->
      "(" ^ String.concat ", " (List.map string_of_value li) ^ ")"
  | VUnit ->
      "()"

let rec string_of_pattern pattern =
  match pattern with
  | PReg s ->
      s
  | PTuple li ->
      "(" ^ String.concat ", " (List.map string_of_pattern li) ^ ")"
  | PWildcard ->
      "_"

(** Add fresh name in [substitution] for every register that is part of the
   intersection of [values] and [pattern] *)
let _update_substitution subst pattern values =
  (* Add a new rule to the substitution for every element of the intersection
     of [pattern] and [value] *)
  let add_intersection pattern subst value =
    RegisterSet.fold
      (fun reg subst ->
        let reg' = suffix reg (fresh_int ()) in
        Subst.extend reg (VReg reg') subst)
      (intersection pattern value)
      subst
  in
  List.fold_left (add_intersection pattern) subst values

let rec needed_value = function
  | VReg reg ->
      RegisterSet.singleton reg
  | VTag _ | VUnit ->
      RegisterSet.empty
  | VTuple li ->
      List.fold_left RegisterSet.union RegisterSet.empty
        (List.map needed_value li)

let rec value_need_state = function
  | VReg reg when reg = fstate ->
      true
  | VTuple li ->
      List.exists value_need_state li
  | _ ->
      false

let subst_call subst (f, args) = (f, List.map (Subst.apply subst) args)

let cells_intersection cells1 cells2 =
  let len1 = Array.length cells1 in
  let len2 = Array.length cells2 in
  let i = ref 0 in
  while
    let i = !i in
    i < len1 && i < len2 && cells1.(i) = cells2.(i)
  do
    i := !i + 1
  done ;
  Array.sub cells1 0 !i

let cells_list_intersection cellss =
  match cellss with
  | cells :: cellss ->
      List.fold_left cells_intersection cells cellss
  | [] ->
      assert false

let stack_type_intersection states taglist =
  cells_list_intersection
    (List.map
       (fun tag ->
         let s = Lr1.node_of_number tag in
         Lr1.NodeMap.find s states)
       taglist)

let ( >>> ) f g x = f (g x)

let commute_pushes_t_block program t_block =
  let cfg = program.cfg in
  let pushes_conflit_with_reg pushes reg =
    List.exists (value_refers_to_register reg) pushes
  in
  let counter = ref 0 in
  let rec aux (pushes : (value * cell_info * int) list) (subst : Subst.t)
      final_type (known_cells : cell_info list) : block -> block * bool =
    (* [push_list] is the list of pushes that we need to restore, and hopefully
       cancel out with a pop. Every time there is a definition of a new value,
       in order to not disturb the pushes, if the pattern is in conflict with
       one of the pushed value, we change the name of the culprit register, and
       we save the change in a [Subst.t]. In subsequent code, every time
       we refer to a register, we use the name from the substitution's
       right-hand side.
       The code produced by [aux pushes substitution block] should be
       equivalent to
       [restore_pushes pushes (Subst.restore_defs substitution block)] *)
    function
    | INeed (registers, block) ->
        let block', need_state =
          aux pushes subst final_type known_cells block
        in
        let registers =
          RegisterSet.union
            (List.fold_left RegisterSet.union RegisterSet.empty
               (List.map (needed_value >>> fstt) pushes))
            (Subst.apply_registers subst registers)
        in
        let registers =
          if need_state then registers else RegisterSet.remove fstate registers
        in
        (INeed (registers, block'), need_state)
    | IPush (value, cell, block) ->
        (* We push the substituted name *)
        let id = fresh_int () in
        let block', need_state =
          aux
            ((Subst.apply subst value, cell, id) :: pushes)
            subst final_type known_cells block
        in
        ( IComment
            (sprintf "Commuting push_%i %s" id (string_of_value value), block')
        , need_state )
    | IPop (pattern, block) -> (
      (* A pop is a special kind of definition, so you may think that we need
         to generate new substition rules from it, but there is no need
         because we only keep the pop if there is no push that can conflict
         with it. The pattern we pop into is authoritative and we remove it
         from the substitution. *)
      match pushes with
      | [] -> (
        match known_cells with
        | [] ->
            let block', need_state =
              aux [] (Subst.remove subst pattern) final_type known_cells block
            in
            ( IComment ("This will probably fail", IPop (pattern, block'))
            , need_state )
        | cell :: known_cells ->
            let block', need_state =
              aux [] (Subst.remove subst pattern) final_type known_cells block
            in
            let need_state = if cell.hold_state then false else need_state in
            (IPop (pattern, block'), need_state) )
      | (value, _cell, id) :: push_list ->
          counter := !counter + 1 ;
          let block', need_state =
            aux push_list
              (Subst.extend_pattern
                 (Subst.remove_value subst value)
                 pattern value)
              final_type known_cells block
          in
          let need_state = value_need_state value || need_state in
          ( IComment
              ( sprintf "Cancelled push_%i %s with pop %s" id
                  (string_of_value value)
                  (string_of_pattern pattern)
              , (*IDef
                  ( pattern
                  , value
                  , aux push_list (Subst.remove subst pattern) block ) *)
                block' )
          , need_state ) )
    | IDef (pattern, value, block) ->
        (* As explained above, for every conflict between the definition and a
           push currently commuting, we add a new substitution rule
           We do not want to shadow definition useful for the push train *)
        (* Currently, the value is raw, we need to apply the substitution to it
           in order for it to refer to the correct definition. *)
        (*let value' = Subst.apply subst value' in
          let subst = update_substitution subst pattern (List.map fst pushes) in
          IDef (Subst.apply_pattern subst pattern, value', aux pushes subst block)*)
        let subst = Subst.extend_pattern subst pattern value in
        let value' = Subst.apply subst value in
        let block', need_state =
          aux pushes subst final_type known_cells block
        in
        let need_state = value_need_state value' || need_state in
        ( IComment
            ( sprintf "Inlining def : %s = %s"
                (string_of_pattern pattern)
                (string_of_value value')
            , block' )
        , need_state )
    | IPrim (reg, prim, block) ->
        (* A primitive is a like def except it has a simple register instead of a
           pattern *)
        let reg' =
          if pushes_conflit_with_reg (List.map fstt pushes) reg then
            suffix reg (fresh_int ())
          else reg
        in
        let subst' =
          Subst.extend reg (VReg reg') (Subst.remove subst (PReg reg'))
        in
        let block, need_state =
          aux pushes subst' final_type known_cells block
        in
        let prim =
          match prim with
          | PrimOCamlCall (f, args) ->
              let f, args = subst_call subst (f, args) in
              PrimOCamlCall (f, args)
          | PrimSubstOcamlAction (subst', action) ->
              PrimSubstOcamlAction (Subst.compose subst subst', action)
          | PrimOCamlAction action ->
              PrimSubstOcamlAction (subst, action)
          | prim ->
              prim
        in
        (IPrim (reg', prim, block), need_state)
    | ITrace (register, block) ->
        let block', need_state =
          aux pushes subst final_type known_cells block
        in
        (ITrace (register, block'), need_state)
    | IComment (comment, block) ->
        let block', need_state =
          aux pushes subst final_type known_cells block
        in
        (IComment (comment, block'), need_state)
    | IDie ->
        (IDie, false)
    | IReturn v ->
        (IReturn (Subst.apply subst v), false)
    | IJump label ->
        (* We first restore the pushes we failed to eliminate, then we restore
           every value to its original name, so that the jump does not fail. *)
        aux_jump pushes subst label
    | ISubstitutedJump (label, subst') ->
        (* This case is quite tricky.
           A substituted jump is a jump, but with a built-in substitution to
           be able to change the name of the parameters without restoring the
           definitions before the jumps. *)
        let subst = Subst.compose subst subst' in
        aux_jump pushes subst label
    | ICaseToken (reg, branches, odefault) ->
        aux_case_token pushes subst reg branches odefault final_type known_cells
    | ICaseTag (reg, branches) -> (
      match Subst.apply subst (VReg reg) with
      | VTag tag ->
          let block =
            snd
            @@ List.find
                 (fun (TagMultiple taglist, _) ->
                   List.exists (( = ) tag) taglist)
                 branches
          in
          aux pushes subst final_type known_cells block
      | VReg reg ->
          let branches =
            List.map
              (fun (TagMultiple taglist, block) ->
                let known_cells' =
                  stack_type_intersection program.states taglist
                in
                let len' = Array.length known_cells' in
                let known_cells' = Array.to_list known_cells' in
                let len = List.length known_cells in
                let known_cells =
                  if len > len' then known_cells else known_cells'
                in
                let subst, pushes =
                  match taglist with
                  | [tag] ->
                      ( Subst.extend reg (VTag tag) subst
                      , let tmp_subst =
                          Subst.extend reg (VTag tag) Subst.empty
                        in
                        List.map
                          (fun (value, cell, id) ->
                            (Subst.apply tmp_subst value, cell, id))
                          pushes )
                  | _ ->
                      (subst, pushes)
                in
                ( TagMultiple taglist
                , aux pushes subst final_type known_cells block ))
              branches
          in
          let branches, _need_state_list =
            List.split
            @@ List.map
                 (fun (tagpat, (block, ns)) -> ((tagpat, block), ns))
                 branches
          in
          let need_state =
            true
            (*List.fold_left ( || ) false need_state_list*)
          in
          (ICaseTag (Subst.apply_reg subst reg, branches), need_state)
      | _ ->
          assert false )
    | ITypedBlock
        ( {block; stack_type; needed_registers; final_type= final_type'} as
        t_block ) ->
        (* We alter the type information according to the number of commuting
           pushes : Every push that is commuting removes a known stack symbol *)
        let state =
          match Subst.apply subst (VReg fstate) with
          | VTag tag ->
              Some tag
          | VReg reg when reg = fstate ->
              None
          | _ ->
              assert false
        in
        let final_type =
          match final_type with
          | None -> (
            match state with
            | Some tag -> (
                let s = Lr1.node_of_number tag in
                match Lr1.is_start_or_exit s with
                | None ->
                    None
                | Some nonterminal ->
                    Some
                      (IL.TypTextual
                         (Grammar.Nonterminal.ocamltype_of_start_symbol
                            nonterminal)) )
            | None ->
                final_type' )
          | Some _ ->
              final_type
        in
        let block, need_state = aux pushes subst final_type known_cells block in
        let stack_types =
          pop_n_cells stack_type (List.length pushes)
          :: (if not need_state then [Array.of_list known_cells] else [])
          @
          match state with
          | Some tag ->
              let tag = Lr1.node_of_number tag in
              [ pop_n_cells
                  (Lr1.NodeMap.find tag program.states)
                  (List.length pushes) ]
          | None ->
              []
        in
        let stack_type =
          List.hd
          @@ List.sort
               (fun s1 s2 -> -compare (Array.length s1) (Array.length s2))
               stack_types
        in
        let needed_registers =
          RegisterSet.union
            (List.fold_left RegisterSet.union RegisterSet.empty
               (List.map needed_value (List.map fstt pushes)))
            (Subst.apply_registers subst needed_registers)
        in
        let needed_registers =
          if need_state then RegisterSet.add fstate needed_registers
          else RegisterSet.remove fstate needed_registers
        in
        ( ITypedBlock
            { t_block with
              block= IComment (sprintf "need_state=%b" need_state, block)
            ; final_type
            ; needed_registers
            ; stack_type }
        , need_state )
  and aux_jump pushes subst label =
    let needed_registers = needed (StringMap.find label cfg) in
    let needed_registers = Subst.apply_registers subst needed_registers in
    let needed_registers =
      List.fold_left
        (fun needed_registers (value, _, _) ->
          let value_registers = value_registers value in
          RegisterSet.union needed_registers value_registers)
        needed_registers pushes
    in
    ( restore_pushes pushes
        (IComment
           ( sprintf "Needed registers : %s"
               (String.concat " " (RegisterSet.elements needed_registers))
           , ISubstitutedJump (label, subst) ))
    , RegisterSet.mem fstate needed_registers )
  and aux_case_token pushes subst reg branches odefault final_type known_cells =
    let need_state = ref false in
    let aux_branch = function
      (* Every [TokSingle] introduces a definition of a register. *)
      | TokSingle (tok, reg'), block ->
          let reg'' =
            if pushes_conflit_with_reg (List.map fstt pushes) reg' then
              suffix reg' (fresh_int ())
            else reg'
          in
          let subst = Subst.extend reg' (VReg reg'') subst in
          let block', need_state' =
            aux pushes subst final_type known_cells block
          in
          need_state := !need_state || need_state' ;
          (TokSingle (tok, reg''), block')
      (* [TokMultiple] does not introduce new definitions *)
      | TokMultiple terminals, block ->
          let block', need_state' =
            aux pushes subst final_type known_cells block
          in
          need_state := !need_state || need_state' ;
          (TokMultiple terminals, block')
    in
    let branches = List.map aux_branch branches in
    ( IComment
        ( sprintf "Final type : %s"
            (match final_type with Some _ -> "Some" | None -> "None")
        , ICaseToken
            ( reg
            , branches
            , Option.map fst
                (Option.map (aux pushes subst final_type known_cells) odefault)
            ) )
    , !need_state )
  in
  let {block; stack_type} = t_block in
  let candidate, _tag =
    aux [] Subst.empty None (Array.to_list stack_type) block
  in
  {t_block with block= (if !counter > 0 then candidate else block)}

let commute_pushes program =
  { program with
    cfg=
      RegisterMap.map
        (fun t_block ->
          let r = commute_pushes_t_block program t_block in
          assert (t_block.needed_registers == r.needed_registers) ;
          r)
        program.cfg }

let count_pushes program =
  let rec aux i block =
    match block with
    | IPush (_, _, block) ->
        aux (i + 1) block
    | INeed (_, block)
    | IPop (_, block)
    | IDef (_, _, block)
    | IPrim (_, _, block)
    | ITrace (_, block)
    | ITypedBlock {block}
    | IComment (_, block) ->
        aux i block
    | IDie | IReturn _ | IJump _ | ISubstitutedJump _ ->
        i
    | ICaseToken (_, branches, _) -> (
      match branches with
      | [] ->
          0
      | _ :: _ ->
          List.fold_left ( + ) 0 (List.map (branch_iter (aux i)) branches)
          / List.length branches )
    | ICaseTag (_, branches) -> (
      match branches with
      | [] ->
          0
      | _ :: _ ->
          List.fold_left ( + ) 0 (List.map (branch_iter (aux i)) branches)
          / List.length branches )
  in
  RegisterMap.fold (fun _ {block} acc -> acc + aux 0 block) program.cfg 0

(** remove definitions of shape [x = x], or shape [_ = x] *)
let remove_useless_defs program =
  let rec aux block =
    match block with
    | IDef (pattern, value, block)
      when is_pattern_equivalent_to_value pattern value ->
        aux block
    | _ ->
        block_map aux block
  in
  { program with
    cfg=
      RegisterMap.map
        (fun t_block -> {t_block with block= aux t_block.block})
        program.cfg }

let optimize program =
  let original_count = count_pushes program in
  remove_useless_defs
    ( if Settings.commute_pushes then (
      (*let program = inline_tags program in*)
      let program = commute_pushes program in
      let commuted_count = count_pushes program in
      if Settings.stacklang_dump then
        Printf.printf
          "Original pushes count : %d\nCommuted pushes count : %d \n"
          original_count commuted_count ;
      program )
    else program )
