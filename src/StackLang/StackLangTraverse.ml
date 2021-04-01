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

let fresh_int =
  let n = ref (-1) in
  fun () ->
    n := !n + 1 ;
    !n

let fstt (e, _, _) = e

let suffix name i = Printf.sprintf "%s_%i" name i

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
    exit 1 )

let wf_reg label rs r = wf_regs label rs (RegisterSet.singleton r)

let rec wf_value label rs v =
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
      wf_regs label rs rs' ;
      (* A [need] instruction undefines the registers that it does not
         mention, so we continue with [rs']. *)
      let rs = rs' in
      wf_block cfg label rs block
  | IPush (v, block) ->
      wf_value label rs v ;
      wf_block cfg label rs block
  | IPop (p, block) ->
      let rs = def rs p in
      wf_block cfg label rs block
  | IDef (p, v, block) ->
      wf_value label rs v ;
      let rs = def rs p in
      wf_block cfg label rs block
  | IPrim (r, p, block) ->
      wf_prim label rs p ;
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
  LabelMap.iter (wf_block program.cfg) program.cfg ;
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

let measure_t_block measure {block} =
  measure_block measure block

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

(*
let is_intersection_empty pattern value =
  not (is_intersection_non_empty pattern value) *)

let rec value_refers_to_register register value =
  match value with
  | VReg register'
    when register = register' -> true
  | VTuple li -> List.exists (value_refers_to_register register) li
  | _ -> false

let block_map f = function
    | INeed (registers, block) ->
        INeed(registers, f block)
    | IPush (value, block) ->
        IPush(value, f block)
    | IPop (register, block) ->
        IPop(register, f block)
    | IDef (pattern, value, block) ->
        IDef (pattern, value, f block)
    | IPrim (register, primitive, block) ->
        IPrim (register, primitive, f block)
    | ITrace (register, block) ->
        ITrace (register, f block)
    | IComment (comment, block) ->
        IComment (comment, f block)
    | ((IDie | IReturn _ | IJump _ | ISubstitutedJump _ ) as end_block) ->
        end_block
    | ICaseToken (reg, branches, odefault) ->
        ICaseToken ( reg
                , List.map
                    ( fun (tokpat, block) ->
                        (tokpat, f block) )
                    branches
                , Option.map f odefault )
    | ICaseTag (reg, branches) ->
        ICaseTag ( reg
                , List.map
                    ( fun (tokpat, block) ->
                        (tokpat, f block) )
                    branches )
    | ITypedBlock t_block ->
        ITypedBlock ({ t_block
                    with block = f t_block.block })


let pop_type stack_type =
  if stack_type = [||] then
    [||]
  else
    (*

    Array.sub stack_type 0 ((Array.length stack_type) - 1) *)
    let n = ref ((Array.length stack_type) - 1) in
    while
      !n >= 0
      && ( let cell = stack_type.(!n) in
           not ( cell.hold_semv
                 || cell.hold_state
                 || cell.hold_startpos
                 || cell.hold_endpos ))
    do
      n := !n - 1
    done ;
    if !n < 0 then
      [||]
    else
      Array.sub stack_type 0 !n

let pop_n_types stack_type n =
  let r = ref stack_type in
  for _=1 to n do
    r := pop_type !r
  done;
  !r

module Substitution :
sig
    type t

    (** empty substitution *)
    val empty : t

    (** [add register value s] adds a rule [register -> value] to [s]*)
    val add : register -> value -> t -> t

    (** [remove s pattern] remove every rule of the shape [r -> _] for every [r] a register occuring in [pattern] *)
    val remove : t -> pattern -> t

    (** [remove s pattern] remove every rule of the shape [r -> _] for every [r] a register occuring in [pattern] *)
    val remove_val : t -> value -> t

    (** [substitute s value] apply to rules of the substitution [s] to [value] recursively *)
    val substitute : t -> value -> value

    (** [substitute s pattern] apply the rules of the substitution [s] to [pattern] recursively.
        It assumes that every relevant rule has shape [_ -> VReg(_)] *)
    val substitute_pattern : t -> pattern -> pattern

    (** [restore_defs s block] generate a definition block with a definition [let x = y] for every rule [x -> y] in [s]*)
    val restore_defs : t -> block -> block

    (** [tight_restore_defs s registers block] generate a definition block with a definition
        [let x = y] for every rule [x -> y] in [s] such that [x] is in [registers] *)
    val tight_restore_defs : t -> registers -> block -> block
end =
struct
    type t = value RegisterMap.t

    let empty = RegisterMap.empty
    let add register value map =
        RegisterMap.add register value map
        (* match value with
        | VReg register' when register = register' -> map
        | _ -> RegisterMap.add register value map *)

    let rec remove substitution pattern =
        match pattern with
        | PReg reg -> RegisterMap.remove reg substitution
        | PWildcard -> substitution
        | PTuple li -> List.fold_left remove substitution li

    let rec remove_val substitution value =
      match value with
      | VReg reg -> RegisterMap.remove reg substitution
      | VTag _ | VUnit -> substitution
      | VTuple li -> List.fold_left remove_val substitution li
    let rec substitute substitution =
    function
    | VReg register ->
        Option.value
          (RegisterMap.find_opt register substitution)
          ~default:(VReg register)
    | VTuple li -> VTuple (List.map (substitute substitution) li)
    | v -> v

    let rec substitute_pattern substitution =
      function
      | PReg register ->
            (match RegisterMap.find_opt register substitution with
            | Some (VReg reg) -> PReg reg
            | Some _ -> failwith "Could not transform value into pattern"
            | None -> PReg register)
      | PTuple li -> PTuple ( List.map
                                (substitute_pattern substitution)
                                li )
      | v -> v

    let restore_defs substitution block =
        RegisterMap.fold
            (fun register value block ->
               IDef(PReg register, value, block) )
            substitution
            block
    let tight_restore_defs substitution registers block =
      RegisterSet.fold
          (fun register block ->
              let value = substitute substitution (VReg register) in
              if value = VReg register then
                block
              else
                IDef(PReg register, value, block) )
          registers
          block

end
let inline_tags program =
  let rec aux substitution block =
      match block with
      | IDef (PReg name, VTag tag, block) ->
          aux (Substitution.add name (VTag tag) substitution) block
      | IDef (pattern, value, block) ->
          IDef ( pattern
                , Substitution.apply substitution value
                , aux (Substitution.remove substitution pattern) block )
      | IPush (value, block) ->
          IPush ( Substitution.apply substitution value
                , aux substitution block )
      | IPrim (register, primitive, block) ->
          IPrim ( register
                , primitive
                , aux ( Substitution.remove
                          substitution
                          (PReg register) )
                      block )
      | IPop(pattern, block) ->
          IPop( pattern, aux
                            (Substitution.remove substitution pattern)
                            block )
      | IJump reg ->
            ISubstitutedJump(reg, substitution)
      | ICaseTag (reg, branches) ->
        Substitution.restore_defs
          substitution
          ( ICaseTag ( reg, List.map
                              ( fun (tagpat, block) ->
                                  tagpat, aux Substitution.empty block )
                              branches ) )
      | _ -> block_map (aux substitution) block

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
  let wt_cells block reason known_cells needed_cells =
    wt_cells_arrays block reason (Array.rev_of_list known_cells) needed_cells
  in
  (* TODO : document and make this display *)
  let _enrich_known_cells_with_state_knowledge known_cells tag =
    let state_known_cells = (lookup_tag tag states).known_cells in
    if Array.length state_known_cells > List.length known_cells then
      Array.rev_to_list state_known_cells
    else known_cells
  in
  substitution
let commute_pushes program =
    let rec aux (push_list: value list) substitution =
        (* [push_list] is the list of pushes that we need to restore,
           and hopefully cancel out with a pop.
           Every time there is a definition of a new value,
          in order to not disturb the pushes,
          if the pattern is in conflict with one of the pushed value,
          we change the name of the culprit register, and we save the change in a [Substitution.t].
          In subsequent code, every time we refer to a register,
          we use the name from the substitution's right-hand side. *)
        function
        | INeed (_registers, block) ->
           (* Deleted for now *)
            aux push_list substitution block
        | IPush (value, block) ->
            (* We push the substituted name *)
            IComment (sprintf "Commuted push of %s" (string_of_value value),
            aux
              ( ( Substitution.substitute
                    substitution
                    value ) :: push_list )
              substitution
              block)
        | IPop (pattern, block) ->
            (* A pop is a special kind of definition,
               so you may think that we need to generate new substition rules from it,
               but there is no need because we only keep the pop if there is no push that can conflict with it. *)
            (match push_list with
             | [] -> IPop (pattern, aux [] (Substitution.remove substitution pattern) block)
             | value :: push_list ->
                 IDef(pattern, value, aux push_list (Substitution.remove substitution pattern) block))
        | IDef (pattern, value', block) ->
            (* As explained above, for every conflict between the definition and a push currently commuting,
               we add a new substitution rule *)
            (* Currently, the value is raw,
               we need to apply the substitution to it in order for it to refer to the correct definition. *)
            let value' = Substitution.substitute substitution value' in
            let substitution = update_substitution substitution pattern push_list in
            IDef ( Substitution.apply_pattern substitution pattern
                 , value'
                 , aux push_list substitution block )
        | IPrim (register, primitive, block) ->
          (* A primitive is a like def except it has a simple register instead of a pattern *)
          let register' =
            if List.exists
                 (value_refers_to_register register)
                 push_list then
              suffix register (fresh_int () )
            else register
          in
          let substitution =
            Substitution.add register (VReg register') substitution
          in
          IPrim(register', primitive, aux push_list substitution block)
        | ITrace (register, block) ->
            ITrace (register, aux push_list substitution block)
        | IComment (comment, block) ->
            IComment (comment, aux push_list substitution block)
        | IDie ->
            IDie
        | IReturn r ->
            IReturn r
        | IJump j ->
            (* We first restore the pushes we failed to eliminate,
               then we restore every value to its original name,
               so that the jump does not fail. *)
            restore_pushes
              push_list
              ( Substitution.tight_restore_defs
                  substitution
                  (needed (StringMap.find j program.cfg))
                  (IJump j) )
        | ISubstitutedJump (label, substitution') ->
            (*print_string "\nMerging :"  ;
            StackLangPrinter.print_substitution stdout substitution ;
            print_string "\nwith :" ;
            StackLangPrinter.print_substitution stdout substitution' ;*)
            (* Warning/TODO : incorrect, but never used *)
            restore_pushes
              push_list
              ( ISubstitutedJump ( label
                                  , Substitution.compose
                                      substitution
                                      substitution' ) )
        | ICaseToken (reg, branches, odefault) ->
            ICaseToken ( reg
                       , List.map
                           ( function
                               (* Every [TokSingle] introduces a definition of a register. *)
                               | TokSingle (tok , register'), block ->
                                   let new_register =
                                     if List.exists
                                       (value_refers_to_register register')
                                       push_list then
                                     suffix register' (fresh_int () )
                                   else register'
                                 in
                                 let substitution =
                                   Substitution.add
                                     register'
                                     (VReg new_register)
                                     substitution
                                 in
                                 ( TokSingle(tok, new_register)
                                 , aux push_list substitution block )
                               (* [TokMultiple] does not introduce new definitions*)
                               | TokMultiple terminals, block ->
                                   ( TokMultiple terminals
                                   , aux push_list substitution block )  )
                           branches
                       , Option.map
                           (aux push_list substitution)
                           odefault )
        | ICaseTag (reg, branches) ->
            (* [ICaseTag] does not introduce new definitions *)
            ICaseTag ( reg
                     , List.map
                         ( fun (tagpat, block) ->
                             (tagpat, aux push_list substitution block) )
                         branches )
        | ITypedBlock ({stack_type} as t_block) ->
            (* We alter the type information according to the number of commuting pushes :
               Every push that is commuting removes a known stack symbol *)
            ITypedBlock ({ t_block
                           with block = aux push_list substitution t_block.block
                           ;    stack_type = Array.sub
                                               stack_type
                                               0
                                               ( max
                                                   0
                                                   ( (Array.length stack_type)
                                                   - (List.length push_list) ) ) })
        in
      { program
        with cfg =
        RegisterMap.map
            ( fun t_block -> { t_block
                               with block = aux [] Substitution.empty t_block.block } )
            program.cfg }

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
    if Settings.commute_pushes then
      ( let program = inline_tags program in
        let program = commute_pushes program in
        let commuted_count = count_pushes program in
        if Settings.stacklang_dump then
          Printf.printf
            "Original pushes count : %d\n\
            Commuted pushes count : %d \n"
            original_count commuted_count ;
        program )
    else
      program
