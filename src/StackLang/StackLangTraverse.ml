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

let exit =
  if Settings.unit_test then fun i -> failwith (string_of_int i) else exit

let stderr = if Settings.unit_test then open_out "/dev/null" else stderr

let eprintf format = fprintf stderr format

let print_context program block context =
  eprintf "\nBlock :" ;
  StackLangPrinter.print_block stderr block ;
  eprintf "\nContext :" ;
  StackLangPrinter.print_block stderr (ITypedBlock (lookup context program.cfg))

let print_context_with_states program block context =
  eprintf "\nBlock :" ;
  StackLangPrinter.print_block stderr block ;
  eprintf "\nContext :" ;
  StackLangPrinter.print_block stderr (ITypedBlock (lookup context program.cfg)) ;
  eprintf "\n" ;
  StackLangPrinter.print_states stderr program.states ;
  eprintf "\n"

let state_reg = state

(* Checking that a StackLang program contains no references to undefined
   registers. *)

let wf_regs program label block rs rs' =
  (* Check that [rs'] is a subset of [rs]. *)
  let stray = RegisterSet.diff rs' rs in
  if not (RegisterSet.is_empty stray) then (
    eprintf "StackLang: in block %s, reference to undefined register%s:\n  %s\n"
      label
      (if RegisterSet.cardinal stray > 1 then "s" else "")
      (RegisterSet.print stray) ;
    eprintf "StackLang: the following registers are defined:\n  %s\n"
      (RegisterSet.print rs) ;
    print_context program block label ;
    exit 1 )

let wf_regs_jump program label label_jump rs rs' =
  let {cfg} = program in
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

let wf_reg program label block rs r =
  wf_regs program label block rs (RegisterSet.singleton r)

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

let rec wf_block program label rs block =
  let {cfg} = program in
  match block with
  | INeed (rs', block) ->
      wf_regs program label (INeed (rs', block)) rs rs' ;
      (* A [need] instruction undefines the registers that it does not
         mention, so we continue with [rs']. *)
      let rs = rs' in
      wf_block program label rs block
  | IPush (v, _cell, block) ->
      (* TODO : check that the cell and v are in sync *)
      wf_value program label (IPush (v, _cell, block)) rs v ;
      wf_block program label rs block
  | IPop (p, block) ->
      let rs = def rs p in
      wf_block program label rs block
  | IDef (p, v, block) ->
      wf_value program label (IDef (p, v, block)) rs v ;
      let rs = def rs p in
      wf_block program label rs block
  | IPrim (r, p, block) ->
      wf_prim program label (IPrim (r, p, block)) rs p ;
      let rs = def rs (PReg r) in
      wf_block program label rs block
  | ITrace (_, block) | IComment (_, block) ->
      wf_block program label rs block
  | IDie ->
      ()
  | IReturn v ->
      wf_value program label (IReturn v) rs v
  | IJump label' ->
      (* Check that every register that is needed at the destination label
         is defined here. *)
      wf_regs_jump program label label' rs (needed (lookup label' cfg))
  | ISubstitutedJump (label', substitution) ->
      wf_regs_jump program label label' rs
        (Subst.apply_registers substitution (needed (lookup label' cfg)))
  | ICaseToken (r, branches, odefault) ->
      wf_reg program label (ICaseToken (r, branches, odefault)) rs r ;
      List.iter (wf_branch program label rs) branches ;
      Option.iter (wf_block program label rs) odefault
  | ICaseTag (r, branches) ->
      wf_reg program label (ICaseTag (r, branches)) rs r ;
      List.iter (branch_iter (wf_block program label rs)) branches
  | ITypedBlock ({block; needed_registers= rs'} as t_block) ->
      wf_regs program label (ITypedBlock t_block) rs rs' ;
      (* A [need] instruction undefines the registers that it does not
         mention, so we continue with [rs']. *)
      let rs = rs' in
      wf_block program label rs block

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

let wf_t_block program label {block; needed_registers} =
  wf_block program label needed_registers block

(* [wf program] checks that the program [program] contains no references to
   undefined registers. *)

let wf program =
  Program.iter (wf_t_block program) program ;
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

let _value_refers_to_state value = RegisterSet.mem state (value_registers value)

let get_args_map block_map =
  StringMap.map
    (function
      | INeed (registers, _block) ->
          StringSet.elements registers
      | _ ->
          assert false)
    block_map

let wt_knowncells_tblock program label t_block =
  let cfg = program.cfg in
  let states = program.states in
  let check_cells block reason known_cells needed_cells =
    let fail message =
      eprintf "\nWhile checking %s, in block %s : %s\n" reason label message ;
      eprintf "Known cells :" ;
      StackLangPrinter.print_known_cells stderr known_cells ;
      eprintf "\nNeeded cells :" ;
      StackLangPrinter.print_known_cells stderr needed_cells ;
      print_context_with_states program block label ;
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
  (*
  [known_cells]: cells we can pop and pass through a typed_block and a jump
  [extra_known_cells]: when we match on a state, known_cells become
    [extra_known_cells @ state_known_cells]
    TODO : better description of what the information is, instead of what it does
  *)
  let rec wtkc_block (known_cells : cell_info array)
      (extra_known_cells : cell_info array) state block =
    Block.iter_unit
      (wtkc_block known_cells extra_known_cells state)
      ~push:(fun _value cell block ->
        wtkc_block
          (Array.push known_cells cell)
          (Array.push extra_known_cells cell)
          state block)
      ~pop:(fun pattern block ->
        if known_cells = [||] then exit 1 ;
        let known_cells = Array.pop known_cells in
        (* If the state is shadowed, then it becomes unknown. *)
        let state = if pattern_shadow_state pattern then None else state in
        wtkc_block known_cells [||] state block)
      ~def:(fun pattern value block ->
        let state, extra_known_cells =
          match detect_def_state pattern value with
          | None ->
              (state, extra_known_cells)
          | Some tag ->
              (Some tag, [||])
        in
        wtkc_block known_cells extra_known_cells state block)
      ~jump:(fun label ->
        let target = lookup label cfg in
        ( if RegisterSet.mem state_reg target.needed_registers then
          (* This check that the stack is compatible with the state we are passing
             to the routine. This is only needed if we are actually passing a
             state*)
          (* assert (extra_known_cells = [||]) ; *)
          match state with
          | Some tag ->
              check_cells block "state sync" known_cells
                (lookup_tag tag states).known_cells ;
              check_cells block "state sync" (lookup_tag tag states).known_cells
                target.stack_type
          | None ->
              (* If the state is unknown, we cannot check that the stack is
                 compatible with it. *)
              () ) ;
        (* We check that the stack has at least the amount of cells the target
           routine expects *)
        check_cells block "jump" known_cells target.stack_type)
      ~substituted_jump:(fun label subst ->
        (* Same as above, except that there are multiple ways for the state to
           have a value. *)
        let target = lookup label cfg in
        ( if RegisterSet.mem state_reg target.needed_registers then
          match (Subst.apply subst (VReg state_reg), state) with
          | VTag tag, _ | _, Some tag ->
              check_cells block "state sync" known_cells
                (lookup_tag tag states).known_cells ;
              check_cells block "subst state sync"
                (lookup_tag tag states).known_cells target.stack_type
          | _, None ->
              () ) ;
        check_cells block "subst jump" known_cells target.stack_type)
      ~case_tag:(fun _reg branches ->
        let branch_aux (TagMultiple taglist, block) =
          (* By matching on the state, we discover state information.
             We can enrich the known cells with theses. *)
          let known_cells =
            Array.append (state_info_intersection states taglist).known_cells
              extra_known_cells
          in
          (* We are matching on a state, therefore state is always needed,
             and we can discard these values. *)
          wtkc_block known_cells extra_known_cells state block
        in
        List.iter branch_aux branches)
      ~typed_block:(fun {block= block'; stack_type} ->
        (* We check that the stack has at least the number of known cells that
           the type annotation expect. *)
        check_cells block "typed block" known_cells stack_type ;
        (* Inside the typed block, we are only allowed to use the cell from the
           annotation. *)
        let known_cells = stack_type in
        wtkc_block known_cells extra_known_cells state block')
      block
  in
  let {block; stack_type} = t_block in
  wtkc_block stack_type [||] None block

let well_final_typed_tblock program label t_block =
  let {cfg; states} = program in
  let check_final_types reason computed_final_type expected_final_type block =
    let fail () =
      eprintf "\nWhile checking %s, in block %s : final types differ" reason
        label ;
      eprintf "\nComputed final type : " ;
      ( match computed_final_type with
      | None ->
          eprintf "None"
      | Some _typ ->
          eprintf "Some" ) ;
      eprintf "\nExpected final type : " ;
      ( match expected_final_type with
      | None ->
          eprintf "None"
      | Some _typ ->
          eprintf "Some" ) ;
      print_context_with_states program block label ;
      StackLangPrinter.print_states stderr program.states ;
      eprintf "\n" ;
      exit 1
    in
    if computed_final_type <> expected_final_type then fail ()
  in
  let rec wft_block final_type block =
    Block.iter_unit (wft_block final_type)
      ~return:(fun _value ->
        match final_type with None -> exit 1 | Some _ -> ())
      ~jump:(fun label ->
        let target = lookup label cfg in
        match target.final_type with
        | Some _ as final_type' ->
            check_final_types "jump" final_type final_type' block
        | None ->
            ())
      ~substituted_jump:(fun label _subst ->
        let target = lookup label cfg in
        match target.final_type with
        | Some _ as final_type' ->
            check_final_types "subst jump" final_type final_type' block
        | None ->
            ())
      ~case_tag:(fun _reg branches ->
        let branch_aux (TagMultiple taglist, block) =
          (* By matching on the state, we discover state information.
             We can enrich the known cells with theses. *)
          let final_type =
            (state_info_intersection states taglist).sfinal_type
          in
          (* We are matching on a state, therefore state is always needed,
             and we can discard these values. *)
          wft_block final_type block
        in
        List.iter branch_aux branches)
      ~typed_block:(fun {block= block'; final_type= final_type'} ->
        ( match final_type' with
        | Some _ ->
            check_final_types "typed block" final_type final_type' block
        | None ->
            () ) ;
        wft_block final_type' block')
      block
  in
  let {block; final_type} = t_block in
  wft_block final_type block

let wt program =
  Program.iter (wt_knowncells_tblock program) program ;
  Program.iter (well_final_typed_tblock program) program

(* -------------------------------------------------------------------------- *)

let ( => ) pat block = (TagMultiple pat, block)

let ( := ) pat value = Block.def pat value

open Block

let good1 =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type= [||]
          ; final_type= None
          ; has_case_tag= true
          ; name= None
          ; needed_registers= RegisterSet.of_list [state_reg]
          ; block=
              Block.(
                case_tag state_reg
                  [ [0]
                    => pop (PTuple [PReg "b"; PReg state_reg])
                       @@ pop (PReg "a") @@ die
                  ; [1] => pop (PReg "c") @@ die
                  ; [2] => die ]) } )
      ; ( "g"
        , { stack_type= [|a_cell|]
          ; final_type= None
          ; has_case_tag= false
          ; name= None
          ; needed_registers= RegisterSet.empty
          ; block=
              Block.(
                (PReg state_reg := VTag 0)
                @@ (PReg "b" := VUnit)
                @@ push (VReg "b") b_cell @@ jump "f") } ) ]
  in
  let states =
    TagMap.of_list
      [ (0, {sfinal_type= None; known_cells= [|a_cell; b_cell|]})
      ; (1, {sfinal_type= None; known_cells= [|c_cell|]})
      ; (2, {sfinal_type= None; known_cells= [||]}) ]
  in
  {cfg; entry= StringMap.empty; states}

let bad_sync =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type= [|b_cell|]
          ; final_type= None
          ; has_case_tag= true
          ; name= None
          ; needed_registers= RegisterSet.of_list [state_reg]
          ; block=
              case_tag state_reg
                [ [0]
                  => pop (PTuple [PReg "b"; PReg state_reg])
                     @@ pop (PReg "a") @@ die
                ; [1] => pop (PReg "c") @@ die
                ; [2] => die ] } )
      ; ( "g"
        , { stack_type= [||]
          ; final_type= None
          ; has_case_tag= false
          ; name= None
          ; needed_registers= RegisterSet.empty
          ; block=
              (PReg state_reg := VTag 0)
              @@ (PReg "b" := VUnit)
              @@ push (VReg "b") b_cell @@ jump "f" } ) ]
  in
  let states =
    TagMap.of_list
      [ (0, {sfinal_type= None; known_cells= [|a_cell; b_cell|]})
      ; (1, {sfinal_type= None; known_cells= [|c_cell|]})
      ; (2, {sfinal_type= None; known_cells= [||]}) ]
  in
  {cfg; entry= StringMap.empty; states}

let bad_pop =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type= [||]
          ; final_type= None
          ; has_case_tag= true
          ; name= None
          ; needed_registers= RegisterSet.of_list [state_reg]
          ; block=
              case_tag state_reg
                [ [0]
                  => pop (PTuple [PReg "b"; PReg state_reg])
                     @@ pop (PReg "a") @@ pop (PReg "c") @@ die
                ; [1] => pop (PReg "c") die
                ; [2] => die ] } )
      ; ( "g"
        , { stack_type= [||]
          ; final_type= None
          ; has_case_tag= false
          ; name= None
          ; needed_registers= RegisterSet.empty
          ; block=
              (PReg state_reg := VTag 0)
              @@ (PReg "b" := VUnit)
              @@ push (VReg "b") b_cell @@ jump "f" } ) ]
  in
  let states =
    TagMap.of_list
      [ (0, {sfinal_type= None; known_cells= [|a_cell; b_cell|]})
      ; (1, {sfinal_type= None; known_cells= [|c_cell|]})
      ; (2, {sfinal_type= None; known_cells= [||]}) ]
  in
  {cfg; entry= StringMap.empty; states}

let bad_final =
  let a_type = Stretch.Inferred "a" in
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let cfg =
    LabelMap.of_list
      [ ( "g"
        , { stack_type= [||]
          ; final_type= Some a_type
          ; has_case_tag= false
          ; name= None
          ; needed_registers= RegisterSet.empty
          ; block=
              Block.(
                def (PReg state_reg) (VTag 0)
                @@ def (PReg "b") VUnit @@ push (VReg "b") b_cell
                @@ typed_block [||]
                     (RegisterSet.of_list [state_reg])
                     true
                     (case_tag state_reg
                        [ [0]
                          => pop (PTuple [PReg "b"; PReg state_reg])
                             @@ pop (PReg "a") @@ return (VReg "a")
                        ; [1] => pop (PReg "c") die
                        ; [2] => die ])) } ) ]
  in
  let states =
    TagMap.of_list
      [ (0, {sfinal_type= None; known_cells= [|a_cell; b_cell|]})
      ; (1, {sfinal_type= None; known_cells= [|c_cell|]})
      ; (2, {sfinal_type= None; known_cells= [||]}) ]
  in
  {cfg; entry= StringMap.empty; states}

let bad_final_2 =
  let a_type = Stretch.Inferred "a" in
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type= [||]
          ; final_type= Some a_type
          ; has_case_tag= false
          ; name= None
          ; needed_registers= RegisterSet.singleton state_reg
          ; block=
              case_tag state_reg
                [ [0]
                  => typed_block [||]
                       (RegisterSet.of_list [state_reg])
                       false
                       ((PReg "r" := VUnit) @@ return (VReg "r")) ] } ) ]
  in
  let states =
    TagMap.of_list
      [ (0, {sfinal_type= Some a_type; known_cells= [|a_cell; b_cell|]})
      ; (1, {sfinal_type= None; known_cells= [|c_cell|]})
      ; (2, {sfinal_type= None; known_cells= [||]}) ]
  in
  {cfg; entry= StringMap.empty; states}

let bad_final_3 =
  let a_type = Stretch.Inferred "a" in
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type= [||]
          ; final_type= Some a_type
          ; has_case_tag= true
          ; name= None
          ; needed_registers= RegisterSet.of_list [state_reg]
          ; block= return VUnit } )
      ; ( "g"
        , { stack_type= [||]
          ; final_type= None
          ; has_case_tag= false
          ; name= None
          ; needed_registers= RegisterSet.empty
          ; block=
              case_tag state_reg
                [ [0]
                  => typed_block [||]
                       (RegisterSet.of_list [state_reg])
                       false (jump "f") ] } ) ]
  in
  let states =
    TagMap.of_list
      [ (0, {sfinal_type= Some a_type; known_cells= [|a_cell; b_cell|]})
      ; (1, {sfinal_type= None; known_cells= [|c_cell|]})
      ; (2, {sfinal_type= None; known_cells= [||]}) ]
  in
  {cfg; entry= StringMap.empty; states}

let bad_final_4 =
  let a_type = Stretch.Inferred "a" in
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type= [||]
          ; final_type= Some a_type
          ; has_case_tag= true
          ; name= None
          ; needed_registers= RegisterSet.of_list [state_reg]
          ; block= return VUnit } )
      ; ( "g"
        , { stack_type= [||]
          ; final_type= None
          ; has_case_tag= false
          ; name= None
          ; needed_registers= RegisterSet.empty
          ; block= jump "f" } ) ]
  in
  let states =
    TagMap.of_list
      [ (0, {sfinal_type= Some a_type; known_cells= [|a_cell; b_cell|]})
      ; (1, {sfinal_type= None; known_cells= [|c_cell|]})
      ; (2, {sfinal_type= None; known_cells= [||]}) ]
  in
  {cfg; entry= StringMap.empty; states}

let test_wt () =
  wt good1 ;
  (match wt bad_pop with exception Failure _ -> () | () -> assert false) ;
  (match wt bad_sync with exception Failure _ -> () | () -> assert false) ;
  (match wt bad_final with exception Failure _ -> () | () -> assert false) ;
  (match wt bad_final_2 with exception Failure _ -> () | () -> assert false) ;
  (match wt bad_final_4 with exception Failure _ -> () | () -> assert false) ;
  (match wt bad_final_3 with exception Failure _ -> () | () -> assert false) ;
  ()

let test () = test_wt ()
