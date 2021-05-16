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
open NamingConventions

(* -------------------------------------------------------------------------- *)

type error =
  { context : label
  ; culprit : block
  ; message : string
  ; state_relevance : bool
  }

exception StackLangError of error

let fail ?(state_relevance = false) context culprit message =
  raise (StackLangError { context; culprit; message; state_relevance })


let state_reg = state

(* Checking that a StackLang program contains no references to undefined
   registers. *)

let wf_regs label culprit rs rs' =
  (* Check that [rs'] is a subset of [rs]. *)
  let stray = RegisterSet.diff rs' rs in
  if not (RegisterSet.is_empty stray)
  then
    let message =
      sprintf
        "StackLang: in block %s, reference to undefined register%s:\n\
        \  %s\n\
         StackLang: the following registers are defined:\n\
        \  %s\n"
        label
        (if RegisterSet.cardinal stray > 1 then "s" else "")
        (RegisterSet.print stray)
        (RegisterSet.print rs)
    in
    fail label culprit message


let wf_reg label block rs r = wf_regs label block rs (RegisterSet.singleton r)

let wf_tag label culprit states tag =
  match TagMap.find_opt tag states with
  | None ->
      fail ~state_relevance:true label culprit "reference to unrepresented tag"
  | Some _ ->
      ()


let rec wf_value program label culprit rs v =
  match v with
  | VTag tag ->
      wf_tag label culprit program.states tag
  | VReg r ->
      wf_reg label culprit rs r
  | VTuple vs ->
      List.iter (wf_value program label culprit rs) vs
  | VUnit ->
      ()


let rec def rs p =
  match p with
  | PWildcard ->
      rs
  | PReg r ->
      (* Check that no name is bound twice by a pattern. *)
      assert (not (RegisterSet.mem r rs));
      RegisterSet.add r rs
  | PTuple ps ->
      List.fold_left def rs ps


let def rs p =
  (* The newly defined registers are the previously defined registers
     plus the registers defined by the pattern [p]. *)
  RegisterSet.union rs (def RegisterSet.empty p)


let wf_prim program label block rs p =
  match p with
  | PrimOCamlCall (_, args) ->
      List.iter (wf_value program label block rs) args
  | PrimOCamlFieldAccess (v, _) ->
      wf_value program label block rs v
  | PrimOCamlDummyPos ->
      ()
  | PrimOCamlAction (bindings, action) ->
      wf_regs label block rs (Bindings.codomain bindings);
      let rs = RegisterSet.union rs (Bindings.domain bindings) in
      wf_regs label block rs (Action.vars action)


(* [wf_block cfg label rs block] checks that the block [block] does not refer
   to an undefined register, under the assumption that the registers [rs] are
   initially defined. The control flow graph [cfg] is used to map labels to
   blocks. [label] is the label of the current block and is used only as part
   of error messages. *)

let rec wf_block program label rs block =
  let { cfg } = program in
  let wf_regs = wf_regs label block rs in
  let wf_value = wf_value program label block rs in
  let wf_prim = wf_prim program label block rs in
  match block with
  | INeed (rs', block) ->
      wf_regs rs';
      (* A [need] instruction undefines the registers that it does not
         mention, so we continue with [rs']. *)
      let rs = rs' in
      wf_block program label rs block
  | IPush (v, _cell, block) ->
      (* TODO : check that the cell and v are in sync *)
      wf_value v;
      wf_block program label rs block
  | IPop (p, block) ->
      let rs = def rs p in
      wf_block program label rs block
  | IDef (bindings, block) ->
      let values = Bindings.values bindings in
      List.iter wf_value values;
      let rs = RegisterSet.union (Bindings.domain bindings) rs in
      wf_block program label rs block
  | IPrim (r, p, block) ->
      wf_prim p;
      let rs = RegisterSet.add r rs in
      wf_block program label rs block
  | ITrace (_, block) | IComment (_, block) ->
      wf_block program label rs block
  | IDie ->
      ()
  | IReturn v ->
      wf_value v
  | IJump label' ->
      (* Check that every register that is needed at the destination label
         is defined here. *)
      wf_regs (needed (lookup label' cfg))
  | ICaseToken (r, branches, odefault) ->
      wf_reg label (ICaseToken (r, branches, odefault)) rs r;
      List.iter (wf_branch program label rs) branches;
      Option.iter (wf_block program label rs) odefault
  | ICaseTag (r, branches) ->
      wf_reg label (ICaseTag (r, branches)) rs r;
      List.iter
        (fun (TagMultiple tags, _) ->
          List.iter (wf_tag label block program.states) tags )
        branches;
      List.iter (branch_iter (wf_block program label rs)) branches
  | ITypedBlock { block; needed_registers = rs' } ->
      wf_regs rs';
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

let wf_routine program label { block; needed_registers } =
  wf_block program label needed_registers block


(* [wf program] checks that the program [program] contains no references to
   undefined registers. *)

let wf program =
  Program.iter (wf_routine program) program;
  Time.tick "Checking the StackLang code for well-formedness"


(* -------------------------------------------------------------------------- *)

(* Measuring the size of a StackLang program. *)

type measure =
  { mutable push : int
  ; mutable pop : int
  ; mutable def : int
  ; mutable prim : int
  ; mutable trace : int
  ; mutable die : int
  ; mutable return : int
  ; mutable jump : int
  ; mutable casetoken : int
  ; mutable casetag : int
  ; mutable total : int
  }

let zero () =
  { push = 0
  ; pop = 0
  ; def = 0
  ; prim = 0
  ; trace = 0
  ; die = 0
  ; return = 0
  ; jump = 0
  ; casetoken = 0
  ; casetag = 0
  ; total = 0
  }


let print m =
  let pad i = Misc.padded_index m.total i in
  printf "PUSH    %s\n" (pad m.push);
  printf "POP     %s\n" (pad m.pop);
  printf "DEF     %s\n" (pad m.def);
  printf "PRIM    %s\n" (pad m.prim);
  printf "TRCE    %s\n" (pad m.trace);
  printf "DIE     %s\n" (pad m.die);
  printf "RET     %s\n" (pad m.return);
  printf "JUMP    %s\n" (pad m.jump);
  printf "CASEtok %s\n" (pad m.casetoken);
  printf "CASEtag %s\n" (pad m.casetag);
  printf "total   %s\n" (pad m.total);
  ()


let rec measure_block m block =
  match block with
  | INeed (_, block) ->
      measure_block m block
  | IPush (_, _, block) ->
      m.total <- m.total + 1;
      m.push <- m.push + 1;
      measure_block m block
  | IPop (_, block) ->
      m.total <- m.total + 1;
      m.pop <- m.pop + 1;
      measure_block m block
  | IDef (_, block) ->
      m.total <- m.total + 1;
      m.def <- m.def + 1;
      measure_block m block
  | IPrim (_, _, block) ->
      m.total <- m.total + 1;
      m.prim <- m.prim + 1;
      measure_block m block
  | ITrace (_, block) ->
      m.total <- m.total + 1;
      m.trace <- m.trace + 1;
      measure_block m block
  | IComment (_, block) ->
      measure_block m block
  | IDie ->
      m.total <- m.total + 1;
      m.die <- m.die + 1
  | IReturn _ ->
      m.total <- m.total + 1;
      m.return <- m.return + 1
  | IJump _ ->
      m.total <- m.total + 1;
      m.jump <- m.jump + 1
  | ICaseToken (_, branches, odefault) ->
      m.total <- m.total + 1;
      m.casetoken <- m.casetoken + 1;
      List.iter (branch_iter (measure_block m)) branches;
      Option.iter (measure_block m) odefault
  | ICaseTag (_, branches) ->
      m.total <- m.total + 1;
      m.casetag <- m.casetag + 1;
      List.iter (branch_iter (measure_block m)) branches
  | ITypedBlock { block } ->
      measure_block m block


let measure_t_block measure { block } = measure_block measure block

let measure program =
  let m = zero () in
  Program.iter (fun _ -> measure_t_block m) program;
  m


(* -------------------------------------------------------------------------- *)
(* Utility functions used below. *)

let rec pattern_shadow_state = function
  | PReg reg when reg = state ->
      true
  | PTuple li ->
      List.exists pattern_shadow_state li
  | _ ->
      false


let _value_refers_to_state value = RegisterSet.mem state (Value.registers value)

let get_args_map block_map =
  StringMap.map
    (function
      | INeed (registers, _block) ->
          StringSet.elements registers
      | _ ->
          assert false )
    block_map


(* -------------------------------------------------------------------------- *)
(* Well-typedness with regard to knowledge of stack cells. *)

type sync =
  | Synced of int
  | Unsynced of tag

let rec get_state_value sync = function
  | VReg reg when reg = state ->
    (match sync with Synced _ -> None | Unsynced tag -> Some tag)
  | VTag tag ->
      Some tag
  | VTuple li ->
      Option.first_value (List.map (get_state_value sync) li)
  | _ ->
      None


let update_sync_with_bindings bindings sync =
  match Bindings.apply bindings (VReg state_reg) with
  | VTag tag ->
      Unsynced tag
  | VReg reg when reg = state_reg ->
      sync
  | _ ->
      assert false


let fail_sync label block =
  let message =
    sprintf
      "While checking that there is no match on an out of sync state, in block \
       %s."
      label
  in
  fail label block message


let check_cells label block reason known_cells needed_cells =
  let fail message =
    let message =
      sprintf
        "While checking %s, in block %s : %s\n\
         Known cells : %s\n\
         Needed cells : %s"
        reason
        label
        message
        (StackLangPrinter.known_cells_to_string known_cells)
        (StackLangPrinter.known_cells_to_string needed_cells)
    in
    fail ~state_relevance:true label block message
  in
  let l1 = Array.length known_cells in
  let l2 = Array.length needed_cells in
  if l1 < l2
  then fail @@ sprintf "Could discover %i known cells, but %i are needed." l1 l2;
  for i = 1 to l2 do
    if known_cells.(l1 - i) <> needed_cells.(l2 - i)
    then fail @@ sprintf "Enough cells are known, but their content differ."
  done


let check_state_sync program label block known_cells expected_cells tag =
  let states = program.states in
  (* Check that we have enough cells to assert that the state is  *)
  check_cells
    label
    block
    "state sync"
    known_cells
    (lookup_tag tag states).known_cells;
  check_cells
    label
    block
    "state sync"
    (lookup_tag tag states).known_cells
    expected_cells


let check_state_sync program label block known_cells expected_cells = function
  | Unsynced tag ->
      check_state_sync program label block known_cells expected_cells tag
  | Synced n when n <> 0 ->
      fail
        ~state_relevance:true
        label
        block
        (sprintf
           "State in sync with %i deep cell when jumping or going in a typed \
            block."
           n )
  | _ ->
      ()


let wt_knowncells_routine program label (t_block : typed_block) =
  let cfg = program.cfg in
  let states = program.states in

  (*
  [known_cells]: cells we can pop and pass through a typed_block and a jump
  [extra_known_cells]: Cells on top of the information carried by the state.
  They are extra in the sense that when we match on a state, known_cells become
    [Array.append state_known_cells extra_known_cells] *)
  let rec wtkc_block (known_cells : cell_info array) (sync : sync) block =
    Block.iter_unit
      (wtkc_block known_cells sync)
      ~push:(fun value cell block ->
        ( if cell.hold_state
        then
          match get_state_value sync value with
          | None ->
              ()
          | Some tag ->
              let tag_type = (lookup_tag tag states).known_cells in
              check_cells label block "push" known_cells tag_type );
        let sync =
          match sync with Synced n -> Synced (n + 1) | sync -> sync
        in
        wtkc_block (MArray.push known_cells cell) sync block )
      ~pop:(fun pattern block' ->
        ( if known_cells = [||]
        then
          let message = "Tried to pop unknown cell" in
          fail label block message );
        let known_cells = MArray.pop known_cells in
        (* If the state is shadowed, then it becomes unknown. *)
        let sync = if pattern_shadow_state pattern then Synced 0 else sync in
        wtkc_block known_cells sync block' )
      ~def:(fun bindings block ->
        (* If the state variable is redefined, then matching on the state will
           not give any new information. Therefore, [extra_known_cells]
           becomes empty. However there is an issue : it is possible to trick
           this function into not raising an error when it should. *)
        let sync = update_sync_with_bindings bindings sync in
        wtkc_block known_cells sync block )
      ~jump:(fun label ->
        let target = lookup label cfg in
        if RegisterSet.mem state_reg target.needed_registers
        then
          (* This checks that the stack is compatible with the state we are
             passing to the routine. This is only needed if we are actually
             passing a state *)
          check_state_sync
            program
            label
            block
            known_cells
            target.stack_type
            sync;
        (* We check that the stack has at least the amount of cells the target
           routine expects. *)
        check_cells label block "jump" known_cells target.stack_type )
      ~case_tag:(fun _reg branches ->
        match sync with
        | Synced n ->
            let branch_aux (TagMultiple taglist, block) =
              (* By matching on the state, we discover state information.
                 We can enrich the known cells with this information. *)
              let known_cells =
                Array.append
                  (state_info_intersection states taglist).known_cells
                  (MArray.suffix known_cells n)
              in
              (* We are matching on a state, therefore state is always needed,
                 and we can discard these values. *)
              wtkc_block known_cells sync block
            in
            List.iter branch_aux branches
        | Unsynced _tag ->
            fail_sync label block )
      ~typed_block:(fun { block = block'; stack_type; needed_registers } ->
        (* We check that the stack has at least the number of known cells that
           the type annotation requires. *)
        check_cells label block "typed block" known_cells stack_type;
        if RegisterSet.mem state_reg needed_registers
        then check_state_sync program label block known_cells stack_type sync;
        (* Inside the typed block, we are only allowed to use the cell from the
           annotation. *)
        let known_cells = stack_type in
        wtkc_block known_cells (Synced 0) block' )
      block
  in
  let { block; stack_type } = t_block in
  wtkc_block stack_type (Synced 0) block


let well_known_cells_typed program =
  Program.iter (wt_knowncells_routine program) program


(* -------------------------------------------------------------------------- *)
(* Well-typedness with regard to knowledge of the final type. *)

let check_final_types label reason computed_final_type expected_final_type block
    =
  let fail () =
    let message =
      sprintf
        "While checking %s, in block %s : final types differ\n\
         Computed final type : %s\n\
         Expected final type : %s"
        reason
        label
        (match computed_final_type with None -> "None" | Some _typ -> "Some")
        (match expected_final_type with None -> "None" | Some _typ -> "Some")
    in
    fail ~state_relevance:true label block message
  in
  if computed_final_type <> expected_final_type then fail ()


(* The rules for being well-typed with regard to the final type are simple :
     - When we return, the final type must be known.
     - When we jump or enter a typed block, we can lose knowledge of the final
       type, but not gain it.
     - We can gain knowledge of the final type either by having it at the start
       of the block, or with a case-tag. *)
let rec wft_block program label final_type block =
  let { cfg; states } = program in
  let check_final_types = check_final_types label in
  Block.iter_unit
    (wft_block program label final_type)
    ~return:(fun _value ->
      match final_type with
      | None ->
          fail label block "Tried to return with final type unknown"
      | Some _ ->
          () )
    ~jump:(fun label ->
      let target = lookup label cfg in
      match target.final_type with
      | Some _ as final_type' ->
          check_final_types "jump" final_type final_type' block
      | None ->
          () )
    ~case_tag:(fun _reg branches ->
      let branch_aux (TagMultiple taglist, block) =
        (* By matching on the state, we discover state information.
           We can enrich the known cells with theses. *)
        let final_type = (state_info_intersection states taglist).sfinal_type in
        (* We are matching on a state, therefore state is always needed,
           and we can discard these values. *)
        wft_block program label final_type block
      in
      List.iter branch_aux branches )
    ~typed_block:(fun { block = block'; final_type = final_type' } ->
      ( match final_type' with
      | Some _ ->
          check_final_types "typed block" final_type final_type' block
      | None ->
          () );
      wft_block program label final_type' block' )
    block


let well_final_typed_routine program label (t_block : typed_block) =
  let { block; final_type } = t_block in
  wft_block program label final_type block


let well_final_typed program =
  Program.iter (well_final_typed_routine program) program


(* -------------------------------------------------------------------------- *)

let wt program =
  well_known_cells_typed program;
  well_final_typed program


(* -------------------------------------------------------------------------- *)
(* Testing wt. *)

(* These two infix functions are used to build example programs more easily. *)
let ( => ) pat block = (TagMultiple pat, block)

let ( := ) pat value = Block.sdef pat value

open Block

let t0 = tag_of_int 0

let t1 = tag_of_int 1

let t2 = tag_of_int 2

let good1 =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = None; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = true
          ; name = None
          ; needed_registers = RegisterSet.of_list [ state_reg ]
          ; block =
              case_tag
                state_reg
                [ [ t0 ]
                  => pop (PTuple [ PReg "b"; PReg state_reg ])
                     @@ pop (PReg "a")
                     @@ die
                ; [ t1 ] => pop (PReg "c") @@ die
                ; [ t2 ] => die
                ]
          } )
      ; ( "g"
        , { stack_type = [| a_cell |]
          ; final_type = None
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block =
              (PReg state_reg := VTag t0)
              @@ (PReg "b" := VUnit)
              @@ push (VReg "b") b_cell
              @@ jump "f"
          } )
      ]
  in
  { cfg; entry = StringMap.empty; states }


let good2 =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = None; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell; b_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [| b_cell |]
          ; final_type = None
          ; has_case_tag = true
          ; name = None
          ; needed_registers = RegisterSet.of_list [ state_reg ]
          ; block = pop (PTuple [ PReg "b"; PReg state_reg ]) @@ die
          } )
      ; ( "g"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block = case_tag state_reg [ [ t0; t1 ] => jump "f" ]
          } )
      ]
  in
  { cfg; entry = StringMap.empty; states }


let good_sync =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = None; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = true
          ; name = None
          ; needed_registers = RegisterSet.of_list [ state_reg ]
          ; block =
              case_tag
                state_reg
                [ [ t0 ]
                  => pop (PTuple [ PReg "b"; PReg state_reg ])
                     @@ pop (PReg "a")
                     @@ die
                ; [ t1 ] => pop (PReg "c") @@ die
                ; [ t2 ] => die
                ]
          } )
      ; ( "g"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block =
              (PReg state_reg := VTag t0)
              @@ (PReg "b" := VUnit)
              @@ push VUnit a_cell
              @@ push (VReg "b") b_cell
              @@ typed_block
                   [||]
                   (RegisterSet.singleton state_reg)
                   false
                   (jump "f")
          } )
      ]
  in
  { cfg; entry = StringMap.empty; states }


(* This is ill-typed because when we match on the state, the value of the state
   is a value we just made-up, and therefore it does not give us any information
   on the stack. *)
let bad_shadow_state =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = None; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = true
          ; name = None
          ; needed_registers = RegisterSet.of_list [ state_reg ]
          ; block =
              (PReg state_reg := VTag t0)
              @@ case_tag
                   state_reg
                   [ [ t0 ]
                     => pop (PTuple [ PReg "b"; PReg state_reg ])
                        @@ pop (PReg "a")
                        @@ die
                   ; [ t1 ] => pop (PReg "c") @@ die
                   ; [ t2 ] => die
                   ]
          } )
      ; ( "g"
        , { stack_type = [| a_cell |]
          ; final_type = None
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block =
              (PReg state_reg := VTag t0)
              @@ (PReg "b" := VUnit)
              @@ push (VReg "b") b_cell
              @@ jump "f"
          } )
      ]
  in
  { cfg; entry = StringMap.empty; states }


(* Illegal because of the [jump f] in [g] : passing state [0] requires knowing
   2 cells, and only one is known.   *)
let bad_sync =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = None; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [| b_cell |]
          ; final_type = None
          ; has_case_tag = true
          ; name = None
          ; needed_registers = RegisterSet.of_list [ state_reg ]
          ; block =
              case_tag
                state_reg
                [ [ t0 ]
                  => pop (PTuple [ PReg "b"; PReg state_reg ])
                     @@ pop (PReg "a")
                     @@ die
                ; [ t1 ] => pop (PReg "c") @@ die
                ; [ t2 ] => die
                ]
          } )
      ; ( "g"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block =
              (PReg state_reg := VTag t0)
              @@ (PReg "b" := VUnit)
              @@ push (VReg "b") b_cell
              @@ jump "f"
          } )
      ]
  in
  { cfg; entry = StringMap.empty; states }


(* Illegal because of the [pop c] in [f] *)
let bad_pop =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = None; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = true
          ; name = None
          ; needed_registers = RegisterSet.of_list [ state_reg ]
          ; block =
              case_tag
                state_reg
                [ [ t0 ]
                  => pop (PTuple [ PReg "b"; PReg state_reg ])
                     @@ pop (PReg "a")
                     @@ pop (PReg "c")
                     @@ die
                ; [ t1 ] => pop (PReg "c") die
                ; [ t2 ] => die
                ]
          } )
      ; ( "g"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block =
              (PReg state_reg := VTag t0)
              @@ (PReg "b" := VUnit)
              @@ push (VReg "b") b_cell
              @@ jump "f"
          } )
      ]
  in
  { cfg; entry = StringMap.empty; states }


let bad_push =
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = None; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block = push (VTuple [ VReg "b"; VTag t0 ]) b_cell @@ die
          } )
      ]
  in
  { cfg; entry = StringMap.empty; states }


(* Illegal because we return in [g] despite the fact that we do not know the
   final type. *)
let bad_final =
  let a_type = Stretch.Inferred "a" in
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = None; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  let cfg =
    LabelMap.of_list
      [ ( "g"
        , { stack_type = [||]
          ; final_type = Some a_type
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block =
              (PReg state_reg := VTag t0)
              @@ (PReg "b" := VUnit)
              @@ push (VReg "b") b_cell
              @@ typed_block
                   [||]
                   (RegisterSet.of_list [ state_reg ])
                   true
                   (case_tag
                      state_reg
                      [ [ t0 ]
                        => pop (PTuple [ PReg "b"; PReg state_reg ])
                           @@ pop (PReg "a")
                           @@ return (VReg "a")
                      ; [ t1 ] => pop (PReg "c") die
                      ; [ t2 ] => die
                      ] )
          } )
      ]
  in
  { cfg; entry = StringMap.empty; states }


(* Illegal because in the [t0] branch, we gain knowledge of the final type, but
   this knowledge is discarded by the typed block that follows and then we
   return, which requires the discarded knowledge. *)
let bad_final_2 =
  let a_type = Stretch.Inferred "a" in
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = Some a_type; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [||]
          ; final_type = Some a_type
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.singleton state_reg
          ; block =
              case_tag
                state_reg
                [ [ t0 ]
                  => typed_block
                       [||]
                       (RegisterSet.of_list [ state_reg ])
                       false
                       ((PReg "r" := VUnit) @@ return (VReg "r"))
                ]
          } )
      ]
  in
  { cfg; entry = StringMap.empty; states }


(* Illegal because in the [t0] branch of [g], we gain knowledge of the final type, but
   this knowledge is discarded by the typed block that follows and then we
   jump to [f], which requires the discarded knowledge. *)
let bad_final_3 =
  let a_type = Stretch.Inferred "a" in
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [||]
          ; final_type = Some a_type
          ; has_case_tag = true
          ; name = None
          ; needed_registers = RegisterSet.of_list [ state_reg ]
          ; block = return VUnit
          } )
      ; ( "g"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block =
              case_tag
                state_reg
                [ [ t0 ]
                  => typed_block
                       [||]
                       (RegisterSet.of_list [ state_reg ])
                       false
                       (jump "f")
                ]
          } )
      ]
  in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = Some a_type; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  { cfg; entry = StringMap.empty; states }


let bad_final_4 =
  let a_type = Stretch.Inferred "a" in
  let c_cell = Cell.make ~typ:(Stretch.Inferred "c") true true false false in
  let b_cell = Cell.make ~typ:(Stretch.Inferred "b") true true false false in
  let a_cell = Cell.make ~typ:(Stretch.Inferred "a") false true false false in
  let cfg =
    LabelMap.of_list
      [ ( "f"
        , { stack_type = [||]
          ; final_type = Some a_type
          ; has_case_tag = true
          ; name = None
          ; needed_registers = RegisterSet.of_list [ state_reg ]
          ; block = return VUnit
          } )
      ; ( "g"
        , { stack_type = [||]
          ; final_type = None
          ; has_case_tag = false
          ; name = None
          ; needed_registers = RegisterSet.empty
          ; block = jump "f"
          } )
      ]
  in
  let states =
    TagMap.of_list
      [ (t0, { sfinal_type = Some a_type; known_cells = [| a_cell; b_cell |] })
      ; (t1, { sfinal_type = None; known_cells = [| c_cell |] })
      ; (t2, { sfinal_type = None; known_cells = [||] })
      ]
  in
  { cfg; entry = StringMap.empty; states }


let assert_fails f =
  match f () with exception StackLangError _ -> () | _ -> assert false


let test_wt () =
  wt good1;
  wt good2;
  wt good_sync;
  assert_fails (fun () -> wt bad_shadow_state);
  assert_fails (fun () -> wt bad_pop);
  assert_fails (fun () -> wt bad_push);
  assert_fails (fun () -> wt bad_sync);
  assert_fails (fun () -> wt bad_final);
  assert_fails (fun () -> wt bad_final_2);
  assert_fails (fun () -> wt bad_final_3);
  assert_fails (fun () -> wt bad_final_4)


let test () = test_wt ()
