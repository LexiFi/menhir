open StackLang
open Infix

(* -------------------------------------------------------------------------- *)

(* A few constructors. *)

let vreg r = VReg r

let vregs rs = List.map vreg rs

(* A few accessors. *)

let lookup label map =
  try LabelMap.find label map with Not_found -> assert false

let lookup_tag tag states = TagMap.find tag states

let lookup_state state states = lookup_tag (Lr1.number state) states

let entry_labels program = StringMap.domain program.entry

(* We assume that every labeled block in a well-formed control flow graph
   begins with an [INeed] instruction that determines which registers are
   defined upon entry to this block. *)

let needed t_block = t_block.needed_registers

let rec value_registers = function
  | VReg reg ->
      RegisterSet.singleton reg
  | VTuple li ->
      List.fold_left RegisterSet.union RegisterSet.empty
        (List.map value_registers li)
  | _ ->
      RegisterSet.empty

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

let rec value_refers_to_register register value =
  match value with
  | VReg register' when register = register' ->
      true
  | VTuple li ->
      List.exists (value_refers_to_register register) li
  | _ ->
      false

let cells_intersection cells1 cells2 =
  let len1 = Array.length cells1 in
  let len2 = Array.length cells2 in
  (* let cells1 = Array.rev cells1 in
     let cells2 = Array.rev cells2 in *)
  let i = ref 0 in
  while
    let i = !i in
    i < len1 && i < len2 && cells1.(len1 - i - 1) = cells2.(len2 - i - 1)
  do
    i += 1
  done ;
  let i = !i in
  Array.sub cells1 (len1 - i) i

let state_info_intersection s1 s2 =
  let {known_cells= cells1; sfinal_type= f1} = s1 in
  let {known_cells= cells2; sfinal_type= f2} = s2 in
  let sfinal_type =
    match (f1, f2) with Some f1, Some f2 when f1 = f2 -> Some f1 | _ -> None
  in
  let known_cells = cells_intersection cells1 cells2 in
  {known_cells; sfinal_type}

let state_info_list_intersection state_infos =
  match state_infos with
  | state_info :: state_infos ->
      List.fold_left state_info_intersection state_info state_infos
  | [] ->
      assert false

let state_info_intersection states taglist =
  state_info_list_intersection
    (List.map (fun tag -> lookup_tag tag states) taglist)

let filter_stack stack =
  Array.of_list
    (List.filter
       (function
         | { hold_state= false
           ; hold_semv= false
           ; hold_startpos= false
           ; hold_endpos= false } ->
             false
         | _ ->
             true)
       (Array.to_list stack))

(* -------------------------------------------------------------------------- *)
(* Testing *)

let build_cell hold_state hold_semv hold_startpos hold_endpos =
  {typ= None; hold_state; hold_semv; hold_startpos; hold_endpos}

let empty_cell = build_cell false false false false

let test_cells_intersection () =
  (* check that [cells_intersection a a = a] *)
  assert (
    cells_intersection
      [| build_cell true false false false
       ; build_cell false false true false
       ; empty_cell
       ; build_cell false false true true |]
      [| build_cell true false false false
       ; build_cell false false true false
       ; empty_cell
       ; build_cell false false true true |]
    = [| build_cell true false false false
       ; build_cell false false true false
       ; empty_cell
       ; build_cell false false true true |] ) ;
  (* check that if [a] is a suffix of [b] then [cells_intersection a b = a]*)
  assert (
    cells_intersection
      [| build_cell true false false false
       ; build_cell false false true false
       ; empty_cell
       ; build_cell false false true true |]
      [|empty_cell; build_cell false false true true|]
    = [|empty_cell; build_cell false false true true|] ) ;
  (* check that if the biggest common suffix of [a] and [b] is [ [||] ] then,
     [cells_intersection a b = [||]]*)
  assert (
    cells_intersection
      [| build_cell true false false false
       ; build_cell false false true false
       ; empty_cell
       ; build_cell false false true true
       ; build_cell false false true true |]
      [| build_cell true false false false
       ; build_cell false false true false
       ; empty_cell
       ; build_cell false false true true
       ; build_cell true true false false |]
    = [||] )

let test_filter_stack () =
  assert (filter_stack [|empty_cell|] = [||]) ;
  assert (filter_stack [|empty_cell; empty_cell; empty_cell|] = [||]) ;
  assert (
    filter_stack
      [| build_cell true false false false
       ; build_cell false false true false
       ; empty_cell
       ; build_cell false false true true |]
    = [| build_cell true false false false
       ; build_cell false false true false
       ; build_cell false false true true |] )

let test () = test_filter_stack () ; test_cells_intersection ()
