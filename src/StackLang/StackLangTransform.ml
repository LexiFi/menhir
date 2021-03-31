open Printf
open StackLang
open StackLangUtils
module Subst = Substitution
open NamingConventions
open Infix

let fresh_int =
  let n = ref (-1) in
  fun () -> n += 1 ; !n

let fstt (e, _, _) = e
let state_reg = state

let suffix name i = Printf.sprintf "%s_%i" name i

(** [pop_n_cells stack_type n] return a new stack_type, with [n] top cells
    removed. *)
let pop_n_cells stack_type n =
  let length = Array.length stack_type in
  let new_length = max (length - n) 0 in
  Array.sub stack_type 0 new_length

let restore_pushes push_list block =
  List.fold_left
    (fun block (value, cell, id) ->
      let comment = sprintf "Restoring push_%i" id in
      let block = IPush (value, cell, block) in
      IComment (comment, block))
    block push_list

(** Apply the substitution to the arguments of a PrimOcamlCall. *)
let subst_prim_call subst (f, args) = (f, List.map (Subst.apply subst) args)

let find_tagpat_branch branches tag =
  List.find
    (fun (TagMultiple taglist, _) -> List.exists (( = ) tag) taglist)
    branches

let pushcell_apply subst (value, cell, id) = (Subst.apply subst value, cell, id)

let needed_registers_pushes pushes =
  List.fold_left
    (fun needed_registers (value, _, _) ->
      let value_registers = value_registers value in
      RegisterSet.union needed_registers value_registers)
    RegisterSet.empty pushes

let commute_pushes_t_block program t_block =
  let cfg = program.cfg in
  let pushes_conflit_with_reg pushes reg =
    List.exists (value_refers_to_register reg) pushes
  in
  let cancelled_pop = ref 0 in
  let eliminated_branches = ref 0 in
  let rec commute_pushes_block pushes subst final_type known_cells =
    (* [push_list] is the list of pushes that we need to restore, and hopefully
       cancel out with a pop. Every definition is inlined in order to make it
       impossible that a commuted pushes refers to a shadowed definition.
       There should be no definition in the resulting code. Registers are still
       refered to, but their value only ever change by performing an
       ISubstitutedJump.
       The code produced by [aux pushes substitution block] is
       equivalent to
       [restore_pushes pushes (Subst.restore_defs substitution block)].
       The arguments [final_type] and [known_cells] are used along the way to
       modify correctly the ITypedBlock.
       [final_type] starts as None, and if we go through a state were the final
       type is known, it is instanciated. If instanciated it is never changed,
       and replaces the [final_type] field of every ITypedBlock along the way.
       [known_cells] should be initialized to an array of the known cells.
       Then a cell is popped when a pop cannot be cancelled, and it is enriched
       with more information when matching on a type. *)
    function
    | INeed (registers, block) ->
        let block' =
          commute_pushes_block pushes subst final_type known_cells block
        in
        let registers =
          RegisterSet.union
            (needed_registers_pushes pushes)
            (Subst.apply_registers subst registers)
        in
        INeed (registers, block')
    | IPush (value, cell, block) ->
        (*  *)
        let id = fresh_int () in
        let block' =
          commute_pushes_block
            ((Subst.apply subst value, cell, id) :: pushes)
            subst final_type known_cells block
        in
        let comment =
          sprintf "Commuting push_%i %s" id
            (StackLangPrinter.value_to_string value)
        in
        IComment (comment, block')
    | IPop (pattern, block) -> (
      (* A pop is a special kind of definition, so you may think that we need
         to generate new substition rules from it, but there is no need
         because we only keep the pop if there is no push that can conflict
         with it. The pattern we pop into is authoritative and we remove it
         from the substitution. *)
      match pushes with
      | [] ->
          assert (known_cells <> [||]) ;
          let known_cells = pop_n_cells known_cells 1 in
          let block' =
            commute_pushes_block []
              (Subst.remove subst pattern)
              final_type known_cells block
          in
          IPop (pattern, block')
      | (value, _cell, id) :: push_list ->
          (* We remove every register refered in the value from the
             substitution, because we need to access the value as it was when
             pushed, and add that to the substitution. *)
          let subst = Subst.remove_value subst value in
          let subst = Subst.extend_pattern subst pattern value in
          (* We have cancelled a pop ! *)
          cancelled_pop += 1 ;
          let block =
            commute_pushes_block push_list subst final_type known_cells block
          in
          let comment =
            sprintf "Cancelled push_%i %s with pop %s" id
              (StackLangPrinter.value_to_string value)
              (StackLangPrinter.pattern_to_string pattern)
          in
          IComment (comment, block) )
    | IDef (pattern, value, block) ->
        (* As explained above, for every conflict between the definition and a
           push currently commuting, we add a new substitution rule
           We do not want to shadow definition useful for the push train *)
        let subst = Subst.extend_pattern subst pattern value in
        (* Currently, the value is raw, we need to apply the substitution to it
           in order for it to refer to the correct definition. *)
        let value' = Subst.apply subst value in
        let block' =
          commute_pushes_block pushes subst final_type known_cells block
        in
        IComment
          ( sprintf "Inlining def : %s = %s"
              (StackLangPrinter.pattern_to_string pattern)
              (StackLangPrinter.value_to_string value')
          , block' )
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
        let block =
          commute_pushes_block pushes subst' final_type known_cells block
        in
        let prim =
          match prim with
          | PrimOCamlCall (f, args) ->
              let f, args = subst_prim_call subst (f, args) in
              PrimOCamlCall (f, args)
          | PrimSubstOcamlAction (subst', action) ->
              PrimSubstOcamlAction (Subst.compose subst subst', action)
          | PrimOCamlAction action ->
              PrimSubstOcamlAction (subst, action)
          | prim ->
              prim
        in
        IPrim (reg', prim, block)
    | ITrace (register, block) ->
        let block' =
          commute_pushes_block pushes subst final_type known_cells block
        in
        ITrace (register, block')
    | IComment (comment, block) ->
        let block =
          commute_pushes_block pushes subst final_type known_cells block
        in
        IComment (comment, block)
    | IDie ->
        cancelled_pop += List.length pushes ;
        IDie
    | IReturn v ->
        cancelled_pop += List.length pushes ;
        IReturn (Subst.apply subst v)
    | IJump label ->
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
    | ICaseTag (reg, branches) ->
        aux_icase_tag pushes subst final_type known_cells reg branches
    | ITypedBlock t_block ->
        aux_itblock pushes subst final_type known_cells t_block
  and aux_icase_tag pushes subst final_type known_cells reg branches =
    match Subst.apply subst (VReg reg) with
    | VTag tag ->
        let block = snd @@ find_tagpat_branch branches tag in
        eliminated_branches += (List.length branches - 1) ;
        commute_pushes_block pushes subst final_type known_cells block
    | VReg reg ->
        let branch_aux (TagMultiple taglist, block) =
          let state_info = state_info_intersection program.states taglist in
          let known_cells =
            longest_known_cells [known_cells; state_info.known_cells]
          in
          let subst, pushes =
            match taglist with
            | [tag] ->
                ( Subst.extend reg (VTag tag) subst
                , let tmp_subst = Subst.extend reg (VTag tag) Subst.empty in
                  List.map (pushcell_apply tmp_subst) pushes )
            | _ ->
                (subst, pushes)
          in
          (* We are matching on a state, therefore state is always needed,
             and we can discard these values. *)
          let block =
            commute_pushes_block pushes subst final_type known_cells block
          in
          (TagMultiple taglist, block)
        in
        let branches = List.map branch_aux branches in
        ICaseTag (Subst.apply_reg subst reg, branches)
    | _ ->
        assert false
  and aux_itblock pushes subst final_type known_cells t_block =
    let {block; stack_type; needed_registers; final_type= t_block_final_type} =
      t_block
    in
    (* We alter the type information according to the number of commuting
       pushes : Every push that is commuting removes a known stack symbol *)
    let state =
      match Subst.apply subst (VReg state_reg) with
      | VTag tag ->
          Some tag
      | VReg reg when reg = state_reg ->
          None
      | _ ->
          assert false
    in
    let state_info =
      Option.map (fun tag -> lookup_tag tag program.states) state
    in
    let state_final_type =
      Option.simplify @@ Option.map (fun info -> info.sfinal_type) state_info
    in
    let final_type =
      Option.first_value [final_type; state_final_type; t_block_final_type]
    in
    let needed_registers =
      RegisterSet.union
        (needed_registers_pushes pushes)
        (Subst.apply_registers subst needed_registers)
    in
    let need_state = RegisterSet.mem state_reg needed_registers in
    let push_n = List.length pushes in
    let stack_types =
      (pop_n_cells stack_type push_n :: List.if1 (not need_state) known_cells)
      @
      match state_info with
      | Some {known_cells} ->
          [pop_n_cells known_cells push_n]
      | None ->
          []
    in
    let stack_type = longest_known_cells stack_types in
    let block = commute_pushes_block pushes subst final_type stack_type block in
    ITypedBlock
      { t_block with
        block= IComment (sprintf "need_state=%b" need_state, block)
      ; final_type
      ; needed_registers
      ; stack_type }
  and aux_jump pushes subst label =
    (* Raw registers needed by the block we jump to. *)
    let needed_registers = needed (lookup label cfg) in
    (* We apply the substitution. *)
    let needed_registers = Subst.apply_registers subst needed_registers in
    (* We add the registers needed by the pushes we are restoring. *)
    let needed_registers =
      RegisterSet.union needed_registers (needed_registers_pushes pushes)
    in
    restore_pushes pushes
      (IComment
         ( sprintf "Needed registers : %s"
             (String.concat " " (RegisterSet.elements needed_registers))
         , ISubstitutedJump (label, subst) ))
  and aux_case_token pushes subst reg branches odefault final_type known_cells =
    let aux_branch = function
      (* Every [TokSingle] introduces a definition of a register. *)
      | TokSingle (tok, reg'), block ->
          let reg'' =
            if pushes_conflit_with_reg (List.map fstt pushes) reg' then
              suffix reg' (fresh_int ())
            else reg'
          in
          let subst = Subst.extend reg' (VReg reg'') subst in
          let block' =
            commute_pushes_block pushes subst final_type known_cells block
          in
          (TokSingle (tok, reg''), block')
      (* [TokMultiple] does not introduce new definitions *)
      | TokMultiple terminals, block ->
          let block' =
            commute_pushes_block pushes subst final_type known_cells block
          in
          (TokMultiple terminals, block')
    in
    let branches = List.map aux_branch branches in
    ICaseToken
      ( reg
      , branches
      , Option.map
          (commute_pushes_block pushes subst final_type known_cells)
          odefault )
  in
  let {block; stack_type} = t_block in
  let candidate = commute_pushes_block [] Subst.empty None stack_type block in
  { t_block with
    block=
      ( if !cancelled_pop > 0 || !eliminated_branches > 0 then candidate
      else block ) }

let commute_pushes program =
  { program with
    cfg= RegisterMap.map (commute_pushes_t_block program) program.cfg }

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

let test () =
  let module SL = EmitStackLang.Run () in
  let program = SL.program in
  let optimized_program = optimize program in
  StackLangTester.test program ;
  StackLangTester.test optimized_program
