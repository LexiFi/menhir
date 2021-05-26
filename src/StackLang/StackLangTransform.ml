open Printf
open StackLang
open StackLangUtils
open Infix

let state_reg = NamingConventions.state

let fresh_int =
  let n = ref (-1) in
  fun () ->
    n += 1;
    !n


let fstt (e, _, _) = e

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
      IComment (comment, block) )
    block
    push_list


let find_tagpat_branch branches tag =
  List.find
    (fun (TagMultiple taglist, _) -> List.exists (( = ) tag) taglist)
    branches


let pushcell_apply binds (value, cell, id) =
  (Bindings.apply binds value, cell, id)


(* let enrich_known_cells_with_state_info known_cells possible_states states =
  let state_known_cells =
    (state_info_intersection states possible_states).known_cells
  in
  Array.append known_cells state_known_cells *)

let needed_registers_pushes pushes =
  List.fold_left
    (fun needed_registers (value, _, _) ->
      let value_registers = Value.registers value in
      RegisterSet.union needed_registers value_registers )
    RegisterSet.empty
    pushes


let pushes_conflit_with_reg pushes reg =
  MList.exists (value_refers_to_register reg) pushes


let cancelled_pop = ref 0

let eliminated_branches = ref 0

let rec commute_pushes_block program pushes bindings final_type known_cells =
  (* [pushes] is the list of pushes that we need to restore, or cancel out with
     a pop if we are lucky.
     Every definition is inlined in order to make it impossible that a commuted
     pushes refers to a shadowed definition. There should be no definition in
     the resulting code, except before jumps.
     The code produced by [commute_pushes_block pushes bindings block _ _] is
     equivalent to [restore_pushes pushes (Block.def bindings block)].
     The arguments [final_type] and [known_cells] are used along the way to
     modify correctly the ITypedBlock :
     - [final_type] starts as the routine's final type, and if we go through a
       state were the final type is known, it is instanciated.
       If instanciated it is never changed, and replaces the [final_type] field
       of every ITypedBlock along the way.
     - [known_cells] starts as the routine's stack type.
       When a pop cannot be cancelled, a cell is removed from [known_cells].
       It is enriched with more information when matching on a type.
       [possible_states] contains the possible values of ["_menhir_s"].
       It is used to filter useless (and in some cases, ill-typed) branches from
       casetag.
       Inlining a definition of shape ["_menhir_s = i"] will not affect it,
       because if we have such a definition, we will not match on ["_menhir_s"],
       as it is defined as ill-typed by [StackLangTraverse.wt]. *)
  let commute_pushes_block = commute_pushes_block program in
  function
  | INeed (registers, block) ->
      (* First, we will not need the registers we are defining in the binding,
         since they are shadowed by said binding. *)
      let registers = RegisterSet.diff registers (Bindings.domain bindings) in
      (* However we need the registers referred by the bindings, even if they
         are shadowed after the bindings. *)
      let registers =
        RegisterSet.union registers (Bindings.codomain bindings)
      in
      (* We also need every register refered to by the pushes, even if said
         register is defined by the bindings, for the pushes will be restored
         before the bindings. *)
      let registers =
        RegisterSet.union (needed_registers_pushes pushes) registers
      in
      let block =
        commute_pushes_block pushes bindings final_type known_cells block
      in
      INeed (registers, block)
  | IPush (value, cell, block) ->
      (* We save the number of cancelled pop, to check whether commuting this
         push will make any difference. Not commuting a push when there is no
         reason to is useful, because commuting a push can mean duplicating code *)
      let current_cancelled_pop = !cancelled_pop in
      (* We get a new id, such that we are able to trace commuted pushes with
         their origin. *)
      let id = fresh_int () in
      let pushes = (Bindings.apply bindings value, cell, id) :: pushes in
      let block' =
        commute_pushes_block pushes bindings final_type known_cells block
      in
      if current_cancelled_pop < !cancelled_pop
      then
        let comment =
          sprintf
            "Commuting push_%i %s"
            id
            (StackLangPrinter.value_to_string value)
        in
        IComment (comment, block')
      else restore_pushes pushes (IDef (bindings, block))
  | IPop (pattern, block) ->
    (* A pop is a special kind of definition, so you may think that we need
       to generate new substition rules from it, but there is no need
       because we only keep the pop if there is no push that can conflict
       with it. The pattern we pop into is authoritative and we remove it
       from the substitution. *)
    ( match pushes with
    | [] ->
        assert (known_cells <> [||]);
        (* We lose a known cell here *)
        let known_cells = pop_n_cells known_cells 1 in
        (* However, since we popped the state from the stack, it is now in
           sync with the stack. Therefore, no information we have was
           discovered by inspecting the stack *)
        let block' =
          commute_pushes_block
            []
            (Bindings.remove bindings pattern)
            final_type
            known_cells
            block
        in
        IPop (pattern, block')
    | (value, _cell, id) :: push_list ->
        (* We remove every register refered in the value from the
           substitution, because we need to access the value as it was when
           pushed, and add that to the substitution. *)
        let bindings = Bindings.remove_value bindings value in
        let bindings = Bindings.extend_pattern bindings pattern value in
        (* We have cancelled a pop ! *)
        cancelled_pop += 1;
        let block =
          commute_pushes_block push_list bindings final_type known_cells block
        in
        let comment =
          sprintf
            "Cancelled push_%i %s with pop %s"
            id
            (StackLangPrinter.value_to_string value)
            (StackLangPrinter.pattern_to_string pattern)
        in
        IComment (comment, block) )
  | IDef (bindings', block) ->
      (* As explained above, for every conflict between the definition and a
         push currently commuting, we add a new substitution rule
         We do not want to shadow definition useful for the push train *)
      let bindings = Bindings.compose bindings bindings' in
      let block =
        commute_pushes_block pushes bindings final_type known_cells block
      in
      if bindings' <> Bindings.empty
      then
        IComment
          ( sprintf
              "Inlining def : %s"
              (StackLangPrinter.bindings_to_string bindings')
          , block )
      else block
  | IPrim (reg, prim, block) ->
      (* A primitive is a like def except it has a simple register instead of a
         pattern *)
      let reg' =
        if pushes_conflit_with_reg (List.map fstt pushes) reg
        then suffix reg (fresh_int ())
        else reg
      in
      let subst' =
        Bindings.extend (Bindings.remove bindings (PReg reg')) reg (VReg reg')
      in
      let block =
        commute_pushes_block pushes subst' final_type known_cells block
      in
      let prim =
        match prim with
        | PrimOCamlCall (f, args) ->
            let args = List.map (Bindings.apply bindings) args in
            PrimOCamlCall (f, args)
        | PrimOCamlAction (bindings', action) ->
            PrimOCamlAction (Bindings.compose bindings bindings', action)
        | prim ->
            prim
      in
      IPrim (reg', prim, block)
  | ITrace (register, block) ->
      let block' =
        commute_pushes_block pushes bindings final_type known_cells block
      in
      ITrace (register, block')
  | IComment (comment, block) ->
      let block =
        commute_pushes_block pushes bindings final_type known_cells block
      in
      IComment (comment, block)
  | IDie ->
      cancelled_pop += List.length pushes;
      IDie
  | IReturn v ->
      cancelled_pop += List.length pushes;
      IReturn (Bindings.apply bindings v)
  | IJump label ->
      aux_jump pushes bindings label
  | ICaseToken (reg, branches, odefault) ->
      aux_case_token
        program
        pushes
        bindings
        reg
        branches
        odefault
        final_type
        known_cells
  | ICaseTag (reg, branches) ->
      commute_pushes_icase_tag
        program
        pushes
        bindings
        final_type
        known_cells
        reg
        branches
  | ITypedBlock ({ needed_registers } as t_block) ->
      let bindings = Bindings.restrict bindings needed_registers in
      commute_pushes_itblock
        program
        pushes
        bindings
        final_type
        known_cells
        t_block


and commute_pushes_icase_tag
    program pushes bindings final_type known_cells reg branches =
  let branch_aux =
    branch_aux program pushes bindings final_type known_cells reg
  in
  match Bindings.apply bindings (VReg reg) with
  | VTag tag ->
      (* If the value of the state is known, then we remove the match. *)
      let block = snd @@ find_tagpat_branch branches tag in
      eliminated_branches += (List.length branches - 1);
      let comment = "Eliminated case tag" in
      IComment
        ( comment
        , commute_pushes_block
            program
            pushes
            bindings
            final_type
            known_cells
            block )
  | VReg reg ->
      let branches = List.map branch_aux branches in
      ICaseTag (reg, branches)
  | _ ->
      (* We should never perform a case tag on a register that is bound to
         something else than a tag. *)
      assert false


and branch_aux program pushes bindings final_type known_cells reg branch =
  let TagMultiple taglist, block = branch in
  let state_info = state_info_intersection program.states taglist in
  let known_cells =
    longest_known_cells [ state_info.known_cells; known_cells ]
  in
  let subst, pushes =
    match taglist with
    | [ tag ] ->
        (* In this case, we can inline the state value inside the
           pushes. *)
        let subst = Bindings.extend bindings reg (VTag tag) in
        let tmp_subst = Bindings.singleton reg (VTag tag) in
        let pushes = List.map (pushcell_apply tmp_subst) pushes in
        (subst, pushes)
    | _ ->
        (bindings, pushes)
  in
  let final_type = Option.first_value [ state_info.sfinal_type; final_type ] in
  let block =
    commute_pushes_block program pushes subst final_type known_cells block
  in
  (TagMultiple taglist, block)


and commute_pushes_itblock
    program pushes bindings final_type known_cells t_block =
  let { block; needed_registers; stack_type; final_type = tb_final_type } =
    t_block
  in
  let stack_type = pop_n_cells stack_type (List.length pushes) in
  (* First, we will not need the registers we are defining in the binding,
     since they are shadowed by said binding. *)
  let needed_registers =
    RegisterSet.diff needed_registers (Bindings.domain bindings)
  in
  (* However we need the registers referred by the bindings, even if they
     are shadowed after the bindings. *)
  let needed_registers =
    RegisterSet.union needed_registers (Bindings.codomain bindings)
  in
  (* We also need every register refered to by the pushes, even if said
     register is defined by the bindings, for the pushes will be restored
     before the bindings. *)
  let needed_registers =
    RegisterSet.union (needed_registers_pushes pushes) needed_registers
  in
  let final_type = Option.first_value [ final_type; tb_final_type ] in
  let known_cells = longest_known_cells [ stack_type; known_cells ] in
  let block =
    commute_pushes_block program pushes bindings final_type known_cells block
  in
  ITypedBlock
    { t_block with
      block
    ; final_type
    ; needed_registers
    ; stack_type = known_cells
    }


and aux_jump pushes bindings label =
  restore_pushes pushes (Block.def bindings @@ Block.jump label)


and aux_case_token
    program pushes bindings reg branches odefault final_type known_cells =
  let aux_branch = function
    (* Every [TokSingle] introduces a definition of a register. *)
    | TokSingle (tok, reg'), (block : block) ->
        let reg'' =
          if pushes_conflit_with_reg (List.map fstt pushes) reg'
          then suffix reg' (fresh_int ())
          else reg'
        in
        let subst = Bindings.extend bindings reg' (VReg reg'') in
        let block' =
          commute_pushes_block program pushes subst final_type known_cells block
        in
        (TokSingle (tok, reg''), block')
    (* [TokMultiple] does not introduce new definitions *)
    | TokMultiple terminals, block ->
        let block' =
          commute_pushes_block
            program
            pushes
            bindings
            final_type
            known_cells
            block
        in
        (TokMultiple terminals, block')
  in
  let branches = List.map aux_branch branches in
  ICaseToken
    ( reg
    , branches
    , Option.map
        (commute_pushes_block program pushes bindings final_type known_cells)
        odefault )


and commute_pushes_t_block program t_block =
  cancelled_pop := 0;
  eliminated_branches := 0;
  let { block; stack_type; final_type } = t_block in
  let candidate =
    commute_pushes_block program [] Bindings.empty final_type stack_type block
  in
  { t_block with
    block =
      ( if !cancelled_pop > 0 || !eliminated_branches > 0
      then candidate
      else block )
  }


let represented_states program =
  let states = program.states in
  TagMap.domain states


let rec remove_dead_branches_block program possible_states block =
  let remove_dead_branches_block = remove_dead_branches_block program in
  Block.map
    (remove_dead_branches_block possible_states)
    ~pop:(fun pattern block ->
      let possible_states = represented_states program in
      let block = remove_dead_branches_block possible_states block in
      (pattern, block) )
    ~def:(fun bindings block ->
      let possible_states =
        match Bindings.apply bindings (VReg state_reg) with
        | VTag tag ->
            TagSet.singleton tag
        | VReg reg when state_reg = reg ->
            possible_states
        | _ ->
            assert false
      in
      let block = remove_dead_branches_block possible_states block in
      (bindings, block) )
    ~case_tag:(fun reg branches ->
      let branch_aux (TagMultiple taglist, block) =
        let taglist' =
          List.filter (fun tag -> TagSet.mem tag possible_states) taglist
        in
        match taglist' with
        | [] ->
            None
        | _ :: _ ->
            Some
              (let possible_states = TagSet.of_list taglist' in
               let block = remove_dead_branches_block possible_states block in
               (TagMultiple taglist', block) )
      in
      let branches = List.filter_map branch_aux branches in
      (reg, branches) )
    block


let remove_dead_branches_t_block program t_block =
  let { block } = t_block in
  let all = represented_states program in
  { t_block with block = remove_dead_branches_block program all block }


let remove_dead_branches program =
  Program.map (remove_dead_branches_t_block program) program


let commute_pushes program =
  remove_dead_branches (Program.map (commute_pushes_t_block program) program)


(** remove definitions of shape [x = x], or shape [_ = x] *)
let remove_useless_defs program =
  let rec aux block =
    match block with
    | IDef (binds, block) when binds = Bindings.empty ->
        aux block
    | _ ->
        Block.map aux block
  in
  Program.map
    (fun t_block -> { t_block with block = aux t_block.block })
    program


let rec compute_has_case_tag_block = function
  | INeed (regs, block) ->
      let block', hct = compute_has_case_tag_block block in
      (INeed (regs, block'), hct)
  | IPush (value, cell, block) ->
      let block', hct = compute_has_case_tag_block block in
      (IPush (value, cell, block'), hct)
  | IPop (reg, block) ->
      let block', hct = compute_has_case_tag_block block in
      (IPop (reg, block'), hct)
  | IDef (bindings, block) ->
      let block', hct = compute_has_case_tag_block block in
      (IDef (bindings, block'), hct)
  | IPrim (reg, prim, block) ->
      let block', hct = compute_has_case_tag_block block in
      (IPrim (reg, prim, block'), hct)
  | ITrace (reg, block) ->
      let block', hct = compute_has_case_tag_block block in
      (ITrace (reg, block'), hct)
  | IComment (comment, block) ->
      let block', hct = compute_has_case_tag_block block in
      (IComment (comment, block'), hct)
  | (IDie | IReturn _ | IJump _) as block ->
      (block, false)
  | ICaseToken (reg, branches, odefault) ->
      let branches =
        List.map
          (fun (tokpat, block) ->
            let block, hct = compute_has_case_tag_block block in
            ((tokpat, block), hct) )
          branches
      in
      let branches, hcts = List.split branches in
      let hct = List.exists Fun.id hcts in
      (ICaseToken (reg, branches, odefault), hct)
  | ICaseTag (reg, branches) ->
      let branches =
        List.map
          (fun (tokpat, block) ->
            let block, _hct = compute_has_case_tag_block block in
            (tokpat, block) )
          branches
      in
      (ICaseTag (reg, branches), true)
  | ITypedBlock t_block ->
      let { block } = t_block in
      let block, has_case_tag = compute_has_case_tag_block block in
      (ITypedBlock { t_block with block; has_case_tag }, false)


let compute_has_case_tag_t_block t_block =
  let { block } = t_block in
  let block, has_case_tag = compute_has_case_tag_block block in
  { t_block with block; has_case_tag }


let compute_has_case_tag program =
  Program.map compute_has_case_tag_t_block program


let optimize program =
  remove_useless_defs
  @@ remove_dead_branches
       ( if Settings.commute_pushes
       then
         let program = compute_has_case_tag @@ commute_pushes program in
         program
       else program )


let test () =
  let module SL = EmitStackLang.Run () in
  let program = SL.program in
  let optimized_program = optimize program in
  StackLangTester.test program;
  StackLangTester.test optimized_program
