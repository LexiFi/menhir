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


let restore_pushes pushes block =
  List.fold_left
    (fun block (value, cell, id) ->
      let comment = sprintf "Restoring push_%i" id in
      let block = IPush (value, cell, block) in
      IComment (comment, block) )
    block
    pushes


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

let rec commute_pushes_block program pushes bindings final_type cells =
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
        commute_pushes_block pushes bindings final_type cells block
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
      commute_pushes_pop program pushes bindings final_type cells pattern block
  | IDef (bindings', block) ->
      (* As explained above, for every conflict between the definition and a
         push currently commuting, we add a new substitution rule
         We do not want to shadow definition useful for the push train *)
      let comment1 =
        sprintf
          "Going in with defs : %s"
          (StackLangPrinter.bindings_to_string bindings)
      in
      let comment2 =
        sprintf
          "Propagating def : %s"
          (StackLangPrinter.bindings_to_string bindings')
      in
      let bindings = Bindings.let_in bindings bindings' in
      let comment3 =
        sprintf
          "Going out with def : %s"
          (StackLangPrinter.bindings_to_string bindings)
      in
      let block = commute_pushes_block pushes bindings final_type cells block in
      IComment (comment1, IComment (comment2, IComment (comment3, block)))
  | IPrim (reg, prim, block) ->
      (* A primitive is a like def except it has a simple register instead of a
         pattern *)
      let reg' =
        if pushes_conflit_with_reg (List.map fstt pushes) reg
        then suffix (reg ^ "fresh") (fresh_int ())
        else reg
      in
      let bindings' =
        Bindings.(
          let_in
            (remove
               bindings
               (RegisterSet.singleton reg')
               (* It should be impossible that this changes anything *) )
            (assign (PReg reg) (VReg reg')))
      in
      let block =
        commute_pushes_block pushes bindings' final_type cells block
      in
      let prim =
        match prim with
        | PrimOCamlCall (f, args) ->
            let args = List.map (Bindings.apply bindings) args in
            PrimOCamlCall (f, args)
        | PrimOCamlAction (bindings', action) ->
            PrimOCamlAction (Bindings.let_in bindings bindings', action)
        | PrimOCamlFieldAccess (value, field) ->
            PrimOCamlFieldAccess (Bindings.apply bindings value, field)
        | PrimOCamlDummyPos ->
            PrimOCamlDummyPos
      in

      IPrim (reg', prim, block)
  | ITrace (trace, block) ->
      let trace =
        match trace with
        | TraceMessage s ->
            TraceMessage s
        | TracePositions (s, startp, endp) ->
            TracePositions
              ( s
              , Option.map (Bindings.apply bindings) startp
              , Option.map (Bindings.apply bindings) endp )
      in
      let block = commute_pushes_block pushes bindings final_type cells block in
      ITrace (trace, block)
  | IComment (comment, block) ->
      let block = commute_pushes_block pushes bindings final_type cells block in
      IComment (comment, block)
  | IDie ->
      cancelled_pop += List.length pushes;
      IDie
  | IReturn v ->
      cancelled_pop += List.length pushes;
      IReturn (Bindings.apply bindings v)
  | IJump label ->
      restore_pushes pushes (Block.def bindings @@ Block.jump label)
  | ICaseToken (reg, branches, odefault) ->
      commute_pushes_token
        program
        pushes
        bindings
        reg
        branches
        odefault
        final_type
        cells
  | ICaseTag (reg, branches) ->
      commute_pushes_icase_tag
        program
        pushes
        bindings
        final_type
        cells
        reg
        branches
  | ITypedBlock t_block ->
      commute_pushes_itblock program pushes bindings final_type cells t_block


and commute_pushes_pop program pushes bindings final_type cells pattern block =
  (* A pop is a special kind of definition, so you may think that we need
     to generate new substition rules from it, but there is no need
     because we only keep the pop if there is no push that can conflict
     with it. The pattern we pop into is authoritative and we remove it
     from the substitution. *)
  match pushes with
  | [] ->
      assert (cells <> [||]);
      (* We lose a known cell here *)
      let cells = pop_n_cells cells 1 in
      let block' =
        commute_pushes_block program [] Bindings.empty final_type cells block
      in
      IDef (bindings, IPop (pattern, block'))
  | (value, _cell, id) :: push_list ->
      let comment1 =
        sprintf
          "Going in with defs : %s"
          (StackLangPrinter.bindings_to_string bindings)
      in
      let comment2 =
        sprintf
          "Cancelled push_%i %s with pop %s"
          id
          (StackLangPrinter.value_to_string value)
          (StackLangPrinter.pattern_to_string pattern)
      in
      (*
      We have
      ```
      push value ;
      def bindings ;
      pop pattern ;
      ( ... )
      ```
      and we want to get a single bindings.
      If there is a register bound both in the pop and in the def, it is going
      to have the value to which it is bound by the pop, since it is last.
      Appart from that, the value pushed are not affected by the def, which
      means that this is equivalent to a let-and binding.
      *)
      let bindings = Bindings.remove bindings (Pattern.registers pattern) in
      let bindings = Bindings.(let_and (assign pattern value) bindings) in
      (* We have cancelled a pop ! *)
      cancelled_pop += 1;
      let block =
        commute_pushes_block program push_list bindings final_type cells block
      in

      let comment3 =
        sprintf
          "Going out with def : %s"
          (StackLangPrinter.bindings_to_string bindings)
      in

      IComment (comment1, IComment (comment2, IComment (comment3, block)))


and commute_pushes_icase_tag
    program pushes bindings final_type known_cells reg branches =
  let branch_aux =
    commute_pushes_tagpat_branch
      program
      pushes
      bindings
      final_type
      known_cells
      reg
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


and commute_pushes_tagpat_branch
    program pushes bindings final_type cells reg branch =
  let TagMultiple taglist, block = branch in
  let final_type' = final_type_intersection program.states taglist in
  let bindings, pushes, cells =
    match taglist with
    | [ tag ] ->
        (* In this case, we can inline the state value inside the
           pushes. We also need more information to be able to push the litteral
           state *)
        let tmp_bindings = Bindings.assign (PReg reg) (VTag tag) in
        let pushes = List.map (pushcell_apply tmp_bindings) pushes in
        let bindings = Bindings.let_in bindings tmp_bindings in
        let cells = (lookup_tag tag program.states).known_cells in
        (bindings, pushes, cells)
    | _ ->
        (bindings, pushes, cells)
  in
  let final_type = Option.first_value [ final_type'; final_type ] in
  let block =
    commute_pushes_block program pushes bindings final_type cells block
  in
  (TagMultiple taglist, block)


and commute_pushes_itblock
    program pushes bindings final_type known_cells t_block =
  let { block; needed_registers; stack_type; final_type = tb_final_type } =
    t_block
  in
  let stack_type = pop_n_cells stack_type (List.length pushes) in
  let bindings = Bindings.restrict needed_registers bindings in
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


and commute_pushes_token
    program pushes bindings reg branches odefault final_type cells =
  let aux_branch = function
    (* Every [TokSingle] introduces a definition of a register. *)
    | TokSingle (tok, reg'), block ->
        let reg'' =
          if pushes_conflit_with_reg (List.map fstt pushes) reg'
          then suffix reg' (fresh_int ())
          else reg'
        in
        let bindings =
          Bindings.(let_in bindings (assign (PReg reg') (VReg reg'')))
        in
        let block' =
          commute_pushes_block program pushes bindings final_type cells block
        in
        (TokSingle (tok, reg''), block')
    (* [TokMultiple] does not introduce new definitions *)
    | TokMultiple terminals, block ->
        let block' =
          commute_pushes_block program pushes bindings final_type cells block
        in
        (TokMultiple terminals, block')
  in
  let branches = List.map aux_branch branches in
  ICaseToken
    ( reg
    , branches
    , Option.map
        (commute_pushes_block program pushes bindings final_type cells)
        odefault )


and commute_pushes_routine program t_block =
  cancelled_pop := 0;
  eliminated_branches := 0;
  let { block; stack_type; final_type } = t_block in
  let candidate =
    commute_pushes_block program [] Bindings.empty final_type stack_type block
  in
  let block =
    if !cancelled_pop > 0 || !eliminated_branches > 0 then candidate else block
  in
  { t_block with block }


let represented_states program =
  let states = program.states in
  TagMap.domain states


let rec remove_dead_branches_block program possible_states cells sync block =
  let remove_dead_branches_block = remove_dead_branches_block program in
  let states = program.states in
  let module Annotate =
    Annotate.Curry (struct
      let program = program

      let cells = cells

      let sync = sync
    end)
  in
  Block.map
    (remove_dead_branches_block possible_states cells sync)
    ~push:(fun value cell block ->
      let cells, sync = Annotate.push value cell in
      let block = remove_dead_branches_block possible_states cells sync block in
      (value, cell, block) )
    ~typed_block:(fun tblock ->
      let { block } = tblock in
      let cells, sync = Annotate.typed_block tblock in
      let block = remove_dead_branches_block possible_states cells sync block in
      { tblock with block } )
    ~pop:(fun pattern block ->
      let possible_states = represented_states program in
      let cells, sync = Annotate.pop pattern in
      let block = remove_dead_branches_block possible_states cells sync block in
      (pattern, block) )
    ~def:(fun bindings block ->
      let cells, sync = Annotate.def bindings in
      let possible_states =
        match Bindings.apply bindings (VReg state_reg) with
        | VTag tag ->
            TagSet.singleton tag
        | VReg reg when state_reg = reg ->
            possible_states
        | _ ->
            assert false
      in
      let block = remove_dead_branches_block possible_states cells sync block in
      (bindings, block) )
    ~case_tag:(fun reg branches ->
      let n = match sync with Sync.Synced n -> n | _ -> assert false in
      let branch_aux (TagMultiple taglist, block) =
        let cells', sync =
          Annotate.case_tag_branch (TagMultiple taglist) block
        in
        let taglist' =
          List.filter
            (fun tag ->
              TagSet.mem tag possible_states
              &&
              let cells_state = (lookup_tag tag states).known_cells in
              let cells'' = Array.append cells_state (MArray.suffix cells n) in
              is_compatible cells' cells'' )
            taglist
        in
        match taglist' with
        | [] ->
            None
        | _ :: _ ->
            if not @@ is_compatible cells cells'
            then None
            else
              let possible_states = TagSet.of_list taglist' in
              let block =
                remove_dead_branches_block possible_states cells sync block
              in
              Some (TagMultiple taglist', block)
      in
      let branches = List.filter_map branch_aux branches in
      (*if branches = []
        then begin
          StackLangPrinter.print_block stderr block;
          output_string stderr "\n";
          StackLangPrinter.print_states stderr program.states;
          exit 1
        end;*)
      (reg, branches) )
    block


let remove_dead_branches_routine program t_block =
  let { block; stack_type } = t_block in
  let all = represented_states program in
  { t_block with
    block =
      remove_dead_branches_block program all stack_type (Sync.Synced 0) block
  }


let remove_dead_branches program =
  Program.map (remove_dead_branches_routine program) program


let commute_pushes program =
  remove_dead_branches (Program.map (commute_pushes_routine program) program)


let ( + ) = RegisterSet.union

let ( - ) = RegisterSet.diff

let ( -^ ) regs reg = RegisterSet.remove reg regs

let ( +^ ) regs reg = RegisterSet.add reg regs

let empty = RegisterSet.empty

let set_of_option = function Some s -> s | None -> empty

let unions = List.fold_left ( + ) empty

let rec rud_block program block =
  let cfg = program.cfg in
  let rud_block = rud_block program in
  match block with
  | IDef (binds, block) ->
      let used, block = rud_block block in
      let binds = Bindings.restrict used binds in
      let used = used - Bindings.domain binds in
      let used = used + Bindings.codomain binds in
      (used, Block.def binds block)
  | IPop (pattern, block) ->
      let used, block = rud_block block in
      let pattern = Pattern.restrict used pattern in
      let used = used - Pattern.registers pattern in
      (used, IPop (pattern, block))
  | IPush (value, cell, block) ->
      let used, block = rud_block block in
      let used = used + Value.registers value in
      (used, IPush (value, cell, block))
  | IPrim (register, prim, block) ->
      let used, block = rud_block block in
      let comment1 =
        sprintf
          "used before prim : %s"
          (String.concat " " (RegisterSet.elements used))
      in
      let prim =
        match prim with
        | PrimOCamlAction (bds, action) ->
            let bds = Bindings.restrict (Action.vars action) bds in
            PrimOCamlAction (bds, action)
        | _ ->
            prim
      in
      let used = used -^ register in
      let used = used + Primitive.registers prim in
      let comment2 =
        sprintf
          "used after prim : %s"
          (String.concat " " (RegisterSet.elements used))
      in
      ( used
      , IComment (comment1, IComment (comment2, IPrim (register, prim, block)))
      )
  | ITrace (trace, block) ->
      let used, block = rud_block block in
      let used_trace =
        match trace with
        | TraceMessage _ ->
            empty
        | TracePositions (_, v1, v2) ->
            let s1 = set_of_option @@ Option.map Value.registers v1 in
            let s2 = set_of_option @@ Option.map Value.registers v2 in
            s1 + s2
      in
      let used = used + used_trace in
      (used, ITrace (trace, block))
  | IComment (comment, block) ->
      let used, block = rud_block block in
      (used, IComment (comment, block))
  | IDie ->
      (empty, IDie)
  | IReturn v ->
      (Value.registers v, IReturn v)
  | IJump label ->
      let used = needed (lookup label cfg) in
      (used, IJump label)
  | ICaseToken (register, branches, default) ->
      let used_li, branches =
        List.split @@ List.map (rud_case_tok_branch program) branches
      in
      let used_default, default =
        Option.split @@ Option.map rud_block default
      in
      let used_default = set_of_option used_default in
      let used = used_default + unions used_li +^ register in
      (used, ICaseToken (register, branches, default))
  | ICaseTag (register, branches) ->
      let used_li, branches =
        List.split @@ List.map (rud_case_tag_branch program) branches
      in
      let used = unions used_li +^ register in
      (used, ICaseTag (register, branches))
  | ITypedBlock tblock ->
      let { block; needed_registers } = tblock in
      let used, block = rud_block block in
      assert (RegisterSet.subset used needed_registers);
      (used, ITypedBlock { tblock with block; needed_registers = used })


and rud_case_tok_branch program (tokpat, block) =
  let used, block = rud_block program block in
  match tokpat with
  | TokSingle (tok, register) ->
      let used = used -^ register in
      (used, (TokSingle (tok, register), block))
  | TokMultiple toks ->
      (used, (TokMultiple toks, block))


and rud_case_tag_branch program (tags, block) =
  let used, block = rud_block program block in
  (used, (tags, block))


(** remove definitions of register that are not used.
    This not guaranteed to delete every unused register : some become unused
    across jump, and a more complicated fixed-point computation that is absent
    here would required to delete them. *)
let remove_unused_defs program =
  Program.map
    (fun t_block ->
      let needed_registers, block = rud_block program t_block.block in
      { t_block with block; needed_registers } )
    program


let rec compute_has_case_tag_block = function
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


let rec rsbct_block program cells sync final_type block =
  let module Annotate =
    Annotate.Curry (struct
      let program = program

      let cells = cells

      let sync = sync
    end)
  in
  match block with
  | IPush (value, cell, block) ->
      let cells, sync = Annotate.push value cell in
      let block = rsbct_block program cells sync final_type block in
      IPush (value, cell, block)
  | IPop (pattern, block) ->
      let cells, sync = Annotate.pop pattern in
      let block = rsbct_block program cells sync final_type block in
      IPop (pattern, block)
  | ICaseTag (reg, branches) ->
    begin
      match branches with
      | _ :: _ :: _ ->
          let branches =
            List.map
              (fun (tagpat, block) ->
                let cells, sync = Annotate.case_tag_branch tagpat block in
                let block = rsbct_block program cells sync final_type block in
                (tagpat, block) )
              branches
          in
          ICaseTag (reg, branches)
      | [ (tagpat, block) ] ->
          let (TagMultiple taglist) = tagpat in
          let final_type' = final_type_intersection program.states taglist in
          let cells', sync' = Annotate.case_tag_branch tagpat block in
          let block = rsbct_block program cells' sync' final_type' block in
          if Array.length cells >= Array.length cells'
             && final_type = final_type'
             (* TODO : *)
          then
            let comment =
              sprintf
                "Single branch optim : performed Removed single branch casetag \
                 on %s"
                (String.concat " | " (List.map string_of_tag taglist))
            in
            IComment (comment, block)
          else
            let block = ICaseTag (reg, [ (tagpat, block) ]) in
            let comment =
              if final_type = final_type'
              then "Single branch optim : not performed, because of cells."
              else "Single branch optim : not performed, because of final type."
            in
            IComment (comment, block)
      | [] ->
          IDie
    end
  | ITypedBlock t_block ->
      let cells, sync = Annotate.typed_block t_block in
      let block =
        rsbct_block program cells sync t_block.final_type t_block.block
      in
      ITypedBlock { t_block with block }
  | block ->
      Block.map (rsbct_block program cells sync final_type) block


let remove_single_branch_casetag_routine program _label routine =
  (* try *)
  { routine with
    block =
      rsbct_block
        program
        routine.stack_type
        (Sync.Synced 0)
        routine.final_type
        routine.block
  }


(* with
   | Assert_failure _e ->
       StackLangPrinter.print_tblock stderr routine;
       output_string stderr "\n";
       failwith label *)

let remove_single_branch_casetag program =
  Program.mapi (remove_single_branch_casetag_routine program) program


let compute_has_case_tag_t_block t_block =
  let { block } = t_block in
  let block, has_case_tag = compute_has_case_tag_block block in
  { t_block with block; has_case_tag }


let compute_has_case_tag program =
  Program.map compute_has_case_tag_t_block program


let optimize program =
  program
  |> (if Settings.commute_pushes then commute_pushes else Fun.id)
  |> remove_dead_branches
  |> remove_single_branch_casetag
  |> remove_unused_defs
  |> compute_has_case_tag


let test () =
  let module SL = EmitStackLang.Run () in
  let program = SL.program in
  let optimized_program = optimize program in
  StackLangTester.test program;
  StackLangTester.test optimized_program
