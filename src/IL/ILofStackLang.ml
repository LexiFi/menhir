module S = StackLang
module T = IL
open T
open CodePieces
open CodeBits
open Printf
open StackLangUtils
module Subst = S.Substitution

module SSymbols = StackSymbols.Run ()

(* Name used for the tail type variable. *)
let tctail = tprefix "tail"

(* Name used for the final type variable. *)
let tcfinal = tprefix "final"

let flexer = prefix "lexer"

let flexbuf = prefix "lexbuf"

let _fstate = prefix "s"

let fstack = prefix "stack"

let discard = prefix "discard"

let statecon s = dataprefix (sprintf "State%d" s)

let estatecon s = EData (statecon s, [])

let pstatecon s = PData (statecon s, [])

let pstatescon ss = POr (List.map pstatecon ss)

let discarddef =
  { valpublic= false
  ; valpat= PVar discard
  ; valval=
      EFun ([PVar flexer; PVar flexbuf], EApp (EVar flexer, [EVar flexbuf])) }

let e_common_args = [(*EVar flexer; EVar flexbuf;*) EVar fstack]

let p_common_args = [(*PVar flexer; PVar flexbuf;*) PVar fstack]

(* The type of environments. *)
let tcstate = prefix "state"

let base_tcstack = "stack"

let nth_bit x n = x land (1 lsl n) <> 0

let tcstack_of_int n =
  (* [binary_print size () x] return the binary representation of [x] in a
     string of size [size] *)
  let binary_print size () x =
    let bytes = Bytes.create size in
    for i = 0 to size - 1 do
      Bytes.set bytes i (if nth_bit x i then '1' else '0')
    done ;
    Bytes.to_string bytes
  in
  prefix (sprintf "%s_%a" base_tcstack (binary_print 4) n)

let tcstack hold_state hold_semv hold_startpos hold_endpos =
  prefix
    (sprintf "%s_%d%d%d%d" base_tcstack (Bool.to_int hold_state)
       (Bool.to_int hold_semv)
       (Bool.to_int hold_startpos)
       (Bool.to_int hold_endpos))

(** A list of stack types abbrevations. Since there are 4 different fields a
    stack cell can hold or not hold, there are 15 different stack type
    abbrevations needed to represent all of them (2^4 - 1) *)
let stacktypeabbrevdefs =
  (* The 15 numbers. In binary representation, each bit represent whether
     a particular field is present or not. The values range from [0001]^2 to
     [1111]^2. *)
  let numbers = Array.init 15 (( + ) 1) in
  let typesdefs =
    Array.map
      (fun i ->
        let hold_state = nth_bit i 0 in
        let hold_semv = nth_bit i 1 in
        let hold_startpos = nth_bit i 2 in
        let hold_endpos = nth_bit i 3 in
        { typename= tcstack_of_int i
        ; typeparams= ["tail"] @ List.if1 hold_semv "semantic" @ ["final"]
        ; typerhs=
            TAbbrev
              (TypTuple
                 ( [TypVar "tail"]
                 @ List.if1 hold_state
                     (TypApp (tcstate, [TypVar "tail"; TypVar "final"]))
                 @ List.if1 hold_semv (TypVar "semantic")
                 @ List.if1 hold_startpos (TypName "Lexing.position")
                 @ List.if1 hold_endpos (TypName "Lexing.position") ))
        ; typeconstraint= None })
      numbers
  in
  Array.to_list typesdefs

(** [stack_type_of_cell_info tail final cell] return a [T.typ] of a stack where
    we the top stack cell is of shape [cell]. *)

let stack_type_of_cell_info tail final =
  S.(
    function
    | { hold_state= false
      ; hold_semv= false
      ; hold_startpos= false
      ; hold_endpos= false } ->
        tail
    | {typ; hold_state; hold_semv; hold_startpos; hold_endpos} ->
        TypApp
          ( tcstack hold_state hold_semv hold_startpos hold_endpos
          , [tail]
            @ List.if1 hold_semv
                (match typ with None -> tunit | Some typ -> TypTextual typ)
            @ [final] ))

(** [typ_stack_app tail final cells] return a value of type [T.typ] that
    correspond to the known part of the stack when [cells] are the known stack
    cells. *)
let typ_stack_app tail_type final_type cells =
  let cell_typ tail cell = stack_type_of_cell_info tail final_type cell in
  Array.fold_left cell_typ tail_type cells

(* type definition for states *)
let statetypedef states =
  { typename= tcstate
  ; typeparams= [tctail; tcfinal]
  ; typerhs=
      TDefSum
        (S.TagMap.fold
           (fun tag S.{known_cells; sfinal_type} defs ->
             let final_type =
               match sfinal_type with
               | None ->
                   TypVar tcfinal
               | Some typ ->
                   TypTextual typ
             in
             { dataname= statecon tag
             ; datavalparams= []
             ; datatypeparams=
                 Some
                   [ typ_stack_app (TypVar tctail) final_type known_cells
                   ; final_type ]
             ; comment=
                 None
                 (* Some
                    (sprintf " Known stack symbols : %s "
                       (SSymbols.print_stack_symbols s)) } *) }
             :: defs)
           states [])
  ; typeconstraint= None }

open BasicSyntax

(* -------------------------------------------------------------------------- *)
(* Code production for the entry points. *)

(* This is the entry point associated with a start state [s]. By convention,
   it is named after the nonterminal [nt] that corresponds to this state.
   This is a public definition.

   The code initializes an empty stack, and invokes
   [run]. *)

let entrydef name call cfg =
  let S.{needed_registers} = StringMap.find call cfg in
  { valpublic= true
  ; valpat= PVar name
  ; valval=
      EAnnot
        ( EFun
            ( [PVar flexer; PVar flexbuf]
            , blet
                ( [(PVar stack, EUnit)]
                , EApp
                    ( EVar call
                    , e_common_args @ evars
                      @@ StringSet.elements needed_registers ) ) )
        , type2scheme
            (marrow [arrow tlexbuf (TypName "token"); tlexbuf] (TypName "_")) )
  }

let function_type S.{final_type; stack_type; needed_registers; state_register} =
  let final =
    match final_type with None -> TypName tcfinal | Some typ -> TypTextual typ
  in
  { quantifiers=
      (match final_type with None -> [tctail; tcfinal] | Some _ -> [tctail])
  ; locally_abstract= true
  ; body=
      (let typ_tail = typ_stack_app (TypName tctail) final stack_type in
       marrow
         ( typ_tail
         :: List.map
              (function
                | s when s = state_register ->
                    TypApp (tcstate, [typ_tail; final])
                | _ ->
                    TypVar "_")
              (S.RegisterSet.elements needed_registers) )
         final) }

let rec compile_pattern = function
  | S.PWildcard ->
      PWildcard
  | S.PReg x ->
      PVar x
  | S.PTuple [] ->
      assert false
  | S.PTuple li ->
      PTuple (List.map compile_pattern li)

let rec compile_value = function
  | S.VTag tag ->
      estatecon tag
  | S.VReg register ->
      EVar register
  | S.VTuple value_list ->
      ETuple (List.map compile_value value_list)
  | S.VUnit ->
      EUnit

let compile_primitive = function
  | S.PrimOCamlCall (f, args) ->
      EApp (EVar f, List.map (fun arg -> compile_value arg) args)
  | S.PrimOCamlFieldAccess (record, field) ->
      ERecordAccess (EVar record, field)
  | S.PrimOCamlDummyPos ->
      EVar "Lexing.dummy_pos"
  | S.PrimOCamlAction action ->
      Action.to_il_expr action
  | S.PrimSubstOcamlAction (subst, action) ->
      EComment
        ( "Restoring definitions"
        , blet
            ( Subst.fold
                (fun reg value defs ->
                  if
                    StringSet.mem reg (Action.semvars action)
                    || StringSet.mem reg (Action.posvars action)
                  then (PVar reg, compile_value value) :: defs
                  else defs)
                subst []
            , EComment ("End of restore", Action.to_il_expr action) ) )

let rec compile_t_block (program : S.program) t_block =
  let cfg = S.(program.cfg) in
  (* let states = S.(program.states) in *)
  let final_type =
    match S.(t_block.final_type) with
    | None ->
        TypName tcfinal
    | Some typ ->
        TypTextual typ
  in
  let rec compile_block = function
    | S.INeed (registers, block) ->
        EComment
          ( sprintf "Needed registers : { %s }"
              (String.concat "; " (StringSet.elements registers))
          , compile_block block )
    | S.IPush (value, _cell, block) -> (
      match value with
      | S.VTuple [] ->
          compile_block block
      | _ ->
          let value =
            match compile_value value with ETuple li -> li | _ -> assert false
          in
          ELet
            ([(PVar fstack, ETuple (EVar fstack :: value))], compile_block block)
      )
    | S.IPop (pattern, block) -> (
      match pattern with
      | S.PTuple [] ->
          compile_block block
      | _ ->
          let pattern =
            match compile_pattern pattern with
            | T.PTuple li ->
                li
            | _ ->
                assert false
          in
          ELet
            ( [(T.PTuple (PVar fstack :: pattern), EVar fstack)]
            , compile_block block ) )
    | S.IDef (pattern, value, block) -> (
      match pattern with
      | S.PTuple [] ->
          compile_block block
      | _ ->
          ELet
            ( [(compile_pattern pattern, compile_value value)]
            , compile_block block ) )
    | S.IPrim (register, primitive, block) ->
        EComment
          ( "Primitive"
          , ELet
              ( [(PVar register, compile_primitive primitive)]
              , compile_block block ) )
    | S.ITrace (message, block) ->
        ELet
          ( [(PVar "_", EApp (EVar "Printf.eprintf", [EStringConst message]))]
          , compile_block block )
    | S.IComment (comment, block) ->
        EComment (comment, compile_block block)
    | S.IDie ->
        ERaise (EVar "_eRR")
    | S.IReturn value ->
        (* If there is a known final type, we need to annotate the return value
           with it. It is unclear why, but if we dont, the program may fail to
           type.*)
        EAnnot (compile_value value, type2scheme final_type)
    | S.IJump label ->
        EApp
          ( EVar label
          , e_common_args @ evars @@ S.RegisterSet.elements
            @@ needed (StringMap.find label cfg) )
    | S.ISubstitutedJump (label, substitution) ->
        EApp
          ( EVar label
          , e_common_args
            @ List.map
                (fun s -> compile_value (Subst.apply substitution (S.VReg s)))
                (S.RegisterSet.elements @@ needed @@ StringMap.find label cfg)
          )
    | S.ICaseToken (register, tokpat_block_list, block_option) ->
        compile_ICaseToken register tokpat_block_list block_option
    | S.ICaseTag (register, tagpat_block_list) ->
        compile_ICaseTag register tagpat_block_list
    | S.ITypedBlock t_block ->
        compile_ITypedBlock t_block
  and compile_ICaseToken register tokpat_block_list block_option =
    EMatch
      ( EVar register
      , List.map
          (fun (tokpat, block) ->
            match tokpat with
            | S.TokSingle (terminal, register) ->
                { branchpat= CodePieces.tokpat terminal (PVar register)
                ; branchbody=
                    ( match Grammar.Terminal.ocamltype terminal with
                    | None ->
                        ELet ([(PVar register, EUnit)], compile_block block)
                    | Some _ ->
                        compile_block block ) }
            | S.TokMultiple terminals ->
                { branchpat=
                    POr
                      (List.map
                         (fun terminal -> CodePieces.tokpat terminal PWildcard)
                         (Grammar.TerminalSet.elements terminals))
                ; branchbody= compile_block block })
          tokpat_block_list
        @
        match block_option with
        | None ->
            []
        | Some block ->
            [{branchpat= PWildcard; branchbody= compile_block block}] )
  and compile_ICaseTag register tagpat_block_list =
    let branches =
      List.map
        (fun (tagpat, block) ->
          let (S.TagMultiple tag_list) = tagpat in
          match tag_list with
          | [] ->
              assert false
          | [tag] ->
              ( None
              , [{branchpat= pstatescon [tag]; branchbody= compile_block block}]
              )
          | _ -> (
            match block with
            | S.ITypedBlock (S.{needed_registers} as t_block) ->
                let name = fresh_name () in
                let f = compile_function t_block program in
                ( Some (PVar name, f)
                , (*[ { branchpat= pstatescon tag_list
                    ; branchbody= compile_block_aux block } ] ))*)
                  List.map
                    (fun tag ->
                      { branchpat= pstatescon [tag]
                      ; branchbody=
                          EApp
                            ( EVar name
                            , e_common_args @ evars
                              @@ StringSet.elements needed_registers ) })
                    tag_list )
            | (S.IJump _ | S.ISubstitutedJump _) as block ->
                ( None
                , List.map
                    (fun tag ->
                      { branchpat= pstatescon [tag]
                      ; branchbody= compile_block block })
                    tag_list )
            | _ ->
                failwith "cannot find t_block" ))
        tagpat_block_list
    in
    let prelude, branches = List.split branches in
    let prelude = List.filter_map Fun.id prelude in
    let branches = List.concat branches in
    EInlinedLet
      ( prelude
      , EMatch
          ( EVar register
          , if List.length branches = Invariant.n_represented then branches
            else
              branches
              @ [ (* TODO : remove this *)
                  { branchpat= PWildcard
                  ; branchbody= EApp (EVar "assert", [EVar "false"]) } ] ) )
  and compile_ITypedBlock = function
    | S.({has_case_tag= true} as t_block) ->
        (* If a typed block contains a match on tags, then we need to make it
           an inlined function call : it is not possible to inline it
           ourselves, because of GADT shenanigans.*)
        let block_name = fresh_name () in
        EInlinedLet
          ( [ ( PVar block_name
              , compile_function (*~quantify_final:false*) t_block program ) ]
          , EApp
              ( EVar block_name
              , e_common_args @ evars @@ StringSet.elements @@ needed t_block )
          )
    | t_block ->
        compile_t_block program t_block
  in
  let S.{block; stack_type; state_register; final_type; needed_registers} =
    t_block
  in
  EComment
    ( sprintf "Typed block : stack_type=%i, state_register=%s, final_type=%s"
        (Array.length stack_type) state_register
        (match final_type with None -> "None" | Some _ -> "Some")
    , EComment
        ( sprintf "Needed registers : { %s }"
            (String.concat "; " (StringSet.elements needed_registers))
        , compile_block block ) )

and compile_function t_block (program : S.program) =
  EAnnot
    ( EFun
        ( p_common_args @ pvars @@ S.RegisterSet.elements @@ needed t_block
        , compile_t_block program t_block )
    , function_type t_block )

let compile (S.({cfg; entry; states} as program) : S.program) =
  [ SIFunctor
      ( Front.grammar.parameters
      , mbasics Front.grammar
        @ [ SIStretch Front.grammar.preludes
          ; SITypeDefs (statetypedef states :: stacktypeabbrevdefs)
          ; SIValDefs (false, [discarddef])
          ; SIValDefs
              ( true
              , List.map snd
                  (StringMap.bindings
                     (StringMap.mapi
                        (fun name t_block ->
                          { valpublic= false
                          ; valpat= PVar name
                          ; valval= compile_function t_block program })
                        cfg)) )
          ; SIValDefs
              ( false
              , StringMap.fold
                  (fun name call defs -> entrydef name call cfg :: defs)
                  entry [] )
          ; SIStretch Front.grammar.postludes ] ) ]
