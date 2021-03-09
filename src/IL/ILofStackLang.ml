module S = StackLang
module T = IL
open T
open CodePieces
open CodeBits

module SSymbols = StackSymbols.Run ()

(* Name used for the tail type variable. *)
let tctail = "t_" ^ prefix "tail"

(* Name used for the final type variable. *)
let tcfinal = "t_" ^ prefix "final"

let flexer = prefix "lexer"

let flexbuf = prefix "lexbuf"

let fstate = prefix "s"

let fstack = prefix "stack"

let discard = prefix "discard"

let statecon s = dataprefix (Printf.sprintf "State%d" s)

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

let typ_stack_app tail_type final_type cells =
  let cell_typ (tail : typ)
      S.{typ; hold_semv; hold_state; hold_startpos; hold_endpos} =
    TypTuple
      ( [tail]
      @ if1 hold_state (TypApp (tcstate, [tail; final_type]))
      @ if1 hold_semv
          (match typ with None -> tunit | Some typ -> TypTextual typ)
      @ if1 hold_startpos tposition
      @ if1 hold_endpos tposition )
  in
  Array.fold_left cell_typ tail_type cells

(* type definition for states *)
let statetypedef states =
  let final_type s =
    (* The final return type is only known for the start and end states. *)
    match Lr1.is_start_or_exit s with
    | None ->
        TypVar tcfinal
    | Some (nonterminal : Grammar.Nonterminal.t) ->
        TypTextual (Grammar.Nonterminal.ocamltype_of_start_symbol nonterminal)
  in
  let type_of_tag s cells =
    typ_stack_app (TypVar tctail) (final_type s) cells
  in
  { typename= tcstate
  ; typeparams= [tctail; tcfinal]
  ; typerhs=
      TDefSum
        (Lr1.NodeMap.fold
           (fun s cells defs ->
             (* TODO : restore the condition *)
             { dataname= statecon @@ Lr1.number s
             ; datavalparams= []
             ; datatypeparams= Some [type_of_tag s cells; final_type s]
             ; comment=
                 Some
                   ( (" Known stack symbols : " ^ SSymbols.print_stack_symbols s)
                   ^ " " ) }
             :: defs)
           states [])
  ; typeconstraint= None }

(*type s_program = S.program*)

open BasicSyntax

(* ------------------------------------------------------------------------ *)
(* Code production for the entry points. *)

(* This is the entry point associated with a start state [s]. By convention,
   it is named after the nonterminal [nt] that corresponds to this state.
   This is a public definition.

   The code initializes a parser environment, an empty stack, and invokes
   [run].

   2015/11/11. If the state [s] can reduce an epsilon production whose left-hand
   symbol keeps track of its start or end position, or if [s] can reduce any
   production that mentions [$endpos($0)], then the initial stack should contain
   a sentinel cell with a valid [endp] field at offset 1. For simplicity, we
   always create a sentinel cell. *)

(* When we allocate a fresh parser environment, the [token] field receives a
   dummy value. It will be overwritten by the first call to [run], which will
   invoke [discard]. This allows us to invoke the lexer in just one place. *)

let entrydef s =
  let run s =
    prefix (Printf.sprintf "run_%s" (Misc.padded_index Lr1.n (Lr1.number s)))
  in
  let nt = Item.startnt (Lr1.start2item s) in
  { valpublic= true
  ; valpat= PVar (Grammar.Nonterminal.print true nt)
  ; valval=
      EFun
        ( [PVar flexer; PVar flexbuf]
        , EApp (EVar (run s), [EUnit; EVar flexer; EVar flexbuf]) ) }

let grammar = Front.grammar

let function_type block =
  let final =
    match S.(block.final_type) with None -> TypName tcfinal | Some typ -> typ
  in
  { quantifiers=
      ( match S.(block.final_type) with
      | None ->
          [tctail; tcfinal]
      | Some _ ->
          [tctail] )
  ; locally_abstract= true
  ; body=
      (let typ_tail =
         typ_stack_app (TypName tctail) final S.(block.stack_type)
       in
       marrow
         ( typ_tail
         :: List.map
              (function
                | s when s = fstate ->
                    TypApp (tcstate, [typ_tail; final])
                | _ ->
                    TypVar "_")
              S.(block.needed_registers) )
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
      EApp (EVar f, List.map (fun arg -> EVar arg) args)
  | S.PrimOCamlFieldAccess (record, field) ->
      ERecordAccess (EVar record, field)
  | S.PrimOCamlDummyPos ->
      EVar "Lexing.dummy_pos"
  | S.PrimOCamlAction action ->
      Action.to_il_expr action

let rec compile_block (cfg : StackLang.typed_block StringMap.t) t_block =
  let rec compile_ICaseToken register tokpat_block_list block_option =
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
                        ELet ([(PVar register, EUnit)], compile_block_aux block)
                    | Some _ ->
                        compile_block_aux block ) }
            | S.TokMultiple terminals ->
                { branchpat=
                    POr
                      (List.map
                         (fun terminal -> CodePieces.tokpat terminal PWildcard)
                         (Grammar.TerminalSet.elements terminals))
                ; branchbody= compile_block_aux block })
          tokpat_block_list
        @
        match block_option with
        | None ->
            []
        | Some block ->
            [{branchpat= PWildcard; branchbody= compile_block_aux block}] )
  and compile_ICaseTag register tagpat_block_list =
    EMatch
      ( EVar register
      , let branches =
          List.concat
          @@ List.map
               (fun (tagpat, block) ->
                 let (S.TagMultiple tag_list) = tagpat in
                 List.map
                   (fun tag ->
                     { branchpat= pstatescon [tag]
                     ; branchbody= compile_block_aux block })
                   tag_list)
               tagpat_block_list
        in
        if List.length branches = Invariant.n_represented then branches
        else
          branches
          @ [ (* TODO : remove this *)
              { branchpat= PWildcard
              ; branchbody= EApp (EVar "assert", [EVar "false"]) } ] )
  and compile_block_aux =
    S.(
      function
      | S.INeed (_registers, block) ->
          compile_block_aux block
      | S.IPush (value, block) -> (
        match value with
        | S.VTuple [] ->
            compile_block_aux block
        | _ ->
            let value =
              match compile_value value with
              | ETuple li ->
                  li
              | _ ->
                  assert false
            in
            ELet
              ( [(PVar fstack, ETuple (EVar fstack :: value))]
              , compile_block_aux block ) )
      | S.IPop (pattern, block) -> (
        match pattern with
        | S.PTuple [] ->
            compile_block_aux block
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
              , compile_block_aux block ) )
      | S.IDef (pattern, value, block) -> (
        match pattern with
        | S.PTuple [] ->
            compile_block_aux block
        | _ ->
            ELet
              ( [(compile_pattern pattern, compile_value value)]
              , compile_block_aux block ) )
      | S.IPrim (register, primitive, block) ->
          ELet
            ( [(PVar register, compile_primitive primitive)]
            , compile_block_aux block )
      | S.ITrace (message, block) ->
          ELet
            ( [(PVar "_", EApp (EVar "Printf.eprintf", [EStringConst message]))]
            , compile_block_aux block )
      | S.IComment (comment, block) ->
          EComment (comment, compile_block_aux block)
      | S.IDie ->
          ERaise (EVar "_eRR")
      | S.IReturn register -> (
        match S.(t_block.final_type) with
        | None ->
            EVar register
        | Some body ->
            EAnnot
              (EVar register, {quantifiers= []; locally_abstract= false; body})
        )
      | S.IJump label ->
          EApp
            ( EVar label
            , e_common_args
              @ List.map
                  (fun s -> EVar s)
                  S.((StringMap.find label cfg).needed_registers) )
      | S.ISubstitutedJump (label, substitution) ->
          EApp
            ( EVar label
            , e_common_args
              @ List.map
                  (fun s ->
                    compile_value (Substitution.apply substitution (S.VReg s)))
                  S.((StringMap.find label cfg).needed_registers) )
      | S.ICaseToken (register, tokpat_block_list, block_option) ->
          compile_ICaseToken register tokpat_block_list block_option
      | S.ICaseTag (register, tagpat_block_list) ->
          compile_ICaseTag register tagpat_block_list
      | S.ITypedBlock ({needed_registers; has_case_tag} as t_block) ->
          (* If a typed block contains a match on tags, then we need to make it
             an inlined function call : it is not possible to inline it
             ourselve, because of GADT shenanigans.*)
          if has_case_tag then
            let block_name = fresh_name () in
            EInlinedLet
              ( [(PVar block_name, compile_function t_block cfg)]
              , EApp
                  ( EVar block_name
                  , e_common_args @ List.map (fun s -> EVar s) needed_registers
                  ) )
          else compile_block cfg t_block)
  in
  compile_block_aux S.(t_block.block)

and compile_function t_block cfg =
  EAnnot
    ( EFun
        ( p_common_args @ List.map (fun s -> PVar s) S.(t_block.needed_registers)
        , compile_block cfg t_block )
    , function_type t_block )

let compile (S.{cfg; entry; states} : S.program) =
  let entries =
    Lr1.NodeMap.fold
      (fun _ label acc -> StringSet.add label acc)
      entry StringSet.empty
  in
  [ SIFunctor
      ( grammar.parameters
      , mbasics grammar
        @ [ SITypeDefs [statetypedef states]
          ; SIStretch grammar.preludes
          ; SIValDefs (false, [discarddef])
          ; SIValDefs
              ( true
              , StringMap.fold
                  (fun _ block acc -> block :: acc)
                  (StringMap.mapi
                     (fun name t_block ->
                       { valpublic= StringSet.mem name entries
                       ; valpat= PVar name
                       ; valval= compile_function t_block cfg })
                     cfg)
                  [] )
          ; SIValDefs
              ( false
              , Grammar.ProductionMap.fold
                  (fun _ s defs -> entrydef s :: defs)
                  Lr1.entry [] )
          ; SIStretch grammar.postludes ] ) ]
