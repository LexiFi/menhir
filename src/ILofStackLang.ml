module S = StackLang
module T = IL
open T
open CodePieces
open CodeBits

module SSymbols = StackSymbols.Run ()

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
let tcstack = prefix "stack"

let typ_stack_app tail_type final_type types =
  Array.fold_left
    (fun tail sem_type -> TypApp (tcstack, [tail; sem_type; final_type]))
    tail_type types

(* This is the type of states. Only states that are represented are
   declared. *)
(* The type of states. *)

let tcstate = prefix "state"

(*let tstate = TypApp (tcstate, [])*)

let stacktypedef =
  { typename= tcstack
  ; typeparams= ["tail"; "semantic"; "final"]
  ; typerhs=
      TAbbrev
        (TypTuple
           [ TypVar "tail"
           ; TypApp (tcstate, [TypVar "tail"; TypVar "final"])
           ; TypVar "semantic"
           ; TypName "Lexing.position"
           ; TypName "Lexing.position" ])
  ; typeconstraint= None }

let tag_types = Array.make Lr1.n (Obj.magic ())

let statetypedef =
  let final_type s =
    match Lr1.is_start_or_exit s with
    | None ->
        TypVar "final"
    | Some (nonterminal : Grammar.Nonterminal.t) ->
        TypTextual (Grammar.Nonterminal.ocamltype_of_start_symbol nonterminal)
  in
  let type_of_tag s =
    let f tail_name final =
      let word = Invariant.stack s in
      Invariant.fold
        (fun typ_tail _ symbol _ ->
          let typ_semantic =
            match semvtype symbol with
            | [] ->
                TypName "unit"
            | [ty] ->
                ty
            | li ->
                TypTuple li
          in
          TypApp (tcstack, [typ_tail; typ_semantic; final]))
        (TypName tail_name) word
    in
    tag_types.(Lr1.number s) <- f ;
    f "'tail" (final_type s)
  in
  { typename= tcstate
  ; typeparams= ["tail"; "final"]
  ; typerhs=
      TDefSum
        (Lr1.fold
           (fun defs s ->
             (* TODO : restore the condition *)
             if (*Invariant.represented s*) true then
               { dataname= statecon @@ Lr1.number s
               ; datavalparams= []
               ; datatypeparams= Some [type_of_tag s; final_type s]
               ; comment=
                   Some
                     ( ( " Know stack symbols : "
                       ^ SSymbols.print_stack_symbols s )
                     ^ " " ) }
               :: defs
             else defs)
           [])
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
    match S.(block.final_type) with None -> TypName "final" | Some typ -> typ
  in
  { quantifiers=
      ( match S.(block.final_type) with
      | None ->
          ["tail"; "final"]
      | Some _ ->
          ["tail"] )
  ; locally_abstract= true
  ; body=
      (let typ_tail =
         typ_stack_app (TypName "tail") final S.(block.stack_type)
       in
       marrow
         ( typ_tail
         :: List.map
              (function
                (*| s when s = fstack ->
                    TypName "tail"*)
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
  | S.PTuple li ->
      PTuple (List.map compile_pattern li)

let rec compile_value = function
  | S.VTag tag ->
      estatecon tag
  | S.VReg register ->
      EVar register
  | S.VTuple value_list ->
      ETuple (List.map compile_value value_list)

let compile_primitive = function
  | S.PrimOCamlCall (f, args) ->
      EApp (EVar f, List.map (fun arg -> EVar arg) args)
  | S.PrimOCamlFieldAccess (record, field) ->
      ERecordAccess (EVar record, field)
  | S.PrimOCamlDummyPos ->
      EVar "Lexing.dummy_pos"
  | S.PrimOCamlAction action ->
      Action.to_il_expr action

let rec compile_block (cfg : StackLang.typed_block StringMap.t) =
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
    EMatch
      ( EVar register
      , ( List.concat
        @@ List.map
             (fun (tagpat, block) ->
               let (S.TagMultiple tag_list) = tagpat in
               List.map
                 (fun tag ->
                   {branchpat= pstatescon [tag]; branchbody= compile_block block})
                 tag_list)
             tagpat_block_list )
        @ [ (* TODO : remove this *)
            { branchpat= PWildcard
            ; branchbody= EApp (EVar "assert", [EVar "false"]) } ] )
  and compile_block =
    S.(
      function
      (* [INeed] is a special pseudo-instruction that is expected to appear at
         least at the beginning of every block. (It can also be used inside a
         block.) It indicates which registers are expected to be defined at this
         point, and it un-defines any registers that are not explicitly listed. *)
      | S.INeed (_registers, block) ->
          compile_block block
      (* [IPush] pushes a value onto the stack. [IPop] pops a value off the stack.
         [IDef] can be viewed as a sequence of a push and a pop. It can be used to
         move data between registers or to load a value into a register. *)
      | S.IPush (value, block) ->
          let value =
            match compile_value value with ETuple li -> li | _ -> assert false
          in
          ELet
            ([(PVar fstack, ETuple (EVar fstack :: value))], compile_block block)
      | S.IPop (pattern, block) ->
          let pattern =
            match compile_pattern pattern with
            | T.PTuple li ->
                li
            | _ ->
                assert false
          in
          ELet
            ( [(T.PTuple (PVar fstack :: pattern), EVar fstack)]
            , compile_block block )
      | S.IDef (pattern, value, block) ->
          ELet
            ( [(compile_pattern pattern, compile_value value)]
            , compile_block block )
      (* [IPrim] invokes a primitive operation and stores its result in a
         register. *)
      | S.IPrim (register, primitive, block) ->
          ELet
            ([(PVar register, compile_primitive primitive)], compile_block block)
      (* [ITrace] logs a message on [stderr]. *)
      | S.ITrace (message, block) ->
          ELet
            ( [(PVar "_", EApp (EVar "Printf.eprintf", [EStringConst message]))]
            , compile_block block )
      (* [IComment] is a comment. *)
      | S.IComment (comment, block) ->
          EComment (comment, compile_block block)
      (* Group 2: Instructions with zero successor. *)

      (* [IDie] causes an abrupt termination of the program. It is translated
         into OCaml by raising the exception [Error]. *)
      | S.IDie ->
          ERaise (EVar "_eRR")
      (* [IReturn] causes the normal termination of the program. A value read
         from a register is returned. *)
      | S.IReturn register ->
          EVar register
      (* [IJump] causes a jump to a block identified by its label. The registers
         that are needed by the destination block must form a subset of the
         registers that are defined at the point of the jump. *)
      | S.IJump label ->
          EApp
            ( EVar label
            , e_common_args
              @ List.map
                  (fun s -> EVar s)
                  S.((StringMap.find label cfg).needed_registers) )
      (* Group 3: Case analysis instructions. *)

      (* [ICaseToken] performs a case analysis on a token (which is held in a
         register). It carries a list of branches, each of which is guarded by
         a pattern, and an optional default branch. *)
      | S.ICaseToken (register, tokpat_block_list, block_option) ->
          compile_ICaseToken register tokpat_block_list block_option
      (* [ICaseTag] performs a case analysis on a tag (which is held in a
         register). It carries a list of branches, each of which is guarded by a
         pattern. There is no default branch; it is up to the user to ensure that
         the case analysis is exhaustive. *)
      | S.ICaseTag (register, tagpat_block_list) ->
          compile_ICaseTag register tagpat_block_list
      | S.ITypedBlock ({needed_registers} as t_block) ->
          let block_name = fresh_name () in
          ELet
            ( [(PVar block_name, compile_function t_block cfg)]
            , EApp
                ( EVar block_name
                , e_common_args @ List.map (fun s -> EVar s) needed_registers )
            )
      (*match final_type with
        | None ->
            EComment
              ( "Typed block with no final type"
              , ELet
                  ( [ ( T.PVar fstack
                      , EAnnot
                          ( EVar fstack
                          , { quantifiers= []
                            ; locally_abstract= false
                            ; body=
                                typ_stack_app (TypVar "tail") (TypVar "final")
                                  stack_type } ) ) ]
                  , compile_block block ) )
        | Some final_type ->
            EComment
              ( "Typed block with final type"
              , ELet
                  ( [ ( T.PVar fstack
                      , EAnnot
                          ( EVar fstack
                          , { quantifiers= []
                            ; locally_abstract= false
                            ; body=
                                typ_stack_app (TypVar "tail") final_type
                                  stack_type } ) ) ]
                  , EAnnot
                      ( compile_block block
                      , { quantifiers= []
                        ; locally_abstract= false
                        ; body= final_type } ) ) )*))
  in
  compile_block

and compile_function t_block cfg =
  EAnnot
    ( EFun
        ( p_common_args @ List.map (fun s -> PVar s) S.(t_block.needed_registers)
        , compile_block cfg S.(t_block.block) )
    , function_type t_block )

(* returns the number of pushes, and if found, the corresponding state *)

let compile (S.{cfg; entry} : S.program) =
  let entries =
    Lr1.NodeMap.fold
      (fun _ label acc -> StringSet.add label acc)
      entry StringSet.empty
  in
  [ SIFunctor
      ( grammar.parameters
      , mbasics grammar
        @ [ SITypeDefs [stacktypedef; statetypedef]
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
