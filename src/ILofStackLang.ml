module S = StackLang
module T = IL
open CodePieces
open CodeBits

module SSymbols = StackSymbols.Run ()

let flexer = prefix "lexer"

let flexbuf = prefix "lexbuf"

let fstate = prefix "s"

let fstack = prefix "stack"

let discard = prefix "discard"

let statecon s = dataprefix (Printf.sprintf "State%d" s)

let estatecon s = T.EData (statecon s, [])

let pstatecon s = T.PData (statecon s, [])

let pstatescon ss = T.POr (List.map pstatecon ss)

let discarddef =
  T.
    { valpublic= false
    ; valpat= PVar discard
    ; valval=
        T.EFun
          ( [T.PVar flexer; T.PVar flexbuf]
          , T.EApp (T.EVar flexer, [T.EVar flexbuf]) ) }

let e_common_args = [(*T.EVar flexer; T.EVar flexbuf;*) T.EVar fstack]

let p_common_args = [(*T.PVar flexer; T.PVar flexbuf;*) T.PVar fstack]

(* The type of environments. *)
let tcstack = prefix "stack"

let typ_stack_app tail_type types =
  Array.fold_left
    (fun tail sem_type -> T.TypApp (tcstack, [tail; sem_type]))
    tail_type types

(* This is the type of states. Only states that are represented are
   declared. *)
(* The type of states. *)

let tcstate = prefix "state"

(*let tstate = T.TypApp (tcstate, [])*)

let stacktypedef =
  T.
    { typename= tcstack
    ; typeparams= ["tail"; "semantic"]
    ; typerhs=
        TAbbrev
          (T.TypTuple
             [ T.TypVar "tail"
             ; T.TypApp (tcstate, [T.TypVar "tail"])
             ; T.TypVar "semantic"
             ; T.TypName "Lexing.position"
             ; T.TypName "Lexing.position" ])
    ; typeconstraint= None }

let tag_types = Array.make Lr1.n (Obj.magic ())

let statetypedef =
  let type_of_tag s =
    let f tail_name =
      let word = Invariant.stack s in
      Invariant.fold
        (fun typ_tail _ symbol _ ->
          let typ_semantic =
            match semvtype symbol with
            | [] ->
                T.TypName "unit"
            | li ->
                T.TypTuple li
          in
          T.TypApp (tcstack, [typ_tail; typ_semantic]))
        (T.TypName tail_name) word
    in
    tag_types.(Lr1.number s) <- f ;
    f "'tail"
  in
  T.
    { typename= tcstate
    ; typeparams= ["tail"]
    ; typerhs=
        TDefSum
          (Lr1.fold
             (fun defs s ->
               (* TODO : restore the condition *)
               if (*Invariant.represented s*) true then
                 { dataname= statecon @@ Lr1.number s
                 ; datavalparams= []
                 ; datatypeparams= Some [type_of_tag s] }
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
  T.
    { valpublic= true
    ; valpat= PVar (Grammar.Nonterminal.print true nt)
    ; valval=
        EFun
          ( [PVar flexer; PVar flexbuf]
          , EApp (EVar (run s), [EUnit; EVar flexbuf; EVar flexer]) ) }

let grammar = Front.grammar

type block_info = {needed_registers: string list}

(*let stack_type (cfg : S.typed_block StringMap.t) label =
  let n_pops block =
    let rec aux (i : int) = function
      | S.INeed (_registers, block) ->
          aux i block
      | S.IPush (_value, block) ->
          aux (i - 1) block
      | S.IPop (_pattern, block) ->
          aux (i + 1) block
      | S.IDef (_pattern, _value, block) ->
          aux i block
      | S.IPrim (_register, _primitive, block) ->
          aux i block
      | S.ITrace (_message, block) ->
          aux i block
      | S.IComment (_comment, block) ->
          aux i block
      | S.IDie | S.IReturn _ | S.IJump _ | S.ICaseTag _ ->
          i
      | S.ICaseToken (_register, tokpat_block_list, _block_option) ->
          let li = List.map (fun (_, block) -> aux i block) tokpat_block_list in
          list_max compare li
    in
    aux 0 block
  in
  let block = StringMap.find label cfg in
  let n_pops = n_pops S.(block.block) in
  ( ( match S.(block.info) with
    | S.InfGoto _ ->
        T.TypName "tail"
    | S.InfReduce production ->
        let symbols = Grammar.Production.rhs production in
        let types = Array.map semvtype symbols in
        assert (n_pops >= 0) ;
        typ_stack_app (T.TypName "tail") types
    | S.InfRun node ->
        assert (n_pops <= 0) ;
        let symbols = SSymbols.stack_symbols node in
        let types = Array.map semvtype symbols in
        typ_stack_app (T.TypName "tail")
          (Array.sub types 0 (Array.length types + n_pops)) )
  , n_pops )
*)
let get_env block_map =
  StringMap.mapi
    (fun _label -> function
      | S.{block= S.INeed (registers, _block); stack_type= _} ->
          {needed_registers= StringSet.elements registers} | _ -> assert false)
    block_map

let rec compile_pattern = function
  | S.PWildcard ->
      T.PWildcard
  | S.PReg x ->
      T.PVar x
  | S.PTuple li ->
      T.PTuple (List.map compile_pattern li)

let rec compile_value = function
  | S.VTag tag ->
      estatecon tag
  | S.VReg register ->
      T.EVar register
  | S.VTuple value_list ->
      T.ETuple (List.map compile_value value_list)

let compile_primitive = function
  | S.PrimOCamlCall (f, args) ->
      T.EApp (T.EVar f, List.map (fun arg -> T.EVar arg) args)
  | S.PrimOCamlFieldAccess (record, field) ->
      T.ERecordAccess (T.EVar record, field)
  | S.PrimOCamlDummyPos ->
      T.EVar "Lexing.dummy_pos"
  | S.PrimOCamlAction action ->
      Action.to_il_expr action

let compile_block env =
  let rec compile_ICaseToken register tokpat_block_list block_option =
    T.EMatch
      ( T.EVar register
      , List.map
          (fun (tokpat, block) ->
            match tokpat with
            | S.TokSingle (terminal, register) ->
                T.
                  { branchpat= CodePieces.tokpat terminal (T.PVar register)
                  ; branchbody=
                      ( match Grammar.Terminal.ocamltype terminal with
                      | None ->
                          T.ELet
                            ([(T.PVar register, T.EUnit)], compile_block block)
                      | Some _ ->
                          compile_block block ) }
            | S.TokMultiple terminals ->
                T.
                  { branchpat=
                      T.POr
                        (List.map
                           (fun terminal ->
                             CodePieces.tokpat terminal T.PWildcard)
                           (Grammar.TerminalSet.elements terminals))
                  ; branchbody= compile_block block })
          tokpat_block_list
        @
        match block_option with
        | None ->
            []
        | Some block ->
            [T.{branchpat= T.PWildcard; branchbody= compile_block block}] )
  and compile_ICaseTag register tagpat_block_list =
    T.EMatch
      ( T.EVar register
      , ( List.concat
        @@ List.map
             (fun (tagpat, block) ->
               let (S.TagMultiple tag_list) = tagpat in
               List.map
                 (fun tag ->
                   T.
                     { branchpat= pstatescon [tag]
                     ; branchbody= compile_block block })
                 tag_list)
             tagpat_block_list )
        @ [ (* TODO : remove this *)
            T.
              { branchpat= T.PWildcard
              ; branchbody= T.EApp (T.EVar "assert", [T.EVar "false"]) } ] )
  and compile_block = function
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
          match compile_value value with T.ETuple li -> li | _ -> assert false
        in
        T.ELet
          ( [(T.PVar fstack, T.ETuple (T.EVar fstack :: value))]
          , compile_block block )
    | S.IPop (pattern, block) ->
        let pattern =
          match compile_pattern pattern with
          | T.PTuple li ->
              li
          | _ ->
              assert false
        in
        T.ELet
          ( [(T.PTuple (T.PVar fstack :: pattern), T.EVar fstack)]
          , compile_block block )
    | S.IDef (pattern, value, block) ->
        T.ELet
          ([(compile_pattern pattern, compile_value value)], compile_block block)
    (* [IPrim] invokes a primitive operation and stores its result in a
       register. *)
    | S.IPrim (register, primitive, block) ->
        T.ELet
          ([(T.PVar register, compile_primitive primitive)], compile_block block)
    (* [ITrace] logs a message on [stderr]. *)
    | S.ITrace (message, block) ->
        T.ELet
          ( [ ( T.PVar "_"
              , T.EApp (T.EVar "Printf.eprintf", [T.EStringConst message]) ) ]
          , compile_block block )
    (* [IComment] is a comment. *)
    | S.IComment (comment, block) ->
        T.EComment (comment, compile_block block)
    (* Group 2: Instructions with zero successor. *)

    (* [IDie] causes an abrupt termination of the program. It is translated
       into OCaml by raising the exception [Error]. *)
    | S.IDie ->
        T.ERaise (T.EVar "_eRR")
    (* [IReturn] causes the normal termination of the program. A value read
       from a register is returned. *)
    | S.IReturn register ->
        T.EMagic (T.EVar register)
    (* [IJump] causes a jump to a block identified by its label. The registers
       that are needed by the destination block must form a subset of the
       registers that are defined at the point of the jump. *)
    | S.IJump label ->
        T.EApp
          ( T.EVar label
          , e_common_args
            @ List.map
                (fun s -> T.EVar s)
                (StringMap.find label env).needed_registers )
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
  in
  compile_block

(* returns the number of pushes, and if found, the corresponding state *)

let function_type name block env =
  let env_current = StringMap.find name env in
  T.
    { quantifiers= ["tail"]
    ; locally_abstract= true
    ; body=
        (let typ_tail = typ_stack_app (T.TypName "tail") S.(block.stack_type) in
         marrow
           ( typ_tail
           :: List.map
                (function
                  (*| s when s = fstack ->
                      T.TypName "tail"*)
                  | s when s = fstate ->
                      T.TypApp (tcstate, [typ_tail])
                  | _ ->
                      T.TypVar "_")
                env_current.needed_registers )
           (T.TypVar "_")) }

let compile (S.{cfg; entry} : S.program) =
  let env = get_env cfg in
  let entries =
    Lr1.NodeMap.fold
      (fun _ label acc -> StringSet.add label acc)
      entry StringSet.empty
  in
  T.
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
                         ; valpat= T.PVar name
                         ; valval=
                             T.EAnnot
                               ( EFun
                                   ( p_common_args
                                     @ List.map
                                         (fun s -> T.PVar s)
                                         (StringMap.find name env)
                                           .needed_registers
                                   , compile_block env S.(t_block.block) )
                               , function_type name t_block env ) })
                       cfg)
                    [] )
            ; SIValDefs
                ( false
                , Grammar.ProductionMap.fold
                    (fun _ s defs -> entrydef s :: defs)
                    Lr1.entry [] )
            ; SIStretch grammar.postludes ] ) ]
