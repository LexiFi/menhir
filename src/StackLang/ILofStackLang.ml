module S = StackLang
module T = IL
open T
open CodePieces
open CodeBits
open Printf
open StackLangUtils

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
  { valpublic = false
  ; valpat = PVar discard
  ; valval =
      EFun ([ PVar flexer; PVar flexbuf ], EApp (EVar flexer, [ EVar flexbuf ]))
  }


let e_common_args = [ (*EVar flexer; EVar flexbuf;*) EVar fstack ]

let p_common_args = [ (*PVar flexer; PVar flexbuf;*) PVar fstack ]

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
    done;
    Bytes.to_string bytes
  in
  prefix (sprintf "%s_%a" base_tcstack (binary_print 4) n)


let tcstack hold_state hold_semv hold_startpos hold_endpos =
  prefix
    (sprintf
       "%s_%d%d%d%d"
       base_tcstack
       (Bool.to_int hold_state)
       (Bool.to_int hold_semv)
       (Bool.to_int hold_startpos)
       (Bool.to_int hold_endpos) )


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
        { typename = tcstack_of_int i
        ; typeparams = [ "tail" ] @ MList.if1 hold_semv "semantic" @ [ "final" ]
        ; typerhs =
            TAbbrev
              (TypTuple
                 ( [ TypVar "tail" ]
                 @ MList.if1
                     hold_state
                     (TypApp (tcstate, [ TypVar "tail"; TypVar "final" ]))
                 @ MList.if1 hold_semv (TypVar "semantic")
                 @ MList.if1 hold_startpos tposition
                 @ MList.if1 hold_endpos tposition ) )
        ; typeconstraint = None
        } )
      numbers
  in
  Array.to_list typesdefs


(** [stack_type_of_cell_info tail final cell] return a [T.typ] of a stack where
    we the top stack cell is of shape [cell]. *)

let stack_type_of_cell_info tail final =
  S.(
    function
    | { hold_state = false
      ; hold_semv = false
      ; hold_startpos = false
      ; hold_endpos = false
      } ->
        tail
    | { typ; hold_state; hold_semv; hold_startpos; hold_endpos } ->
        TypApp
          ( tcstack hold_state hold_semv hold_startpos hold_endpos
          , [ tail ]
            @ MList.if1
                hold_semv
                (match typ with None -> tunit | Some typ -> TypTextual typ)
            @ [ final ] ))


(** [typ_stack_app tail final cells] return a value of type [T.typ] that
    correspond to the known part of the stack when [cells] are the known stack
    cells. *)
let typ_stack_app tail_type final_type cells =
  let cell_typ tail cell = stack_type_of_cell_info tail final_type cell in
  Array.fold_left cell_typ tail_type cells


(* type definition for states *)
let statetypedef states =
  { typename = tcstate
  ; typeparams = [ tctail; tcfinal ]
  ; typerhs =
      TDefSum
        (S.TagMap.fold
           (fun tag S.{ known_cells; sfinal_type } defs ->
             let final_type =
               match sfinal_type with
               | None ->
                   TypVar tcfinal
               | Some typ ->
                   TypTextual typ
             in
             { dataname = statecon tag
             ; datavalparams = []
             ; datatypeparams =
                 Some
                   [ typ_stack_app (TypVar tctail) final_type known_cells
                   ; final_type
                   ]
             ; comment =
                 None
                 (* Some
                    (sprintf " Known stack symbols : %s "
                       (SSymbols.print_stack_symbols s)) } *)
             }
             :: defs )
           states
           [] )
  ; typeconstraint = None
  }


open BasicSyntax

(* -------------------------------------------------------------------------- *)
(* Code production for the entry points. *)

(* This is the entry point associated with a start state [s]. By convention,
   it is named after the nonterminal [nt] that corresponds to this state.
   This is a public definition.

   The code initializes an empty stack, and invokes
   [run]. *)

let entrydef name call cfg =
  let S.{ needed_registers } = StringMap.find call cfg in
  { valpublic = true
  ; valpat = PVar name
  ; valval =
      EAnnot
        ( EFun
            ( [ PVar flexer; PVar flexbuf ]
            , blet
                ( [ (PVar stack, EUnit) ]
                , EApp
                    ( EVar call
                    , e_common_args
                      @ evars
                      @@ StringSet.elements needed_registers ) ) )
        , type2scheme
            (marrow [ arrow tlexbuf (tname "token"); tlexbuf ] (tname "_")) )
  }


let function_type S.{ final_type; stack_type; needed_registers } =
  let final =
    match final_type with None -> tname tcfinal | Some typ -> TypTextual typ
  in
  { quantifiers =
      ( match final_type with
      | None ->
          [ tctail; tcfinal ]
      | Some _ ->
          [ tctail ] )
  ; locally_abstract = true
  ; body =
      (let typ_tail = typ_stack_app (tname tctail) final stack_type in
       marrow
         (typ_tail
          ::
          List.map
            (function
              | s when s = state ->
                  TypApp (tcstate, [ typ_tail; final ])
              | _ ->
                  TypVar "_" )
            (S.RegisterSet.elements needed_registers) )
         final )
  }


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


let compile_value bindings value = compile_value (Bindings.apply bindings value)

let compile_bindings bindings expr =
  blet
    ( Bindings.fold
        (fun reg value defs -> (PVar reg, compile_value bindings value) :: defs)
        bindings
        []
    , expr )


let compile_primitive bindings = function
  | S.PrimOCamlCall (f, args) ->
      EApp (EVar f, List.map (fun arg -> compile_value bindings arg) args)
  | S.PrimOCamlFieldAccess (record, field) ->
      ERecordAccess (compile_value bindings record, field)
  | S.PrimOCamlDummyPos ->
      EVar "Lexing.dummy_pos"
  | S.PrimOCamlAction (bindings', action) ->
      let needed =
        S.RegisterSet.union (Action.semvars action) (Action.posvars action)
      in
      let bindings = Bindings.compose bindings bindings' in
      let bindings = Bindings.restrict bindings needed in
      EComment
        ( "Restoring definitions"
        , compile_bindings
            bindings
            (EComment ("End of restore", Action.to_il_expr action)) )


let compile_final_type = function
  | None ->
      tname tcfinal
  | Some typ ->
      TypTextual typ


let rec compile_routine (program : S.program) t_block =
  let final_type = compile_final_type S.(t_block.final_type) in
  let S.{ block } = t_block in
  compile_block program Bindings.empty final_type block


and compile_block program bindings final_type =
  let cfg = S.(program.cfg) in
  let compile_block = compile_block program in
  function
  | S.INeed (registers, block) ->
      let bindings = Bindings.restrict bindings registers in
      EComment
        ( sprintf
            "Needed registers : { %s }"
            (String.concat "; " (StringSet.elements registers))
        , compile_block bindings final_type block )
  | S.IPush (value, _cell, block) ->
      assert (value <> S.VTuple []);
      let value =
        match compile_value bindings value with
        | ETuple li ->
            li
        | _ ->
            assert false
      in
      blet
        ( [ (PVar fstack, ETuple (EVar fstack :: value)) ]
        , compile_block bindings final_type block )
  | S.IPop (pattern, block) ->
      assert (pattern <> S.PTuple []);
      let pattern =
        match compile_pattern pattern with
        | T.PTuple li ->
            li
        | _ ->
            assert false
      in
      blet
        ( [ (T.PTuple (PVar fstack :: pattern), EVar fstack) ]
        , compile_block bindings final_type block )
  | S.IDef (bindings', block) ->
      let bindings = Bindings.compose bindings bindings' in
      compile_block bindings final_type block
  | S.IPrim (register, primitive, block) ->
      EComment
        ( "Primitive"
        , blet
            ( [ (PVar register, compile_primitive bindings primitive) ]
            , compile_block bindings final_type block ) )
  | S.ITrace (message, block) ->
      blet
        ( [ (PVar "_", EApp (EVar "Printf.eprintf", [ EStringConst message ])) ]
        , compile_block bindings final_type block )
  | S.IComment (comment, block) ->
      EComment (comment, compile_block bindings final_type block)
  | S.IDie ->
      ERaise (EVar "_eRR")
  | S.IReturn value ->
      (* If there is a known final type, we need to annotate the return value
         with it. It is unclear why, but if we dont, the program may fail to
         type. *)
      EAnnot (compile_value bindings value, type2scheme final_type)
  | S.IJump label ->
      EApp
        ( EVar label
        , e_common_args
          @ List.map
              (fun s -> compile_value bindings (S.VReg s))
              (S.RegisterSet.elements @@ needed @@ StringMap.find label cfg) )
  | S.ICaseToken (register, tokpat_block_list, block_option) ->
      compile_ICaseToken
        program
        bindings
        final_type
        register
        tokpat_block_list
        block_option
  | S.ICaseTag (register, tagpat_block_list) ->
      compile_ICaseTag program bindings final_type register tagpat_block_list
  | S.ITypedBlock t_block ->
      compile_ITypedBlock program bindings t_block


and compile_ICaseToken
    program bindings final_type register tokpat_block_list block_option =
  EMatch
    ( EVar register
    , List.map
        (fun (tokpat, block) ->
          match tokpat with
          | S.TokSingle (terminal, register) ->
              { branchpat = CodePieces.tokpat terminal (PVar register)
              ; branchbody =
                  ( match Grammar.Terminal.ocamltype terminal with
                  | None ->
                      blet
                        ( [ (PVar register, EUnit) ]
                        , compile_block program bindings final_type block )
                  | Some _ ->
                      compile_block program bindings final_type block )
              }
          | S.TokMultiple terminals ->
              { branchpat =
                  POr
                    (List.map
                       (fun terminal -> CodePieces.tokpat terminal PWildcard)
                       (Grammar.TerminalSet.elements terminals) )
              ; branchbody = compile_block program bindings final_type block
              } )
        tokpat_block_list
      @
      match block_option with
      | None ->
          []
      | Some block ->
          [ { branchpat = PWildcard
            ; branchbody = compile_block program bindings final_type block
            }
          ] )


and compile_ICaseTag program bindings final_type register tagpat_block_list =
  let S.{ states } = program in
  (* For a given branch, return an optionnal prelude to be inserted before the
     match and the code for the branch *)
  let aux_branch (S.TagMultiple tag_list, block) =
    match tag_list with
    (* Or-pattern cannot be empty *)
    | [] ->
        assert false
    (* If the Or-pattern contains only one element, then no action is to be taken *)
    | [ tag ] ->
        ( None
        , [ { branchpat = pstatescon [ tag ]
            ; branchbody = compile_block program bindings final_type block
            }
          ] )
    | _ ->
      ( match block with
      | S.ITypedBlock (S.{ needed_registers; name } as t_block) ->
          let name =
            match name with None -> fresh_name () | Some name -> name
          in
          let f = compile_function t_block program in
          let regs = StringSet.elements needed_registers in
          let regs = List.map (fun reg -> S.VReg reg) regs in
          let vargs = e_common_args @ List.map (compile_value bindings) regs in
          let call_f = EApp (EVar name, vargs) in
          (* We do not duplicate the code, and instead use the same local
             function for every subroutine. *)
          let tag_aux tag =
            { branchpat = pstatescon [ tag ]; branchbody = call_f }
          in
          (Some (PVar name, f), List.map tag_aux tag_list)
      | S.IJump _ as block ->
          (* We duplicate this code that is very short. *)
          let expr = compile_block program bindings final_type block in
          ( None
          , List.map
              (fun tag -> { branchpat = pstatescon [ tag ]; branchbody = expr })
              tag_list )
      | b ->
          StackLangPrinter.print_block stderr b;
          failwith "cannot find t_block or jump" )
  in
  let branches = List.map aux_branch tagpat_block_list in
  let preludes, branches = List.split branches in
  (* Only keep preludes that exists *)
  let preludes = List.filter_map Fun.id preludes in
  (* Make the nested Or-patterns into toplevel ones. The code was already
     duplicated in order for this to work. *)
  let branches = List.concat branches in
  (* Add a default branch except if every represented state is matched on. *)
  let branches =
    if List.length branches = S.TagMap.cardinal states
    then branches
    else
      branches
      @ [ (* TODO : remove this *)
          { branchpat = PWildcard
          ; branchbody = EApp (EVar "assert", [ EVar "false" ])
          }
        ]
  in
  blet ~local:true (preludes, EMatch (EVar register, branches))


and compile_ITypedBlock program bindings = function
  | S.({ has_case_tag = true; name; needed_registers } as t_block) ->
      (* If a typed block contains a match on tags, then we need to make it
         an inlined function call : it is not possible to inline it
         ourselves, because of GADT shenanigans.*)
      let block_name =
        match name with None -> fresh_name () | Some name -> name
      in
      let func = EVar block_name in
      let regs = StringSet.elements needed_registers in
      let sargs = List.map (fun reg -> S.VReg reg) regs in
      let args = e_common_args @ List.map (compile_value bindings) sargs in
      blet
        ~local:true
        ( [ (PVar block_name, compile_function t_block program) ]
        , EComment ("Not inlined because of case tag", EApp (func, args)) )
  | S.{ block; needed_registers; final_type } ->
      let bindings = Bindings.restrict bindings needed_registers in
      let final_type = compile_final_type final_type in
      compile_block program bindings final_type block


and compile_function t_block (program : S.program) =
  EAnnot
    ( EFun
        ( p_common_args @ pvars @@ S.RegisterSet.elements @@ needed t_block
        , compile_routine program t_block )
    , function_type t_block )


let compile (S.({ cfg; entry; states } as program) : S.program) =
  [ SIFunctor
      ( Front.grammar.parameters
      , mbasics Front.grammar
        @ [ SIStretch Front.grammar.preludes
          ; SITypeDefs (statetypedef states :: stacktypeabbrevdefs)
          ; SIValDefs (false, [ discarddef ])
          ; SIValDefs
              ( true
              , List.map
                  snd
                  (StringMap.bindings
                     (StringMap.mapi
                        (fun name t_block ->
                          { valpublic = false
                          ; valpat = PVar name
                          ; valval = compile_function t_block program
                          } )
                        cfg ) ) )
          ; SIValDefs
              ( false
              , StringMap.fold
                  (fun name call defs -> entrydef name call cfg :: defs)
                  entry
                  [] )
          ; SIStretch Front.grammar.postludes
          ] )
  ]
