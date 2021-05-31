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

let lexer = prefix "lexer"

let lexbuf = prefix "lexbuf"

let stack = prefix "stack"

let discard = prefix "discard"

(** The [print_token] function. This automatically generated function
     is used in [--trace] mode. *)
let print_token = prefix "print_token"

let statecon s = dataprefix (sprintf "State%s" (S.string_of_tag s))

let estatecon s = EData (statecon s, [])

let pstatecon s = PData (statecon s, [])

let pstatescon ss = POr (List.map pstatecon ss)

let eprintf format args =
  EApp
    ( EVar "Printf.fprintf"
    , EVar "stderr" :: EStringConst (format ^ "\n%!") :: args )


let trace (format : string) (args : expr list) : (pattern * expr) list =
  if Settings.trace then [ (PUnit, eprintf format args) ] else []


(** This is [print_token], used to print tokens in [--trace] mode. *)
let printtokendef =
  destructuretokendef print_token tstring false (fun tok ->
      EStringConst (Grammar.Terminal.print tok) )


(* This is [discard], used to take a token off the input stream and
     query the lexer for a new one. The code queries the lexer for a new
     token and stores it into [env.token], overwriting the previous
     token. It also stores the start and positions of the new token.
     Last, [env.error] is cleared.

     We use the lexer's [lex_start_p] and [lex_curr_p] fields to extract
     the start and end positions of the token that we just read. In
     practice, it seems that [lex_start_p] can be inaccurate (that is
     the case when the lexer calls itself recursively, instead of simply
     recognizing an atomic pattern and returning immediately). However,
     we are 100% compatible with ocamlyacc here, and there is no better
     solution anyway.

     As of 2014/12/12, we re-allocate the environment record instead of
     updating it. Perhaps surprisingly, this makes the code TWICE FASTER
     overall. The write barrier is really costly! *)

let discardbody =
  let lexer = "lexer"
  and lexbuf = "lexbuf" in
  EFun
    ( [ PVar lexer; PVar lexbuf ]
    , blet
        ( trace
            "Lookahead token is now %s (%d-%d)"
            [ EApp (EVar print_token, [ EVar token ])
            ; ERecordAccess
                ( ERecordAccess (EVar lexbuf, "Lexing.lex_start_p")
                , "Lexing.pos_cnum" )
            ; ERecordAccess
                ( ERecordAccess (EVar lexbuf, "Lexing.lex_curr_p")
                , "Lexing.pos_cnum" )
            ]
        , EApp (EVar lexer, [ EVar lexbuf ]) ) )


let discarddef =
  { valpublic = false; valpat = PVar discard; valval = discardbody }


let e_common_args = [ EVar stack ]

let p_common_args = [ PVar stack ]

(* The type of environments. *)
let tcstate = prefix "state"

let base_tcstack = "cell"

let nth_bit x n = x land (1 lsl n) <> 0

(** [binary_print size () x] return the binary representation of [x] in a
    string of size [size] *)
let binary_print size () x =
  let bytes = Bytes.create size in
  for i = 0 to size - 1 do
    Bytes.set bytes i (if nth_bit x i then '1' else '0')
  done;
  Bytes.to_string bytes


(** [tcstack_of_int n] return the type constructor for a cell where [n] is a
    number whose binary representation indicates whether each field of the cell
    represented or not. *)
let tcstack_of_int n = prefix (sprintf "%s_%a" base_tcstack (binary_print 4) n)

(** [tcstack hold_state hold_semv hold_startpos hold_endpos] return the type
    constructor for a cell where each [hold_x] argument indicate whether [x] is
    represented in that cell. *)
let tcstack hold_state hold_semv hold_startpos hold_endpos =
  prefix
    (sprintf
       "%s_%d%d%d%d"
       base_tcstack
       (Bool.to_int hold_state)
       (Bool.to_int hold_semv)
       (Bool.to_int hold_startpos)
       (Bool.to_int hold_endpos) )


(** A list of abbrevations for stack cells. There are 4 different fields a
    stack cell can hold or not hold, and the empty cell does not require an
    abbreviation. This means there are 15 different stack type
    abbrevations needed to represent all of them (2^4 - 1).
    If the semantic value is represented, the abbreviation has 3 parameter :
    - [tail], the type of the tail of the stack.
    - [semantic], the type of the semantic value.
    - [final] the type returned by the current entrypoint.
    If the semantic value is not represented, there is one less parameter.  *)
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


(* If a terminal does not have a semantic value attached to it, and we are not
   representing the stack in a optimised way, then the "semantic value" of the
   terminal is represented by a unit value. *)
let typ_of_ocamltype_option typ =
  match typ with None -> tunit | Some typ -> TypTextual typ


let typ_of_symbol = function
  | Grammar.Symbol.T t ->
      Grammar.Terminal.ocamltype t
  | Grammar.Symbol.N nt ->
      Grammar.Nonterminal.ocamltype nt


(** [stack_type_of_cell_info tail final cell] return a [T.typ] of a stack where
    we the top stack cell is of shape [cell]. *)
let stack_type_of_cell_info tail final = function
  | Invariant.
      { holds_state = false
      ; holds_semv = false
      ; holds_startp = false
      ; holds_endp = false
      } ->
      tail
  | Invariant.{ symbol; holds_state; holds_semv; holds_startp; holds_endp } ->
      let typ = typ_of_symbol symbol in
      TypApp
        ( tcstack holds_state holds_semv holds_startp holds_endp
        , [ tail ]
          @ MList.if1 holds_semv (typ_of_ocamltype_option typ)
          @ [ final ] )


(** [typ_stack_app tail final cells] return a value of type [T.typ] that
    correspond to the known part of the stack when [cells] are the known stack
    cells. *)
let typ_stack_app tail_type final_type cells =
  let cell_typ tail cell = stack_type_of_cell_info tail final_type cell in
  Array.fold_left cell_typ tail_type cells


let compile_final_type_name = function
  | None ->
      tname tcfinal
  | Some typ ->
      TypTextual typ


let compile_final_type_var = function
  | None ->
      TypVar tcfinal
  | Some typ ->
      TypTextual typ


(* type definition for states *)
let statetypedef states =
  { typename = tcstate
  ; typeparams = [ tctail; tcfinal ]
  ; typerhs =
      TDefSum
        (S.TagMap.fold
           (fun tag S.{ known_cells; sfinal_type } defs ->
             let final_type = compile_final_type_var sfinal_type in
             { dataname = statecon tag
             ; datavalparams = []
             ; datatypeparams =
                 Some
                   [ typ_stack_app (TypVar tctail) final_type known_cells
                   ; final_type
                   ]
             ; comment =
                 Some
                   (sprintf
                      " Known stack symbols : %s "
                      (StackLangPrinter.known_cells_to_string known_cells) )
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
            ( [ PVar lexer; PVar lexbuf ]
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
  (* Every function is polymorphic in tail, because every function may be called
     (or jumped to in stacklang terms) by another arbitrary function, that may
     have an arbitrary long stack.
     However only some of them are not polymorphic in final :
     Final is the type that the parser will return. It can change because a
     parser may have multiple entry point with a different return type.
     However, the functions that return the value and initialise the parser are
     unique to a specific entry point, and therefore not polymorphic in final. *)
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


(** The compilation of StackLang to IL is parameterized by a set of bindings that
    permit to avoid most [let] constructs. This means that when compiling a
    value, we need to first apply the bindings to it. *)
let compile_value bindings value = compile_value (Bindings.apply bindings value)

let compile_bindings bindings expr =
  bindings
  |> Bindings.to_list
  |> List.map (fun (r, v) -> (PVar r, compile_value Bindings.empty v))
  |> (fun pes -> eletand (pes, expr))


let compile_primitive bindings = function
  | S.PrimOCamlCall (f, args) ->
      EApp (EVar f, List.map (fun arg -> compile_value bindings arg) args)
  | S.PrimOCamlFieldAccess (record, field) ->
      ERecordAccess (compile_value bindings record, field)
  | S.PrimOCamlDummyPos ->
      EVar "Lexing.dummy_pos"
  | S.PrimOCamlAction (bindings', action) ->
      (* action have a local binding associated to them, because we cannot
         rename the variables in the semantic action. *)
      let needed =
        S.RegisterSet.union (Action.semvars action) (Action.posvars action)
      in
      let bindings = Bindings.compose bindings bindings' in
      (* We restrict the binding to the needed values. *)
      let bindings = Bindings.restrict needed bindings in
      compile_bindings bindings (Action.to_il_expr action)


let add_to_etuple e tuple =
  match tuple with ETuple li -> ETuple (e :: li) | _ -> assert false


let add_to_ptuple p tuple =
  match tuple with PTuple li -> PTuple (p :: li) | _ -> assert false


let rec compile_routine (program : S.program) t_block =
  let final_type = compile_final_type_name S.(t_block.final_type) in
  let S.{ block } = t_block in
  compile_block program Bindings.empty final_type block


and compile_block program bindings final_type block =
  let cfg = S.(program.cfg) in
  let compile_block = compile_block program in
  match block with
  | S.IPush (value, _cell, block) ->
      assert (value <> S.VTuple []);
      let new_stack =
        add_to_etuple (EVar stack) (compile_value bindings value)
      in
      blet ([ (PVar stack, new_stack) ], compile_block bindings final_type block)
  | S.IPop (pattern, block) ->
      assert (pattern <> S.PTuple []);
      let pattern = add_to_ptuple (PVar stack) (compile_pattern pattern) in
      blet ([ (pattern, EVar stack) ], compile_block bindings final_type block)
  | S.IDef (bindings', block) ->
      let bindings = Bindings.compose bindings bindings' in
      compile_block bindings final_type block
  | S.IPrim (register, primitive, block) ->
      blet
        ( [ (PVar register, compile_primitive bindings primitive) ]
        , compile_block bindings final_type block )
  | S.ITrace (message, block) ->
      blet (trace message [], compile_block bindings final_type block)
  | S.IComment (comment, block) ->
      EComment (comment, compile_block bindings final_type block)
  | S.IDie ->
      ERaise (EVar "_eRR")
  | S.IReturn value ->
      (* If there is a known final type, we need to annotate the return value
         with it. It is unclear why, but if we dont, the program will fail to
         type. *)
      EAnnot (compile_value bindings value, type2scheme final_type)
  | S.IJump label ->
      let needed_registers = needed (StringMap.find label cfg) in
      let needed_list = S.RegisterSet.elements needed_registers in
      let args =
        List.map (fun s -> compile_value bindings (S.VReg s)) needed_list
      in
      let args = e_common_args @ args in
      EApp (EVar label, args)
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


and compile_case_token_branch program bindings final_type (tokpat, block) =
  match tokpat with
  | S.TokSingle (terminal, register) ->
      { branchpat = CodePieces.tokpat terminal (PVar register)
      ; branchbody =
          tok_bind_unit
            terminal
            (PVar register)
            (compile_block program bindings final_type block)
      }
  | S.TokMultiple terminals ->
      { branchpat = tokspat terminals
      ; branchbody = compile_block program bindings final_type block
      }


and compile_ICaseToken
    program bindings final_type register tokpat_block_list block_option =
  EMatch
    ( EVar register
    , List.map
        (compile_case_token_branch program bindings final_type)
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


(** Compile a branch of a case tag
  The branches for a case tag are always or-patterns, and or patterns on GADT do
  not unify the type information. The solution found is to duplicate the code if
  very short (a function call), or replace it by a call to a function defined
  just before the match. This is why this function returns an optional prelude,
  in addition to a list of branches with duplicated code. *)
and compile_case_tag_branch
    program bindings final_type (S.TagMultiple tag_list, block) =
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
  (* if  *)
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


and compile_ICaseTag program bindings final_type register tagpat_block_list =
  let S.{ states } = program in
  let branches =
    List.map
      (compile_case_tag_branch program bindings final_type)
      tagpat_block_list
  in
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
      let bindings = Bindings.restrict needed_registers bindings in
      let final_type = compile_final_type_name final_type in
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
          ; SIValDefs (false, [ printtokendef; discarddef ])
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
