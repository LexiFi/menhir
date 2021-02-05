module S = StackLang
module T = IL
open CodePieces
open CodeBits
open TokenType

let env = prefix "env"

let flexer = prefix "lexer"

let flexbuf = prefix "lexbuf"

let ftoken = prefix "token"

let ferror = prefix "error"

let fstack = prefix "stack"

let discard = prefix "discard"

let statecon s = dataprefix (Printf.sprintf "State%d" s)

let estatecon s = T.EData (statecon s, [])

let pstatecon s = T.PData (statecon s, [])

let pstatescon ss = T.POr (List.map pstatecon ss)

let discarddef =
  T.
    {
      valpublic = false;
      valpat = PVar discard;
      valval =
        T.EFun
          ( [ T.PVar flexer; T.PVar flexbuf ],
            T.EApp (T.EVar flexer, [ T.EVar flexbuf ]) );
    }

let e_common_args =
  [ (*T.EVar flexer; T.EVar flexbuf;*) T.EMagic (T.EVar fstack) ]

let p_common_args = [ (*T.PVar flexer; T.PVar flexbuf;*) T.PVar fstack ]

let tlexer = T.TypArrow (tlexbuf, ttoken)

(* The type of environments. *)

let tcenv = env

let envtypedef =
  T.
    {
      typename = tcenv;
      typeparams = [];
      typerhs =
        TDefRecord
          [
            (* The lexer itself. *)
            field false flexer tlexer;
            (* The lexing buffer. *)
            field false flexbuf tlexbuf;
            (* The last token that was read from the lexer. This is the
               head of the token stream, unless [env.error] is set. *)
            field false ftoken ttoken;
            (* A flag which tells whether we currently have an [error] token
               at the head of the stream. When this flag is set, the head
               of the token stream is the [error] token, and the contents of
               the [token] field is irrelevant. The token following [error]
               is obtained by invoking the lexer again. *)
            field true ferror tbool;
          ];
      typeconstraint = None;
    }

(* This is the type of states. Only states that are represented are
   declared. *)
(* The type of states. *)

let tcstate = prefix "state"

(*let tstate = T.TypApp (tcstate, [])*)

let statetypedef =
  T.
    {
      typename = tcstate;
      typeparams = [];
      typerhs =
        TDefSum
          (Lr1.fold
             (fun defs s ->
               (* TODO : restore the condition *)
               if (*Invariant.represented s*) true then
                 {
                   dataname = statecon @@ Lr1.number s;
                   datavalparams = [];
                   datatypeparams = None;
                 }
                 :: defs
               else defs)
             []);
      typeconstraint = None;
    }

(*type s_program = S.program*)

open BasicSyntax

let grammar = Front.grammar

let get_env block_map =
  StringMap.map
    (function
      | S.INeed (registers, _block) -> StringSet.elements registers
      | _ -> assert false)
    block_map

let rec compile_pattern = function
  | S.PWildcard -> T.PWildcard
  | S.PReg x -> T.PVar x
  | S.PTuple li -> T.PTuple (List.map compile_pattern li)

let rec compile_value = function
  | S.VTag tag -> estatecon tag
  | S.VReg register -> T.EVar register
  | S.VTuple value_list -> T.ETuple (List.map compile_value value_list)

let compile_primitive = function
  | S.PrimOCamlCall (f, args) ->
      T.EApp (T.EVar f, List.map (fun arg -> T.EVar arg) args)
  | S.PrimOCamlFieldAccess (record, field) ->
      T.ERecordAccess (T.EVar record, field)
  | S.PrimOCamlDummyPos -> T.EVar "Lexing.dummy_pos"
  | S.PrimOCamlAction action -> Action.to_il_expr action

let compile_block env =
  let rec compile_ICaseToken register tokpat_block_list block_option =
    T.EMatch
      ( T.EVar register,
        List.map
          (fun (tokpat, block) ->
            match tokpat with
            | S.TokSingle (terminal, register) ->
                T.
                  {
                    branchpat = CodePieces.tokpat terminal (T.PVar register);
                    branchbody =
                      ( match Grammar.Terminal.ocamltype terminal with
                      | None ->
                          T.ELet
                            ([ (T.PVar register, T.EUnit) ], compile_block block)
                      | Some _ -> compile_block block );
                  }
            | S.TokMultiple terminals ->
                T.
                  {
                    branchpat =
                      T.POr
                        (List.map
                           (fun terminal ->
                             CodePieces.tokpat terminal T.PWildcard)
                           (Grammar.TerminalSet.elements terminals));
                    branchbody = compile_block block;
                  })
          tokpat_block_list
        @
        match block_option with
        | None -> []
        | Some block ->
            [ T.{ branchpat = T.PWildcard; branchbody = compile_block block } ]
      )
  and compile_ICaseTag register tagpat_block_list =
    T.EMatch
      ( T.EVar register,
        List.map
          (fun (tagpat, block) ->
            let (S.TagMultiple tag_list) = tagpat in
            T.
              {
                branchpat = pstatescon tag_list;
                branchbody = compile_block block;
              })
          tagpat_block_list
        @ [
            (* TODO : remove this *)
            T.
              {
                branchpat = T.PWildcard;
                branchbody = T.EApp (T.EVar "assert", [ T.EVar "false" ]);
              };
          ] )
  and compile_block = function
    (* [INeed] is a special pseudo-instruction that is expected to appear at
       least at the beginning of every block. (It can also be used inside a
       block.) It indicates which registers are expected to be defined at this
       point, and it un-defines any registers that are not explicitly listed. *)
    | S.INeed (_registers, block) -> compile_block block
    (* [IPush] pushes a value onto the stack. [IPop] pops a value off the stack.
       [IDef] can be viewed as a sequence of a push and a pop. It can be used to
       move data between registers or to load a value into a register. *)
    | S.IPush (value, block) ->
        let value =
          match compile_value value with T.ETuple li -> li | _ -> assert false
        in
        T.ELet
          ( [ (T.PVar fstack, T.ETuple (T.EVar fstack :: value)) ],
            compile_block block )
    | S.IPop (pattern, block) ->
        let pattern =
          match compile_pattern pattern with
          | T.PTuple li -> li
          | _ -> assert false
        in
        T.ELet
          ( [ (T.PTuple (T.PVar fstack :: pattern), T.EVar fstack) ],
            compile_block block )
    | S.IDef (pattern, value, block) ->
        T.ELet
          ( [ (compile_pattern pattern, compile_value value) ],
            compile_block block )
    (* [IPrim] invokes a primitive operation and stores its result in a
       register. *)
    | S.IPrim (register, primitive, block) ->
        T.ELet
          ( [ (T.PVar register, compile_primitive primitive) ],
            compile_block block )
    (* [ITrace] logs a message on [stderr]. *)
    | S.ITrace (message, block) ->
        T.ELet
          ( [
              ( T.PVar "_",
                T.EApp (T.EVar "Printf.eprintf", [ T.EStringConst message ]) );
            ],
            compile_block block )
    (* [IComment] is a comment. *)
    | S.IComment (comment, block) -> T.EComment (comment, compile_block block)
    (* Group 2: Instructions with zero successor. *)

    (* [IDie] causes an abrupt termination of the program. It is translated
       into OCaml by raising the exception [Error]. *)
    | S.IDie -> T.ERaise (T.EVar "_eRR")
    (* [IReturn] causes the normal termination of the program. A value read
       from a register is returned. *)
    | S.IReturn register -> T.EVar register
    (* [IJump] causes a jump to a block identified by its label. The registers
       that are needed by the destination block must form a subset of the
       registers that are defined at the point of the jump. *)
    | S.IJump label ->
        T.EApp
          ( T.EVar label,
            e_common_args
            @ List.map (fun s -> T.EVar s) (StringMap.find label env) )
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

let compile (S.{ cfg; entry } : S.program) =
  let env = get_env cfg in
  let entries =
    Lr1.NodeMap.fold
      (fun _ label acc -> StringSet.add label acc)
      entry StringSet.empty
  in
  T.
    [
      SIFunctor
        ( grammar.parameters,
          mbasics grammar
          @ [
              SITypeDefs [ envtypedef; statetypedef ];
              SIStretch grammar.preludes;
              SIValDefs (false, [ discarddef ]);
              SIValDefs
                ( true,
                  List.map
                    (fun (name, expr) ->
                      {
                        valpublic = StringSet.mem name entries;
                        valpat = T.PVar name;
                        valval =
                          EFun
                            ( p_common_args
                              @ List.map
                                  (fun s -> T.PVar s)
                                  (StringMap.find name env),
                              expr );
                      })
                    (List.of_seq
                       (StringMap.to_seq
                          (StringMap.map (compile_block env) cfg))) );
              SIValDefs
                ( false,
                  [
                    {
                      valpublic = true;
                      (* Definition's left-hand side. *)
                      valpat = T.PVar "main";
                      (* Value to which it is bound. *)
                      valval =
                        EFun
                          ( [ PVar flexer; PVar flexbuf ],
                            EApp
                              ( EVar (StringSet.choose entries),
                                [ EMagic EUnit; EVar flexbuf; EVar flexer ] ) );
                    };
                  ] );
              SIStretch grammar.postludes;
            ] );
    ]
