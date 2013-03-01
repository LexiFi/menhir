type token =
  | EOF of (Positions.position)
  | LID of (Positions.position * string)
  | LET of (Positions.position)
  | IN of (Positions.position)
  | DOT of (Positions.position)
  | LBRACE of (Positions.position)
  | RBRACE of (Positions.position)
  | LBRACKET of (Positions.position)
  | RBRACKET of (Positions.position)
  | LESS of (Positions.position)
  | EXISTS of (Positions.position)
  | FORALL of (Positions.position)
  | ARROW of (Positions.position)
  | TIMES of (Positions.position)
  | LPAREN of (Positions.position)
  | RPAREN of (Positions.position)
  | SEMI of (Positions.position)
  | COMMA of (Positions.position)
  | AND of (Positions.position)
  | COLON of (Positions.position)
  | END of (Positions.position)
  | TRUE of (Positions.position)
  | FALSE of (Positions.position)
  | DUMP of (Positions.position)
  | EQ of (Positions.position)
  | BACKSLASH of (Positions.position)

open Parsing;;
# 4 "constraintParser.mly"

open Sig
open Positions
open AstPositions
open MiniAst

let fold_pair f ts = 
  match ts with 
    | a :: b :: q -> List.fold_left f (f a b) q
    | _ -> assert false

let tuple2 pos t1 t2 = 
  EDCon (pos, "_Tuple", [ t1; t2 ])

let tuple pos = 
  fold_pair (tuple2 pos) 

let arrow_type pos t1 t2 =
  TypApp (pos, TypVar (pos, "->"), [ t1; t2 ])

let tuple_type2 pos t1 t2 = 
  TypApp (pos, TypVar (pos, "*"), [ t1; t2 ])

let tuple_type pos = 
  fold_pair (tuple_type2 pos)  

let unclosed b e l1 l2 = 
  let l1 = lex_join (Parsing.rhs_start_pos l1) (Parsing.rhs_end_pos l1)
  and l2 = lex_join (Parsing.rhs_start_pos l2) (Parsing.rhs_end_pos l2)
  in
    raise (ParsingExceptions.Unclosed (b, e, l1, l2))

let clet envs body =
  fun (tenv, pool) -> 
    CLet (envs (tenv, pool), (body (tenv, pool)))

let cexists pos vars c = 
  fun (tenv, pool) ->
    let vars = snd (List.split vars) in
    let rqs, fqs, tenv = MiniTypes.intern_let_env pos tenv [] vars in
    CLet ([
      Scheme (pos, rqs, fqs, c (tenv, pool), 
	      Misc.StringMap.empty
	     )
	  ], CTrue pos)

let cequation t1 t2 = 
  fun (tenv, pool) -> 
    let p = tjoin t1 t2
    and it1 = MiniTypes.intern (tposition t1) tenv t1 
    and it2 = MiniTypes.intern (tposition t2) tenv t2 in
      CEquation (p, it1, it2)

let cinstance (p1, id) t = 
  fun (tenv, _) ->
    let p = join p1 (tposition t) in
    CInstance (p, id, MiniTypes.intern (tposition t) tenv t)

let scheme pos (rvs, fvs) c g = 
  fun (tenv, pool) -> 
    let rqs, fqs, tenv = MiniTypes.intern_let_env pos tenv rvs fvs in
      Scheme (pos, rqs, fqs, c (tenv, pool),
              List.fold_left
                (fun m (n, ty) ->
                   let t = MiniTypes.intern pos tenv ty in
                     Misc.StringMap.add n (t, pos) m)
                Misc.StringMap.empty
                g
             )

let appl y = 
  List.map (fun x -> x y) 

let conjunction cs = 
  fun p -> CConjunction (appl p cs)
    
let mkArrow (p1, t1) (p2, t2) =
  let p = join p1 p2 in
  (p, TypApp (p, TypVar (p, "->"), [t1; t2]))

let mkApp (p, t) ts =
  let p' = join p (ljoinf fst ts) in
  let _, ts = List.split ts in
    (p', TypApp (p', t, ts))

let mkRow r = 
  assert false

let typeid (p, id) = 
    (p, TypVar (p, id))

type row =
    Partial of MiniAst.typ
  | Row of (string * MiniAst.typ)

# 127 "constraintParser.ml"
let yytransl_const = [|
    0|]

let yytransl_block = [|
    0 (* EOF *);
  257 (* LID *);
  258 (* LET *);
  259 (* IN *);
  260 (* DOT *);
  261 (* LBRACE *);
  262 (* RBRACE *);
  263 (* LBRACKET *);
  264 (* RBRACKET *);
  265 (* LESS *);
  266 (* EXISTS *);
  267 (* FORALL *);
  268 (* ARROW *);
  269 (* TIMES *);
  270 (* LPAREN *);
  271 (* RPAREN *);
  272 (* SEMI *);
  273 (* COMMA *);
  274 (* AND *);
  275 (* COLON *);
  276 (* END *);
  277 (* TRUE *);
  278 (* FALSE *);
  279 (* DUMP *);
  280 (* EQ *);
  281 (* BACKSLASH *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\002\000\005\000\005\000\007\000\007\000\
\007\000\007\000\007\000\007\000\009\000\003\000\003\000\010\000\
\010\000\010\000\013\000\013\000\004\000\004\000\014\000\014\000\
\014\000\011\000\011\000\012\000\012\000\016\000\015\000\015\000\
\006\000\006\000\017\000\017\000\018\000\008\000\019\000\019\000\
\020\000\021\000\021\000\022\000\022\000\023\000\025\000\025\000\
\025\000\025\000\024\000\024\000\026\000\026\000\000\000"

let yylen = "\002\000\
\002\000\004\000\004\000\001\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\003\000\003\000\001\000\003\000\000\000\
\005\000\002\000\000\000\001\000\001\000\002\000\001\000\001\000\
\001\000\000\000\003\000\000\000\003\000\003\000\001\000\003\000\
\003\000\003\000\001\000\003\000\003\000\001\000\003\000\001\000\
\001\000\003\000\001\000\001\000\002\000\001\000\001\000\003\000\
\003\000\005\000\001\000\002\000\001\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\007\000\
\008\000\009\000\000\000\055\000\000\000\004\000\005\000\000\000\
\000\000\038\000\000\000\041\000\000\000\044\000\046\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\035\000\
\000\000\000\000\023\000\024\000\025\000\000\000\000\000\000\000\
\000\000\047\000\045\000\001\000\000\000\000\000\000\000\000\000\
\052\000\011\000\000\000\018\000\000\000\000\000\000\000\000\000\
\000\000\000\000\048\000\000\000\000\000\022\000\012\000\049\000\
\000\000\034\000\000\000\010\000\039\000\042\000\000\000\002\000\
\015\000\000\000\000\000\013\000\037\000\036\000\003\000\000\000\
\000\000\000\000\027\000\000\000\000\000\000\000\000\000\050\000\
\020\000\017\000\000\000\029\000\000\000\054\000\030\000\032\000"

let yydgoto = "\002\000\
\012\000\013\000\027\000\038\000\014\000\015\000\016\000\017\000\
\028\000\029\000\056\000\076\000\090\000\039\000\085\000\086\000\
\033\000\034\000\018\000\019\000\020\000\021\000\022\000\023\000\
\024\000\081\000"

let yysindex = "\004\000\
\028\255\000\000\000\255\006\255\007\255\033\255\028\255\000\000\
\000\000\000\000\106\255\000\000\023\000\000\000\000\000\013\255\
\012\255\000\000\025\255\000\000\026\255\000\000\000\000\106\255\
\010\255\109\255\040\255\029\255\037\255\035\255\010\255\000\000\
\042\255\052\255\000\000\000\000\000\000\065\255\033\255\068\255\
\048\255\000\000\000\000\000\000\056\255\010\255\010\255\010\255\
\000\000\000\000\033\255\000\000\028\255\006\255\028\255\057\255\
\010\255\245\254\000\000\007\255\028\255\000\000\000\000\000\000\
\010\255\000\000\013\255\000\000\000\000\000\000\078\255\000\000\
\000\000\077\255\097\255\000\000\000\000\000\000\000\000\083\255\
\094\255\033\255\000\000\084\255\100\255\101\255\010\255\000\000\
\000\000\000\000\010\255\000\000\097\255\000\000\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\075\255\088\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\000\000\027\000\000\000\014\000\000\000\000\000\001\000\
\000\000\000\000\000\000\110\255\004\255\061\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\090\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\088\255\000\000\022\255\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\019\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\103\255\
\000\000\098\255\000\000\000\000\000\000\104\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\003\000\069\000\233\255\000\000\079\000\080\000\251\255\
\000\000\000\000\000\000\000\000\000\000\000\000\033\000\000\000\
\067\000\000\000\081\000\000\000\082\000\000\000\118\000\107\000\
\000\000\045\000"

let yytablesize = 307
let yytable = "\032\000\
\051\000\041\000\052\000\064\000\001\000\065\000\026\000\030\000\
\025\000\040\000\042\000\005\000\006\000\043\000\005\000\062\000\
\026\000\026\000\033\000\050\000\031\000\026\000\044\000\031\000\
\028\000\058\000\040\000\071\000\003\000\004\000\045\000\011\000\
\005\000\035\000\011\000\046\000\047\000\006\000\048\000\028\000\
\068\000\007\000\053\000\055\000\036\000\037\000\054\000\059\000\
\008\000\009\000\010\000\077\000\011\000\057\000\032\000\072\000\
\003\000\074\000\089\000\080\000\005\000\047\000\064\000\079\000\
\065\000\047\000\047\000\060\000\061\000\007\000\075\000\046\000\
\047\000\047\000\047\000\047\000\008\000\009\000\010\000\047\000\
\011\000\080\000\063\000\082\000\083\000\095\000\047\000\047\000\
\047\000\047\000\016\000\047\000\021\000\021\000\016\000\021\000\
\021\000\084\000\047\000\087\000\019\000\016\000\091\000\021\000\
\019\000\016\000\042\000\021\000\088\000\035\000\005\000\019\000\
\014\000\051\000\092\000\019\000\093\000\053\000\031\000\031\000\
\036\000\037\000\073\000\066\000\067\000\096\000\078\000\069\000\
\043\000\070\000\049\000\094\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\051\000\000\000\
\051\000\000\000\000\000\000\000\051\000\051\000\000\000\051\000\
\051\000\051\000\051\000\043\000\006\000\043\000\000\000\000\000\
\051\000\043\000\033\000\006\000\043\000\043\000\043\000\043\000\
\040\000\033\000\040\000\000\000\000\000\043\000\000\000\000\000\
\000\000\040\000\040\000\040\000\040\000\000\000\000\000\000\000\
\000\000\000\000\040\000"

let yycheck = "\005\000\
\000\000\007\000\026\000\015\001\001\000\017\001\003\001\001\001\
\009\001\007\000\001\001\005\001\000\000\000\000\005\001\039\000\
\011\001\014\001\000\000\025\000\014\001\018\001\000\000\014\001\
\003\001\031\000\000\000\051\000\001\001\002\001\018\001\025\001\
\005\001\001\001\025\001\024\001\012\001\010\001\013\001\018\001\
\046\000\014\001\003\001\007\001\012\001\013\001\018\001\006\001\
\021\001\022\001\023\001\057\000\025\001\019\001\060\000\053\000\
\001\001\055\000\082\000\065\000\005\001\001\001\015\001\061\000\
\017\001\005\001\006\001\016\001\004\001\014\001\014\001\024\001\
\012\001\013\001\014\001\001\001\021\001\022\001\023\001\005\001\
\025\001\087\000\015\001\006\001\008\001\091\000\012\001\013\001\
\014\001\015\001\003\001\017\001\003\001\004\001\007\001\006\001\
\007\001\001\001\024\001\017\001\003\001\014\001\019\001\014\001\
\007\001\018\001\001\001\018\001\015\001\001\001\005\001\014\001\
\003\001\005\001\015\001\018\001\016\001\015\001\015\001\014\001\
\012\001\013\001\054\000\045\000\045\000\093\000\060\000\047\000\
\011\000\048\000\024\000\087\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\006\001\255\255\
\008\001\255\255\255\255\255\255\012\001\013\001\255\255\015\001\
\016\001\017\001\018\001\006\001\008\001\008\001\255\255\255\255\
\024\001\012\001\008\001\015\001\015\001\016\001\017\001\018\001\
\006\001\015\001\008\001\255\255\255\255\024\001\255\255\255\255\
\255\255\015\001\016\001\017\001\018\001\255\255\255\255\255\255\
\255\255\255\255\024\001"

let yynames_const = "\
  "

let yynames_block = "\
  EOF\000\
  LID\000\
  LET\000\
  IN\000\
  DOT\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  LESS\000\
  EXISTS\000\
  FORALL\000\
  ARROW\000\
  TIMES\000\
  LPAREN\000\
  RPAREN\000\
  SEMI\000\
  COMMA\000\
  AND\000\
  COLON\000\
  END\000\
  TRUE\000\
  FALSE\000\
  DUMP\000\
  EQ\000\
  BACKSLASH\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'constraint_exp) in
    let _2 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 139 "constraintParser.mly"
                                ( _1 )
# 355 "constraintParser.ml"
               : MiniTypingEnvironment.environment * MiniMultiEquation.pool-> MiniConstraint.tconstraint))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : Positions.position) in
    let _2 = (peek_val parser_env 2 : 'let_envs) in
    let _3 = (peek_val parser_env 1 : Positions.position) in
    let _4 = (peek_val parser_env 0 : 'constraint_exp) in
    Obj.repr(
# 143 "constraintParser.mly"
                                        ( clet _2 _4 )
# 365 "constraintParser.ml"
               : 'constraint_exp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 3 : Positions.position) in
    let _2 = (peek_val parser_env 2 : 'vars) in
    let _3 = (peek_val parser_env 1 : Positions.position) in
    let _4 = (peek_val parser_env 0 : 'constraint_exp) in
    Obj.repr(
# 144 "constraintParser.mly"
                                 ( cexists _1 _2 _4 )
# 375 "constraintParser.ml"
               : 'constraint_exp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'constraint_exp1) in
    Obj.repr(
# 145 "constraintParser.mly"
                  ( _1 )
# 382 "constraintParser.ml"
               : 'constraint_exp))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'conjunction) in
    Obj.repr(
# 149 "constraintParser.mly"
                ( conjunction _1 )
# 389 "constraintParser.ml"
               : 'constraint_exp1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'constraint_exp0) in
    Obj.repr(
# 150 "constraintParser.mly"
                  ( _1 )
# 396 "constraintParser.ml"
               : 'constraint_exp1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 154 "constraintParser.mly"
                  ( fun _ -> CTrue _1 )
# 403 "constraintParser.ml"
               : 'constraint_exp0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 155 "constraintParser.mly"
                   ( fun _ -> CFalse _1 )
# 410 "constraintParser.ml"
               : 'constraint_exp0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 156 "constraintParser.mly"
           ( fun _ -> CDump _1 )
# 417 "constraintParser.ml"
               : 'constraint_exp0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'typ) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'typ) in
    Obj.repr(
# 157 "constraintParser.mly"
                ( cequation _1 _3 )
# 426 "constraintParser.ml"
               : 'constraint_exp0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Positions.position * string) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'typ) in
    Obj.repr(
# 158 "constraintParser.mly"
                  ( cinstance _1 _3 )
# 435 "constraintParser.ml"
               : 'constraint_exp0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Positions.position) in
    let _2 = (peek_val parser_env 1 : 'constraint_exp) in
    let _3 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 159 "constraintParser.mly"
                                ( _2 )
# 444 "constraintParser.ml"
               : 'constraint_exp0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'opt_env_vars) in
    let _2 = (peek_val parser_env 1 : 'opt_constraint) in
    let _3 = (peek_val parser_env 0 : 'opt_env_ids) in
    Obj.repr(
# 163 "constraintParser.mly"
                                          ( 
    (* FIXME: Fix positions. *)
    scheme undefined_position _1 _2 _3 
  )
# 456 "constraintParser.ml"
               : 'let_env))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'let_env) in
    Obj.repr(
# 170 "constraintParser.mly"
             ( fun p -> [ _1 p ] )
# 463 "constraintParser.ml"
               : 'let_envs))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'let_env) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'let_envs) in
    Obj.repr(
# 171 "constraintParser.mly"
                          ( fun p -> (_1 p) :: (_3 p) )
# 472 "constraintParser.ml"
               : 'let_envs))
; (fun parser_env ->
    Obj.repr(
# 175 "constraintParser.mly"
                  ( ([], []) )
# 478 "constraintParser.ml"
               : 'opt_env_vars))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : Positions.position) in
    let _2 = (peek_val parser_env 3 : Positions.position) in
    let _3 = (peek_val parser_env 2 : 'vars) in
    let _4 = (peek_val parser_env 1 : Positions.position) in
    let _5 = (peek_val parser_env 0 : 'opt_vars) in
    Obj.repr(
# 176 "constraintParser.mly"
                                       ( (snd (List.split _3), 
					     (snd (List.split _5))) )
# 490 "constraintParser.ml"
               : 'opt_env_vars))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Positions.position) in
    let _2 = (peek_val parser_env 0 : 'vars) in
    Obj.repr(
# 178 "constraintParser.mly"
                ( ([], (snd (List.split _2))) )
# 498 "constraintParser.ml"
               : 'opt_env_vars))
; (fun parser_env ->
    Obj.repr(
# 181 "constraintParser.mly"
                      ( [] )
# 504 "constraintParser.ml"
               : 'opt_vars))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'vars) in
    Obj.repr(
# 182 "constraintParser.mly"
       ( _1 )
# 511 "constraintParser.ml"
               : 'opt_vars))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'var) in
    Obj.repr(
# 185 "constraintParser.mly"
              ( [ _1 ] )
# 518 "constraintParser.ml"
               : 'vars))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'var) in
    let _2 = (peek_val parser_env 0 : 'vars) in
    Obj.repr(
# 186 "constraintParser.mly"
               ( _1 :: _2 )
# 526 "constraintParser.ml"
               : 'vars))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Positions.position * string) in
    Obj.repr(
# 189 "constraintParser.mly"
         ( _1 )
# 533 "constraintParser.ml"
               : 'var))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 190 "constraintParser.mly"
        ( (_1, "->") )
# 540 "constraintParser.ml"
               : 'var))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 191 "constraintParser.mly"
        ( (_1, "*") )
# 547 "constraintParser.ml"
               : 'var))
; (fun parser_env ->
    Obj.repr(
# 196 "constraintParser.mly"
  ( fun pool -> CTrue undefined_position )
# 553 "constraintParser.ml"
               : 'opt_constraint))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Positions.position) in
    let _2 = (peek_val parser_env 1 : 'constraint_exp) in
    let _3 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 197 "constraintParser.mly"
                                         ( _2 )
# 562 "constraintParser.ml"
               : 'opt_constraint))
; (fun parser_env ->
    Obj.repr(
# 201 "constraintParser.mly"
                  ( [] )
# 568 "constraintParser.ml"
               : 'opt_env_ids))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Positions.position) in
    let _2 = (peek_val parser_env 1 : 'env_ids) in
    let _3 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 202 "constraintParser.mly"
                           ( _2 )
# 577 "constraintParser.ml"
               : 'opt_env_ids))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Positions.position * string) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'typ) in
    Obj.repr(
# 205 "constraintParser.mly"
                         ( (snd _1, _3) )
# 586 "constraintParser.ml"
               : 'env_id))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'env_id) in
    Obj.repr(
# 209 "constraintParser.mly"
  ( [ _1 ] )
# 593 "constraintParser.ml"
               : 'env_ids))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'env_id) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'env_ids) in
    Obj.repr(
# 211 "constraintParser.mly"
      ( _1 :: _3 )
# 602 "constraintParser.ml"
               : 'env_ids))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'constraint_exp0) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'constraint_exp0) in
    Obj.repr(
# 214 "constraintParser.mly"
                                                 ( [ _1; _3  ] )
# 611 "constraintParser.ml"
               : 'conjunction))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'constraint_exp0) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'conjunction) in
    Obj.repr(
# 215 "constraintParser.mly"
                                  ( _1 :: _3 )
# 620 "constraintParser.ml"
               : 'conjunction))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'typ) in
    Obj.repr(
# 219 "constraintParser.mly"
               ( [], _1 )
# 627 "constraintParser.ml"
               : 'attributes))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'attribute) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'attributes) in
    Obj.repr(
# 220 "constraintParser.mly"
                                  ( _1 :: (fst _3), snd _3 )
# 636 "constraintParser.ml"
               : 'attributes))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Positions.position * string) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'typ) in
    Obj.repr(
# 224 "constraintParser.mly"
                        ( snd _1, _3 )
# 645 "constraintParser.ml"
               : 'attribute))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'type2) in
    Obj.repr(
# 228 "constraintParser.mly"
                                                    ( _1 )
# 652 "constraintParser.ml"
               : 'typ))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'type10) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'type2) in
    Obj.repr(
# 233 "constraintParser.mly"
  ( arrow_type (tjoin _1 _3) _1 _3  )
# 661 "constraintParser.ml"
               : 'type2))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'type10) in
    Obj.repr(
# 234 "constraintParser.mly"
                                                     ( _1 )
# 668 "constraintParser.ml"
               : 'type2))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'star_types) in
    Obj.repr(
# 239 "constraintParser.mly"
( match _1 with
    | [] -> assert false
    | [ a ] -> a
    | l -> tuple_type (tlposition _1) l
)
# 679 "constraintParser.ml"
               : 'type10))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'type1) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'star_types) in
    Obj.repr(
# 247 "constraintParser.mly"
                                                     ( _1 :: _3 )
# 688 "constraintParser.ml"
               : 'star_types))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'type1) in
    Obj.repr(
# 248 "constraintParser.mly"
                 ( [ _1 ] )
# 695 "constraintParser.ml"
               : 'star_types))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'type0) in
    Obj.repr(
# 252 "constraintParser.mly"
        ( _1 )
# 702 "constraintParser.ml"
               : 'type1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : Positions.position) in
    let _2 = (peek_val parser_env 0 : 'type0) in
    Obj.repr(
# 253 "constraintParser.mly"
                         ( TypRowUniform (_1, _2) )
# 710 "constraintParser.ml"
               : 'type1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'type00s) in
    Obj.repr(
# 258 "constraintParser.mly"
  ( 
    match _1 with
	[] -> assert false
      | [ t ] -> t
      | t :: q -> 
	  TypApp (join (tposition t)
		    (tlposition q),
		    t,
		    q) 
  )
# 726 "constraintParser.ml"
               : 'type0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : Positions.position * string) in
    Obj.repr(
# 271 "constraintParser.mly"
                                                    ( TypVar (fst _1, snd _1) )
# 733 "constraintParser.ml"
               : 'type00))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Positions.position) in
    let _2 = (peek_val parser_env 1 : 'attributes) in
    let _3 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 272 "constraintParser.mly"
                                 ( TypRowCons 
							(join _1 _3, 
							 fst _2, 
							 snd _2) 
						    )
# 746 "constraintParser.ml"
               : 'type00))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : Positions.position) in
    let _2 = (peek_val parser_env 1 : 'typ) in
    let _3 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 277 "constraintParser.mly"
                                                    ( _2 )
# 755 "constraintParser.ml"
               : 'type00))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : Positions.position) in
    let _2 = (peek_val parser_env 3 : 'typ) in
    let _3 = (peek_val parser_env 2 : Positions.position) in
    let _4 = (peek_val parser_env 1 : 'types) in
    let _5 = (peek_val parser_env 0 : Positions.position) in
    Obj.repr(
# 278 "constraintParser.mly"
                                                    ( tuple_type (join _1 _5)
							(_2 :: _4) )
# 767 "constraintParser.ml"
               : 'type00))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'type00) in
    Obj.repr(
# 285 "constraintParser.mly"
                      ( [ _1 ] )
# 774 "constraintParser.ml"
               : 'type00s))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'type00) in
    let _2 = (peek_val parser_env 0 : 'type00s) in
    Obj.repr(
# 286 "constraintParser.mly"
                       ( _1 :: _2 )
# 782 "constraintParser.ml"
               : 'type00s))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'typ) in
    Obj.repr(
# 290 "constraintParser.mly"
                                                    ( [ _1 ] )
# 789 "constraintParser.ml"
               : 'types))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'typ) in
    let _2 = (peek_val parser_env 1 : Positions.position) in
    let _3 = (peek_val parser_env 0 : 'types) in
    Obj.repr(
# 291 "constraintParser.mly"
                                                    ( _1 :: _3 )
# 798 "constraintParser.ml"
               : 'types))
(* Entry tconstraint *)
; (fun parser_env -> raise (YYexit (peek_val parser_env 0)))
|]
let yytables =
  { actions=yyact;
    transl_const=yytransl_const;
    transl_block=yytransl_block;
    lhs=yylhs;
    len=yylen;
    defred=yydefred;
    dgoto=yydgoto;
    sindex=yysindex;
    rindex=yyrindex;
    gindex=yygindex;
    tablesize=yytablesize;
    table=yytable;
    check=yycheck;
    error_function=parse_error;
    names_const=yynames_const;
    names_block=yynames_block }
let tconstraint (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : MiniTypingEnvironment.environment * MiniMultiEquation.pool-> MiniConstraint.tconstraint)
