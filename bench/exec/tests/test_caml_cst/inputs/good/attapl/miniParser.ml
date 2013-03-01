type token =
  | LID of (string)
  | UID of (string)
  | INTEGER of (int)
  | LET
  | IN
  | EQUAL
  | BACKSLASH
  | DOT
  | LBRACE
  | RBRACE
  | LBRACKET
  | RBRACKET
  | LANGLE
  | RANGLE
  | TYPE
  | EXISTS
  | FORALL
  | ARROW
  | LPAREN
  | RPAREN
  | SEMI
  | COMMA
  | EOF
  | BANG
  | BAR
  | COLON
  | SLASH
  | DATA
  | MUTABLE
  | LEFTARROW
  | WILD
  | AS
  | REC
  | AND
  | MATCH
  | WITH
  | END
  | UNIT
  | STAR
  | INFIXOP0 of (string)
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | INFIXOP4 of (string)

open Parsing;;
# 4 "miniParser.mly"

open Sig
open MiniPrimitives

let app e1 e2 =
  EApp (e1, e2)

let infix id e1 e2 =
  app (app (EVar id) e1) e2

let seq e1 e2 =
  EBinding (BindValue [[], PTuple [], e1], e2)

# 63 "miniParser.ml"
let yytransl_const = [|
  260 (* LET *);
  261 (* IN *);
  262 (* EQUAL *);
  263 (* BACKSLASH *);
  264 (* DOT *);
  265 (* LBRACE *);
  266 (* RBRACE *);
  267 (* LBRACKET *);
  268 (* RBRACKET *);
  269 (* LANGLE *);
  270 (* RANGLE *);
  271 (* TYPE *);
  272 (* EXISTS *);
  273 (* FORALL *);
  274 (* ARROW *);
  275 (* LPAREN *);
  276 (* RPAREN *);
  277 (* SEMI *);
  278 (* COMMA *);
    0 (* EOF *);
  279 (* BANG *);
  280 (* BAR *);
  281 (* COLON *);
  282 (* SLASH *);
  283 (* DATA *);
  284 (* MUTABLE *);
  285 (* LEFTARROW *);
  286 (* WILD *);
  287 (* AS *);
  288 (* REC *);
  289 (* AND *);
  290 (* MATCH *);
  291 (* WITH *);
  292 (* END *);
  293 (* UNIT *);
  294 (* STAR *);
    0|]

let yytransl_block = [|
  257 (* LID *);
  258 (* UID *);
  259 (* INTEGER *);
  295 (* INFIXOP0 *);
  296 (* INFIXOP1 *);
  297 (* INFIXOP2 *);
  298 (* INFIXOP3 *);
  299 (* INFIXOP4 *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\004\000\004\000\006\000\006\000\007\000\005\000\
\008\000\008\000\009\000\009\000\010\000\010\000\010\000\010\000\
\011\000\011\000\012\000\013\000\013\000\013\000\013\000\013\000\
\016\000\016\000\019\000\019\000\019\000\019\000\019\000\019\000\
\019\000\018\000\018\000\017\000\017\000\017\000\017\000\017\000\
\017\000\017\000\017\000\020\000\020\000\002\000\002\000\015\000\
\015\000\021\000\021\000\022\000\024\000\024\000\024\000\025\000\
\025\000\014\000\026\000\026\000\027\000\027\000\028\000\028\000\
\023\000\023\000\023\000\023\000\023\000\023\000\030\000\030\000\
\029\000\029\000\031\000\031\000\000\000"

let yylen = "\002\000\
\002\000\002\000\000\000\003\000\001\000\002\000\001\000\001\000\
\003\000\001\000\003\000\001\000\001\000\001\000\003\000\005\000\
\001\000\003\000\001\000\004\000\003\000\004\000\003\000\001\000\
\005\000\001\000\003\000\003\000\003\000\003\000\003\000\003\000\
\001\000\002\000\001\000\001\000\001\000\003\000\003\000\002\000\
\003\000\005\000\005\000\001\000\003\000\000\000\002\000\002\000\
\003\000\001\000\003\000\003\000\002\000\004\000\002\000\001\000\
\003\000\001\000\001\000\003\000\001\000\003\000\001\000\002\000\
\001\000\001\000\001\000\002\000\003\000\005\000\001\000\003\000\
\000\000\002\000\000\000\001\000\002\000"

let yydefred = "\000\000\
\046\000\000\000\077\000\000\000\000\000\001\000\047\000\000\000\
\000\000\000\000\000\000\050\000\007\000\000\000\005\000\000\000\
\065\000\067\000\000\000\066\000\000\000\000\000\004\000\006\000\
\000\000\073\000\068\000\000\000\063\000\000\000\059\000\061\000\
\000\000\000\000\000\000\052\000\051\000\000\000\000\000\069\000\
\000\000\000\000\036\000\037\000\000\000\046\000\000\000\000\000\
\053\000\019\000\000\000\000\000\000\000\000\000\000\000\013\000\
\000\000\014\000\000\000\008\000\000\000\000\000\055\000\062\000\
\074\000\000\000\000\000\060\000\000\000\000\000\000\000\040\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\070\000\000\000\039\000\000\000\041\000\000\000\000\000\021\000\
\023\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\015\000\000\000\054\000\009\000\011\000\072\000\
\020\000\022\000\000\000\000\000\000\000\000\000\038\000\000\000\
\000\000\000\000\043\000\042\000\000\000\000\000\016\000\045\000\
\018\000"

let yydgoto = "\002\000\
\003\000\004\000\000\000\010\000\120\000\014\000\015\000\060\000\
\061\000\062\000\121\000\115\000\050\000\066\000\051\000\052\000\
\053\000\054\000\055\000\116\000\011\000\012\000\029\000\036\000\
\000\000\030\000\031\000\032\000\039\000\067\000\000\000"

let yysindex = "\011\000\
\000\000\000\000\000\000\004\000\248\254\000\000\000\000\013\255\
\006\255\052\255\005\255\000\000\000\000\012\255\000\000\005\255\
\000\000\000\000\016\255\000\000\053\255\006\255\000\000\000\000\
\019\255\000\000\000\000\070\255\000\000\020\255\000\000\000\000\
\125\255\015\255\053\255\000\000\000\000\046\255\052\255\000\000\
\046\255\015\255\000\000\000\000\046\255\000\000\013\255\084\255\
\000\000\000\000\072\255\036\255\056\255\130\255\001\255\000\000\
\015\255\000\000\083\255\000\000\091\255\063\255\000\000\000\000\
\000\000\093\255\098\255\000\000\119\255\029\255\014\255\000\000\
\059\255\125\255\125\255\129\255\127\255\130\255\130\255\130\255\
\130\255\130\255\130\255\090\255\125\255\015\255\015\255\046\255\
\000\000\125\255\000\000\125\255\000\000\125\255\015\255\000\000\
\000\000\108\255\137\255\127\255\124\255\124\255\124\255\018\255\
\102\255\102\255\000\000\015\255\000\000\000\000\000\000\000\000\
\000\000\000\000\120\255\123\255\126\255\130\255\000\000\133\255\
\131\255\125\255\000\000\000\000\130\255\015\255\000\000\000\000\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\067\255\000\000\000\000\000\000\
\067\255\000\000\008\000\000\000\000\000\000\000\000\000\073\000\
\000\000\000\000\000\000\000\000\000\000\067\255\000\000\000\000\
\128\255\000\000\000\000\000\000\000\000\094\255\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\132\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\253\000\026\000\075\000\222\000\000\000\
\000\000\000\000\000\000\000\000\100\255\099\255\000\000\000\000\
\000\000\136\255\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\051\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\001\000\000\000\026\000\169\000\191\000\200\000\147\000\
\099\000\123\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\148\255\000\000\000\000\000\000\000\000\150\255\
\000\000\000\000\000\000\000\000\231\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\125\000\000\000\000\000\224\255\126\000\247\255\086\000\
\087\000\000\000\049\000\229\255\021\000\248\255\252\255\000\000\
\205\255\058\000\080\000\055\000\170\000\156\000\059\000\145\000\
\000\000\000\000\143\000\000\000\000\000\094\000\000\000"

let yytablesize = 542
let yytable = "\007\000\
\038\000\059\000\077\000\006\000\024\000\049\000\078\000\048\000\
\008\000\068\000\028\000\001\000\013\000\013\000\013\000\056\000\
\025\000\026\000\018\000\023\000\073\000\092\000\008\000\009\000\
\084\000\035\000\100\000\100\000\100\000\100\000\100\000\100\000\
\005\000\057\000\019\000\027\000\069\000\022\000\091\000\079\000\
\080\000\081\000\082\000\083\000\042\000\020\000\025\000\026\000\
\018\000\038\000\034\000\058\000\017\000\017\000\018\000\018\000\
\075\000\109\000\033\000\082\000\083\000\024\000\117\000\076\000\
\019\000\007\000\100\000\003\000\021\000\003\000\019\000\019\000\
\049\000\077\000\033\000\020\000\074\000\034\000\093\000\035\000\
\094\000\020\000\020\000\095\000\043\000\003\000\044\000\005\000\
\085\000\040\000\045\000\041\000\046\000\035\000\096\000\097\000\
\003\000\065\000\030\000\047\000\087\000\058\000\048\000\072\000\
\012\000\010\000\012\000\010\000\086\000\107\000\113\000\108\000\
\114\000\058\000\088\000\058\000\012\000\089\000\012\000\010\000\
\012\000\010\000\031\000\012\000\010\000\043\000\090\000\044\000\
\005\000\098\000\043\000\045\000\044\000\046\000\099\000\065\000\
\118\000\119\000\046\000\064\000\047\000\122\000\123\000\048\000\
\083\000\124\000\029\000\065\000\048\000\065\000\127\000\064\000\
\065\000\064\000\126\000\071\000\064\000\101\000\102\000\103\000\
\104\000\105\000\106\000\080\000\081\000\082\000\083\000\044\000\
\032\000\017\000\070\000\110\000\071\000\111\000\129\000\125\000\
\128\000\037\000\016\000\063\000\064\000\112\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\027\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\028\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\026\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\025\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\024\000\000\000\000\000\000\000\
\000\000\038\000\000\000\038\000\038\000\038\000\038\000\005\000\
\038\000\038\000\038\000\048\000\048\000\000\000\000\000\000\000\
\000\000\048\000\000\000\038\000\038\000\038\000\038\000\000\000\
\000\000\038\000\035\000\000\000\035\000\035\000\035\000\035\000\
\000\000\038\000\035\000\035\000\000\000\000\000\000\000\038\000\
\038\000\038\000\038\000\038\000\035\000\035\000\035\000\035\000\
\000\000\000\000\035\000\034\000\000\000\034\000\034\000\034\000\
\034\000\000\000\035\000\034\000\034\000\000\000\000\000\000\000\
\035\000\035\000\035\000\035\000\035\000\034\000\034\000\034\000\
\034\000\000\000\000\000\034\000\049\000\049\000\033\000\033\000\
\033\000\000\000\049\000\034\000\033\000\000\000\000\000\000\000\
\000\000\034\000\034\000\034\000\034\000\034\000\033\000\033\000\
\033\000\000\000\000\000\033\000\000\000\000\000\030\000\030\000\
\030\000\000\000\000\000\033\000\030\000\000\000\000\000\000\000\
\000\000\033\000\033\000\033\000\033\000\033\000\030\000\030\000\
\030\000\000\000\000\000\030\000\000\000\000\000\031\000\031\000\
\031\000\000\000\000\000\030\000\031\000\000\000\000\000\000\000\
\000\000\030\000\030\000\030\000\030\000\000\000\031\000\031\000\
\031\000\000\000\000\000\031\000\000\000\000\000\029\000\029\000\
\029\000\000\000\000\000\031\000\029\000\000\000\000\000\000\000\
\000\000\031\000\031\000\031\000\031\000\000\000\029\000\029\000\
\029\000\000\000\000\000\029\000\032\000\032\000\032\000\000\000\
\000\000\000\000\032\000\029\000\000\000\000\000\000\000\000\000\
\000\000\029\000\029\000\029\000\032\000\032\000\032\000\000\000\
\000\000\032\000\027\000\027\000\027\000\000\000\000\000\000\000\
\027\000\032\000\000\000\028\000\028\000\028\000\000\000\032\000\
\000\000\028\000\027\000\027\000\027\000\000\000\000\000\027\000\
\000\000\000\000\000\000\028\000\028\000\028\000\000\000\027\000\
\028\000\026\000\026\000\000\000\000\000\027\000\000\000\026\000\
\028\000\000\000\025\000\025\000\000\000\000\000\028\000\000\000\
\025\000\026\000\026\000\026\000\000\000\000\000\026\000\000\000\
\000\000\000\000\025\000\025\000\025\000\000\000\026\000\025\000\
\024\000\024\000\000\000\000\000\000\000\000\000\024\000\025\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\024\000\000\000\024\000\000\000\000\000\024\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\024\000"

let yycheck = "\004\000\
\000\000\034\000\054\000\000\000\014\000\033\000\006\001\000\000\
\017\001\042\000\019\000\001\000\001\001\001\001\001\001\001\001\
\001\001\002\001\003\001\008\001\048\000\008\001\017\001\032\001\
\057\000\000\000\078\000\079\000\080\000\081\000\082\000\083\000\
\004\001\019\001\019\001\020\001\045\000\033\001\010\001\039\001\
\040\001\041\001\042\001\043\001\025\001\030\001\001\001\002\001\
\003\001\031\001\000\000\037\001\001\001\001\001\003\001\003\001\
\021\001\085\000\006\001\042\001\043\001\071\000\095\000\008\001\
\019\001\070\000\118\000\001\001\010\000\003\001\019\001\019\001\
\000\000\125\000\000\000\030\001\005\001\025\001\020\001\021\000\
\022\001\030\001\030\001\025\001\001\001\019\001\003\001\004\001\
\006\001\020\001\007\001\022\001\009\001\035\000\074\000\075\000\
\030\001\039\000\000\000\016\001\038\001\008\001\019\001\020\001\
\006\001\006\001\008\001\008\001\018\001\020\001\090\000\022\001\
\092\000\020\001\022\001\022\001\018\001\020\001\020\001\020\001\
\022\001\022\001\000\000\025\001\025\001\001\001\008\001\003\001\
\004\001\001\001\001\001\007\001\003\001\009\001\008\001\008\001\
\029\001\001\001\009\001\008\001\016\001\022\001\020\001\019\001\
\043\001\020\001\000\000\020\001\019\001\022\001\020\001\020\001\
\025\001\022\001\022\001\020\001\025\001\078\000\079\000\080\000\
\081\000\082\000\083\000\040\001\041\001\042\001\043\001\020\001\
\000\000\020\001\046\000\086\000\047\000\087\000\126\000\118\000\
\122\000\022\000\009\000\035\000\038\000\088\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\000\000\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\001\001\255\255\003\001\004\001\005\001\006\001\004\001\
\008\001\009\001\010\001\004\001\005\001\255\255\255\255\255\255\
\255\255\010\001\255\255\019\001\020\001\021\001\022\001\255\255\
\255\255\025\001\001\001\255\255\003\001\004\001\005\001\006\001\
\255\255\033\001\009\001\010\001\255\255\255\255\255\255\039\001\
\040\001\041\001\042\001\043\001\019\001\020\001\021\001\022\001\
\255\255\255\255\025\001\001\001\255\255\003\001\004\001\005\001\
\006\001\255\255\033\001\009\001\010\001\255\255\255\255\255\255\
\039\001\040\001\041\001\042\001\043\001\019\001\020\001\021\001\
\022\001\255\255\255\255\025\001\004\001\005\001\004\001\005\001\
\006\001\255\255\010\001\033\001\010\001\255\255\255\255\255\255\
\255\255\039\001\040\001\041\001\042\001\043\001\020\001\021\001\
\022\001\255\255\255\255\025\001\255\255\255\255\004\001\005\001\
\006\001\255\255\255\255\033\001\010\001\255\255\255\255\255\255\
\255\255\039\001\040\001\041\001\042\001\043\001\020\001\021\001\
\022\001\255\255\255\255\025\001\255\255\255\255\004\001\005\001\
\006\001\255\255\255\255\033\001\010\001\255\255\255\255\255\255\
\255\255\039\001\040\001\041\001\042\001\255\255\020\001\021\001\
\022\001\255\255\255\255\025\001\255\255\255\255\004\001\005\001\
\006\001\255\255\255\255\033\001\010\001\255\255\255\255\255\255\
\255\255\039\001\040\001\041\001\042\001\255\255\020\001\021\001\
\022\001\255\255\255\255\025\001\004\001\005\001\006\001\255\255\
\255\255\255\255\010\001\033\001\255\255\255\255\255\255\255\255\
\255\255\039\001\040\001\041\001\020\001\021\001\022\001\255\255\
\255\255\025\001\004\001\005\001\006\001\255\255\255\255\255\255\
\010\001\033\001\255\255\004\001\005\001\006\001\255\255\039\001\
\255\255\010\001\020\001\021\001\022\001\255\255\255\255\025\001\
\255\255\255\255\255\255\020\001\021\001\022\001\255\255\033\001\
\025\001\004\001\005\001\255\255\255\255\039\001\255\255\010\001\
\033\001\255\255\004\001\005\001\255\255\255\255\039\001\255\255\
\010\001\020\001\021\001\022\001\255\255\255\255\025\001\255\255\
\255\255\255\255\020\001\021\001\022\001\255\255\033\001\025\001\
\004\001\005\001\255\255\255\255\255\255\255\255\010\001\033\001\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\020\001\255\255\022\001\255\255\255\255\025\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\033\001"

let yynames_const = "\
  LET\000\
  IN\000\
  EQUAL\000\
  BACKSLASH\000\
  DOT\000\
  LBRACE\000\
  RBRACE\000\
  LBRACKET\000\
  RBRACKET\000\
  LANGLE\000\
  RANGLE\000\
  TYPE\000\
  EXISTS\000\
  FORALL\000\
  ARROW\000\
  LPAREN\000\
  RPAREN\000\
  SEMI\000\
  COMMA\000\
  EOF\000\
  BANG\000\
  BAR\000\
  COLON\000\
  SLASH\000\
  DATA\000\
  MUTABLE\000\
  LEFTARROW\000\
  WILD\000\
  AS\000\
  REC\000\
  AND\000\
  MATCH\000\
  WITH\000\
  END\000\
  UNIT\000\
  STAR\000\
  "

let yynames_block = "\
  LID\000\
  UID\000\
  INTEGER\000\
  INFIXOP0\000\
  INFIXOP1\000\
  INFIXOP2\000\
  INFIXOP3\000\
  INFIXOP4\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'bindings) in
    Obj.repr(
# 78 "miniParser.mly"
                                                                ( List.rev _1 )
# 405 "miniParser.ml"
               : MiniPrimitives.t Sig.binding list))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'forall) in
    let _2 = (peek_val parser_env 0 : 'typ) in
    Obj.repr(
# 82 "miniParser.mly"
                                                                ( _1, _2 )
# 413 "miniParser.ml"
               : 'scheme))
; (fun parser_env ->
    Obj.repr(
# 86 "miniParser.mly"
                                                                ( [] )
# 419 "miniParser.ml"
               : 'forall))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'quantifiers) in
    Obj.repr(
# 87 "miniParser.mly"
                                                                ( _2 )
# 426 "miniParser.ml"
               : 'forall))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'quantifier) in
    Obj.repr(
# 91 "miniParser.mly"
                                                                ( [ _1 ] )
# 433 "miniParser.ml"
               : 'quantifiers))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'quantifiers) in
    let _2 = (peek_val parser_env 0 : 'quantifier) in
    Obj.repr(
# 92 "miniParser.mly"
                                                                ( _2 :: _1 )
# 441 "miniParser.ml"
               : 'quantifiers))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 96 "miniParser.mly"
                                                                ( _1 )
# 448 "miniParser.ml"
               : 'quantifier))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'type2) in
    Obj.repr(
# 100 "miniParser.mly"
                                                                ( _1 )
# 455 "miniParser.ml"
               : 'typ))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'type1) in
    let _3 = (peek_val parser_env 0 : 'type2) in
    Obj.repr(
# 104 "miniParser.mly"
                                                                ( TypArrow (_1, _3) )
# 463 "miniParser.ml"
               : 'type2))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'type1) in
    Obj.repr(
# 105 "miniParser.mly"
                                                                ( _1 )
# 470 "miniParser.ml"
               : 'type2))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'type0) in
    let _3 = (peek_val parser_env 0 : 'type1) in
    Obj.repr(
# 109 "miniParser.mly"
                                                                ( match _3 with
                                                                  | TypTuple typs -> TypTuple (_1 :: typs)
								  | _ -> TypTuple [ _1; _3 ] )
# 480 "miniParser.ml"
               : 'type1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'type0) in
    Obj.repr(
# 112 "miniParser.mly"
                                                                ( _1 )
# 487 "miniParser.ml"
               : 'type1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 116 "miniParser.mly"
                                                                ( TypVar _1 )
# 494 "miniParser.ml"
               : 'type0))
; (fun parser_env ->
    Obj.repr(
# 117 "miniParser.mly"
                                                                ( TypTuple [] )
# 500 "miniParser.ml"
               : 'type0))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'typ) in
    Obj.repr(
# 118 "miniParser.mly"
                                                                ( _2 )
# 507 "miniParser.ml"
               : 'type0))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'typ) in
    let _4 = (peek_val parser_env 1 : 'types) in
    Obj.repr(
# 119 "miniParser.mly"
                                                                ( TypTuple (_2 :: _4) )
# 515 "miniParser.ml"
               : 'type0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'typ) in
    Obj.repr(
# 125 "miniParser.mly"
                                                                ( [ _1 ] )
# 522 "miniParser.ml"
               : 'types))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'typ) in
    let _3 = (peek_val parser_env 0 : 'types) in
    Obj.repr(
# 126 "miniParser.mly"
                                                                ( _1 :: _3 )
# 530 "miniParser.ml"
               : 'types))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'expression400) in
    Obj.repr(
# 130 "miniParser.mly"
                                                                ( _1 )
# 537 "miniParser.ml"
               : 'expression))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'pattern) in
    let _4 = (peek_val parser_env 0 : 'expression400) in
    Obj.repr(
# 134 "miniParser.mly"
                                                                ( ELambda (_2, _4) )
# 545 "miniParser.ml"
               : 'expression400))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'binding) in
    let _3 = (peek_val parser_env 0 : 'expression400) in
    Obj.repr(
# 135 "miniParser.mly"
                                                                ( EBinding (_1, _3) )
# 553 "miniParser.ml"
               : 'expression400))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'quantifiers) in
    let _4 = (peek_val parser_env 0 : 'expression400) in
    Obj.repr(
# 136 "miniParser.mly"
                                                                ( EExists (_2, _4) )
# 561 "miniParser.ml"
               : 'expression400))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expression300) in
    let _3 = (peek_val parser_env 0 : 'expression400) in
    Obj.repr(
# 140 "miniParser.mly"
                                                                ( seq _1 _3 )
# 569 "miniParser.ml"
               : 'expression400))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'expression300) in
    Obj.repr(
# 141 "miniParser.mly"
                                                                ( _1 )
# 576 "miniParser.ml"
               : 'expression400))
; (fun parser_env ->
    let _1 = (peek_val parser_env 4 : 'expression0) in
    let _3 = (peek_val parser_env 2 : string) in
    let _5 = (peek_val parser_env 0 : 'expression100) in
    Obj.repr(
# 145 "miniParser.mly"
                                                                ( ERecordUpdate (_1, _3, _5) )
# 585 "miniParser.ml"
               : 'expression300))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'expression200) in
    Obj.repr(
# 146 "miniParser.mly"
                                                                ( _1 )
# 592 "miniParser.ml"
               : 'expression300))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expression200) in
    let _2 = (peek_val parser_env 1 : string) in
    let _3 = (peek_val parser_env 0 : 'expression200) in
    Obj.repr(
# 150 "miniParser.mly"
                                                                ( infix _2 _1 _3 )
# 601 "miniParser.ml"
               : 'expression200))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expression200) in
    let _2 = (peek_val parser_env 1 : string) in
    let _3 = (peek_val parser_env 0 : 'expression200) in
    Obj.repr(
# 151 "miniParser.mly"
                                                                ( infix _2 _1 _3 )
# 610 "miniParser.ml"
               : 'expression200))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expression200) in
    let _2 = (peek_val parser_env 1 : string) in
    let _3 = (peek_val parser_env 0 : 'expression200) in
    Obj.repr(
# 152 "miniParser.mly"
                                                                ( infix _2 _1 _3 )
# 619 "miniParser.ml"
               : 'expression200))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expression200) in
    let _2 = (peek_val parser_env 1 : string) in
    let _3 = (peek_val parser_env 0 : 'expression200) in
    Obj.repr(
# 153 "miniParser.mly"
                                                                ( infix _2 _1 _3 )
# 628 "miniParser.ml"
               : 'expression200))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expression200) in
    let _2 = (peek_val parser_env 1 : string) in
    let _3 = (peek_val parser_env 0 : 'expression200) in
    Obj.repr(
# 154 "miniParser.mly"
                                                                ( infix _2 _1 _3 )
# 637 "miniParser.ml"
               : 'expression200))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expression200) in
    let _3 = (peek_val parser_env 0 : 'expression200) in
    Obj.repr(
# 155 "miniParser.mly"
                                                                ( infix "=" _1 _3 )
# 645 "miniParser.ml"
               : 'expression200))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'expression100) in
    Obj.repr(
# 156 "miniParser.mly"
                                                                ( _1 )
# 652 "miniParser.ml"
               : 'expression200))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'expression100) in
    let _2 = (peek_val parser_env 0 : 'expression0) in
    Obj.repr(
# 160 "miniParser.mly"
                                                                ( EApp (_1, _2) )
# 660 "miniParser.ml"
               : 'expression100))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'expression0) in
    Obj.repr(
# 161 "miniParser.mly"
                                                                ( _1 )
# 667 "miniParser.ml"
               : 'expression100))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 165 "miniParser.mly"
                                                                ( EVar _1 )
# 674 "miniParser.ml"
               : 'expression0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : int) in
    Obj.repr(
# 166 "miniParser.mly"
                                                                ( EPrimApp (PIntegerConstant _1, []) )
# 681 "miniParser.ml"
               : 'expression0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expression0) in
    let _3 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 167 "miniParser.mly"
                                                                ( ERecordAccess (_1, _3) )
# 689 "miniParser.ml"
               : 'expression0))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'bindings) in
    Obj.repr(
# 168 "miniParser.mly"
                                                                ( ERecordExtend (List.rev _2, ERecordEmpty) )
# 696 "miniParser.ml"
               : 'expression0))
; (fun parser_env ->
    Obj.repr(
# 169 "miniParser.mly"
                                                                ( ETuple [] )
# 702 "miniParser.ml"
               : 'expression0))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'expression) in
    Obj.repr(
# 170 "miniParser.mly"
                                                                ( _2 )
# 709 "miniParser.ml"
               : 'expression0))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'expression) in
    let _4 = (peek_val parser_env 1 : 'typ) in
    Obj.repr(
# 171 "miniParser.mly"
                                                                ( ETypeConstraint (_2, _4) )
# 717 "miniParser.ml"
               : 'expression0))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'expression) in
    let _4 = (peek_val parser_env 1 : 'expressions) in
    Obj.repr(
# 172 "miniParser.mly"
                                                                ( ETuple (_2 :: _4) )
# 725 "miniParser.ml"
               : 'expression0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'expression) in
    Obj.repr(
# 176 "miniParser.mly"
                                                                ( [ _1 ] )
# 732 "miniParser.ml"
               : 'expressions))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'expression) in
    let _3 = (peek_val parser_env 0 : 'expressions) in
    Obj.repr(
# 177 "miniParser.mly"
                                                                ( _1 :: _3 )
# 740 "miniParser.ml"
               : 'expressions))
; (fun parser_env ->
    Obj.repr(
# 181 "miniParser.mly"
                                                                ( [] )
# 746 "miniParser.ml"
               : 'bindings))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'bindings) in
    let _2 = (peek_val parser_env 0 : 'binding) in
    Obj.repr(
# 182 "miniParser.mly"
                                                                ( _2 :: _1 )
# 754 "miniParser.ml"
               : 'bindings))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'value_definitions) in
    Obj.repr(
# 186 "miniParser.mly"
                                                                ( BindValue _2 )
# 761 "miniParser.ml"
               : 'binding))
; (fun parser_env ->
    let _3 = (peek_val parser_env 0 : 'value_definitions) in
    Obj.repr(
# 187 "miniParser.mly"
                                                                ( BindRecValue _3 )
# 768 "miniParser.ml"
               : 'binding))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'value_definition) in
    Obj.repr(
# 191 "miniParser.mly"
                                                                ( [ _1 ] )
# 775 "miniParser.ml"
               : 'value_definitions))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'value_definitions) in
    let _3 = (peek_val parser_env 0 : 'value_definition) in
    Obj.repr(
# 192 "miniParser.mly"
                                                                ( _3 :: _1 )
# 783 "miniParser.ml"
               : 'value_definitions))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'forall) in
    let _2 = (peek_val parser_env 1 : 'pattern0) in
    let _3 = (peek_val parser_env 0 : 'equal_expression) in
    Obj.repr(
# 196 "miniParser.mly"
                                                                ( (_1, _2, _3) )
# 792 "miniParser.ml"
               : 'value_definition))
; (fun parser_env ->
    let _2 = (peek_val parser_env 0 : 'expression) in
    Obj.repr(
# 200 "miniParser.mly"
                                                                ( _2 )
# 799 "miniParser.ml"
               : 'equal_expression))
; (fun parser_env ->
    let _2 = (peek_val parser_env 2 : 'typ) in
    let _4 = (peek_val parser_env 0 : 'expression) in
    Obj.repr(
# 201 "miniParser.mly"
                                                                ( ETypeConstraint (_4, _2) )
# 807 "miniParser.ml"
               : 'equal_expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'pattern0) in
    let _2 = (peek_val parser_env 0 : 'equal_expression) in
    Obj.repr(
# 202 "miniParser.mly"
                                                                ( ELambda (_1, _2) )
# 815 "miniParser.ml"
               : 'equal_expression))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 206 "miniParser.mly"
                                                                ( [ _1 ] )
# 822 "miniParser.ml"
               : 'record_fields))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'record_fields) in
    let _3 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 207 "miniParser.mly"
                                                                ( _3 :: _1 )
# 830 "miniParser.ml"
               : 'record_fields))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'pattern3) in
    Obj.repr(
# 211 "miniParser.mly"
                                                                ( _1 )
# 837 "miniParser.ml"
               : 'pattern))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'pattern2) in
    Obj.repr(
# 215 "miniParser.mly"
                                                                ( _1 )
# 844 "miniParser.ml"
               : 'pattern3))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'pattern3) in
    let _3 = (peek_val parser_env 0 : 'typ) in
    Obj.repr(
# 216 "miniParser.mly"
                                                                ( PTypeConstraint (_1, _3) )
# 852 "miniParser.ml"
               : 'pattern3))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'pattern1) in
    Obj.repr(
# 220 "miniParser.mly"
                                                                ( _1 )
# 859 "miniParser.ml"
               : 'pattern2))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : string) in
    let _3 = (peek_val parser_env 0 : 'pattern2) in
    Obj.repr(
# 221 "miniParser.mly"
                                                                ( PAlias (_1, _3) )
# 867 "miniParser.ml"
               : 'pattern2))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'pattern0) in
    Obj.repr(
# 225 "miniParser.mly"
                                                                ( _1 )
# 874 "miniParser.ml"
               : 'pattern1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : string) in
    let _2 = (peek_val parser_env 0 : 'pattern0s) in
    Obj.repr(
# 226 "miniParser.mly"
                                                                ( PData (_1, List.rev _2) )
# 882 "miniParser.ml"
               : 'pattern1))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : string) in
    Obj.repr(
# 230 "miniParser.mly"
                                                                ( PVar _1 )
# 889 "miniParser.ml"
               : 'pattern0))
; (fun parser_env ->
    Obj.repr(
# 231 "miniParser.mly"
                                                                ( PWildcard )
# 895 "miniParser.ml"
               : 'pattern0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : int) in
    Obj.repr(
# 232 "miniParser.mly"
                                                                ( assert false (* TEMPORARY *) )
# 902 "miniParser.ml"
               : 'pattern0))
; (fun parser_env ->
    Obj.repr(
# 233 "miniParser.mly"
                                                                ( PTuple [] )
# 908 "miniParser.ml"
               : 'pattern0))
; (fun parser_env ->
    let _2 = (peek_val parser_env 1 : 'pattern) in
    Obj.repr(
# 234 "miniParser.mly"
                                                                ( _2 )
# 915 "miniParser.ml"
               : 'pattern0))
; (fun parser_env ->
    let _2 = (peek_val parser_env 3 : 'pattern) in
    let _4 = (peek_val parser_env 1 : 'patterns) in
    Obj.repr(
# 235 "miniParser.mly"
                                                                ( PTuple (_2 :: _4) )
# 923 "miniParser.ml"
               : 'pattern0))
; (fun parser_env ->
    let _1 = (peek_val parser_env 0 : 'pattern) in
    Obj.repr(
# 239 "miniParser.mly"
                                                                ( [ _1 ] )
# 930 "miniParser.ml"
               : 'patterns))
; (fun parser_env ->
    let _1 = (peek_val parser_env 2 : 'pattern) in
    let _3 = (peek_val parser_env 0 : 'patterns) in
    Obj.repr(
# 240 "miniParser.mly"
                                                                ( _1 :: _3 )
# 938 "miniParser.ml"
               : 'patterns))
; (fun parser_env ->
    Obj.repr(
# 244 "miniParser.mly"
                                                                ( [] )
# 944 "miniParser.ml"
               : 'pattern0s))
; (fun parser_env ->
    let _1 = (peek_val parser_env 1 : 'pattern0s) in
    let _2 = (peek_val parser_env 0 : 'pattern0) in
    Obj.repr(
# 245 "miniParser.mly"
                                                                ( _2 :: _1 )
# 952 "miniParser.ml"
               : 'pattern0s))
; (fun parser_env ->
    Obj.repr(
# 249 "miniParser.mly"
                                                                ( () )
# 958 "miniParser.ml"
               : 'opt_bar))
; (fun parser_env ->
    Obj.repr(
# 250 "miniParser.mly"
                                                                ( () )
# 964 "miniParser.ml"
               : 'opt_bar))
(* Entry program *)
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
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (yyparse yytables 1 lexfun lexbuf : MiniPrimitives.t Sig.binding list)
