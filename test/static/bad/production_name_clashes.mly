(* Tokens. *)

%token <int> INT
%token PLUS "+"
%token MINUS "-"
%token TIMES "*"
%token DIV "/"
%token LPAREN "("
%token RPAREN ")"
%token EOL

(* Precedence declarations. *)

(* [UNARY] is not a token; it is only a precedence level. *)

%left PLUS MINUS (*  lowest precedence *)
%left TIMES DIV  (*  medium precedence *)
%nonassoc UNARY  (* highest precedence *)

(* Type annotations and start symbols. *)

%type  <AST.expr> expr
%start <AST.main> main

(* OCaml prologue. *)

%{ open AST %}

%%

(* Now, the grammar itself. *)

(* We annotate each production with a [@name] attribute, because we
   need every production to have a unique name. The two productions
   that construct [EUnOp] and [EBinOp] expressions do not need a name,
   though, because they receive a name when the terminal symbols
   [unop] and [binop] are inlined away. *)

main:
| e = expr EOL
    { e }
    [@name unary_neg] (* name clash *)
| EOL
    { () }
    (* missing name *)

expr:
| i = INT
    { EConst i }
    [@name paren] (* name clash *)

| LPAREN
  e = expr
  RPAREN
    { e }
    [@name paren] (* name clash *)

| op = unop
  e = expr
    %prec UNARY
    { EUnOp (op, e) }
    [@name unary] (* name clash, due to inlining of [op] *)

| e1 = expr
  op = binop
  e2 = expr
    { EBinOp (e1, op, e2) }
    [@name binary]

%inline unop:
| MINUS { UNeg } [@name neg]

%inline binop:
| PLUS  { BAdd } [@name add]
| MINUS { BSub } [@name sub]
| TIMES { BMul } (* name clash: two productions will be named [binary] *)
| DIV   { BDiv } (* name clash: two productions will be named [binary] *)
