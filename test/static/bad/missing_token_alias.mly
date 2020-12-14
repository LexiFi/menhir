(* Token, and token aliases, are declared here: *)

%token<int> INT
%token PLUS       "+"
%token MINUS      "-"
%token TIMES      "*"
%token DIV        "/"
%token LPAREN     "("
%token RPAREN     ")"
%token EOL

(* Token aliases can be used throughout the rest of the grammar. E.g.,
   they can be used in precedence declarations: *)

%left "+" "-"       /* lowest precedence */
%left "*" "/"       /* medium precedence */
%nonassoc UMINUS    /* highest precedence */

%start <int> main

%%

main:
| e = expr EOL
    { e }

(* Token aliases can also be used inside rules: *)

expr:
| i = INT
    { i }
| "(" e = expr ")"
    { e }
| e1 = expr "+" e2 = expr
    { e1 + e2 }
| e1 = expr "-" e2 = expr
    { e1 - e2 }
| e1 = expr "*" e2 = expr
    { e1 * e2 }
| e1 = expr "/" e2 = expr
    { e1 / e2 }
| "-" e = expr %prec UMINUS
    { - e }
