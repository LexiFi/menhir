%location<MyLocation>

%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start <int> main

%%

main:
| e = expr EOL
    { e }

expr:
| i = INT
    { prerr_endline (MyLocation.trace $loc);i }
| LPAREN e = expr RPAREN
    { prerr_endline (MyLocation.trace $loc);e }
| e1 = expr PLUS e2 = expr
    { prerr_endline (MyLocation.trace $loc); e1 + e2 }
| e1 = expr MINUS e2 = expr
    { prerr_endline (MyLocation.trace $loc);e1 - e2 }
| e1 = expr TIMES e2 = expr
    { prerr_endline (MyLocation.trace $loc);e1 * e2 }
| e1 = expr DIV e2 = expr
    { prerr_endline (MyLocation.trace $loc);e1 / e2 }
| MINUS e = expr %prec UMINUS
    { prerr_endline (MyLocation.trace $loc);- e }

