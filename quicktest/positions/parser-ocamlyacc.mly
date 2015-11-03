%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token DOT

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%type<unit> main
%start main

%%

main:
| nothing expr EOL
    {}

/* Added just to exercise productions with an empty right-hand side. */
nothing:
| /* nothing */
    { Aux.print "nothing" (Parsing.symbol_start_pos()) (Parsing.symbol_end_pos()) }

/* Added just to exercise productions with an empty right-hand side, in a choice. */
optional_dot:
| nothing
    { Aux.print "optional_dot" (Parsing.symbol_start_pos()) (Parsing.symbol_end_pos())}
| DOT
    { Aux.print "optional_dot" (Parsing.symbol_start_pos()) (Parsing.symbol_end_pos())}

raw_expr:
| INT
    {}
| optional_dot LPAREN nothing expr RPAREN optional_dot
    {}
| expr PLUS expr
    {}
| expr MINUS expr
    {}
| expr TIMES expr
    {}
| expr DIV expr
    {}
| MINUS expr %prec UMINUS
    {}

expr:
  raw_expr
    { Aux.print "expr" (Parsing.symbol_start_pos()) (Parsing.symbol_end_pos()) }

