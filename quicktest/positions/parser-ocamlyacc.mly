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
| LPAREN nothing expr RPAREN
    {}
| expr PLUS optional_dot expr
    {}
| expr MINUS optional_dot expr
    {}
| expr TIMES optional_dot expr
    {}
| expr DIV optional_dot expr
    {}
| MINUS expr %prec UMINUS
    {}

expr:
  raw_expr
    { Aux.print "expr" (Parsing.symbol_start_pos()) (Parsing.symbol_end_pos()) }

