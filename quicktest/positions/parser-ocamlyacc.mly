%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token DOT COMMA

%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%type<unit> main
%start main

/* For now, we do not use [Parsing.symbol_start_pos()] because it performs
   a computation that Menhir does not (yet) emulate. In order to get the
   "real" startpos, the one that is stored in the stack, we use
   [Parsing.symbol_start_pos()] for epsilon productions and
   [Parsing.rhs_start_pos 1] for non-epsilon productions. */

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
    { Aux.print "optional_dot" (Parsing.rhs_start_pos 1) (Parsing.symbol_end_pos())}

optional_comma:
| nothing
    { Aux.print "optional_comma" (Parsing.symbol_start_pos()) (Parsing.symbol_end_pos())}
| COMMA
    { Aux.print "optional_comma" (Parsing.rhs_start_pos 1) (Parsing.symbol_end_pos())}

annotations:
  optional_dot optional_comma
    { Aux.print "annotations" (Parsing.rhs_start_pos 1) (Parsing.symbol_end_pos())}

raw_expr:
| INT
    {}
| annotations LPAREN nothing expr RPAREN optional_dot
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
    { Aux.print "expr" (Parsing.rhs_start_pos 1) (Parsing.symbol_end_pos()) }

