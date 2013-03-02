%token bla
%token FOO
%start Bla

%%

prod:
  x == X /* intentional syntax error */
    { () }

