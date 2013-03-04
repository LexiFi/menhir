%token A
%start <unit> infinite

%%

infinite:
  A infinite
    { () }

