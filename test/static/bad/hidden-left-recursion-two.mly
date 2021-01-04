%token A B C
%start<unit> a

%%

a:
  A* b C {}

b:
  B C
| A? a   {}
