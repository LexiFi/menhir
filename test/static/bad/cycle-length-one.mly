%start a
%type <unit> a
%token A B C

%%

a:
  B       {}
| A? a C? {}
