%start a
%type <unit> a
%token A B C

%%

a:
  A? b C? | B
    {}

b:
  A* c d
    {}

c:
  a C*
    {}

d:
  B?
    {}
