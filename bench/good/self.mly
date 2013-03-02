%token A
%type<unit> s
%start s

%%

s: b s {}

b: {}

