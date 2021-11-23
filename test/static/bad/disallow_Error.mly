%token A
%token B
%token Error
%start<unit> main

%%

main: A B Error {}
