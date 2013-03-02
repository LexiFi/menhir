%token A B C D E

%type <unit> main
%start main

%%

main : 
  | A     { () }
  | B     { () }
  | C     { () }
  | D     { () }
  | E     { () }

