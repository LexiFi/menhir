%token A B C D E

%type <unit> main
%start main

%%

main : 
  | expression_opt E { () }

expression_opt :
  | expression expression_opt    { () }
  | /* empty */                  { () }

expression :
  | A { () }
  | B { () }
  | C { () }
  | D { () }
