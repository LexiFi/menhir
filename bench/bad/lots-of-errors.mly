( idiot )
%token FOO BAR <int> /* the error is the type */
%type <int> INT
%token ( BAZ: QUUX , %start /* the error is the opening parenthesis */
%token HOP

%%

main: HOP BAZ QUUX { () }

