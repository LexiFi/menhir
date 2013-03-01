%token <int> FOO
%token BAZ QUUX HOP
%token BAR int BAT
%token FOO QWD ASFLJ QWKJH QWDK
%start RAT
%type (int) date time
%token BAZAR
%left FOO BAR
%right
%nonassoc BAR QWD QWD QWD ASD QWD D QWD WQD QWD 
%token BAR

%%

main:
  FOO BAR

