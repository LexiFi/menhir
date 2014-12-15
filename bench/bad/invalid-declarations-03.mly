%token <int> FOO
%token BAZ QUUX HOP
%token BAR BAT
%token FOO QWD ASFLJ QWKJH QWDK
%start<int> RAT
%type (int) date time /* error */

%%

main:
  FOO BAR

