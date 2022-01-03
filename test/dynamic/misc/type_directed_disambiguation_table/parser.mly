%token FOO BAR EOL

%start<Syntax.ast> main

%%

main:
| FOO EOL { Foo }
| BAR EOL { Syntax.Bar }
