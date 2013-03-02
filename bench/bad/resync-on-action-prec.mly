%token A B C
%%
bof:
  erreur1: { action } %prec A
| erreur2: { action }

baz:
    quux {}

