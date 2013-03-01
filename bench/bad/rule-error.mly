%token A B C D

%%

main:
  A B         { () }
| A ( x = B C { () }

bar:
  A           { () }

baz:
  bar main    { () }

