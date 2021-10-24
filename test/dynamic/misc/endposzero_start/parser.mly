%token B C
%start<int> main

%%

main: foo C
  { $1 }
  (* The reduction of the production [foo -> B] brings us back to
     the initial state, so the position that we obtain should be
     the initial position of the lexer, that is, 0. *)

foo: nothing B
  { $1 }
  (* Because [nothing] is inlined, this is equivalent to
     foo: B { $endofs($0) } *)

%inline nothing:
  { $startofs }
