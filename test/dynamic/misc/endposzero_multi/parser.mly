%token B C
%start<int * int> main

%%

main: foo C foo C
  { $1, $3 }
  (* The positions that we expect to obtain are 0 and 2. *)

foo: nothing B
  { $1 }
  (* Because [nothing] is inlined, this is equivalent to
     foo: B { $endofs($0) } *)

%inline nothing:
  { $startofs }
